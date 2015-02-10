{-# LANGUAGE OverloadedStrings #-}

module PaperTitle.IO
       ( getTitleParts )
       where

import Parse.Blacklist
import Parse.NormalizeNoun

import PaperTitle.Gen
import Web.WordsApi
import qualified Web.Twitter as Twitter

import Data.Maybe
import Data.List
    
import Control.Applicative
import Control.Monad.Loops
    
import Test.QuickCheck (generate, elements)

-- SETTINGS

prepositionsFile, hashtagsFile, seedNounsFile :: FilePath
prepositionsFile = "resources/prepositions.txt"
hashtagsFile     = "resources/hashtags.txt"
seedNounsFile    = "resources/seedNouns.txt"


-- ASSEMBLING TitleParts

getTitleParts :: IO (Maybe TitleParts)
getTitleParts = do
    maybeTitleParts <- getTitleParts' 0
    maybe (return ()) (logSeedNoun . seedNoun) maybeTitleParts
    return maybeTitleParts


-- an auxiliary function for getTitleParts. The Int argument is used to
-- enforce an upper limit on the amount of attempts made.

getTitleParts' :: Int -> IO (Maybe TitleParts)
getTitleParts' n
    | n > permittedAtempts = return Nothing
    | otherwise = do
          result <- tryGetTitleParts
          either (genFailRedo n) (return . Just) result
    where
        permittedAtempts  = 100
        genFailRedo n err = do logFailure n err
                               getTitleParts' (n + 1)

{--
tryGetTitleParts sequences the various IO operations needed to gather
the parts of the TitleParts data type.

At some point, I would like to clean this up, as I'm sure there's a
better way to handle the error detection/control flow.
--}

tryGetTitleParts :: IO (Either FailedGen TitleParts)
tryGetTitleParts = do
    eitherSeedNoun <- getSeedNoun
    case eitherSeedNoun of
        Left  err      -> return (Left err)
        Right seedNoun ->
            do
            eitherDefs <- viableDefinitions seedNoun
            case eitherDefs of
                Left  err  -> return (Left err)
                Right defs ->
                    do
                    typeVariant      <- randTypeVariant defs
                    randPrep         <- randPreposition
                    eitherComplement <- getComplement randPrep typeVariant
                    case eitherComplement of
                        Left  err        -> (return . Left) err
                        Right complement -> 
                            (return . Right) TitleParts { seedNoun    = seedNoun
                                                        , typeVariant = typeVariant
                                                        , randPrep    = randPrep
                                                        , definition  = def (head defs)
                                                        , complement  = complement
                                                        }


-- GATHERING THE PARTS OF TitleParts
                        
getSeedNoun :: IO (Either FailedGen String)
getSeedNoun =
    do
        results   <- Twitter.search =<< randHashtag
        maybeNoun <- firstM viableNoun results
        return $ maybe failed succeeded maybeNoun
    where
        viableNoun n = (&&) <$> (queryIsNoun n) <*> (freshSeedNoun n)
        failed    = Left (genFail "seedNoun" "none")
        succeeded = Right

freshSeedNoun :: String -> IO Bool
freshSeedNoun str = (fmap (not . elem str) seedNouns)

viableDefinitions :: String -> IO (Either FailedGen [Definition])
viableDefinitions seedNoun =
    do
        results <- wapiEntryFor seedNoun
        case results of
            Nothing   -> return $ Left (genFail "viableDefinitions" "Nothing")
            Just defs -> return $ nounsWithTypes defs
    where
        nounsWithTypes defs = let d1:nouns = filter isNoun defs
                              in case filter withTypes nouns of
                                     []        -> Left  (genFail "viableDefinitions" defs)
                                     otherwise -> Right (d1:nouns)


-- Returns random a typeVariant from the tail of a list of definitions
randTypeVariant :: [Definition] -> IO String
randTypeVariant (_:defs) = randItem . concat . catMaybes $ defTypes
    where
        defTypes = map hasTypes defs ++ map typeOf defs
       
getComplement :: String -> String -> IO (Either FailedGen String)
getComplement typeVariant randPrep  =
    do
        result       <- searchTwitter searchTerm
        complement   <- findCompliment (words <$> listAfterInfix searchTerm result)
        return $ if allSafeWords complement
                    then Right ((unwords . fromJust) complement)
                    else Left  (genFail "viablecomplement" (show complement))
    where
        searchTerm     = typeVariant ++ " " ++ randPrep
        searchTwitter  = (fmap unwords . Twitter.search . quoteStr)
        findCompliment = maybe (return Nothing) (takeUptoIO' queryIsNoun)
        allSafeWords   = maybe False safeWords
        
safeWords :: [String] -> Bool
safeWords = (not . any (flip elem blacklist))
            
listAfterInfix :: Eq a => [a] -> [a] -> Maybe [a]
listAfterInfix ps = fromMaybe Nothing . find isJust . map (stripPrefix  ps) . tails

quoteStr :: String -> String
quoteStr s = "\"" ++ s ++ "\""


maybeIs :: (a -> Bool) -> a -> Maybe a
maybeIs p a = if p a
              then Just a
              else Nothing

-- GEN FAILURE HANDLING AND LOGGING:

data FailedGen = FailedToObtain String String
               deriving Show

genFail :: Show a => String -> a -> FailedGen
genFail goal result = FailedToObtain goal (show result)

logFailure :: Int -> FailedGen -> IO ()
logFailure n (FailedToObtain goal result) =
    (putStrLn . concat) ["Attempt: ", (show n), "\n",
                         ">>> Failed to obtain:\n    `", goal, "`\n",
                         ">>> Instead obtained:\n    ",  result]

logSeedNoun :: String -> IO ()
logSeedNoun = appendFile seedNounsFile . (++ "\n")

seedNouns :: IO [String]
seedNouns = fmap lines (readFile seedNounsFile)

-- TESTING FOR DEFINITION PARTS ETC.

isNoun :: Definition -> Bool
isNoun = ("noun" ==) . pos

         
withTypes :: Definition -> Bool
withTypes def = isJust t1 || isJust t2
    where
        t1 = hasTypes def
        t2 = typeOf def


-- TODO: Implement these via applicative!
        
takeUptoIO' :: (a -> IO Bool) -> [a] -> IO (Maybe [a])
takeUptoIO' f l = do i <- findIndexIO f l
                     let j = fmap (+ 1) i
                     return (take <$> j <*> Just l)
    
findIndexIO :: (a -> IO Bool) -> [a] -> IO (Maybe Int)
findIndexIO f l = elemIndex True <$> mapM (anyPM [f]) l

takeUptoIO :: (a -> IO Bool) -> [a] -> IO [a]
takeUptoIO f l = case l of
    []     -> return []
    (a:xs) -> do
        bool <- f a
        if bool then return [a]
                else do
                    as <- takeUptoIO f xs
                    return (a:as)
    
-- Querries api with a string, and returns True if the string
-- has noun definition.
queryIsNoun :: String -> IO Bool
queryIsNoun s
    | length s < 3 || inBlackList s =  -- no blacklisted words, and no words shorter than 3 letters.
          return False  
    | otherwise = do                   -- We want anything else, so long as it can be a noun.
          result <- wapiEntryFor (normalizeNoun s) -- (singularize s)
          return $ maybe False (any isNoun) result

-- Temp: Black list will need refinement.
inBlackList :: String -> Bool
inBlackList w = w `elem` ["http", "then"]

-- MAKING RANDOM SELECTIONS
 
randPreposition, randHashtag :: IO String
randPreposition = randItem =<< fmap lines (readFile prepositionsFile)
 
randHashtag = randItem =<< fmap lines (readFile hashtagsFile)

randItem :: [a] -> IO a
randItem = generate . elements
