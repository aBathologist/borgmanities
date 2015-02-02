{-# LANGUAGE OverloadedStrings #-}

module PaperTitleGen.IO
       ( getTitleParts )
       where

import PaperTitleGen.Gen
import Web.WordsApi
import qualified Web.Twitter as Twitter

import Data.Either
import Data.Maybe
import Data.List
import Control.Concurrent
import Test.QuickCheck (generate, elements)

getTitleParts :: IO (Maybe TitleParts)
getTitleParts = getTitleParts' 0

-- an auxiliary function for getTitleParts. The Int argument is to
-- put an upper limit on the amount of attempts made. Set too 100.
getTitleParts' :: Int -> IO (Maybe TitleParts)
getTitleParts' n
    | n > 100   = return Nothing
    | otherwise = do
          result <- tryGetTitleParts
          either (genFailRedo n) (return . Just) result
    where
        genFailRedo n err = do logFailure n err
                               threadDelay 500000
                               getTitleParts' (n + 1)
                               
                                 
-- This is the main function for putting together the various
-- IO operations needed to gather the parts of the TitleParts
-- data type.
-- At some point, I would like to clean this up, as I'm sure there's
-- a better way to handle the error detection/control flow.
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
                        Left err         -> return (Left err)
                        Right complement -> 
                            (return . Right) TitleParts { seedNoun    = seedNoun
                                                        , typeVariant = typeVariant
                                                        , randPrep    = randPrep
                                                        , definition  = def (head defs)
                                                        , complement  = complement
                                                        }


getSeedNoun :: IO (Either FailedGen String)
getSeedNoun =
    do
        results   <- Twitter.search =<< randHashtag
        maybeNoun <- findIO queryIsNoun results
        return $ maybe failed succeeded maybeNoun
    where
        failed    = Left (genFail "seedNoun" "none")
        succeeded = Right


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
getComplement randPrep typeVariant  =
    do
        let searchTerm = typeVariant ++ " " ++ randPrep
        result <- (fmap unwords . Twitter.search . quoteStr) searchTerm
        let complementMatch = (listAfterInfix searchTerm result)
        maybe failed succeeded complementMatch
    where
        failed = return $ Left (genFail "viableComplement" "Nothing")
        succeeded txt = do
            complementWords <- (takeUptoIO queryIsNoun . words) txt
            return $ Right (unwords complementWords)

-- Aux functions for getComplement
listAfterInfix :: Eq a => [a] -> [a] -> Maybe [a]
listAfterInfix ps = fromMaybe Nothing . find isJust . map (stripPrefix  ps) . tails

quoteStr :: String -> String
quoteStr s = "\"" ++ s ++ "\""



-- GEN ATTEMP FAILURE HANDLING:

data FailedGen = FailedToObtain String String
               deriving Show

genFail :: Show a => String -> a -> FailedGen
genFail goal result = FailedToObtain goal (show result)

logFailure :: Int -> FailedGen -> IO ()
logFailure n (FailedToObtain goal result) =
    (putStrLn . concat) ["Attempt: ", (show n), "\n",
                         ">>> Failed to obtain:\n    `", goal, "`\n",
                         ">>> Instead obtained:\n    ",  result]



-- TESTING FOR DEFINITION PARTS ETC.

isNoun :: Definition -> Bool
isNoun = ("noun" ==) . pos

         
withTypes :: Definition -> Bool
withTypes def = isJust t1 || isJust t2
    where
        t1 = hasTypes def
        t2 = typeOf def

-- findIO is just like find, but uses a predicate that returns
-- its boolean wrapped in the IO monad. Needed to work with
-- queryIsNoun.
findIO :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findIO f l = case l of
    []     -> return Nothing
    (a:xs) -> do
        bool <- f a
        if bool then return (Just a)
                else findIO f xs

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
    | length s < 3 || inBlackList s =  -- no blacklisted words, an no words shorter than 3 letters.
          return False  
    | otherwise = do                   -- We want anything else, so long as it can be a noun.
          result <- wapiEntryFor s
          return $ maybe False (any isNoun) result

-- Temp: Black list will need refinement.
inBlackList :: String -> Bool
inBlackList w = w `elem` ["http", "then"]

-- MAKING RANDOM SELECTIONS

prepositionsFile, hashtagsFile :: String
prepositionsFile = "src/resources/prepositions.txt"
hashtagsFile     = "src/resources/hashtags.txt"
 
randPreposition, randHashtag :: IO String
randPreposition = randItem =<< fmap lines (readFile prepositionsFile)
 
randHashtag = randItem =<< fmap lines (readFile hashtagsFile)

randItem :: [a] -> IO a
randItem = generate . elements
