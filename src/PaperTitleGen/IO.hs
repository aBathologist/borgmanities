{-# LANGUAGE OverloadedStrings #-}

module PaperTitleGen.IO
       ( getTitleParts )
       where

import PaperTitleGen.Gen
import Web.WordsApi
import Web.Twitter

import Data.Maybe
import Data.List
import Control.Concurrent
import Test.QuickCheck (generate, elements)
    
-- TODO:
-- # add logging system so I can see what's going on
--   (this should become a standard part of my creation process)
-- # regigger thingy so that is uses Data.Text instead of String?
    
-- TODO: add safety mechanism so it won't get trapped in endless cycle
--       e.g., return erros and quit if fails n times.
getTitleParts :: IO (TitleParts)
getTitleParts = do
    eitherSeedNoun <- getSeedNoun
    case eitherSeedNoun of
        Left  err      -> genFailedRedo err
        Right seedNoun ->
            do
            eitherDefs <- viableDefinitions seedNoun
            case eitherDefs of
                Left  err  -> genFailedRedo err
                Right defs ->
                    do
                    typeVariant      <- randTypeVariant defs
                    randPrep         <- randPreposition
                    eitherComplement <- getComplement typeVariant randPrep
                    case eitherComplement of
                        Left err         -> genFailedRedo err
                        Right complement -> 
                            return TitleParts { seedNoun    = seedNoun
                                              , typeVariant = typeVariant
                                              , randPrep    = randPrep
                                              , definition  = def (head defs)
                                              , complement  = complement
                                              }
                

getSeedNoun :: IO (Either (FailedGen String) String)
getSeedNoun =
    do
        results   <- search =<< randHashtag
        maybeNoun <- findIO queryIsNoun results
        return $ maybe failed succeeded maybeNoun
    where
        failed    = Left (FailedToObtain {goal = "seedNoun", result = "none"})
        succeeded = Right


viableDefinitions :: String -> IO (Either (FailedGen (Maybe [Definition])) [Definition])
viableDefinitions seedNoun =
    do
        results <- wapiEntryFor seedNoun
        case results of
            Nothing   -> return $ Left (FailedToObtain {goal = "viableDefinitions", result = Nothing})
            Just defs -> return $ nounsWithTypes defs
    where
        nounsWithTypes defs = let d1:nouns = filter isNoun defs
                              in case filter withTypes nouns of
                                     []        -> Left  (FailedToObtain {goal = "viableDefinitions", result = Just defs})
                                     otherwise -> Right (d1:nouns)


-- Returns random a typeVariant from the tail of a list of definitions
randTypeVariant :: [Definition] -> IO String
randTypeVariant (_:defs) = randItem . concat . catMaybes $ defTypes
    where
        defTypes = map hasTypes defs ++ map typeOf defs
        
        
getComplement :: String -> String -> IO (Either (FailedGen (Maybe String)) String)
getComplement typeVariant randPrep =
    do
        let searchTerm = typeVariant ++ " " ++ randPrep
        result <- (fmap unwords . search . quoteStr) searchTerm
        let complementMatch = (listAfterInfix searchTerm result)
        maybe failed succeeded complementMatch
    where
        failed = return $ Left (FailedToObtain {goal = "viableComplement", result = Nothing})
        succeeded txt = do
            complementWords <- (takeUptoIO queryIsNoun . words) txt
            return $ Right (unwords complementWords)


listAfterInfix :: Eq a => [a] -> [a] -> Maybe [a]
listAfterInfix ps = fromMaybe Nothing . find isJust . map (stripPrefix  ps) . tails

quoteStr :: String -> String
quoteStr s = "\"" ++ s ++ "\""



-- GEN ATTEMP FAILURE HANDLING:

data FailedGen a = FailedToObtain {goal :: String, result :: a} deriving Show

logFailure :: Show a => FailedGen a -> IO ()
logFailure err = (putStrLn . concat) [">>> Failed to obtain:\n    `", (goal err), "`\n",
                                      ">>> Instead obtained:\n    ", ((show . result) err)]

genFailedRedo :: Show a => FailedGen a -> IO (TitleParts)
genFailedRedo err =  do logFailure err
                        threadDelay 500000
                        getTitleParts



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
    
-- Querries api with a string, and return True if the string
-- has noun definition.
queryIsNoun :: String -> IO Bool
queryIsNoun s
    | length s < 3 || inBlackList s =  -- no blacklisted words, an no words shorter than 3 letters.
          return False  
    | otherwise = do             -- We want anything else, so long as it can be a noun.
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
