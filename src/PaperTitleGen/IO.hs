{-# LANGUAGE OverloadedStrings #-}

module PaperTitleGen.IO
       ( getTitleParts )
       where

import PaperTitleGen.Gen
import Web.WordsApi
import RandSelections
import Web.Twitter
    
import Data.Maybe
import Data.List
import Control.Concurrent

-- TODO:
-- # add logging system so I can see what's going on
--   (this should become a standard part of my creation process)
-- # regigger thingy so that is uses Data.Text instead of String?
    
-- TODO: add safety mechanism so it won't get trapped in endless cycle
--       e.g., return erros and quit if fails n times.
getTitleParts :: IO (TitleParts)
getTitleParts = do
    seedNoun   <- getSeedNoun
    randPrep   <- randPreposition
    viableDefs <- viableDefinitions seedNoun
    case viableDefs of
        Nothing   -> do putStrLn "-- Failed to obtain viable definition. Retrying."
                        threadDelay 500000 -- waits .5 seconds, so it won't flood.
                        getTitleParts -- Run again if seedNoun was barren
        Just defs -> do
            typeVariant <- randTypeVariant defs
            maybeComplement  <- getComplement typeVariant randPrep
            case maybeComplement of
                Nothing -> do
                    putStrLn "-- Failed to obtain compliment. Retrying."
                    threadDelay 500000 -- waits .5 seconds, so it won't flood.
                    getTitleParts -- Run again if seedNoun was barren
                Just complement -> 
                    return TitleParts { seedNoun    = seedNoun
                                      , typeVariant = typeVariant
                                      , randPrep    = randPrep
                                      , definition  = def (head defs)
                                      , complement  = complement
                                      }

-- TODO: add safitey mechanism so it won't get trapped in endless cycle.
getSeedNoun :: IO String
getSeedNoun = do
    results   <- search =<< randHashtag
    maybeNoun <- findIO queryIsNoun results
    case maybeNoun of
         Nothing   -> do putStrLn "-- Failed to find seed noun. Retrying."
                         threadDelay 500000 -- waits .5 seconds, so it won't flood.
                         getSeedNoun 
         Just noun -> return noun


-- getComplement :: String -> String -> IO String
getComplement typeVariant randPrep = do
    let searchTerm = typeVariant ++ " " ++ randPrep
    result <- (fmap unwords . search . quoteStr) searchTerm
    case listAfterInfix searchTerm result of
        Nothing  -> return Nothing
        Just txt -> do
            complementWords <- (takeUptoIO queryIsNoun . words) txt
            (return . Just . unwords) complementWords



-- TODO: reduce the amount of justs !
-- listAfterInfix :: Eq a => [a] -> [a] -> Maybe [a]
listAfterInfix ps = fromMaybe Nothing . find isJust . map (stripPrefix  ps) . tails

quoteStr :: String -> String
quoteStr s = "\"" ++ s ++ "\""

-- Returns random a typeVariant from the tail of a list of definitions
randTypeVariant :: [Definition] -> IO String
randTypeVariant (_:defs) = randItem . concat . catMaybes $ defTypes
    where
        defTypes = map hasTypes defs ++ map typeOf defs

-- Takes seedNoun and returns returns Just a list of nouns, the tail
-- members of which have are withTypes, or else nothing, if the tail
-- is empty or if the qeury returns no nouns.
viableDefinitions :: String -> IO (Maybe [Definition])
viableDefinitions seedNoun =
    do
        results <- wapiEntryFor seedNoun
        putStrLn ("-- Obtained: \n" ++ show results)
        return (maybe Nothing nounsWithTypes results)
    where
        nounsWithTypes defs = let d1:nouns = filter isNoun defs
                              in case filter withTypes nouns of
                                     []        -> Nothing
                                     otherwise -> Just (d1:nouns)
        


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
queryIsNoun s = do
    result <- wapiEntryFor s
    return $ maybe False (any isNoun) result
