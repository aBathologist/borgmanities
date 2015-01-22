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
    
-- TODO: add safety mechanism so it won't get trapped in endless cycle
--       e.g., return erros and quit if fails n times.
getTitleParts :: IO (TitleParts)
getTitleParts = do
    seedNoun   <- getSeedNoun
    randPrep   <- randPreposition
    viableDefs <- viableDefinitions seedNoun
    case viableDefs of
        Nothing   -> getTitleParts -- Run again if seedNoun was barren
        Just defs -> do
            typeVariant <- randTypeVariant defs
            return TitleParts { seedNoun    = seedNoun
                              , typeVariant = typeVariant
                              , randPrep    = randPrep
                              , definition  = def (head defs)
                              , complement  = "complement goes here"
                              }

-- TODO: add safitey mechanism so it won't get trapped in endless cycle.
getSeedNoun :: IO String
getSeedNoun = do
    results <- search =<< randHashtag
    maybeNoun <- findIO queryIsNoun results
    case maybeNoun of
         Nothing   -> threadDelay 500000 -- waits .5 seconds so it won't flood.
                      >> getSeedNoun 
         Just noun -> return noun
 

-- Returns random a typeVariant from the tail of a list of definitions
randTypeVariant :: [Definition] -> IO String
randTypeVariant (_:defs) = randItem . concat . catMaybes $ types
    where
        types = map hasTypes defs ++ map typeOf defs

-- Takes seedNoun and returns returns Just a list of nouns, the tail
-- members of which have are withTypes, or else nothing, if the tail
-- is empty or if the qeury returns no nouns.
viableDefinitions :: String -> IO (Maybe [Definition])
viableDefinitions seedNoun =
    do
        results <- wapiEntryFor seedNoun
        case results of
            Nothing   -> return Nothing
            Just defs -> do
                let d1:rest = filter isNoun defs
                    nouns   = filter withTypes rest
                return $ if (length nouns) > 0
                            then Just (d1:nouns)
                            else Nothing 

isNoun :: Definition -> Bool
isNoun = ("noun" ==) . pos
         
withTypes :: Definition -> Bool
withTypes def = isJust t1 || isJust t2
    where
        t1 = hasTypes def
        t2 = typeOf def

findIO :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findIO f l = case l of
    []     -> return Nothing
    (a:xs) -> do bool <- f a
                 if bool then return (Just a)
                         else findIO f xs
            
queryIsNoun :: String -> IO Bool
queryIsNoun s = do
    result <- wapiEntryFor s
    return $ maybe False (any isNoun) result
