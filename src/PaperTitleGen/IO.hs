module PaperTitleGen.IO
       ( getTitleParts )
       where

import PaperTitleGen.Gen
import Web.WordsApi
import RandSelections
    
import Data.Maybe

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
                              , tweets      = ["Tweets", "go", "here"]
                              }

-- TODO:
getSeedNoun :: IO String
getSeedNoun = do
    hastag <- randHashtag
    -- TODO: search twitter to get seed
    return "test"

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
    where
        isNoun = ("noun" ==) . pos

withTypes :: Definition -> Bool
withTypes def = isJust t1 || isJust t2
    where
        t1 = hasTypes def
        t2 = typeOf def
            -- 
