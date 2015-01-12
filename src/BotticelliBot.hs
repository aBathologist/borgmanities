import PaperTitleGen
import Web.WordsApi
import RandSelections
    
import Data.Maybe

-- Returns random typeVariant from the tail of a list of definitions
typeVariant :: [Definition] -> IO String
typeVariant (_:defs) = randItem . concat . catMaybes $ types
    where
        types = map hasTypes defs ++ map typeOf defs

-- Returns the first two definitions and the typeVariant string if
-- there are >= 2 definitions and the second one has a typeVariant
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
