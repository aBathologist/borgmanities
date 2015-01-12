import PaperTitleGen
import Web.WordsApi
import RandSelections
    
import Data.Maybe

typeVariant :: [Definition] -> IO String
typeVariant (_:defs) = undefined


-- Returns the first two definitions and the typeVariant string if
-- there are >= 2 definitions and the second one has a typeVariant
viableDefinitions :: String -> IO (Maybe [Definition])
viableDefinitions seedNoun =
    do
        results <- wapiEntryFor seedNoun
        let nouns = fmap (filter nounsWithTypes) results
        return $ case fmap ((> 2) . length) nouns of
            Just True -> nouns
            otherwise -> Nothing 
    where
        nounsWithTypes def = pos def == "noun" && hasTypes def
        
hasTypes :: Definition -> Bool
hasTypes def = isJust t1 || isJust t2
    where
        t1 = hasType def
        t2 = typeOf def
