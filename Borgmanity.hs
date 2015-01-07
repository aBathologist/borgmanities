{-# LANGUAGE ImplicitParams #-}

import NLP.WordNet



-- Sets path to WordNet database
-- Currently looks for database in same directory
wordNetDictPath :: FilePath
wordNetDictPath = "dict"

-- Loads the database from the path given by wordNetDictPath 
loadWordNetDict :: IO WordNetEnv
loadWordNetDict = initializeWordNetWithOptions (Just wordNetDictPath) Nothing
