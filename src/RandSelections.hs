module RandSelections
       ( randPreposition
       , randHashtag
       , randItem
       ) where

import Test.QuickCheck (generate, elements)

prepositionsFile, hashtagsFile :: String
prepositionsFile = "resources/prepositions.txt"
hashtagsFile     = "resources/hashtags.txt"
 
randPreposition, randHashtag :: IO String
randPreposition = randItem =<< fmap lines (readFile prepositionsFile)
 
randHashtag = randItem =<< fmap lines (readFile hashtagsFile)

randItem :: [a] -> IO a
randItem = generate . elements