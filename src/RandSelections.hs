module RandSelections
       ( randPreposition
       , randHashtag
       , randItem
       ) where

import Test.QuickCheck (generate, elements)

prepositionsFile, hashtagsFile :: String
prepositionsFile = "src/resources/prepositions.txt"
hashtagsFile     = "src/resources/hashtags.txt"
 
randPreposition, randHashtag :: IO String
randPreposition = randItem =<< fmap lines (readFile prepositionsFile)
 
randHashtag = randItem =<< fmap lines (readFile hashtagsFile)

randItem :: [a] -> IO a
randItem = generate . elements
