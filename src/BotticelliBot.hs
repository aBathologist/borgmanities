module BotticelliBot
       ( main )
       where

import PaperTitleGen.Gen
import PaperTitleGen.IO

main :: IO ()
main = putStrLn =<< makePaperTitle 

makePaperTitle :: IO String
makePaperTitle = fmap generateTitle getTitleParts


