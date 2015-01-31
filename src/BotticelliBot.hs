module BotticelliBot
       ( main )
       where

import PaperTitleGen.Gen
import PaperTitleGen.IO
import qualified Web.Twitter as Twitter

main :: IO ()
main = putStrLn =<< makePaperTitle 

makePaperTitle :: IO String
makePaperTitle = fmap generateTitle getTitleParts

tweetTitle :: IO ()
tweetTitle = do
             title <- makePaperTitle
             case (Twitter.tweet title) of
                 Left err    -> putStrLn err
                 Right tweet -> tweet >>= putStrLn
