module BotticelliBot
       ( main )
       where

import PaperTitleGen.Gen
import PaperTitleGen.IO
import qualified Web.Twitter as Twitter

main :: IO ()
main = putStrLn "temp"-- =<< makePaperTitle 

testPaperTitle :: IO String
testPaperTitle = do
    result <- getTitleParts
    return $ maybe "Failed" generateTitle result
    
-- makePaperTitle :: IO String
-- makePaperTitle = do
--     result <- getTitleParts
--     fmap generateTitle result

tweetTitle :: IO ()
tweetTitle = do
    result <- getTitleParts
    case result of
        Nothing         -> putStrLn "Failed"
        Just titleParts ->
            let title = generateTitle titleParts
            in case Twitter.tweet title of
                   Left err    -> putStrLn err
                   Right tweet -> tweet >>= putStrLn
