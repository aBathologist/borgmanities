module BotticelliBot
       ( main )
       where

import PaperTitle.Gen
import PaperTitle.IO
import qualified Web.Twitter as Twitter

main :: IO ()
main = tweetTitle 


testPaperTitle :: IO (Maybe TitleParts, String)
testPaperTitle = do
    result <- getTitleParts
    return $ (result, maybe "Failed" generateTitle result)


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
