import PaperTitleGen.IO
import PaperTitleGen.Gen

main :: IO ()
main = do
    titleParts <- getTitleParts
    putStrLn (generateTitle titleParts)
