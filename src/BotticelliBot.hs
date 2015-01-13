import PaperTitleGen.IO
import PaperTitleGen.Gen

main :: IO ()
main = putStrLn =<< makePaperTitle 


makePaperTitle :: IO String
makePaperTitle = fmap generateTitle getTitleParts
