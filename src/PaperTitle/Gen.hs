module PaperTitle.Gen
       ( generateTitle
       , TitleParts(..) 
       ) where

import Data.Char
import Text.Regex

-- TODO: Turn into a pure environment for building up a title.
data TitleParts = TitleParts
                  { seedNoun    :: String
                  , definition  :: String
                  , typeVariant :: String
                  , randPrep    :: String
                  , complement  :: String
                  } deriving Show

-- Todo:
-- returns the string to be tweeted.
generateTitle :: TitleParts -> String
generateTitle t = title ++ ": " ++ subtitle
    where
        def     = definition t
        typeVar = typeVariant t
        prep    = randPrep t
        comp    = complement t
        
        title    = (toTitleCase . dropCoordinatedClause . dropAfterPunctuation . withoutParenPhrase) def
        subtitle = (toTitleCase . unwords) ["The", typeVar, prep, comp]


-- TRIM AND MANICURE TITLE PARTS

-- Remove parenthesized phrases
withoutParenPhrase :: String -> String
withoutParenPhrase str = subRegex re str ""
    where
        re = mkRegex "\\(.*\\)"

-- Drop after and/or/of
dropCoordinatedClause :: String -> String
dropCoordinatedClause str =
    if (length ws) > 4
        then (unwords . takeWhile (not . coordinator)) ws
        else unwords ws
    where
        ws = words str
        coordinator = (`elem` ["especially", "with", "having", "and", "or", "as", "used"])

-- Drop after punctuation
dropAfterPunctuation :: String -> String
dropAfterPunctuation = takeWhile (not . punctuation)
    where
        punctuation = (`elem` [';', ',', ':', '.'])

-- Title Case Conversion:
-- Using this as a guide: https://owl.english.purdue.edu/owl/resource/747/05/

toTitleCase :: String -> String
toTitleCase str = (unwords . (first :) . map capitalize) rest
    where
        ((f:irst):rest) = words str
        first = (toUpper f) : irst
        capitalize w@(c:cs) = if w `elem` dontCapitalize
                                then w
                                else (toUpper c) : cs
                               
dontCapitalize, prepositions, articles, conjunctions :: [String]
dontCapitalize = prepositions ++ articles ++ conjunctions
prepositions = ["about","across","amid","as","at", "by", "of",
                "for","from","in","into","opposite", "over","past","per","through","to",
                "toward","towards","until","upon","versus","via","with","within","without"]
articles     = ["a", "the", "an"]
conjunctions = ["and","that","but","or","as","if","when","than","because",
                "while","where","after","so","though","since","until","whether",
                "before","although","nor","like","once","unless","now","except"]

-- For development:
--  These definitions are for testing generateTitle without having
--  to run the full operations to generate title parts.
--  write the algorithm in generateTitle and then run testGenerate
--  to see how it works on sampleTitle.

testGenerate :: String
testGenerate = generateTitle sampleTitle

sampleTitle =
    TitleParts
    { seedNoun    = "innovation"
    , typeVariant = "creativity"
    , randPrep    = "amid"
    , definition  = "a creation (a new device or process) resulting\
                    \ from study and experimentation "
    , complement  = "chaos"
    }
