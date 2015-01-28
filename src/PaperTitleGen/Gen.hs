module PaperTitleGen.Gen
       ( generateTitle
       , testGenerate -- TEMP: for dev use
       , TitleParts(..) 
       ) where

-- TODO: Turn into a pure environment for building up a title.
data TitleParts = TitleParts
                  { seedNoun    :: String
                  , typeVariant :: String
                  , randPrep    :: String
                  , definition  :: String
                  , complement  :: String
                  } deriving Show

-- Todo:
-- returns the string to be tweeted.
generateTitle :: TitleParts -> String
generateTitle titleParts = show titleParts







-- TEMP: for dev use
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
