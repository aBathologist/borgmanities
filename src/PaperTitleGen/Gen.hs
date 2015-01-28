module PaperTitleGen.Gen
       ( generateTitle
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


-- returns the string to be tweeted.
generateTitle :: TitleParts -> String
generateTitle titleParts = show titleParts


sampleTitle = TitleParts
              { seedNoun =
                  "innovation"
              , typeVariant =
                  "creativity"
              , randPrep =
                  "amid"
              , definition =
                  "a creation (a new device or process) resulting from study and experimentation"
              , complement =
                  "chaos"
              }
