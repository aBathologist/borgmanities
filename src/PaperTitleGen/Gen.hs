module PaperTitleGen.Gen
       ( generateTitle
       , TitleParts(..) 
       ) where


--temp
type Tweet = String

-- TODO: Turn into a pure environment for building up a title.

data TitleParts = TitleParts
                  { seedNoun    :: String
                  , typeVariant :: String
                  , randPrep    :: String
                  , definition  :: String
                  , tweets      :: [Tweet]
                  } deriving Show

-- returns the string to be tweeted.
generateTitle :: TitleParts -> String
generateTitle titleParts = show titleParts
