module PaperTitleGen (generateTitle, TitleParts) where

import Web.WordsApi (Definition)

{--
The Definition type is defined thus:

data Definition = Definition
                  { def     :: String           -- definition
                  , pos     :: String           -- part of speech
                  , syn     :: Maybe [String]   -- synonyms
                  , typeOf  :: Maybe [String] 
                  , hasType :: Maybe [String]
                  }

Acess the parts of the definition by calling the
record name on the definition object. If you have a
Definition, `d`, then `def d` will give you the definition
and `pos d` will give you it's grammatical category.

--}

--temp
type Tweet = String

-- TODO: Turn into a pure environment for building up a title.

data TitleParts = TitleParts
                  { seedNoun    :: String
                  , typeVariant :: String
                  , randPred    :: String
                  , definitions :: [Definition]
                  , tweets      :: [Tweet]
                  }

-- returns the string to be tweeted.
generateTitle :: TitleParts -> String
generateTitle = undefined
