{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter
       ( search
       , tweet
       ) where

import System.Process
   
-- This is just a wraper around a command line call to the python program python/twitter.hs
-- It is made into it's own module as a promise that I'll replace the python with a proper haskell
-- implementation.

twitterPy :: String
twitterPy = "python/twitter.py"

search :: String -> IO [String]
search str = fmap words (readProcess twitterPy ["-s", str] [])

tweet :: String -> Either String (IO String)
tweet str = if length str > 140
                then Left "String is too long to tweet: must be less than 140 chars."
                else Right (readProcess twitterPy ["-t", str] [])
