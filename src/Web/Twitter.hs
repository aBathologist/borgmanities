{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter (search) where

import System.Process
   
-- This is just a wraper around a command line call to the python program python/twitter.hs
-- It is made into it's own module as a promise that I'll replace the python with a proper haskell
-- implementation.

search :: String -> IO [String]
search str = fmap words (readProcess "python/twitter.py" ["-s", str] [])

