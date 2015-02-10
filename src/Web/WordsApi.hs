{-# LANGUAGE OverloadedStrings #-}

module Web.WordsApi
       ( wapiEntryFor
       , Definition(..)
       ) where

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.String.Conversions

import Web.SimpleHTTPConduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
    
import Secrets


{--

WordsApi Exports a single function, `wpiEntryFor`.
TODO: explain....

--}


-- Return the JSON Data given a string to query

wapiEntryFor :: String -> IO (Maybe [Definition])
wapiEntryFor query = do
    maybeJson <- getWapiQuery query
    case maybeJson of
        Nothing   -> return Nothing
        Just json ->
            case wapiResultFromJson json of
                Just (WapiResult defs) -> (return . Just) defs
                otherwise -> return Nothing


-- Settings for making the WordsApi Query:
token :: S.ByteString
token = (cs . secret) Secrets.wapiToken
        
wapiEndPoint :: String
wapiEndPoint = "https://wordsapiv1.p.mashape.com/words/"


-- Making the query:
getWapiQuery :: String -> IO (Maybe L.ByteString)
getWapiQuery q = do
    let header = [("X-Mashape-Key", token), ("Accept", "application/json")]
    result <- getSecureBody wapiEndPoint q header
    case result of
        Right body  -> (return . Just) body
        Left status -> do putStrLn (">>> Wapi query failed to find entry for `" ++ q ++ "`: " ++ show status)
                          return Nothing

-- This returns the whole http resonse, including the headers.
getWapiResponse q = do
    let header = [("X-Mashape-Key", token), ("Accept", "application/json")]
    getSecure wapiEndPoint q header


-- Parsing the JSON Data
wapiResultOfStr :: String -> Maybe WapiResult
wapiResultOfStr = (decode . cs)

wapiResultFromJson :: L.ByteString -> Maybe WapiResult
wapiResultFromJson =  decode

-- Definition of the WordsApi data type:
data Definition = Definition
                  { def      :: String
                  , pos      :: String
                  , syn      :: Maybe [String]
                  , typeOf   :: Maybe [String]
                  , hasTypes :: Maybe [String]
                  } deriving Show
                  
newtype WapiResult = WapiResult [Definition]
                   deriving Show

instance FromJSON WapiResult where
    parseJSON (Object o) =
       WapiResult
       <$> (o .: "results")
    parseJSON _ = mzero

instance FromJSON Definition where
    parseJSON (Object o) =
        Definition
        <$> (o .: "definition")
        <*> (o .: "partOfSpeech")
        <*> (o .:? "synonyms")
        <*> (o .:? "typeOf")
        <*> (o .:? "hasTypes")
    parseJSON _ = mzero
