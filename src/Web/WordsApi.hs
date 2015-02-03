{-# LANGUAGE OverloadedStrings #-}

module Web.WordsApi
       ( wapiEntryFor
       , Definition(..)
       ) where

import Control.Lens

import Network.Curl

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.String.Conversions
import qualified Data.HashMap.Strict as Hash

import Web.SimpleHTTPConduit

-- import Network.HTTP.Conduit
-- import qualified Network.HTTP.Types.Header as Header
-- import Network.Connection (TLSSettings (..))
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
        

-- Requesting data from WordsApi (aka "Wapi")

wapiEndpoint :: String
wapiEndpoint = "https://www.wordsapi.com/words/"

wapiQuery :: String -> String -> String
wapiQuery query token = wapiEndpoint ++ query ++ "?accessToken=" ++ token

curlGetWapiQuery :: String -> IO String
curlGetWapiQuery query = do
    let token = secret Secrets.wapiToken
    (curlCode, jsonStr) <- (curlGetString (wapiQuery query token) [])
    case curlCode of
        CurlOK -> return jsonStr
        errStr -> return (show errStr) -- Returns the error code if the GET request goes wrong.

token = (cs . secret) Secrets.wapiToken
wapiEndPoint = "https://wordsapiv1.p.mashape.com/words/"

-- getWapiQuery :: String -> IO L.ByteString
getWapiQuery q = do
    let header = [("X-Mashape-Key", token), ("Accept", "application/json")]
    result <- getSecureBody wapiEndPoint q header
    case result of
        Right body  -> (return . Just) body
        Left status -> do putStrLn (">>> Failed to find Query: " ++ show status)
                          return Nothing

getWapiResponse q = do
    let header = [("X-Mashape-Key", token), ("Accept", "application/json")]
    getSecure wapiEndPoint q header

-- Parsing the JSON Data

wapiResultOfStr :: String -> Maybe WapiResult
wapiResultOfStr = (decode . cs)

wapiResultFromJson :: L.ByteString -> Maybe WapiResult
wapiResultFromJson =  decode

-- TODO: Add in fields for the entire wapi return structure.
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
