{-# LANGUAGE OverloadedStrings #-}

module Web.WordsApi
       ( wapiEntryFor
       , Definition(..)
       -- , def
       -- , pos
       -- , syn     
       -- , typeOf 
       -- , hasTypes
       , ) where

-- import Network.Wreq
import Control.Lens

import Network.Curl
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.String.Conversions
import qualified Data.HashMap.Strict as Hash

import Secrets

{--

WordsApi Exports a single function, `wpiEntryFor`.
TODO: explain....

--}


-- | Return the JSON Data given a string to query

wapiEntryFor :: String -> IO (Maybe [Definition])
wapiEntryFor query = do
    jsonStr <- curlGetWapiQuery query
    case wapiResultOfStr jsonStr of
        Just (WapiResult definitions) ->
            return (Just definitions)
        otherwise ->
            return Nothing



-- | Requesting data from WordsApi (aka "Wapi")

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
        -- Returns the error code if the GET request goes wrong.
        errStr -> return (show errStr) 

-- | Parsing the JSON Data

wapiResultOfStr :: String -> Maybe WapiResult
wapiResultOfStr = (decode . cs)

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
