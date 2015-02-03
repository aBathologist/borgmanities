module Web.SimpleHTTPConduit
       ( getSecure
       , getSecureBody
       , Response(..)
       , Status(..)
       ) where

import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Header as H
import Network.HTTP.Types.Status
import Network.Connection (TLSSettings (..))
import qualified Data.ByteString.Lazy as L

import Data.String.Conversions

-- getSecureBody :: String -> String -> H.RequestHeaders
--                  -> IO (Either FailedStatus (Response L.ByteString))
data FailedStatus = Failed (Int, String) deriving Show
failedStatus :: Status -> FailedStatus
failedStatus status = Failed (code, msg)
    where
        code = statusCode status
        msg  = cs (statusMessage status)

-- Returns either the body of a successfull query (i.e., with status code 200)
-- or the status code and message returned
getSecureBody endpoint query header = do
    response <- getSecure endpoint query header
    let status = responseStatus response
    case statusCode status of
        200       -> (return . Right . responseBody) response
        otherwise -> (return . Left  . failedStatus) status


getSecure :: String -> String -> H.RequestHeaders -> IO (Response L.ByteString)
getSecure endpoint query header = do
    defaultRequest <- parseUrl (endpoint ++ query ++ "/")
    let request = defaultRequest
                    { secure = True
                    , requestHeaders = header
                    , checkStatus = \_ _ _ -> Nothing
                    }
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    withManagerSettings settings $ httpLbs request
