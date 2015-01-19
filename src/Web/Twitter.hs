{-# LANGUAGE OverloadedStrings #-}

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Lens


import Data.String.Conversions
import Secrets

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey    = cs $ public Secrets.twitterToken
    , oauthConsumerSecret = cs $ secret Secrets.twitterToken
    }

credentials :: Credential
credentials = Credential
    [ ("oauth_token",        cs $ public Secrets.twitterKey)
    , ("oauth_token_secret", cs $ secret Secrets.twitterKey) ]

twInfo :: TWInfo
twInfo = def
    { twToken = def { twOAuth = tokens, twCredential = credentials }
    , twProxy = Nothing
    }
