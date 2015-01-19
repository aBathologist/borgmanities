{-# LANGUAGE OverloadedStrings #-}
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Web.Authenticate.OAuth

import Data.String.Conversions
import Secrets

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey    = cs (public Secrets.twitterToken)
    , oauthConsumerSecret = cs (secret Secrets.twitterToken)
    }

credentials :: Credential
credentials = Credential
    [ ("oauth_token",        cs (public Secrets.twitterKey))
    , ("oauth_token_secret", cs (secret Secrets.twitterKey)) ]
