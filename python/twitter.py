#! /usr/bin/env python3

import tweepy
import argparse
import string
import secrets

## This code is a helper program for the BotticelliBot Haskell program.
## It was written because figuring out how to interface with the twitter api
## proved to be a huge pain in the ass. From fathoming the twitter-conduit library
## all the way down to making simple https requets.
##
## It should be eliminated for the sake of a Haskell solution at some point.
##
## Provides a cli with two modes of interaction:
##
##  Search for a term 'term':
##      python3 twitter.py -s term
##
##  Post a tweet 'text':
##      python3 twitter.py -p text

# Command line parser:
def build_parser():
    parser = argparse.ArgumentParser(description="BotticelliBot's sloppy python appendage for twitter searches and posts.")
    parser.add_argument('-s', '--search', action='store_true', help="Search twitter for argument")
    parser.add_argument('search_term', help="The term to search for, passed with the -s flag")
    return parser

# Returns an twitter api object
def twitterApi() :
    auth = tweepy.OAuthHandler(secrets.consumer_key, secrets.consumer_secret)
    auth.set_access_token(secrets.access_token, secrets.access_token_secret)
    return tweepy.API(auth)

# apiObejct -> queryString -> twitterObjects
def search_twitter(twitterApi, q):
    return twitterApi.search(q, "en", "en", 20)

# string -> normalizedString
def normalize_string(term, s):
    transtable = {ord(c): ' ' for c in string.punctuation.replace("'",'')}
    transtable[ord('\n')] = ' '
    return s.translate(transtable).replace(term, ' ').lower()

if __name__ == '__main__':
    args = build_parser().parse_args()
    
    if args.search :
        term       = args.search_term
        tweets     = search_twitter(twitterApi(), term)
        tweet_text = ' '.join(t.text for t in tweets)
        print(normalize_string(term, tweet_text))


