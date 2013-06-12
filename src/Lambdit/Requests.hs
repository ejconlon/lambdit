module Lambdit.Requests where

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import Lambdit.Types
import Network.HTTP
import Network.Stream (Result)
import Network.URI    (parseURI)

data Session = Session String deriving (Show)

class Reddit x where
    mine :: x -> IO (JsonResponse MineResponseData)

instance Reddit Session where
    mine _ = undefined

traceShow' :: (Show a) => a -> a
traceShow' x = traceShow x x

redditBase :: String
redditBase = "http://www.reddit.com"

-- This is PLAINTEXT OVER HTTP --
login :: String -> String -> IO (Maybe Session)
login uname pword = parseSession `liftM` submitPostRequest loginUrl ""
    where loginUrl = redditBase ++ "/api/login?user=" ++ uname ++ "&passwd=" ++ pword ++ "&api_type=json"

parseSession :: Result (Response String) -> Maybe Session
parseSession (Left e) = traceShow e Nothing
parseSession (Right r) = if rspCode r == (2,0,0) then traceShow r (traceShow' (readCookie r)) else traceShow r Nothing
    where readCookie :: Response String -> Maybe Session
          readCookie s = Session `liftM` hdrValue `liftM` listToMaybe (filter (\h -> hdrName h == HdrSetCookie) (rspHeaders s))

submitPostRequest :: String -> String -> IO (Result (Response String))
submitPostRequest uriStr body =
  case parseURI uriStr of
    Nothing -> error "url syntax error"
    Just uri -> simpleHTTP $ traceShow' rq
      where
        rq = Request
             { rqURI = uri
             , rqMethod = POST
             , rqHeaders = [ Header HdrContentType "application/x-www-form-urlencoded"
                           , Header HdrContentLength (show (length body))
                           ]
             , rqBody = body
             }
