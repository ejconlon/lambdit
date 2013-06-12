{-# LANGUAGE OverloadedStrings #-}

module Lambdit.Types where

import Control.Applicative ((<$>), (<*>), empty, pure)
import Data.Aeson
import Data.Text (unpack)
import Data.Vector (toList)

data LoginRequest = LoginRequest {
    username :: String,
    passwd :: String,
    apiType :: String
} deriving (Show)

data JsonResponse a = JsonResponse {
    json :: JsonSubResponse a
} deriving (Show)

data JsonSubResponse a = JsonSubResponse {
    kind :: String,
    errors :: [RError],
    xdata :: a
} deriving (Show)

data RError = RError [String] deriving (Show)

data LoginResponseData = LoginData {
    loginModhash :: String,
    cookie :: String
} deriving (Show)

data MineResponseData = MineResponseData {
    mineModhash :: String,
    children :: [String],
    after :: String,
    before :: String
} deriving (Show)

instance FromJSON RError where
    parseJSON (Array v) = RError <$> subMap v
        where 
            subMap vec = sequence $ fmap subParse (toList vec)
            subParse (String t) = pure $ unpack t
            subParse _ = empty
    parseJSON _ = empty

instance (FromJSON a) => FromJSON (JsonSubResponse a) where
    parseJSON (Object v) = JsonSubResponse <$>
        v .: "kind" <*>
        v .: "errors" <*>
        v .: "data"
    parseJSON _ = empty

instance (FromJSON a) => FromJSON (JsonResponse a) where
    parseJSON (Object v) = JsonResponse <$>
        v .: "json"
    parseJSON _ = empty


