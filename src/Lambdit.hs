module Lambdit where

data LoginRequest = LoginRequest {
    username :: String,
    passwd :: String,
    apiType :: String
} deriving (Show)

data JsonResponse a = JsonResponse {
    json :: JsonSubResponse a
} deriving (Show)

data JsonSubResponse a = JsonSubResponse {
    errors :: [Error],
    xdata :: a
} deriving (Show)

data Error = Error [String] deriving (Show)

data LoginResponseData = LoginData {
    modhash :: String,
    cookie :: String
} deriving (Show)



