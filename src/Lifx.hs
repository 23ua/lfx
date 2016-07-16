{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifx where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LChar
import           GHC.Generics
import           Network.HTTP
import           Network.HTTP.Conduit
import           Network.HTTP.Simple



data LifxLight = LifxLight {
      id         :: String
    , label      :: String
    , connected  :: Bool
    , power      :: String
    , color      :: Color
    , brightness :: Double
    } deriving (Generic, Show)


data Color = Color {
      hue        :: Double
    , saturation :: Double
    , kelvin     :: Integer
} deriving (Generic, Show)


instance FromJSON Color
instance FromJSON LifxLight


listLights :: ByteString -> IO (Either String [LifxLight])
listLights token = do
    response <- httpLifx' token "GET" "lights/all"
    return $ eitherDecode $ Network.HTTP.Simple.getResponseBody response


toggleLights :: ByteString -> String -> IO (Network.HTTP.Simple.Response LChar.ByteString)
toggleLights token selector =
  httpLifx body token "POST" $ "lights/" ++ selector ++ "/toggle"
    where body = "{\"duration\": \"0.0\"}"


httpLifx' :: ByteString -> BS.ByteString -> String -> IO (Network.HTTP.Simple.Response LChar.ByteString)
httpLifx' = httpLifx ""


httpLifx :: ByteString -> ByteString -> BS.ByteString -> String ->
            IO (Network.HTTP.Simple.Response LChar.ByteString)
httpLifx body token method urlSuffix = do
    let request = setRequestMethod method
          $ Network.HTTP.Simple.setRequestBody (RequestBodyBS body)
          $ setRequestHeader "Authorization"
          ["Bearer " `append` token]
          $ parseRequest_ $ "https://api.lifx.com/v1/" ++ urlSuffix
    httpLBS request
