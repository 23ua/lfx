{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifx where


import           Data.Aeson
import           Data.Aeson.TH ( deriveToJSON
                               , deriveFromJSON
                               )
import           Data.ByteString            as BS (ByteString, append)
import qualified Data.ByteString.Lazy.Char8 as LChar
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Lifx.State as LState
import           Network.HTTP.Conduit
import           Network.HTTP.Simple



data Light = Light {
      id         :: String
    , label      :: String
    , connected  :: Bool
    , power      :: String
    , color      :: Color
    , brightness :: Double
    } deriving (Show)


data Color = Color {
      hue        :: Double
    , saturation :: Double
    , kelvin     :: Integer
} deriving (Show)

data SetStates = SetStates {
      states     :: [LState.State]
    , defaults   :: Maybe LState.State
} deriving (Show)

type Token = ByteString
type LifxSelector = String

-- derive aeson instances
concat <$> mapM (deriveFromJSON defaultOptions) [''Color, ''Light]
deriveToJSON defaultOptions { omitNothingFields = True } ''SetStates


listLights :: Token -> LifxSelector -> IO (Either String [Light])
listLights token selector = do
    response <- httpLifx' token "GET" $ "lights/" ++ selector
    return $ eitherDecode $ Network.HTTP.Simple.getResponseBody response


toggleLights :: Token -> LifxSelector -> IO (Network.HTTP.Simple.Response LChar.ByteString)
toggleLights token selector =
    httpLifx body token "POST" $ "lights/" ++ selector ++ "/toggle"
        where body = "{\"duration\": \"0.0\"}"

changePower :: Token -> LifxSelector -> String -> IO ()
changePower token selector newPower = do
    Right lights <- listLights token selector
    let states' = map (setPower newPower) lights
    _ <- setStates token states'
    return ()


setPower :: String -> Light -> LState.State
setPower newPower light =
    LState.defaultState { LState.power    = Just newPower
                        , LState.selector = Just $ selectorFromLight light
                        }


changeBrightness :: Integral a => Token -> LifxSelector -> a -> IO ()
changeBrightness token selector value = do
    Right lights <- listLights token selector
    let newLights = map (\x -> x {brightness = brightness x + fromIntegral value / 100}) lights
    let states' = map setBrightness'' newLights
    let newStates = SetStates { defaults = Nothing
                              , states = states'
                              }
    _ <- httpLifx (LBS.toStrict . encode $ newStates) token "PUT" "lights/states"
    -- TODO: return pretty request result
    return ()


setBrightness :: Integral a => Token -> LifxSelector -> a -> IO ()
setBrightness token selector value = do
    Right lights <- listLights token selector
    let newLights = map (\x -> x {brightness = fromIntegral value / 100}) lights
    let states' = map setBrightness'' newLights
    let newStates = SetStates { defaults = Nothing
                              , states = states'
                              }
    _ <- httpLifx (LBS.toStrict . encode $ newStates) token "PUT" "lights/states"
    -- TODO: return pretty request result
    return ()

setStates :: Token -> [LState.State] -> IO (Response LBS.ByteString)
setStates token states' =
    let newStates = SetStates { defaults = Nothing
                              , states = states'
                              }
    in httpLifx (LBS.toStrict . encode $ newStates) token "PUT" "lights/states"



setBrightness'' :: Light -> LState.State
setBrightness'' = setBrightness' LState.defaultState

setBrightness' :: LState.State -> Light -> LState.State
setBrightness' state light@Light {brightness = brightness'} =
    state { LState.selector = Just $ selectorFromLight light
          , LState.brightness = Just brightness'
          }

selectorFromLight :: Light -> LifxSelector
selectorFromLight =
    ("id:"++) . Lifx.id

httpLifx' :: ByteString -> BS.ByteString -> String -> IO (Network.HTTP.Simple.Response LChar.ByteString)
httpLifx' = httpLifx ""


httpLifx :: ByteString -> Token -> BS.ByteString -> String ->
            IO (Network.HTTP.Simple.Response LChar.ByteString)
httpLifx body token method' urlSuffix = do
    let request = setRequestMethod method'
          $ Network.HTTP.Simple.setRequestBody (RequestBodyBS body)
          $ setRequestHeader "Authorization"
          ["Bearer " `append` token]
          $ parseRequest_ $ "https://api.lifx.com/v1/" ++ urlSuffix
    httpLBS request
