{-# LANGUAGE TemplateHaskell   #-}

module Lifx.State
    ( State(..)
    , defaultState
    ) where

import           Data.Aeson
import           GHC.Generics
import           Data.Aeson.TH (defaultOptions, deriveToJSON, omitNothingFields)


data State = State {
      selector :: Maybe String
    , power :: Maybe String
    , color :: Maybe Double
    , brightness :: Maybe Double
    , duration :: Maybe Double
} deriving (Show)

deriveToJSON defaultOptions { omitNothingFields = True } ''State


defaultState :: State
defaultState = State { selector = Nothing
                , power = Nothing
                , color = Nothing
                , brightness = Nothing
                , duration = Nothing
                }
