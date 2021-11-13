{-# LANGUAGE DeriveGeneric #-}
module Hydra.Ext.Json.Json
  ( Number(..)
  , Value(..)
  , _Number
  , _Number_exponent
  , _Number_fraction
  , _Number_integer
  , _Value
  , _Value_array
  , _Value_boolean
  , _Value_null
  , _Value_number
  , _Value_object
  , _Value_string
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

data Number
  = Number
    {-| @type integer:
                precision: arbitrary -}
    { numberInteger :: Integer
    {-| @comments Positive numbers only
        @type integer:
                precision: arbitrary -}
    , numberFraction :: Integer
    {-| @type integer:
                precision: arbitrary -}
    , numberExponent :: Integer } deriving (Eq, Generic, Ord, Read, Show)

data Value
  -- | @type list: hydra/ext/json/json.Value
  = ValueArray [Value]
  -- | @type boolean
  | ValueBoolean Bool
  | ValueNull
  -- | @type hydra/ext/json/json.Number
  | ValueNumber Number
  {-| @type map:
              keys: string
              values: hydra/ext/json/json.Value -}
  | ValueObject (Map String Value)
  -- | @type string
  | ValueString String deriving (Eq, Generic, Ord, Read, Show)

_Number = "hydra/ext/json/json.Number" :: String
_Number_exponent = "exponent" :: String
_Number_fraction = "fraction" :: String
_Number_integer = "integer" :: String
_Value = "hydra/ext/json/json.Value" :: String
_Value_array = "array" :: String
_Value_boolean = "boolean" :: String
_Value_null = "null" :: String
_Value_number = "number" :: String
_Value_object = "object" :: String
_Value_string = "string" :: String
