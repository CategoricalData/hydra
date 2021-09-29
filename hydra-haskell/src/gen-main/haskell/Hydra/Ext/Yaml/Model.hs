{-# LANGUAGE DeriveGeneric #-}
{-| A basic YAML representation model
    
    @comments Based on https://yaml.org/spec/1.2/spec.html
              The Serialization and Presentation properties of YAML, including
    directives, comments, anchors, style, formatting, and aliases, are not
    supported by this model. In addition, tags are omitted from this model, and
    non-standard scalars are unsupported. -}
module Hydra.Ext.Yaml.Model
  ( Node(..)
  , Scalar(..)
  , _Node
  , _Node_mapping
  , _Node_scalar
  , _Node_sequence
  , _Scalar
  , _Scalar_bool
  , _Scalar_float
  , _Scalar_int
  , _Scalar_null
  , _Scalar_str
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

{-| @comments Every YAML node has an optional scalar tag or non-specific tag
    (omitted from this model) -}
data Node
  {-| @comments Failsafe schema: tag:yaml.org,2002:map
      @type map:
              keys: hydra/ext/yaml/model.Node
              values: hydra/ext/yaml/model.Node -}
  = NodeMapping (Map Node Node)
  -- | @type hydra/ext/yaml/model.Scalar
  | NodeScalar Scalar
  {-| @comments Failsafe schema: tag:yaml.org,2002:seq
      @type list: hydra/ext/yaml/model.Node -}
  | NodeSequence [Node] deriving (Eq, Generic, Ord, Read, Show)

{-| A union of scalars supported in the YAML failsafe and JSON schemas. Other
    scalars are not supported here -}
data Scalar
  {-| Represents a true/false value
      
      @comments JSON schema: tag:yaml.org,2002:bool
      @type boolean -}
  = ScalarBool Bool
  {-| Represents an approximation to real numbers
      
      @comments JSON schema: tag:yaml.org,2002:float
                In addition to arbitrary-precision floating-point numbers in
      scientific notation, YAML allows for three special values, which are not
      supported here: positive and negative infinity (.inf and -.inf), and "not a
      number (.nan)
      @type float:
              precision: arbitrary -}
  | ScalarFloat Double
  {-| Represents arbitrary sized finite mathematical integers
      
      @comments JSON schema: tag:yaml.org,2002:int
      @type integer:
              precision: arbitrary -}
  | ScalarInt Integer
  {-| Represents the lack of a value
      
      @comments JSON schema: tag:yaml.org,2002:null -}
  | ScalarNull
  {-| @comments Failsafe schema: tag:yaml.org,2002:str
      @type string -}
  | ScalarStr String deriving (Eq, Generic, Ord, Read, Show)

_Node = "hydra/ext/yaml/model.Node" :: String
_Node_mapping = "mapping" :: String
_Node_scalar = "scalar" :: String
_Node_sequence = "sequence" :: String
_Scalar = "hydra/ext/yaml/model.Scalar" :: String
_Scalar_bool = "bool" :: String
_Scalar_float = "float" :: String
_Scalar_int = "int" :: String
_Scalar_null = "null" :: String
_Scalar_str = "str" :: String
