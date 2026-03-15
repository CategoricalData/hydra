-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.yaml.model

module Hydra.Dsl.Ext.Org.Yaml.Model where

import qualified Hydra.Ext.Org.Yaml.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

nodeMapping :: (M.Map Model.Node Model.Node -> Model.Node)
nodeMapping x = (Model.NodeMapping x)

nodeScalar :: (Model.Scalar -> Model.Node)
nodeScalar x = (Model.NodeScalar x)

nodeSequence :: ([Model.Node] -> Model.Node)
nodeSequence x = (Model.NodeSequence x)

scalarBool :: (Bool -> Model.Scalar)
scalarBool x = (Model.ScalarBool x)

scalarFloat :: (Double -> Model.Scalar)
scalarFloat x = (Model.ScalarFloat x)

scalarInt :: (Integer -> Model.Scalar)
scalarInt x = (Model.ScalarInt x)

scalarNull :: Model.Scalar
scalarNull = Model.ScalarNull

scalarStr :: (String -> Model.Scalar)
scalarStr x = (Model.ScalarStr x)
