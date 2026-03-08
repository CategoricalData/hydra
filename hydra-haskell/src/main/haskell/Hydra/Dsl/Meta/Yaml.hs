-- | Meta-DSL for constructing YAML node terms

module Hydra.Dsl.Meta.Yaml where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Ext.Org.Yaml.Model as YM

import qualified Data.Map as M


nodeMapping :: TTerm (M.Map YM.Node YM.Node) -> TTerm YM.Node
nodeMapping = inject YM._Node YM._Node_mapping

nodeScalar :: TTerm YM.Scalar -> TTerm YM.Node
nodeScalar = inject YM._Node YM._Node_scalar

nodeSequence :: TTerm [YM.Node] -> TTerm YM.Node
nodeSequence = inject YM._Node YM._Node_sequence

scalarBool :: TTerm Bool -> TTerm YM.Scalar
scalarBool = inject YM._Scalar YM._Scalar_bool

scalarFloat :: TTerm Double -> TTerm YM.Scalar
scalarFloat = inject YM._Scalar YM._Scalar_float

scalarInt :: TTerm Integer -> TTerm YM.Scalar
scalarInt = inject YM._Scalar YM._Scalar_int

scalarNull :: TTerm YM.Scalar
scalarNull = injectUnit YM._Scalar YM._Scalar_null

scalarStr :: TTerm String -> TTerm YM.Scalar
scalarStr = inject YM._Scalar YM._Scalar_str
