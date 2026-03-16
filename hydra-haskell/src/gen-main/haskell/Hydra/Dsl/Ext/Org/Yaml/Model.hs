-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.yaml.model

module Hydra.Dsl.Ext.Org.Yaml.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Yaml.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

nodeMapping :: (Phantoms.TTerm (M.Map Model.Node Model.Node) -> Phantoms.TTerm Model.Node)
nodeMapping x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Node"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mapping"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nodeScalar :: (Phantoms.TTerm Model.Scalar -> Phantoms.TTerm Model.Node)
nodeScalar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Node"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "scalar"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nodeSequence :: (Phantoms.TTerm [Model.Node] -> Phantoms.TTerm Model.Node)
nodeSequence x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Node"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sequence"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

scalarBool :: (Phantoms.TTerm Bool -> Phantoms.TTerm Model.Scalar)
scalarBool x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Scalar"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bool"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

scalarFloat :: (Phantoms.TTerm Double -> Phantoms.TTerm Model.Scalar)
scalarFloat x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Scalar"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "float"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

scalarInt :: (Phantoms.TTerm Integer -> Phantoms.TTerm Model.Scalar)
scalarInt x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Scalar"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "int"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

scalarNull :: (Phantoms.TTerm Model.Scalar)
scalarNull = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Scalar"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "null"),
    Core.fieldTerm = Core.TermUnit}})))

scalarStr :: (Phantoms.TTerm String -> Phantoms.TTerm Model.Scalar)
scalarStr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.yaml.model.Scalar"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "str"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))
