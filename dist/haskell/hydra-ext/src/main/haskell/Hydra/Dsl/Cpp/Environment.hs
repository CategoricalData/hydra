-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.cpp.environment

module Hydra.Dsl.Cpp.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Cpp.Environment as Environment
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

cppEnvironment :: Phantoms.TTerm (Packaging.Namespaces String) -> Phantoms.TTerm ([Core.Name], (M.Map Core.Name String)) -> Phantoms.TTerm Environment.CppEnvironment
cppEnvironment namespaces boundTypeVariables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Phantoms.unTTerm namespaces)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm boundTypeVariables)}]}))

cppEnvironmentBoundTypeVariables :: Phantoms.TTerm Environment.CppEnvironment -> Phantoms.TTerm ([Core.Name], (M.Map Core.Name String))
cppEnvironmentBoundTypeVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
        Core.projectionField = (Core.Name "boundTypeVariables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cppEnvironmentNamespaces :: Phantoms.TTerm Environment.CppEnvironment -> Phantoms.TTerm (Packaging.Namespaces String)
cppEnvironmentNamespaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
        Core.projectionField = (Core.Name "namespaces")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cppEnvironmentWithBoundTypeVariables :: Phantoms.TTerm Environment.CppEnvironment -> Phantoms.TTerm ([Core.Name], (M.Map Core.Name String)) -> Phantoms.TTerm Environment.CppEnvironment
cppEnvironmentWithBoundTypeVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cppEnvironmentWithNamespaces :: Phantoms.TTerm Environment.CppEnvironment -> Phantoms.TTerm (Packaging.Namespaces String) -> Phantoms.TTerm Environment.CppEnvironment
cppEnvironmentWithNamespaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cpp.environment.CppEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
