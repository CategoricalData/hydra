-- | Shared fixtures for the golden-output specs of the hydra-ext / hydra-rust generation coders
-- (Pegasus, C++, Rust; #816). Each coder exposes a `moduleToX` entry point from a Module and its
-- definitions to a map of file path -> generated source text.
module Hydra.ExtCoderTestSupport where

import Hydra.Kernel

import qualified Data.List as L
import qualified Data.Map as M


-- | A module name shared by all fixtures
fixtureModuleName :: ModuleName
fixtureModuleName = ModuleName "example.shapes"

fixtureModule :: [Definition] -> Module
fixtureModule defs = Module {
  moduleName = fixtureModuleName,
  moduleMetadata = Nothing,
  moduleDependencies = [],
  moduleDefinitions = defs}

typeDef :: String -> Type -> Definition
typeDef name body = DefinitionType $ TypeDefinition {
  typeDefinitionName = Name name,
  typeDefinitionMetadata = Nothing,
  typeDefinitionBody = TypeScheme [] body M.empty}

field :: String -> Type -> FieldType
field n = FieldType (Name n)

-- | A record with a required string, a required int32 and an optional boolean
personRecord :: Definition
personRecord = typeDef "example.shapes.Person" $ TypeRecord [
  field "name" (TypeLiteral LiteralTypeString),
  field "age" (TypeLiteral (LiteralTypeInteger IntegerTypeInt32)),
  field "active" (TypeOptional (TypeLiteral LiteralTypeBoolean))]

-- | A union of a string-valued and an int32-valued variant
shapeUnion :: Definition
shapeUnion = typeDef "example.shapes.Shape" $ TypeUnion [
  field "label" (TypeLiteral LiteralTypeString),
  field "size" (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))]

-- | Join lines WITHOUT a trailing newline. Use this (not `unlines`) to build expected strings:
-- `unlines` appends a final "\n" that the printer's output does not have.
expected :: [String] -> String
expected = L.intercalate "\n"
