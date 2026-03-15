-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.graph

module Hydra.Dsl.Graph where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

graph :: (M.Map Core.Name Core.Term -> M.Map Core.Name Core.TypeScheme -> M.Map Core.Name Core.TypeVariableMetadata -> S.Set Core.Name -> M.Map Core.Name Core.Term -> M.Map Core.Name Graph.Primitive -> M.Map Core.Name Core.TypeScheme -> S.Set Core.Name -> Graph.Graph)
graph boundTerms boundTypes classConstraints lambdaVariables metadata primitives schemaTypes typeVariables = Graph.Graph {
  Graph.graphBoundTerms = boundTerms,
  Graph.graphBoundTypes = boundTypes,
  Graph.graphClassConstraints = classConstraints,
  Graph.graphLambdaVariables = lambdaVariables,
  Graph.graphMetadata = metadata,
  Graph.graphPrimitives = primitives,
  Graph.graphSchemaTypes = schemaTypes,
  Graph.graphTypeVariables = typeVariables}

graphBoundTerms :: (Graph.Graph -> M.Map Core.Name Core.Term)
graphBoundTerms = Graph.graphBoundTerms

graphBoundTypes :: (Graph.Graph -> M.Map Core.Name Core.TypeScheme)
graphBoundTypes = Graph.graphBoundTypes

graphClassConstraints :: (Graph.Graph -> M.Map Core.Name Core.TypeVariableMetadata)
graphClassConstraints = Graph.graphClassConstraints

graphLambdaVariables :: (Graph.Graph -> S.Set Core.Name)
graphLambdaVariables = Graph.graphLambdaVariables

graphMetadata :: (Graph.Graph -> M.Map Core.Name Core.Term)
graphMetadata = Graph.graphMetadata

graphPrimitives :: (Graph.Graph -> M.Map Core.Name Graph.Primitive)
graphPrimitives = Graph.graphPrimitives

graphSchemaTypes :: (Graph.Graph -> M.Map Core.Name Core.TypeScheme)
graphSchemaTypes = Graph.graphSchemaTypes

graphTypeVariables :: (Graph.Graph -> S.Set Core.Name)
graphTypeVariables = Graph.graphTypeVariables

graphWithBoundTerms :: (Graph.Graph -> M.Map Core.Name Core.Term -> Graph.Graph)
graphWithBoundTerms original newVal = Graph.Graph {
  Graph.graphBoundTerms = newVal,
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithBoundTypes :: (Graph.Graph -> M.Map Core.Name Core.TypeScheme -> Graph.Graph)
graphWithBoundTypes original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = newVal,
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithClassConstraints :: (Graph.Graph -> M.Map Core.Name Core.TypeVariableMetadata -> Graph.Graph)
graphWithClassConstraints original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = newVal,
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithLambdaVariables :: (Graph.Graph -> S.Set Core.Name -> Graph.Graph)
graphWithLambdaVariables original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = newVal,
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithMetadata :: (Graph.Graph -> M.Map Core.Name Core.Term -> Graph.Graph)
graphWithMetadata original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = newVal,
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithPrimitives :: (Graph.Graph -> M.Map Core.Name Graph.Primitive -> Graph.Graph)
graphWithPrimitives original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = newVal,
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithSchemaTypes :: (Graph.Graph -> M.Map Core.Name Core.TypeScheme -> Graph.Graph)
graphWithSchemaTypes original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = newVal,
  Graph.graphTypeVariables = (Graph.graphTypeVariables original)}

graphWithTypeVariables :: (Graph.Graph -> S.Set Core.Name -> Graph.Graph)
graphWithTypeVariables original newVal = Graph.Graph {
  Graph.graphBoundTerms = (Graph.graphBoundTerms original),
  Graph.graphBoundTypes = (Graph.graphBoundTypes original),
  Graph.graphClassConstraints = (Graph.graphClassConstraints original),
  Graph.graphLambdaVariables = (Graph.graphLambdaVariables original),
  Graph.graphMetadata = (Graph.graphMetadata original),
  Graph.graphPrimitives = (Graph.graphPrimitives original),
  Graph.graphSchemaTypes = (Graph.graphSchemaTypes original),
  Graph.graphTypeVariables = newVal}

primitive :: (Core.Name -> Core.TypeScheme -> (Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term) -> Graph.Primitive)
primitive name type_ implementation = Graph.Primitive {
  Graph.primitiveName = name,
  Graph.primitiveType = type_,
  Graph.primitiveImplementation = implementation}

primitiveName :: (Graph.Primitive -> Core.Name)
primitiveName = Graph.primitiveName

primitiveType :: (Graph.Primitive -> Core.TypeScheme)
primitiveType = Graph.primitiveType

primitiveImplementation :: (Graph.Primitive -> Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term)
primitiveImplementation = Graph.primitiveImplementation

primitiveWithName :: (Graph.Primitive -> Core.Name -> Graph.Primitive)
primitiveWithName original newVal = Graph.Primitive {
  Graph.primitiveName = newVal,
  Graph.primitiveType = (Graph.primitiveType original),
  Graph.primitiveImplementation = (Graph.primitiveImplementation original)}

primitiveWithType :: (Graph.Primitive -> Core.TypeScheme -> Graph.Primitive)
primitiveWithType original newVal = Graph.Primitive {
  Graph.primitiveName = (Graph.primitiveName original),
  Graph.primitiveType = newVal,
  Graph.primitiveImplementation = (Graph.primitiveImplementation original)}

primitiveWithImplementation :: (Graph.Primitive -> (Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term) -> Graph.Primitive)
primitiveWithImplementation original newVal = Graph.Primitive {
  Graph.primitiveName = (Graph.primitiveName original),
  Graph.primitiveType = (Graph.primitiveType original),
  Graph.primitiveImplementation = newVal}

termCoder :: (Core.Type -> (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) t0) -> (Context.Context -> t0 -> Either (Context.InContext Error.Error) Core.Term) -> Graph.TermCoder t0)
termCoder type_ encode decode = Graph.TermCoder {
  Graph.termCoderType = type_,
  Graph.termCoderEncode = encode,
  Graph.termCoderDecode = decode}

termCoderType :: (Graph.TermCoder t0 -> Core.Type)
termCoderType = Graph.termCoderType

termCoderEncode :: (Graph.TermCoder t0 -> Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) t0)
termCoderEncode = Graph.termCoderEncode

termCoderDecode :: (Graph.TermCoder t0 -> Context.Context -> t0 -> Either (Context.InContext Error.Error) Core.Term)
termCoderDecode = Graph.termCoderDecode

termCoderWithType :: (Graph.TermCoder t0 -> Core.Type -> Graph.TermCoder t0)
termCoderWithType original newVal = Graph.TermCoder {
  Graph.termCoderType = newVal,
  Graph.termCoderEncode = (Graph.termCoderEncode original),
  Graph.termCoderDecode = (Graph.termCoderDecode original)}

termCoderWithEncode :: (Graph.TermCoder t0 -> (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) t0) -> Graph.TermCoder t0)
termCoderWithEncode original newVal = Graph.TermCoder {
  Graph.termCoderType = (Graph.termCoderType original),
  Graph.termCoderEncode = newVal,
  Graph.termCoderDecode = (Graph.termCoderDecode original)}

termCoderWithDecode :: (Graph.TermCoder t0 -> (Context.Context -> t0 -> Either (Context.InContext Error.Error) Core.Term) -> Graph.TermCoder t0)
termCoderWithDecode original newVal = Graph.TermCoder {
  Graph.termCoderType = (Graph.termCoderType original),
  Graph.termCoderEncode = (Graph.termCoderEncode original),
  Graph.termCoderDecode = newVal}
