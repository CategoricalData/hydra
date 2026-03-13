-- Note: this is an automatically generated file. Do not edit.

-- | The extension to graphs of Hydra's core type system (hydra.core)

module Hydra.Graph where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A graph, or lexical environment which binds names to terms, types, primitives, and metadata
data Graph = 
  Graph {
    -- | The terms bound by all term variables in scope
    graphBoundTerms :: (M.Map Core.Name Core.Term),
    -- | The type schemes of all term variables in scope
    graphBoundTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.
    graphClassConstraints :: (M.Map Core.Name Core.TypeVariableMetadata),
    -- | The set of term variables introduced by specifically by lambdas
    graphLambdaVariables :: (S.Set Core.Name),
    -- | Any additional metadata bound to term variables in scope
    graphMetadata :: (M.Map Core.Name Core.Term),
    -- | All primitive functions and constants by name
    graphPrimitives :: (M.Map Core.Name Primitive),
    -- | All schema types (type schemes) in scope
    graphSchemaTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | The set of type variables introduced specifically by type lambdas
    graphTypeVariables :: (S.Set Core.Name)}

_Graph = (Core.Name "hydra.graph.Graph")

_Graph_boundTerms = (Core.Name "boundTerms")

_Graph_boundTypes = (Core.Name "boundTypes")

_Graph_classConstraints = (Core.Name "classConstraints")

_Graph_lambdaVariables = (Core.Name "lambdaVariables")

_Graph_metadata = (Core.Name "metadata")

_Graph_primitives = (Core.Name "primitives")

_Graph_schemaTypes = (Core.Name "schemaTypes")

_Graph_typeVariables = (Core.Name "typeVariables")

-- | A built-in function or constant
data Primitive = 
  Primitive {
    -- | The unique name of the primitive function
    primitiveName :: Core.Name,
    -- | The type signature of the primitive function
    primitiveType :: Core.TypeScheme,
    -- | A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting.
    primitiveImplementation :: (Context.Context -> Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term)}

_Primitive = (Core.Name "hydra.graph.Primitive")

_Primitive_name = (Core.Name "name")

_Primitive_type = (Core.Name "type")

_Primitive_implementation = (Core.Name "implementation")

-- | A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms.
data TermCoder a = 
  TermCoder {
    -- | The Hydra type of encoded terms
    termCoderType :: Core.Type,
    -- | An encode function from terms to native values
    termCoderEncode :: (Context.Context -> Graph -> Core.Term -> Either (Context.InContext Error.Error) a),
    -- | A decode function from native values to terms
    termCoderDecode :: (Context.Context -> a -> Either (Context.InContext Error.Error) Core.Term)}

_TermCoder = (Core.Name "hydra.graph.TermCoder")

_TermCoder_type = (Core.Name "type")

_TermCoder_encode = (Core.Name "encode")

_TermCoder_decode = (Core.Name "decode")
