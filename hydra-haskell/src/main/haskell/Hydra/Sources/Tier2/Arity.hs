{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Arity where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


arityDefinition :: String -> TTerm a -> TElement a
arityDefinition = definitionInModule hydraArityModule

hydraArityModule :: Module
hydraArityModule = Module (Namespace "hydra.arity") elements
    []
    [Tier1.hydraCoreModule] $
    Just ("Functions dealing with arguments and arity.")
  where
    elements = [
      el functionArityDef,
      el primitiveArityDef,
      el termArityDef,
      el typeArityDef,
      el uncurryTypeDef]

functionArityDef :: TElement (Function -> Int)
functionArityDef = arityDefinition "functionArity" $
  match _Function Nothing [
    _Function_elimination>>: constant (int32 1),
    _Function_lambda>>: (lambda "i" $ Math.add (int32 1) (var "i")) <.> (ref termArityDef <.> unaryFunction Core.lambdaBody),
    _Function_primitive>>: constant $
      doc "TODO: This function needs to be monadic, so we can look up the primitive" (int32 42)]

primitiveArityDef :: TElement (Primitive -> Int)
primitiveArityDef = arityDefinition "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  (ref typeArityDef <.> unaryFunction Core.typeSchemeType <.> unaryFunction Graph.primitiveType)

termArityDef :: TElement (Term -> Int)
termArityDef = arityDefinition "termArity" $
  match _Term (Just $ int32 0) [
    _Term_application>>: (lambda "xapp" $ Math.sub (var "xapp") (int32 1)) <.> ref termArityDef <.> unaryFunction Core.applicationFunction,
    _Term_function>>: ref functionArityDef]
    -- Note: ignoring variables which might resolve to functions

typeArityDef :: TElement (Type -> Int)
typeArityDef = arityDefinition "typeArity" $
  match _Type (Just $ int32 0) [
    _Type_annotated>>: ref typeArityDef <.> unaryFunction Core.annotatedTypeSubject,
    _Type_application>>: ref typeArityDef <.> unaryFunction Core.applicationTypeFunction,
    _Type_forall>>: ref typeArityDef <.> unaryFunction Core.forallTypeBody,
    _Type_function>>: lambda "f" $
      Math.add (int32 1) (ref typeArityDef @@ (Core.functionTypeCodomain $ var "f"))]

uncurryTypeDef :: TElement (Type -> [Type])
uncurryTypeDef = arityDefinition "uncurryType" $
  doc "Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)" $
  lambda "t" ((match _Type (Just $ list [var "t"]) [
    _Type_annotated>>: ref uncurryTypeDef <.> unaryFunction Core.annotatedTypeSubject,
    _Type_application>>: ref uncurryTypeDef <.> unaryFunction Core.applicationTypeFunction,
    _Type_forall>>: ref uncurryTypeDef <.> unaryFunction Core.forallTypeBody,
    _Type_function>>: lambda "ft" $ Lists.cons
      (Core.functionTypeDomain $ var "ft")
      (ref uncurryTypeDef @@ (Core.functionTypeCodomain $ var "ft"))]) @@ var "t")
