
module Hydra.Sources.Kernel.Terms.Environment where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  definitionAsTypeApplicationTerm,
  graphAsLet,
  graphAsTerm,
  graphAsTypes,
  partitionDefinitions,
  schemaGraphToTypingEnvironment,
  termAsBindings,
  typesToDefinitions,
  withLambdaContext,
  withLetContext,
  withTypeLambdaContext)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Scoping      as Scoping
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip

import qualified Hydra.Sources.Decode.Core  as DecodeCore
import qualified Hydra.Sources.Encode.Core  as EncodeCore


ns :: Namespace
ns = Namespace "hydra.environment"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Lexical.ns, moduleNamespace DecodeCore.module_, moduleNamespace EncodeCore.module_, Scoping.ns, Strip.ns]
    kernelTypesNamespaces $
    Just ("Graph to type environment conversions")
  where
    elements = [
      toDefinition definitionAsTypeApplicationTerm,
      toDefinition graphAsLet,
      toDefinition graphAsTerm,
      toDefinition graphAsTypes,
      toDefinition partitionDefinitions,
      toDefinition schemaGraphToTypingEnvironment,
      toDefinition termAsBindings,
      toDefinition typesToDefinitions,
      toDefinition withLambdaContext,
      toDefinition withLetContext,
      toDefinition withTypeLambdaContext]

definitionAsTypeApplicationTerm :: TTermDefinition (Context -> Binding -> Either (InContext Error) TypeApplicationTerm)
definitionAsTypeApplicationTerm = define "definitionAsTypeApplicationTerm" $
  doc "Convert a definition to a typed term" $
  "cx" ~> "el" ~>
  Maybes.maybe (Ctx.failInContext (Error.errorOther $ Error.otherError (string "missing element type")) (var "cx"))
    ("ts" ~> right (Core.typeApplicationTerm (Core.bindingTerm (var "el")) (Core.typeSchemeType (var "ts"))))
    (Core.bindingType (var "el"))

graphAsLet :: TTermDefinition ([Binding] -> Term -> Let)
graphAsLet = define "graphAsLet" $
  doc "Convert bindings and a body to a let expression" $
  "bindings" ~> "body" ~>
  Core.let_
    (var "bindings")
    (var "body")

graphAsTerm :: TTermDefinition ([Binding] -> Term -> Term)
graphAsTerm = define "graphAsTerm" $
  doc "Convert bindings and a body to a term, using let-term duality" $
  "bindings" ~> "body" ~> Core.termLet (graphAsLet @@ var "bindings" @@ var "body")

graphAsTypes :: TTermDefinition (Context -> Graph -> [Binding] -> Either (InContext DecodingError) (M.Map Name Type))
graphAsTypes = define "graphAsTypes" $
  doc "Decode a list of type-encoding bindings into a map of named types" $
  "cx" ~> "graph" ~> "els" ~>
  "toPair" <~ ("el" ~>
    Eithers.map
      ("typ" ~> pair (Core.bindingName $ var "el") (var "typ"))
      (Ctx.withContext (var "cx") (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm $ var "el")))) $
  Eithers.map (unaryFunction Maps.fromList) (Eithers.mapList (var "toPair") (var "els"))

partitionDefinitions :: TTermDefinition ([Definition] -> ([TypeDefinition], [TermDefinition]))
partitionDefinitions = define "partitionDefinitions" $
  doc "Partition a list of definitions into type definitions and term definitions" $
  "defs" ~>
  "getType" <~ ("def" ~> cases _Definition (var "def") Nothing [
    _Definition_type>>: "td" ~> just (var "td"),
    _Definition_term>>: "_" ~> nothing]) $
  "getTerm" <~ ("def" ~> cases _Definition (var "def") Nothing [
    _Definition_type>>: "_" ~> nothing,
    _Definition_term>>: "td" ~> just (var "td")]) $
  pair
    (Maybes.cat $ Lists.map (var "getType") (var "defs"))
    (Maybes.cat $ Lists.map (var "getTerm") (var "defs"))

schemaGraphToTypingEnvironment :: TTermDefinition (Context -> Graph -> Either (InContext Error) (M.Map Name TypeScheme))
schemaGraphToTypingEnvironment = define "schemaGraphToTypingEnvironment" $
  doc "Convert a schema graph to a typing environment (Either version)" $
  "cx" ~> "g" ~>
  "toTypeScheme" <~ ("vars" ~> "typ" ~> cases _Type (Strip.deannotateType @@ var "typ")
    (Just (Core.typeScheme (Lists.reverse (var "vars")) (var "typ") Phantoms.nothing)) [
    _Type_forall>>: "ft" ~> var "toTypeScheme"
      @@ Lists.cons (Core.forallTypeParameter (var "ft")) (var "vars")
      @@ Core.forallTypeBody (var "ft")]) $
  "decodeType" <~ ("term" ~>
    Ctx.withContext (var "cx")
      (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
        (decoderFor _Type @@ var "g" @@ var "term"))) $
  "decodeTypeScheme" <~ ("term" ~>
    Ctx.withContext (var "cx")
      (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
        (decoderFor _TypeScheme @@ var "g" @@ var "term"))) $
  "toPair" <~ ("el" ~>
    "forTerm" <~ ("term" ~> cases _Term (var "term") (Just (right nothing)) [
      _Term_record>>: "r" ~>
        Logic.ifElse
          (Equality.equal (Core.recordTypeName (var "r")) (Core.nameLift _TypeScheme))
          (Eithers.map (unaryFunction just) (var "decodeTypeScheme" @@ Core.bindingTerm (var "el")))
          (right nothing),
      _Term_union>>: "i" ~>
        Logic.ifElse (Equality.equal (Core.injectionTypeName (var "i")) (Core.nameLift _Type))
          (Eithers.map
            ("decoded" ~> just (var "toTypeScheme" @@ list ([] :: [TTerm Name]) @@ var "decoded"))
            (var "decodeType" @@ Core.bindingTerm (var "el")))
          (right nothing)]) $
    "mts" <<~  optCases (Core.bindingType (var "el"))
      (Eithers.map ("typ" ~> just $ Scoping.fTypeToTypeScheme @@ var "typ") $ var "decodeType" @@ (Core.bindingTerm (var "el")))
      ("ts" ~> Logic.ifElse
        (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _TypeScheme)) Phantoms.nothing))
        (Eithers.map (unaryFunction just) (var "decodeTypeScheme" @@ Core.bindingTerm (var "el")))
        (Logic.ifElse
          (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _Type)) Phantoms.nothing))
          (Eithers.map ("decoded" ~> just (var "toTypeScheme" @@ list ([] :: [TTerm Name]) @@ var "decoded")) (var "decodeType" @@ Core.bindingTerm (var "el")))
          (var "forTerm" @@ (Strip.deannotateTerm @@ (Core.bindingTerm (var "el")))))) $
    right $ Maybes.map ("ts" ~> pair (Core.bindingName (var "el")) (var "ts")) (var "mts")) $
  Eithers.map ("mpairs" ~> Maps.fromList (Maybes.cat (var "mpairs")))
    (Eithers.mapList (var "toPair") (Lexical.graphToBindings @@ var "g"))

-- Note: this is lossy, as it throws away the term body
termAsBindings :: TTermDefinition (Term -> [Binding])
termAsBindings = define "termAsBindings" $
  doc "Extract the bindings from a let term, or return an empty list for other terms" $
  "term" ~> cases _Term (Strip.deannotateTerm @@ var "term")
    (Just (list ([] :: [TTerm Binding]))) [
    _Term_let>>: "lt" ~> Core.letBindings (var "lt")]

typesToDefinitions :: TTermDefinition (M.Map Name Type -> [Binding])
typesToDefinitions = define "typesToDefinitions" $
  doc "Encode a map of named types to a list of bindings" $
  "typeMap" ~>
  "toElement" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    Core.binding
      (var "name")
      (encoderFor _Type @@ (Pairs.second $ var "pair"))
      nothing) $
  Lists.map (var "toElement") $ Maps.toList $ var "typeMap"

withLambdaContext :: TTermDefinition ((e -> Graph) -> (Graph -> e -> f) -> e -> Lambda -> (f -> a) -> a)
withLambdaContext = define "withLambdaContext" $
  doc "Execute a computation in the context of a lambda body, extending the type context with the lambda parameter" $
  "getContext" ~> "setContext" ~> "env" ~> "lam" ~> "body" ~>
  "newContext" <~ Scoping.extendGraphForLambda @@ (var "getContext" @@ var "env") @@ var "lam" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")

withLetContext :: TTermDefinition ((e -> Graph) -> (Graph -> e -> f) -> (Graph -> Binding -> Maybe Term) -> e -> Let -> (f -> a) -> a)
withLetContext = define "withLetContext" $
  doc "Execute a computation in the context of a let body, extending the type context with the let bindings" $
  "getContext" ~> "setContext" ~> "forBinding" ~> "env" ~> "letrec" ~> "body" ~>
  "newContext" <~ Scoping.extendGraphForLet @@ var "forBinding" @@ (var "getContext" @@ var "env") @@ var "letrec" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")

withTypeLambdaContext :: TTermDefinition ((e -> Graph) -> (Graph -> e -> f) -> e -> TypeLambda -> (f -> a) -> a)
withTypeLambdaContext = define "withTypeLambdaContext" $
  doc "Execute a computation in the context of a type lambda body, extending the type context with the type parameter" $
  "getContext" ~> "setContext" ~> "env" ~> "tlam" ~> "body" ~>
  "newContext" <~ Scoping.extendGraphForTypeLambda @@ (var "getContext" @@ var "env") @@ var "tlam" $
  var "body" @@ (var "setContext" @@ var "newContext" @@ var "env")
