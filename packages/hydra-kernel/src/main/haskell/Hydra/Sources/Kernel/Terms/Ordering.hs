module Hydra.Sources.Kernel.Terms.Ordering where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import qualified Hydra.Core.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.core.ordering"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Rewriting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              ("Graph-aware ordering of object-level values: union variants compare in declared order, and"
                L.++ " annotations are ignored at every depth (#848)."))}
  where
   definitions = [
     toDefinition termOrderKey,
     toDefinition unionVariantIndex]

termOrderKey :: TypedTermDefinition (Graph -> Term -> Either Error Term)
termOrderKey = define "termOrderKey" $
  doc ("Compute the order key of an object-level value. Comparing two keys structurally gives the order"
    <> " of the original values: annotations are removed at every depth, and each injection becomes a pair"
    <> " of its variant's declared index and the key of its payload, so union variants compare in"
    <> " declared order. The key is used only for comparison and is never returned to the caller."
    <> " Fails if an injection's type name does not resolve to a union type in the graph.") $
  "graph" ~> "term" ~>
  "toKey" <~ ("recurse" ~> "t" ~>
    "rewritten" <<~ var "recurse" @@ var "t" $
    match _Term (var "rewritten") (Just $ right $ var "rewritten") [
      _Term_annotated>>: "at" ~> right $ Core.annotatedTermBody $ var "at",
      _Term_inject>>: "inj" ~>
        "field" <~ Core.injectionField (var "inj") $
        "index" <<~ unionVariantIndex @@ var "graph" @@ Core.injectionTypeName (var "inj") @@ Core.fieldName (var "field") $
        right $ Core.termPair $ pair
          (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ var "index")
          (Core.fieldTerm $ var "field")]) $
  Rewriting.rewriteTermM @@ var "toKey" @@ var "term"

unionVariantIndex :: TypedTermDefinition (Graph -> Name -> Name -> Either Error Int)
unionVariantIndex = define "unionVariantIndex" $
  doc ("Find the declared (zero-based) index of a variant of a named union type, looking the type up in"
    <> " the graph's schema. Fails if the name does not resolve to a union type with that variant.") $
  "graph" ~> "typeName" ~> "variant" ~>
  "unionFields" <~ ("t" ~> match _Type (var "t") (Just nothing) [
    _Type_annotated>>: "at" ~> var "unionFields" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~> var "unionFields" @@ Core.forallTypeBody (var "ft"),
    _Type_union>>: "fields" ~> just $ var "fields"]) $
  "shapeError" <~ ("msg" ~> left $ Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError
    (string "union type with the injected variant")
    (Strings.concat $ list [Core.unName $ var "typeName", string ".", Core.unName $ var "variant", string ": ", var "msg"])) $
  Optionals.match (Maps.lookup (var "typeName") (Graph.graphSchemaTypes $ var "graph"))
    (left $ Error.errorResolution $ Error.resolutionErrorNoSuchBinding $ Error.noSuchBindingError $ var "typeName")
    ("ts" ~> Optionals.match (var "unionFields" @@ Core.typeSchemeBody (var "ts"))
      (var "shapeError" @@ string "not a union type")
      ("fields" ~>
        "names" <~ Lists.map ("ft" ~> Core.fieldTypeName (var "ft")) (var "fields") $
        "rest" <~ Lists.dropWhile ("n" ~> Logic.not (Equality.equal (var "n") (var "variant"))) (var "names") $
        Logic.ifElse (Lists.isEmpty $ var "rest")
          (var "shapeError" @@ string "no such variant")
          (right $ Math.sub (Lists.length $ var "names") (Lists.length $ var "rest"))))
