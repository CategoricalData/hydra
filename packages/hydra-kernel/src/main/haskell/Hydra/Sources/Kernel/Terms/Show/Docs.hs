module Hydra.Sources.Kernel.Terms.Show.Docs where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (_DefinitionReference, _DefinitionReference_primitive, _DefinitionReference_term, _DefinitionReference_type, _EntityReference, _EntityReference_definition, _EntityReference_module, _EntityReference_package, _EntityReference_term_expr, _EntityReference_type_expr)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Read.Docs as ReadDocs
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.show.docs"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (ReadDocs.ns : kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.docs types")}
  where
   definitions = [
     toDefinition definitionReference,
     toDefinition docSegment,
     toDefinition docSegmentWith,
     toDefinition docSegments,
     toDefinition docSegmentsWith,
     toDefinition entityReference,
     toDefinition renderDocString,
     toDefinition renderDocStringWith]

renderDocString :: TypedTermDefinition (String -> String)
renderDocString = define "renderDocString" $
  doc "Parse a documentation string and re-render it, converting {@tag rhs} escapes through their canonical form" $
  lambda "s" $ renderDocStringWith @@ (asTerm entityReference) @@ var "s"

renderDocStringWith :: TypedTermDefinition ((Term -> String) -> String -> String)
renderDocStringWith = define "renderDocStringWith" $
  doc ("Parse a documentation string and render it using a custom {@type hydra.packaging.EntityReference} renderer."
    <> " Text segments are passed through; ref segments are rendered by the provided function."
    <> " Unrecognized {@...} blocks are passed through as text.") $
  lambda "render" $ lambda "s" $
  docSegmentsWith @@ var "render" @@ (ReadDocs.parseDocString @@ var "s")

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Name constants for hydra.docs types (not yet in generated dist)
_DocSegment :: Name
_DocSegment = Name "hydra.docs.DocSegment"
_DocSegment_ref :: Name
_DocSegment_ref = Name "ref"
_DocSegment_text :: Name
_DocSegment_text = Name "text"

_EntityReference :: Name
_EntityReference = Name "hydra.packaging.EntityReference"
_EntityReference_definition :: Name
_EntityReference_definition = Name "definition"
_EntityReference_module :: Name
_EntityReference_module = Name "module"
_EntityReference_package :: Name
_EntityReference_package = Name "package"
_EntityReference_term_expr :: Name
_EntityReference_term_expr = Name "termExpr"
_EntityReference_type_expr :: Name
_EntityReference_type_expr = Name "typeExpr"

_DefinitionReference :: Name
_DefinitionReference = Name "hydra.packaging.DefinitionReference"
_DefinitionReference_primitive :: Name
_DefinitionReference_primitive = Name "primitive"
_DefinitionReference_term :: Name
_DefinitionReference_term = Name "term"
_DefinitionReference_type :: Name
_DefinitionReference_type = Name "type"


definitionReference :: TypedTermDefinition (Term -> String)
definitionReference = define "definitionReference" $
  doc "Render a DefinitionReference as its fully-qualified name" $
  match _DefinitionReference Nothing [
    _DefinitionReference_primitive>>: lambda "n" $ Core.unName (var "n"),
    _DefinitionReference_term>>:      lambda "n" $ Core.unName (var "n"),
    _DefinitionReference_type>>:      lambda "n" $ Core.unName (var "n")]

docSegment :: TypedTermDefinition (Term -> String)
docSegment = define "docSegment" $
  doc "Render a single DocSegment back to its source string form" $
  lambda "seg" $ docSegmentWith @@ (asTerm entityReference) @@ var "seg"

docSegmentWith :: TypedTermDefinition ((Term -> String) -> Term -> String)
docSegmentWith = define "docSegmentWith" $
  doc "Render a single DocSegment using a custom {@type hydra.packaging.EntityReference} renderer" $
  lambda "render" $
  match _DocSegment Nothing [
    _DocSegment_ref>>:  lambda "r" $ var "render" @@ var "r",
    _DocSegment_text>>: lambda "s" $ var "s"]

docSegments :: TypedTermDefinition ([Term] -> String)
docSegments = define "docSegments" $
  doc "Render a list of DocSegments back to a plain documentation string" $
  lambda "segs" $ Strings.cat (Lists.map (asTerm docSegment) (var "segs"))

docSegmentsWith :: TypedTermDefinition ((Term -> String) -> [Term] -> String)
docSegmentsWith = define "docSegmentsWith" $
  doc "Render a list of DocSegments using a custom {@type hydra.packaging.EntityReference} renderer" $
  lambda "render" $ lambda "segs" $
  Strings.cat (Lists.map (docSegmentWith @@ var "render") (var "segs"))

entityReference :: TypedTermDefinition (Term -> String)
entityReference = define "entityReference" $
  doc "Render a {@type hydra.packaging.EntityReference} as its {@tag rhs} tag string (without the surrounding braces)" $
  match _EntityReference Nothing [
    _EntityReference_definition>>: lambda "d" $ Strings.cat2
      (match _DefinitionReference Nothing [
        _DefinitionReference_primitive>>: constant (string "primitive"),
        _DefinitionReference_term>>:      constant (string "term"),
        _DefinitionReference_type>>:      constant (string "type")]
        @@ var "d")
      (Strings.cat2 (string " ") (definitionReference @@ var "d")),
    _EntityReference_module>>:    lambda "m" $ Strings.cat2 (string "module ")    (unwrap _ModuleName @@ var "m"),
    _EntityReference_package>>:   lambda "p" $ Strings.cat2 (string "package ")   (unwrap _PackageName @@ var "p"),
    _EntityReference_term_expr>>: lambda "s" $ Strings.cat2 (string "term-expr ") (var "s"),
    _EntityReference_type_expr>>: lambda "s" $ Strings.cat2 (string "type-expr ") (var "s")]
