module Hydra.Sources.Kernel.Terms.Parse.Docs where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (_DefinitionReference, _DefinitionReference_primitive, _DefinitionReference_term, _DefinitionReference_type, _EntityReference, _EntityReference_definition, _EntityReference_module, _EntityReference_package, _EntityReference_term_expr, _EntityReference_type_expr)
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
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.core.parse.docs"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Parser for Hydra documentation strings, producing DocSegment lists. A bespoke helper parser, not a parse<T> convention function (see docs/specification/index.md, Conventions).")}
  where
   definitions = [
     toDefinition parseDocAnnotation,
     toDefinition parseDocString]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Name constants for hydra.core.docs types (not yet in generated dist)
_DocSegment :: Name
_DocSegment = Name "hydra.core.docs.DocSegment"
_DocSegment_ref :: Name
_DocSegment_ref = Name "ref"
_DocSegment_text :: Name
_DocSegment_text = Name "text"

_EntityReference :: Name
_EntityReference = Name "hydra.core.packaging.EntityReference"
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
_DefinitionReference = Name "hydra.core.packaging.DefinitionReference"
_DefinitionReference_primitive :: Name
_DefinitionReference_primitive = Name "primitive"
_DefinitionReference_term :: Name
_DefinitionReference_term = Name "term"
_DefinitionReference_type :: Name
_DefinitionReference_type = Name "type"

parseDocAnnotation :: TypedTermDefinition (String -> Maybe Term)
parseDocAnnotation = define "parseDocAnnotation" $
  doc ("Parse the content between the doc-escape delimiters into an EntityReference."
    <> " The input is the inner content (tag and optional rhs), e.g. \"type hydra.core.model.Lambda\"."
    <> " Returns nothing for unrecognized tags.") $
  lambda "inner" $
  lets [
    "parts">: Strings.splitOn (string " ") (var "inner"),
    "tag">:   Optionals.withDefault (string "") (Lists.head (var "parts")),
    "rhs">:   Strings.join (string " ") (Lists.drop (int32 1) (var "parts"))] $
  Logic.ifElse (Equality.equal (var "tag") (string "primitive"))
    (just $ inject _EntityReference _EntityReference_definition
      (inject _DefinitionReference _DefinitionReference_primitive (Core.name (var "rhs"))))
  $ Logic.ifElse (Equality.equal (var "tag") (string "term"))
    (just $ inject _EntityReference _EntityReference_definition
      (inject _DefinitionReference _DefinitionReference_term (Core.name (var "rhs"))))
  $ Logic.ifElse (Equality.equal (var "tag") (string "type"))
    (just $ inject _EntityReference _EntityReference_definition
      (inject _DefinitionReference _DefinitionReference_type (Core.name (var "rhs"))))
  $ Logic.ifElse (Equality.equal (var "tag") (string "module"))
    (just $ inject _EntityReference _EntityReference_module (wrap _ModuleName (var "rhs")))
  $ Logic.ifElse (Equality.equal (var "tag") (string "package"))
    (just $ inject _EntityReference _EntityReference_package (wrap _PackageName (var "rhs")))
  $ Logic.ifElse (Equality.equal (var "tag") (string "term-expr"))
    (just $ inject _EntityReference _EntityReference_term_expr (var "rhs"))
  $ Logic.ifElse (Equality.equal (var "tag") (string "type-expr"))
    (just $ inject _EntityReference _EntityReference_type_expr (var "rhs"))
    nothing

parseDocString :: TypedTermDefinition (String -> [Term])
parseDocString = define "parseDocString" $
  doc ("Parse a documentation string into a list of {@type hydra.core.docs.DocSegment}s."
    <> " Recognized doc-escape tags become DocSegment.ref segments (wrapping a {@type hydra.core.packaging.EntityReference});"
    <> " all other text (including unrecognized doc-escape blocks) becomes"
    <> " DocSegment.text segments."
    <> " Adjacent text fragments are not merged.") $
  lambda "s" $
  lets [
    "parts">: Strings.splitOn (string "{@") (var "s"),
    "head_">: Optionals.withDefault (string "") (Lists.head (var "parts")),
    "tail_">: Lists.drop (int32 1) (var "parts"),
    "toSeg">: lambda "part" $
      lets [
        "subparts">: Strings.splitOn (string "}") (var "part"),
        "inner">:    Optionals.withDefault (string "") (Lists.head (var "subparts")),
        "after">:    Strings.join (string "}") (Lists.drop (int32 1) (var "subparts")),
        "mref">:     parseDocAnnotation @@ var "inner"] $
      Optionals.match (var "mref")
        (list [inject _DocSegment _DocSegment_text (Strings.concat2 (string "{@") (var "part"))])
        (lambda "ref" $ Optionals.givens $ list [
          just (inject _DocSegment _DocSegment_ref (var "ref")),
          Logic.ifElse (Equality.equal (var "after") (string ""))
            nothing
            (just (inject _DocSegment _DocSegment_text (var "after")))])] $
  Lists.cons
    (inject _DocSegment _DocSegment_text (var "head_"))
    (Lists.concat (Lists.map (var "toSeg") (var "tail_")))
