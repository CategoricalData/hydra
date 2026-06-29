module Hydra.Sources.Kernel.Terms.Read.Docs where

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
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.read.docs"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Parser for Hydra documentation strings, producing DocSegment lists")}
  where
   definitions = [
     toDefinition parseDocAnnotation,
     toDefinition parseDocString]

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

parseDocAnnotation :: TypedTermDefinition (String -> Maybe Term)
parseDocAnnotation = define "parseDocAnnotation" $
  doc ("Parse the content between {@...} delimiters into an EntityReference."
    <> " The input is the inner content (tag and optional rhs), e.g. \"type hydra.core.Lambda\"."
    <> " Returns nothing for unrecognized tags.") $
  lambda "inner" $
  lets [
    "parts">: Strings.splitOn (string " ") (var "inner"),
    "tag">:   Optionals.fromOptional (string "") (Lists.maybeHead (var "parts")),
    "rhs">:   Strings.intercalate (string " ") (Lists.drop (int32 1) (var "parts"))] $
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
  doc ("Parse a documentation string into a list of {@type hydra.docs.DocSegment}s."
    <> " Recognized {@tag rhs} escapes become DocSegment.ref segments (wrapping a {@type hydra.packaging.EntityReference});"
    <> " all other text (including unrecognized {@...} blocks) becomes"
    <> " DocSegment.text segments."
    <> " Adjacent text fragments are not merged.") $
  lambda "s" $
  lets [
    "parts">: Strings.splitOn (string "{@") (var "s"),
    "head_">: Optionals.fromOptional (string "") (Lists.maybeHead (var "parts")),
    "tail_">: Lists.drop (int32 1) (var "parts"),
    "toSeg">: lambda "part" $
      lets [
        "subparts">: Strings.splitOn (string "}") (var "part"),
        "inner">:    Optionals.fromOptional (string "") (Lists.maybeHead (var "subparts")),
        "after">:    Strings.intercalate (string "}") (Lists.drop (int32 1) (var "subparts")),
        "mref">:     parseDocAnnotation @@ var "inner"] $
      Optionals.cases (var "mref")
        (list [inject _DocSegment _DocSegment_text (Strings.cat2 (string "{@") (var "part"))])
        (lambda "ref" $ Optionals.cat $ list [
          just (inject _DocSegment _DocSegment_ref (var "ref")),
          Logic.ifElse (Equality.equal (var "after") (string ""))
            nothing
            (just (inject _DocSegment _DocSegment_text (var "after")))])] $
  Lists.cons
    (inject _DocSegment _DocSegment_text (var "head_"))
    (Lists.concat (Lists.map (var "toSeg") (var "tail_")))
