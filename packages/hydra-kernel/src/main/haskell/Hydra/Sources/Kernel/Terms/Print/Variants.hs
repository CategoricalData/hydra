
module Hydra.Sources.Kernel.Terms.Print.Variants where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (termVariant, typeVariant)
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
ns = ModuleName "hydra.print.variants"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.variants types")}
  where
   definitions = [
     toDefinition termVariant,
     toDefinition typeVariant]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

termVariant :: TypedTermDefinition (TermVariant -> String)
termVariant = define "termVariant" $
  doc "Show a term variant as a string" $
  match _TermVariant Nothing [
    _TermVariant_annotated>>: constant $ string "annotated",
    _TermVariant_application>>: constant $ string "application",
    _TermVariant_cases>>: constant $ string "cases",
    _TermVariant_either>>: constant $ string "either",
    _TermVariant_lambda>>: constant $ string "lambda",
    _TermVariant_let>>: constant $ string "let",
    _TermVariant_list>>: constant $ string "list",
    _TermVariant_literal>>: constant $ string "literal",
    _TermVariant_map>>: constant $ string "map",
    _TermVariant_optional>>: constant $ string "optional",
    _TermVariant_pair>>: constant $ string "pair",
    _TermVariant_project>>: constant $ string "project",
    _TermVariant_record>>: constant $ string "record",
    _TermVariant_set>>: constant $ string "set",
    _TermVariant_typeLambda>>: constant $ string "typeLambda",
    _TermVariant_typeApplication>>: constant $ string "typeApplication",
    _TermVariant_inject>>: constant $ string "inject",
    _TermVariant_unit>>: constant $ string "unit",
    _TermVariant_unwrap>>: constant $ string "unwrap",
    _TermVariant_variable>>: constant $ string "variable",
    _TermVariant_wrap>>: constant $ string "wrap"]

typeVariant :: TypedTermDefinition (TypeVariant -> String)
typeVariant = define "typeVariant" $
  doc "Show a type variant as a string" $
  match _TypeVariant Nothing [
    _TypeVariant_annotated>>: constant $ string "annotated",
    _TypeVariant_application>>: constant $ string "application",
    _TypeVariant_effect>>: constant $ string "effect",
    _TypeVariant_either>>: constant $ string "either",
    _TypeVariant_forall>>: constant $ string "forall",
    _TypeVariant_function>>: constant $ string "function",
    _TypeVariant_list>>: constant $ string "list",
    _TypeVariant_literal>>: constant $ string "literal",
    _TypeVariant_map>>: constant $ string "map",
    _TypeVariant_optional>>: constant $ string "optional",
    _TypeVariant_pair>>: constant $ string "pair",
    _TypeVariant_record>>: constant $ string "record",
    _TypeVariant_set>>: constant $ string "set",
    _TypeVariant_union>>: constant $ string "union",
    _TypeVariant_unit>>: constant $ string "unit",
    _TypeVariant_variable>>: constant $ string "variable",
    _TypeVariant_void>>: constant $ string "void",
    _TypeVariant_wrap>>: constant $ string "wrap"]
