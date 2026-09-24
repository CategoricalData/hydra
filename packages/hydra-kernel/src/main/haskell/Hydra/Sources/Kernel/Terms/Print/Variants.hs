
module Hydra.Sources.Kernel.Terms.Print.Variants where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (termVariant, typeVariant)
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
ns = ModuleName "hydra.core.print.variants"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core.variants types")}
  where
   definitions = [
     toDefinition termVariant,
     toDefinition typeVariant]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

termVariant :: TypedTermDefinition (TermVariant -> String)
termVariant = define "termVariant" $
  doc "Show a term variant as a string" $
  cases _TermVariant Nothing [
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
  cases _TypeVariant Nothing [
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
