
module Hydra.Sources.Kernel.Terms.Print.Typing where

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

import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore


ns :: ModuleName
ns = ModuleName "hydra.core.print.typing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([PrintCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core.typing types")}
  where
   definitions = [
     toDefinition typeConstraint,
     toDefinition typeSubst]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

typeConstraint :: TypedTermDefinition (TypeConstraint -> String)
typeConstraint = define "typeConstraint" $
  doc "Show a type constraint as a string" $
  lambda "tc" $ lets [
    "ltyp">: Typing.typeConstraintLeft $ var "tc",
    "rtyp">: Typing.typeConstraintRight $ var "tc"] $
    Strings.concat $ list [
      PrintCore.type_ @@ var "ltyp",
      string "≡",
      PrintCore.type_ @@ var "rtyp"]

typeSubst :: TypedTermDefinition (TypeSubst -> String)
typeSubst = define "typeSubst" $
  doc "Show a type substitution as a string" $
  lambda "ts" $ lets [
    "subst">: Typing.unTypeSubst $ var "ts",
    "pairs">: Maps.toList (var "subst" :: TypedTerm (M.Map Name Type)),
    "showPair">: lambda "pair" $ lets [
      "name">: unwrap _Name @@ (Pairs.first $ var "pair"),
      "typ">: Pairs.second $ var "pair"] $
      Strings.concat $ list [
        var "name",
        string "↦",
        PrintCore.type_ @@ var "typ"],
    "pairStrs">: Lists.map (var "showPair") (var "pairs")] $
    Strings.concat $ list [
      string "{",
      Strings.join (string ",") (var "pairStrs"),
      string "}"]
