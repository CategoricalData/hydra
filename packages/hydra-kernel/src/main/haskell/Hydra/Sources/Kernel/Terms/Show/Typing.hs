
module Hydra.Sources.Kernel.Terms.Show.Typing where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: ModuleName
ns = ModuleName "hydra.show.typing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ShowCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.typing types")}
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
    Strings.cat $ list [
      ShowCore.type_ @@ var "ltyp",
      string "≡",
      ShowCore.type_ @@ var "rtyp"]

typeSubst :: TypedTermDefinition (TypeSubst -> String)
typeSubst = define "typeSubst" $
  doc "Show a type substitution as a string" $
  lambda "ts" $ lets [
    "subst">: Typing.unTypeSubst $ var "ts",
    "pairs">: Maps.toList (var "subst" :: TypedTerm (M.Map Name Type)),
    "showPair">: lambda "pair" $ lets [
      "name">: unwrap _Name @@ (Pairs.first $ var "pair"),
      "typ">: Pairs.second $ var "pair"] $
      Strings.cat $ list [
        var "name",
        string "↦",
        ShowCore.type_ @@ var "typ"],
    "pairStrs">: Lists.map (var "showPair") (var "pairs")] $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ",") (var "pairStrs"),
      string "}"]
