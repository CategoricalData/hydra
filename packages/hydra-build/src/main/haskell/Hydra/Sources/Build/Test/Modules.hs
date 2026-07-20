{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for the pure module-list utilities in hydra.build.modules (#529).
--
-- Each case renders both the actual (function-applied) and expected values to a
-- String via PrintCore helpers, following the universalCase convention: the test
-- runner reduces both string-typed thunks and compares. Rendering to String is
-- required — universalCase's fields are String-typed, so raw list/module/optional
-- terms cannot be passed directly.
module Hydra.Sources.Build.Test.Modules where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms         as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding ((++))
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                ((@@))
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Dsl.Packaging          as Packaging
import qualified Hydra.Dsl.Lib.Strings        as Strings
import qualified Hydra.Dsl.Lib.Optionals      as Optionals
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Sources.Build.Modules as BuildModules
import qualified Hydra.Sources.Kernel.Terms.Scoping       as Scoping
import qualified Hydra.Sources.Kernel.Terms.Print.Core     as PrintCore


ns :: ModuleName
ns = ModuleName "hydra.test.build.modules"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([BuildModules.ns, Scoping.ns, PrintCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for the pure module-list utilities in hydra.build.modules")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

----------------------------------------
-- Toy building blocks

-- A module name from a raw string.
modName :: String -> TypedTerm ModuleName
modName s = Packaging.moduleName2 (Phantoms.string s)

-- A term definition whose body is a bare int, carrying a (vacuous) signature.
typedTermDef :: String -> TypedTerm Definition
typedTermDef nm = Packaging.definitionTerm
  (Packaging.termDefinition
    (Core.name $ Phantoms.string nm)
    Phantoms.nothing
    (Phantoms.just (Scoping.typeSchemeToTermSignature @@ intScheme))
    (Terms.int32 42))

-- A term definition with no signature.
untypedTermDef :: String -> TypedTerm Definition
untypedTermDef nm = Packaging.definitionTerm
  (Packaging.termDefinition
    (Core.name $ Phantoms.string nm)
    Phantoms.nothing
    Phantoms.nothing
    (Terms.int32 42))

-- A type definition.
aTypeDef :: String -> TypedTerm Definition
aTypeDef nm = Packaging.definitionType
  (Packaging.typeDefinition
    (Core.name $ Phantoms.string nm)
    Phantoms.nothing
    (T.mono T.int32))

-- A primitive definition (pure, total, no default implementation).
aPrimDef :: String -> TypedTerm Definition
aPrimDef nm = Packaging.definitionPrimitive
  (Packaging.primitiveDefinition
    (Core.name $ Phantoms.string nm)
    Phantoms.nothing
    (Scoping.typeSchemeToTermSignature @@ intScheme)
    Phantoms.true
    Phantoms.true
    Phantoms.nothing)

intScheme :: TypedTerm TypeScheme
intScheme = T.mono T.int32

-- A module from a name and definitions.
mod' :: String -> [TypedTerm Definition] -> TypedTerm Module
mod' name defs = Packaging.module_
  (modName name)
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list defs)

----------------------------------------
-- Show helpers (render values to String for universalCase comparison)

-- Identity show for strings.
idStr :: TypedTerm (String -> String)
idStr = Phantoms.lambda "s" (Phantoms.var "s")

-- Show a list of strings.
showStrs :: TypedTerm [String] -> TypedTerm String
showStrs xs = PrintCore.list_ @@ idStr @@ xs

-- Show an optional string.
showOptStr :: TypedTerm (Maybe String) -> TypedTerm String
showOptStr mx = PrintCore.optional_ @@ idStr @@ mx

-- Show a list of modules by name only (sufficient for filter selection tests).
showModuleNames :: TypedTerm [Module] -> TypedTerm String
showModuleNames mods = PrintCore.list_ @@ nameFn @@ mods
  where
    nameFn :: TypedTerm (Module -> String)
    nameFn = Phantoms.lambda "m" (Packaging.unModuleName (Packaging.moduleName (Phantoms.var "m")))

-- Show a module's definitions in a shape that reflects term-signature presence,
-- so stripTermTypes' signature-clearing is observable. Each definition renders as:
--   term:<name>:sig=<yes|no>   |   type:<name>   |   prim:<name>
showModuleDetail :: TypedTerm Module -> TypedTerm String
showModuleDetail m = Strings.cat (Phantoms.list [
    Packaging.unModuleName (Packaging.moduleName m),
    Phantoms.string "|",
    PrintCore.list_ @@ defFn @@ Packaging.moduleDefinitions m])
  where
    defFn :: TypedTerm (Definition -> String)
    defFn = Phantoms.lambda "d" $ Phantoms.cases _Definition (Phantoms.var "d") Nothing [
      _Definition_type Phantoms.>>: Phantoms.lambda "td" (Strings.cat (Phantoms.list [
        Phantoms.string "type:", Core.unName (Packaging.typeDefinitionName (Phantoms.var "td"))])),
      _Definition_primitive Phantoms.>>: Phantoms.lambda "pd" (Strings.cat (Phantoms.list [
        Phantoms.string "prim:", Core.unName (Packaging.primitiveDefinitionName (Phantoms.var "pd"))])),
      _Definition_term Phantoms.>>: Phantoms.lambda "td" (Strings.cat (Phantoms.list [
        Phantoms.string "term:",
        Core.unName (Packaging.termDefinitionName (Phantoms.var "td")),
        Phantoms.string ":sig=",
        Optionals.cases (Packaging.termDefinitionSignature (Phantoms.var "td"))
          (Phantoms.string "no")
          (Phantoms.lambda "_" (Phantoms.string "yes"))]))]

-- Show a list of modules, each via showModuleDetail (for stripAllTermTypes tests).
showModulesDetail :: TypedTerm [Module] -> TypedTerm String
showModulesDetail mods = PrintCore.list_ @@ Phantoms.lambda "m" (showModuleDetail (Phantoms.var "m")) @@ mods

----------------------------------------
-- Test groups

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for the pure module-list utilities in hydra.build.modules" $
    supergroup "build.modules" [
      dedupPreservingOrderGroup,
      filterKernelModulesGroup,
      filterTypeModulesGroup,
      secondLevelDirGroup,
      stripAllTermTypesGroup,
      stripTermTypesGroup]

-- dedupPreservingOrder: first-occurrence order preserved.
dedupPreservingOrderGroup :: TypedTerm TestGroup
dedupPreservingOrderGroup = subgroup "dedupPreservingOrder" [
    universalCase "empty"
      (showStrs (BuildModules.dedupPreservingOrder @@ Phantoms.list ([] :: [TypedTerm String])))
      (showStrs (Phantoms.list ([] :: [TypedTerm String]))),
    universalCase "no duplicates unchanged"
      (showStrs (BuildModules.dedupPreservingOrder @@ Phantoms.list [Phantoms.string "a", Phantoms.string "b", Phantoms.string "c"]))
      (showStrs (Phantoms.list [Phantoms.string "a", Phantoms.string "b", Phantoms.string "c"])),
    universalCase "duplicates dropped, first-occurrence order kept"
      (showStrs (BuildModules.dedupPreservingOrder @@ Phantoms.list [Phantoms.string "b", Phantoms.string "a", Phantoms.string "b", Phantoms.string "c", Phantoms.string "a"]))
      (showStrs (Phantoms.list [Phantoms.string "b", Phantoms.string "a", Phantoms.string "c"]))]

-- filterKernelModules: keep only modules NOT under hydra.* and NOT under hydra.json.yaml.*.
filterKernelModulesGroup :: TypedTerm TestGroup
filterKernelModulesGroup = subgroup "filterKernelModules" [
    universalCase "hydra.* dropped, non-hydra kept"
      (showModuleNames (BuildModules.filterKernelModules @@ Phantoms.list [
        mod' "hydra.core" [aTypeDef "hydra.core.Foo"],
        mod' "example.foo" [aTypeDef "example.foo.Bar"]]))
      (showModuleNames (Phantoms.list [mod' "example.foo" [aTypeDef "example.foo.Bar"]])),
    universalCase "hydra.json.yaml.* dropped"
      (showModuleNames (BuildModules.filterKernelModules @@ Phantoms.list [
        mod' "hydra.json.yaml.model" [aTypeDef "hydra.json.yaml.model.X"],
        mod' "other.pkg" [aTypeDef "other.pkg.Y"]]))
      (showModuleNames (Phantoms.list [mod' "other.pkg" [aTypeDef "other.pkg.Y"]])),
    universalCase "a name merely containing hydra is kept"
      (showModuleNames (BuildModules.filterKernelModules @@ Phantoms.list [
        mod' "myhydra.x" [aTypeDef "myhydra.x.Z"]]))
      (showModuleNames (Phantoms.list [mod' "myhydra.x" [aTypeDef "myhydra.x.Z"]])),
    universalCase "empty input"
      (showModuleNames (BuildModules.filterKernelModules @@ Phantoms.list ([] :: [TypedTerm Module])))
      (showModuleNames (Phantoms.list ([] :: [TypedTerm Module])))]

-- filterTypeModules: keep only modules with at least one type definition.
filterTypeModulesGroup :: TypedTerm TestGroup
filterTypeModulesGroup = subgroup "filterTypeModules" [
    universalCase "type-def module kept"
      (showModuleNames (BuildModules.filterTypeModules @@ Phantoms.list [mod' "a.types" [aTypeDef "a.types.T"]]))
      (showModuleNames (Phantoms.list [mod' "a.types" [aTypeDef "a.types.T"]])),
    universalCase "term-only module dropped"
      (showModuleNames (BuildModules.filterTypeModules @@ Phantoms.list [mod' "a.terms" [untypedTermDef "a.terms.f"]]))
      (showModuleNames (Phantoms.list ([] :: [TypedTerm Module]))),
    universalCase "empty module dropped"
      (showModuleNames (BuildModules.filterTypeModules @@ Phantoms.list [mod' "a.empty" []]))
      (showModuleNames (Phantoms.list ([] :: [TypedTerm Module]))),
    universalCase "mixed module (type + term) kept"
      (showModuleNames (BuildModules.filterTypeModules @@ Phantoms.list [mod' "a.mixed" [untypedTermDef "a.mixed.f", aTypeDef "a.mixed.T"]]))
      (showModuleNames (Phantoms.list [mod' "a.mixed" [untypedTermDef "a.mixed.f", aTypeDef "a.mixed.T"]]))]

-- secondLevelDir: first two slash-separated segments joined by "/".
secondLevelDirGroup :: TypedTerm TestGroup
secondLevelDirGroup = subgroup "secondLevelDir" [
    universalCase "two segments plus file"
      (showOptStr (BuildModules.secondLevelDir @@ Phantoms.string "hydra/java/coder.json"))
      (showOptStr (Phantoms.just (Phantoms.string "hydra/java"))),
    universalCase "deep path keeps only first two"
      (showOptStr (BuildModules.secondLevelDir @@ Phantoms.string "hydra/python/model/core.json"))
      (showOptStr (Phantoms.just (Phantoms.string "hydra/python"))),
    universalCase "exactly two segments"
      (showOptStr (BuildModules.secondLevelDir @@ Phantoms.string "hydra/java"))
      (showOptStr (Phantoms.just (Phantoms.string "hydra/java"))),
    universalCase "one segment is nothing"
      (showOptStr (BuildModules.secondLevelDir @@ Phantoms.string "hydra"))
      (showOptStr (Phantoms.nothing :: TypedTerm (Maybe String))),
    universalCase "empty string is nothing"
      (showOptStr (BuildModules.secondLevelDir @@ Phantoms.string ""))
      (showOptStr (Phantoms.nothing :: TypedTerm (Maybe String))),
    -- Trailing separator: Python-reference semantics (empty final segment kept),
    -- so "hydra/" is two segments and yields Just "hydra/". The Java reference
    -- drops trailing empties (would give none); this port matches Python.
    universalCase "trailing separator kept (Python semantics)"
      (showOptStr (BuildModules.secondLevelDir @@ Phantoms.string "hydra/"))
      (showOptStr (Phantoms.just (Phantoms.string "hydra/")))]

-- stripTermTypes: term signatures cleared, type defs unchanged.
stripTermTypesGroup :: TypedTerm TestGroup
stripTermTypesGroup = subgroup "stripTermTypes" [
    universalCase "term signature cleared, body preserved"
      (showModuleDetail (BuildModules.stripTermTypes @@ mod' "a.terms" [typedTermDef "a.terms.f"]))
      (showModuleDetail (mod' "a.terms" [untypedTermDef "a.terms.f"])),
    universalCase "type definition passes through unchanged"
      (showModuleDetail (BuildModules.stripTermTypes @@ mod' "a.types" [aTypeDef "a.types.T"]))
      (showModuleDetail (mod' "a.types" [aTypeDef "a.types.T"])),
    universalCase "mixed module: term stripped, type kept"
      (showModuleDetail (BuildModules.stripTermTypes @@ mod' "a.mixed" [typedTermDef "a.mixed.f", aTypeDef "a.mixed.T"]))
      (showModuleDetail (mod' "a.mixed" [untypedTermDef "a.mixed.f", aTypeDef "a.mixed.T"])),
    universalCase "primitive definition passes through unchanged"
      (showModuleDetail (BuildModules.stripTermTypes @@ mod' "a.prims" [aPrimDef "a.prims.p"]))
      (showModuleDetail (mod' "a.prims" [aPrimDef "a.prims.p"]))]

-- stripAllTermTypes: stripTermTypes applied across a list of modules.
stripAllTermTypesGroup :: TypedTerm TestGroup
stripAllTermTypesGroup = subgroup "stripAllTermTypes" [
    universalCase "empty list"
      (showModulesDetail (BuildModules.stripAllTermTypes @@ Phantoms.list ([] :: [TypedTerm Module])))
      (showModulesDetail (Phantoms.list ([] :: [TypedTerm Module]))),
    universalCase "each module's term signatures cleared"
      (showModulesDetail (BuildModules.stripAllTermTypes @@ Phantoms.list [
        mod' "a.terms" [typedTermDef "a.terms.f"],
        mod' "b.mixed" [typedTermDef "b.mixed.g", aTypeDef "b.mixed.T"]]))
      (showModulesDetail (Phantoms.list [
        mod' "a.terms" [untypedTermDef "a.terms.f"],
        mod' "b.mixed" [untypedTermDef "b.mixed.g", aTypeDef "b.mixed.T"]]))]
