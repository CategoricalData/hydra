
module Hydra.Sources.Kernel.Terms.Languages where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (hydraLanguage)
import           Hydra.Core.File (_FileExtension)
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

import qualified Hydra.Sources.Kernel.Terms.Reflect as Reflect


ns :: ModuleName
ns = ModuleName "hydra.core.languages"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = [toDefinition hydraLanguage],
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Reflect.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Language constraints for Hydra Core")}
hydraLanguage :: TypedTermDefinition Language
hydraLanguage = definitionInModule module_ "hydraLanguage" $
  doc "Language constraints for Hydra Core, i.e. no constraints." $ lets [
  "literalVariants">: Sets.fromList (asTerm Reflect.literalVariants),
  "floatTypes">: Sets.fromList (asTerm Reflect.floatTypes),
  "integerTypes">: Sets.fromList (asTerm Reflect.integerTypes),
  "termVariants">: Sets.fromList (asTerm Reflect.termVariants),
  "typeVariants">: Sets.fromList (asTerm Reflect.typeVariants),
  "types">: "t" ~> match _Type (var "t") (Just true) []] $
  Coders.language
    (Coders.languageName2 (string "hydra.core.model"))
    (Coders.languageConstraints2
      (var "literalVariants")
      (var "floatTypes")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "types"))
    -- supportedFeatures: Hydra Core has no restrictions; all features available
    (Sets.fromList $ list [
      Coders.languageFeaturePartialApplication,
      Coders.languageFeatureNestedCaseStatements,
      Coders.languageFeatureNestedPolymorphicLetBindings])
    -- caseConventions: Hydra-canonical (camelCase identifiers, PascalCase types)
    (Coders.caseConventions
      Util.caseConventionUpperSnake -- constant
      Util.caseConventionPascal     -- directory
      Util.caseConventionPascal     -- enumValue
      Util.caseConventionCamel      -- field
      Util.caseConventionPascal     -- file
      Util.caseConventionLowerSnake -- module
      Util.caseConventionCamel      -- term
      Util.caseConventionCamel      -- termVariable
      Util.caseConventionPascal     -- type
      Util.caseConventionCamel)     -- typeVariable
    -- defaultFileExtension: Hydra Core is abstract; use empty
    (wrap _FileExtension (string ""))
