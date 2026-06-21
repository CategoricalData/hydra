
module Hydra.Sources.Kernel.Terms.Languages where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (hydraLanguage)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
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
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Reflect as Reflect


ns :: ModuleName
ns = ModuleName "hydra.languages"

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
  "types">: "t" ~> cases _Type (var "t") (Just true) []] $
  Coders.language
    (Coders.languageName2 (string "hydra.core"))
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
