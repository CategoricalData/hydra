
module Hydra.Sources.Yaml.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S


ns :: ModuleName
ns = ModuleName "hydra.yaml.language"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = [toDefinition yamlLanguage],
            moduleDependencies = unqualifiedDep <$> ([Strip.ns] L.++ KernelTypes.kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Language constraints for YAML")}
define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

yamlLanguage :: TypedTermDefinition Language
yamlLanguage = define "yamlLanguage" $
  doc "Language constraints for YAML" $ lets [
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBoolean,
    Variants.literalVariantFloat,
    Variants.literalVariantInteger,
    Variants.literalVariantString],
  "floatTypes">: Sets.fromList $ list [Core.floatTypeFloat64],
  "integerTypes">: Sets.fromList $ list [Core.integerTypeBigint],
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantLiteral,
    Variants.termVariantList,
    Variants.termVariantMap,
    Variants.termVariantOptional,
    Variants.termVariantRecord,
    Variants.termVariantUnit],
  "typeVariants">: Sets.fromList $ list [
    Variants.typeVariantLiteral,
    Variants.typeVariantList,
    Variants.typeVariantMap,
    Variants.typeVariantOptional,
    Variants.typeVariantRecord,
    Variants.typeVariantUnit,
    Variants.typeVariantVoid],
  "typePredicate">: lambda "typ" $ cases _Type (Strip.deannotateType @@ var "typ")
    (Just true) [
    _Type_optional>>: lambda "innerType" $
      cases _Type (var "innerType")
        (Just true) [
        _Type_optional>>: constant false]]] $
  Coders.language
    (Coders.languageName2 (string "hydra.yaml"))
    (Coders.languageConstraints2
      (var "literalVariants")
      (var "floatTypes")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))
