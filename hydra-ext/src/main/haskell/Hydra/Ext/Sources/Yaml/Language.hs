
module Hydra.Ext.Sources.Yaml.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S


ns :: Namespace
ns = Namespace "hydra.ext.org.yaml.language"

module_ :: Module
module_ = Module ns
  [toDefinition yamlLanguage]
  [Rewriting.ns]
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints for YAML"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

yamlLanguage :: TTermDefinition Language
yamlLanguage = define "yamlLanguage" $
  doc "Language constraints for YAML" $ lets [
  "eliminationVariants">: Sets.empty,
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBoolean,
    Variants.literalVariantFloat,
    Variants.literalVariantInteger,
    Variants.literalVariantString],
  "floatTypes">: Sets.fromList $ list [Core.floatTypeBigfloat],
  "functionVariants">: Sets.empty,
  "integerTypes">: Sets.fromList $ list [Core.integerTypeBigint],
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantLiteral,
    Variants.termVariantList,
    Variants.termVariantMap,
    Variants.termVariantMaybe,
    Variants.termVariantRecord,
    Variants.termVariantUnit],
  "typeVariants">: Sets.fromList $ list [
    Variants.typeVariantLiteral,
    Variants.typeVariantList,
    Variants.typeVariantMap,
    Variants.typeVariantMaybe,
    Variants.typeVariantRecord,
    Variants.typeVariantUnit,
    Variants.typeVariantVoid],
  "typePredicate">: lambda "typ" $ cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just true) [
    _Type_maybe>>: lambda "innerType" $
      cases _Type (var "innerType")
        (Just true) [
        _Type_maybe>>: constant false]]] $
  Coders.language
    (Coders.languageName_ (string "hydra.ext.yaml"))
    (Coders.languageConstraints_
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))
