module Hydra.Sources.Graphql.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Dsl.Lib.Strings                as Strings
import Hydra.File (_FileExtension)
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

module_ :: Module
module_ = Module {
            moduleName = (ModuleName "hydra.graphql.language"),
            moduleDefinitions = [toDefinition graphqlLanguage, toDefinition graphqlReservedWords],
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Lexical.ns, Strip.ns] L.++ KernelTypes.kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Language constraints and reserved words for GraphQL")}
graphqlLanguage :: TypedTermDefinition Language
graphqlLanguage = define "graphqlLanguage" $
  doc "Language constraints for GraphQL" $ lets [
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBoolean,
    Variants.literalVariantFloat,
    Variants.literalVariantInteger,
    Variants.literalVariantString],
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeFloat64],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeInt32],
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantList,
    Variants.termVariantLiteral,
    Variants.termVariantOptional,
    Variants.termVariantRecord,
    Variants.termVariantInject],
  "typeVariants">: Sets.fromList $ list [
    Variants.typeVariantApplication,
    Variants.typeVariantEither,
    Variants.typeVariantForall,
    Variants.typeVariantFunction,
    Variants.typeVariantList,
    Variants.typeVariantLiteral,
    Variants.typeVariantMap,
    Variants.typeVariantPair,
    Variants.typeVariantSet,
    Variants.typeVariantUnit,
    Variants.typeVariantWrap,
    Variants.typeVariantOptional,
    Variants.typeVariantRecord,
    Variants.typeVariantUnion,
    Variants.typeVariantVariable],
  "typePredicate">: "typ" ~> cases _Type (Strip.deannotateType @@ var "typ")
    (Just true) [
    _Type_optional>>: "inner" ~> cases _Type (Strip.deannotateType @@ var "inner")
      (Just true) [
      _Type_optional>>: constant false]]] $
  Coders.language
    (Coders.languageName2 $ string "hydra.graphql")
    (Coders.languageConstraints2
      (var "literalVariants")
      (var "floatTypes")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))
      (Sets.fromList $ list [
        Coders.languageFeaturePartialApplication,
        Coders.languageFeatureNestedCaseStatements,
        Coders.languageFeatureNestedPolymorphicLetBindings])
      (Coders.caseConventions
        Util.caseConventionUpperSnake Util.caseConventionLowerSnake Util.caseConventionPascal
        Util.caseConventionCamel Util.caseConventionLowerSnake Util.caseConventionLowerSnake
        Util.caseConventionCamel Util.caseConventionCamel Util.caseConventionPascal
        Util.caseConventionPascal)
      (wrap _FileExtension (string "graphql"))

graphqlReservedWords :: TypedTermDefinition (S.Set String)
graphqlReservedWords = define "graphqlReservedWords" $
  doc "A set of reserved words in GraphQL" $
  Sets.fromList $ list $ string <$> ["true", "false"]
