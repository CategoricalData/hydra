module Hydra.Sources.Kernel.Types.Coders where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (language, languageName, languageConstraints)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.coders"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Graph.module_, Compute.module_, Variants.module_] [Core.module_] $
    Just "Abstractions for paired transformations between languages"
  where
    elements = [
      adapterContext,
      coderDirection,
      language,
      languageConstraints,
      languageName,
      symmetricAdapter,
      traversalOrder,
      typeAdapter]

adapterContext :: Binding
adapterContext = define "AdapterContext" $
  doc "An evaluation context together with a source language and a target language" $
  T.record [
    "graph">:
      doc "The underlying graph of elements and primitives"
      Graph.graph,
    "language">:
      doc "The language being encoded or decoded"
      language,
    "adapters">:
      doc "A map of type names to adapters for those types" $
      T.map Core.name (Compute.adapter
        @@ adapterContext @@ adapterContext
        @@ Core.type_ @@ Core.type_
        @@ Core.term @@ Core.term)]

coderDirection :: Binding
coderDirection = define "CoderDirection" $
  doc "Indicates either the 'out' or the 'in' direction of a coder" $
  T.enum [
    "encode",
    "decode"]

language :: Binding
language = define "Language" $
  doc "A named language together with language-specific constraints" $
  T.record [
    "name">:
      doc "The unique name of the language"
      languageName,
    "constraints">:
      doc "The constraints which characterize the language"
      languageConstraints]

languageConstraints :: Binding
languageConstraints = define "LanguageConstraints" $
  doc "A set of constraints on valid type and term expressions, characterizing a language" $
  T.record [
    "eliminationVariants">:
      doc "All supported elimination variants" $
      T.set Variants.eliminationVariant,
    "literalVariants">:
      doc "All supported literal variants" $
      T.set Variants.literalVariant,
    "floatTypes">:
      doc "All supported float types" $
      T.set Core.floatType,
    "functionVariants">:
      doc "All supported function variants" $
      T.set Variants.functionVariant,
    "integerTypes">:
      doc "All supported integer types" $
      T.set Core.integerType,
    "termVariants">:
      doc "All supported term variants" $
      T.set Variants.termVariant,
    "typeVariants">:
      doc "All supported type variants" $
      T.set Variants.typeVariant,
    "types">:
      doc "A logical set of types, as a predicate which tests a type for inclusion" $
      Core.type_ ~> T.boolean]

languageName :: Binding
languageName = define "LanguageName" $
  doc "The unique name of a language" $
  T.wrap T.string

symmetricAdapter :: Binding
symmetricAdapter = define "SymmetricAdapter" $
  doc "A bidirectional encoder which maps between the same type and term languages on either side" $
  T.forAlls ["s", "t", "v"] $ Compute.adapter @@ "s" @@ "s" @@ "t" @@ "t" @@ "v" @@ "v"

traversalOrder :: Binding
traversalOrder = define "TraversalOrder" $
  doc "Specifies either a pre-order or post-order traversal" $
  T.union [
    "pre">: doc "Pre-order traversal" T.unit,
    "post">: doc "Post-order traversal" T.unit]

typeAdapter :: Binding
typeAdapter = define "TypeAdapter" $
  doc "A function which maps a Hydra type to a symmetric adapter between types and terms" $
  Core.type_ ~> Compute.flow @@ adapterContext @@
    (symmetricAdapter @@ adapterContext @@ Core.type_ @@ Core.term)
