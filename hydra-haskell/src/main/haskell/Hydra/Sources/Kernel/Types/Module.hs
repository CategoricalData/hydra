module Hydra.Sources.Kernel.Types.Module where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Graph as Graph


ns :: Namespace
ns = Namespace "hydra.module"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Graph.module_] [Core.module_] $
    Just "A model for Hydra namespaces and modules"
  where
    elements = [
      definition,
      fileExtension,
      library,
      module',
      namespace,
      namespaces,
      qualifiedName,
      termDefinition,
      typeDefinition]

definition :: Binding
definition = define "Definition" $
  doc "A definition, which may be either a term or type definition" $
  T.union [
    "term">:
      doc "A term definition"
      termDefinition,
    "type">:
      doc "A type definition"
      typeDefinition]

fileExtension :: Binding
fileExtension = define "FileExtension" $
  doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
  T.wrap T.string

library :: Binding
library = define "Library" $
  doc "A library of primitive functions" $
  T.record [
    "namespace">:
      doc "A common prefix for all primitive function names in the library"
      namespace,
    "prefix">:
      doc "A preferred namespace prefix for function names in the library"
      T.string,
    "primitives">:
      doc "The primitives defined in this library" $
      T.list Graph.primitive]

module' :: Binding
module' = define "Module" $
  doc "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" $
  T.record [
    "namespace">:
      doc "A common prefix for all element names in the module"
      namespace,
    "elements">:
      doc "The elements defined in this module" $
      T.list Core.binding,
    "termDependencies">:
      doc "Any modules which the term expressions of this module directly depend upon" $
      T.list module',
    "typeDependencies">:
      doc "Any modules which the type expressions of this module directly depend upon" $
      T.list module',
    "description">:
      doc "An optional human-readable description of the module" $
      T.maybe T.string]

namespace :: Binding
namespace = define "Namespace" $
  doc "A prefix for element names" $
  T.wrap T.string

namespaces :: Binding
namespaces = define "Namespaces" $
  doc "A mapping from namespaces to values of type n, with a focus on one namespace" $
  T.forAll "n" $ T.record [
    "focus">:
      doc "The namespace in focus, together with its associated value" $
      T.pair namespace "n",
    "mapping">:
      doc "A mapping of namespaces to values" $
      T.map namespace "n"]

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  doc "A qualified name consisting of an optional namespace together with a mandatory local name" $
  T.record [
    "namespace">:
      doc "The optional namespace" $
      T.maybe namespace,
    "local">:
      doc "The local name"
      T.string]

termDefinition :: Binding
termDefinition = define "TermDefinition" $
  doc "A term-level definition, including a name, a term, and the type of the term" $
  T.record [
    "name">:
      doc "The name of the term"
      Core.name,
    "term">:
      doc "The term being defined"
      Core.term,
    "type">:
      doc "The type of the term"
      Core.type_]

typeDefinition :: Binding
typeDefinition = define "TypeDefinition" $
  doc "A type-level definition, including a name and the type" $
  T.record [
    "name">:
      doc "The name of the type"
      Core.name,
    -- TODO: consider using TypeScheme here instead of Type
    "type">:
      doc "The type being defined"
      Core.type_]
