module Hydra.Sources.Kernel.Types.Paths where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.paths"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "Subterm and subtype access, and the link view of a graph")}
  where
    definitions = [
      subtermAttribute,
      subtermEdge,
      subtermGraph,
      subtermLink,
      subtermNode,
      subtermPath,
      subtermProperty,
      subtermStep,
      subtypeAttribute,
      subtypeEdge,
      subtypeGraph,
      subtypeLink,
      subtypeNode,
      subtypePath,
      subtypeProperty,
      subtypeStep,
      termAttribute,
      typeAttribute]

subtermAttribute :: TypeDefinition
subtermAttribute = define "SubtermAttribute" $
  doc "A link from a node to a non-term attribute of a position" $
  T.record [
    "path">:
      doc "The subterm path at which the attribute occurs"
      subtermPath,
    "target">:
      doc "The attribute value"
      termAttribute]

subtermEdge :: TypeDefinition
subtermEdge = define "SubtermEdge" $
  doc "A link from a node to another binding of the graph, addressed by the path at which it occurs" $
  T.record [
    "path">:
      doc "The subterm path at which the reference occurs"
      subtermPath,
    "target">:
      doc "The name of the referenced binding of the graph"
      Core.name]

subtermGraph :: TypeDefinition
subtermGraph = define "SubtermGraph" $
  doc "The link view of a graph as subterm nodes and their links" $
  T.record [
    "nodes">:
      doc "All nodes in the graph" $
      T.list subtermNode]

subtermLink :: TypeDefinition
subtermLink = define "SubtermLink" $
  doc "An outgoing link of a subterm node: an edge, a property, or an attribute" $
  T.union [
    "edge">:
      doc "A reference to another binding of the graph"
      subtermEdge,
    "property">:
      doc "A leaf term"
      subtermProperty,
    "attribute">:
      doc "A non-term attribute of a position"
      subtermAttribute]

subtermNode :: TypeDefinition
subtermNode = define "SubtermNode" $
  doc "A node in a subterm graph: a binding of the graph, with its type scheme and outgoing links" $
  T.record [
    "name">:
      doc "The name of the binding"
      Core.name,
    "type">:
      doc "The type scheme of the binding"
      Core.typeScheme,
    "links">:
      doc "The outgoing links of the node" $
      T.list subtermLink]

subtermPath :: TypeDefinition
subtermPath = define "SubtermPath" $
  doc "A sequence of subterm steps forming a path through a term, root first" $
  T.wrap $ T.list subtermStep

subtermProperty :: TypeDefinition
subtermProperty = define "SubtermProperty" $
  doc "A link from a node to a leaf term (literal, unit, projection, unwrap, or an unbound variable)" $
  T.record [
    "path">:
      doc "The subterm path at which the leaf term occurs"
      subtermPath,
    "target">:
      doc "The leaf term"
      Core.term]

subtermStep :: TypeDefinition
subtermStep = define "SubtermStep" $
  doc "A function which maps from a term to a particular immediate subterm" $
  T.union [
    "annotatedAnnotation">:
      doc "Access the annotation of an annotated term"
      T.unit,
    "annotatedBody">:
      doc "Access the body of an annotated term"
      T.unit,
    "applicationArgument">:
      doc "Access the argument of an application term"
      T.unit,
    "applicationFunction">:
      doc "Access the function of an application term"
      T.unit,
    "casesCase">:
      doc "Access the handler of a specific case of a case statement by field name"
      Core.name,
    "casesDefault">:
      doc "Access the default case of a case statement"
      T.unit,
    "eitherLeft">:
      doc "Access the left term of an either value"
      T.unit,
    "eitherRight">:
      doc "Access the right term of an either value"
      T.unit,
    "injectField">:
      doc "Access the injected term of a union injection by field name"
      Core.name,
    "lambdaBody">:
      doc "Access the body of a lambda term"
      T.unit,
    "letBinding">:
      doc "Access a specific binding in a let term by variable name"
      Core.name,
    "letBody">:
      doc "Access the body of a let term"
      T.unit,
    "listElement">:
      doc "Access an element of a list by index"
      T.int32,
    "mapKey">:
      doc "Access the key of the map entry at the given index"
      T.int32,
    "mapValue">:
      doc "Access the value of the map entry at the given index"
      T.int32,
    "optionalGiven">:
      doc "Access the term inside a given (present) optional value"
      T.unit,
    "pairFirst">:
      doc "Access the first term of a pair"
      T.unit,
    "pairSecond">:
      doc "Access the second term of a pair"
      T.unit,
    "recordField">:
      doc "Access a field of a record by field name"
      Core.name,
    "setElement">:
      doc "Access an element of a set by index"
      T.int32,
    "typeApplicationBody">:
      doc "Access the body of a type application term"
      T.unit,
    "typeLambdaBody">:
      doc "Access the body of a type lambda term"
      T.unit,
    "wrapBody">:
      doc "Access the body of a wrapped term"
      T.unit]

subtypeAttribute :: TypeDefinition
subtypeAttribute = define "SubtypeAttribute" $
  doc "A link from a node to a non-type attribute of a position" $
  T.record [
    "path">:
      doc "The subtype path at which the attribute occurs"
      subtypePath,
    "target">:
      doc "The attribute value"
      typeAttribute]

subtypeEdge :: TypeDefinition
subtypeEdge = define "SubtypeEdge" $
  doc "A link from a node to a named type, addressed by the path at which it occurs" $
  T.record [
    "path">:
      doc "The subtype path at which the reference occurs"
      subtypePath,
    "target">:
      doc "The name of the referenced type"
      Core.name]

subtypeGraph :: TypeDefinition
subtypeGraph = define "SubtypeGraph" $
  doc "The link view of a schema as subtype nodes and their links" $
  T.record [
    "nodes">:
      doc "All nodes in the graph" $
      T.list subtypeNode]

subtypeLink :: TypeDefinition
subtypeLink = define "SubtypeLink" $
  doc "An outgoing link of a subtype node: an edge, a property, or an attribute" $
  T.union [
    "edge">:
      doc "A reference to a named type"
      subtypeEdge,
    "property">:
      doc "A leaf type"
      subtypeProperty,
    "attribute">:
      doc "A non-type attribute of a position"
      subtypeAttribute]

subtypeNode :: TypeDefinition
subtypeNode = define "SubtypeNode" $
  doc "A node in a subtype graph: a named type, with its outgoing links" $
  T.record [
    "name">:
      doc "The name of the type"
      Core.name,
    "links">:
      doc "The outgoing links of the node" $
      T.list subtypeLink]

subtypePath :: TypeDefinition
subtypePath = define "SubtypePath" $
  doc "A sequence of subtype steps forming a path through a type, root first" $
  T.wrap $ T.list subtypeStep

subtypeProperty :: TypeDefinition
subtypeProperty = define "SubtypeProperty" $
  doc "A link from a node to a leaf type (literal, unit, void, or a bound variable)" $
  T.record [
    "path">:
      doc "The subtype path at which the leaf type occurs"
      subtypePath,
    "target">:
      doc "The leaf type"
      Core.type_]

subtypeStep :: TypeDefinition
subtypeStep = define "SubtypeStep" $
  doc "A function which maps from a type to a particular immediate subtype" $
  T.union [
    "annotatedBody">:
      doc "Access the body of an annotated type (the annotation is a term; there is no step for it)"
      T.unit,
    "applicationArgument">:
      doc "Access the argument of an application type"
      T.unit,
    "applicationFunction">:
      doc "Access the function of an application type"
      T.unit,
    "effectValue">:
      doc "Access the value type of an effect type"
      T.unit,
    "eitherLeft">:
      doc "Access the left type of an either type"
      T.unit,
    "eitherRight">:
      doc "Access the right type of an either type"
      T.unit,
    "forallBody">:
      doc "Access the body of a universally quantified type"
      T.unit,
    "functionCodomain">:
      doc "Access the codomain type of a function type"
      T.unit,
    "functionDomain">:
      doc "Access the domain type of a function type"
      T.unit,
    "listElement">:
      doc "Access the element type of a list type"
      T.unit,
    "mapKeys">:
      doc "Access the key type of a map type"
      T.unit,
    "mapValues">:
      doc "Access the value type of a map type"
      T.unit,
    "optionalElement">:
      doc "Access the element type of an optional type"
      T.unit,
    "pairFirst">:
      doc "Access the first type of a pair type"
      T.unit,
    "pairSecond">:
      doc "Access the second type of a pair type"
      T.unit,
    "recordField">:
      doc "Access a field type of a record type by field name"
      Core.name,
    "setElement">:
      doc "Access the element type of a set type"
      T.unit,
    "unionField">:
      doc "Access a field type of a union type by field name"
      Core.name,
    "wrapBody">:
      doc "Access the body type of a wrapped type"
      T.unit]

termAttribute :: TypeDefinition
termAttribute = define "TermAttribute" $
  doc "A non-term constituent of the term at a path, together with its value" $
  T.union [
    "casesTypeName">:
      doc "The name of the union type eliminated by a case statement"
      Core.name,
    "injectTypeName">:
      doc "The name of the union type of an injection"
      Core.name,
    "lambdaDomainGiven">:
      doc "The (given) domain type of a lambda"
      Core.type_,
    "lambdaParameter">:
      doc "The parameter name of a lambda"
      Core.name,
    "letBindingTypeSchemeGiven">:
      doc "The name of a let binding together with its (given) type scheme" $
      T.pair Core.name Core.typeScheme,
    "projectFieldName">:
      doc "The name of the field projected by a projection"
      Core.name,
    "projectTypeName">:
      doc "The name of the record type of a projection"
      Core.name,
    "recordTypeName">:
      doc "The name of the record type of a record term"
      Core.name,
    "typeApplicationType">:
      doc "The type argument of a type application term"
      Core.type_,
    "typeLambdaParameter">:
      doc "The type-variable parameter of a type lambda term"
      Core.name,
    "wrapTypeName">:
      doc "The name of the wrapper type of a wrapped term"
      Core.name]

typeAttribute :: TypeDefinition
typeAttribute = define "TypeAttribute" $
  doc "A non-type constituent of the type at a path, together with its value" $
  T.union [
    "annotatedAnnotation">:
      doc "The annotation of an annotated type (a term; not descended into)"
      Core.term,
    "forallParameter">:
      doc "The type-variable parameter of a universally quantified type"
      Core.name]
