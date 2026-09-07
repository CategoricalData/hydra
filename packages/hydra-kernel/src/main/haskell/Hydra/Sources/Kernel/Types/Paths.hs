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
            moduleMetadata = descriptionMetadata (Just "Subterm and subtype access, and the term/type graph view of a graph")}
  where
    definitions = [
      lambdaVariableReference,
      letVariableReference,
      nominalTypeReference,
      primitiveReference,
      subtermLink,
      subtermPath,
      subtermStep,
      subtypeLink,
      subtypePath,
      subtypeStep,
      termAttributeLink,
      termGraph,
      termLink,
      termNode,
      termNodeId,
      termReference,
      termReferenceLink,
      typeAttributeLink,
      typeGraph,
      typeLink,
      typeNode,
      typeNodeId,
      typeReference,
      typeReferenceLink,
      typeVariableReference]

lambdaVariableReference :: TypeDefinition
lambdaVariableReference = define "LambdaVariableReference" $
  doc "A reference to a lambda-bound variable: the node introducing it is the binding lambda" $
  T.record [
    "variable">:
      doc "The name of the lambda-bound variable"
      Core.name,
    "boundByNode">:
      doc "The id of the node at the binding lambda"
      termNodeId,
    "type">:
      doc "The type of the variable"
      Core.type_]

letVariableReference :: TypeDefinition
letVariableReference = define "LetVariableReference" $
  doc "A reference to a let-bound variable: the node introducing it is the bound term" $
  T.record [
    "variable">:
      doc "The name of the let-bound variable"
      Core.name,
    "bindingNode">:
      doc "The id of the node at the bound term"
      termNodeId,
    "type">:
      doc "The type of the variable"
      Core.type_]

nominalTypeReference :: TypeDefinition
nominalTypeReference = define "NominalTypeReference" $
  doc "A reference to a named type of the schema (analogous to a let-bound variable)" $
  T.record [
    "name">:
      doc "The name of the referenced schema type"
      Core.name]

primitiveReference :: TypeDefinition
primitiveReference = define "PrimitiveReference" $
  doc "A reference to a primitive function" $
  T.record [
    "name">:
      doc "The name of the primitive"
      Core.name,
    "type">:
      doc "The type of the primitive"
      Core.type_]

subtermLink :: TypeDefinition
subtermLink = define "SubtermLink" $
  doc "An outgoing link to an immediate subterm, carried inline as a child node" $
  T.record [
    "step">:
      doc "The step by which the child subterm is reached"
      subtermStep,
    "child">:
      doc "The child node (inline; the structure is a tree)"
      termNode]

subtermPath :: TypeDefinition
subtermPath = define "SubtermPath" $
  doc "A sequence of subterm steps forming a path through a term, root first" $
  T.wrap $ T.list subtermStep

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
    "mapEntry">:
      doc ("Access the map entry at the given index as a pair (k, v). Accessing this step constructs the "
        <> "pair; the entry is not itself a subterm occurrence, though its key and value components are, "
        <> "reached via pairFirst/pairSecond. Entry indexes follow the total order on serializable terms.")
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

subtypeLink :: TypeDefinition
subtypeLink = define "SubtypeLink" $
  doc "An outgoing link to an immediate subtype, carried inline as a child node" $
  T.record [
    "step">:
      doc "The step by which the child subtype is reached"
      subtypeStep,
    "child">:
      doc "The child node (inline; the structure is a tree)"
      typeNode]

subtypePath :: TypeDefinition
subtypePath = define "SubtypePath" $
  doc "A sequence of subtype steps forming a path through a type, root first" $
  T.wrap $ T.list subtypeStep

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

termAttributeLink :: TypeDefinition
termAttributeLink = define "TermAttributeLink" $
  doc "A non-term constituent of this node's term, with its value" $
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
    "literal">:
      doc "The literal value of a literal term (a leaf)"
      Core.literal,
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
    "unwrapTypeName">:
      doc "The name of the wrapper type eliminated by an unwrap"
      Core.name,
    "wrapTypeName">:
      doc "The name of the wrapper type of a wrapped term"
      Core.name]

termGraph :: TypeDefinition
termGraph = define "TermGraph" $
  doc "A typed graph as a term graph: one root node per binding of the graph" $
  T.record [
    "roots">:
      doc "The root nodes, keyed by binding name" $
      T.map Core.name termNode]

termLink :: TypeDefinition
termLink = define "TermLink" $
  doc "An outgoing link of a term node: an inline subterm, a variable occurrence, or a non-term attribute" $
  T.union [
    "attribute">:
      doc "A non-term constituent of this node's term"
      termAttributeLink,
    "reference">:
      doc "A variable or primitive occurrence"
      termReferenceLink,
    "subterm">:
      doc "An immediate subterm, carried inline"
      subtermLink]

termNode :: TypeDefinition
termNode = define "TermNode" $
  doc "A node in a term graph: a subterm as written, its type, and its outgoing links" $
  T.record [
    "term">:
      doc "The subterm as written"
      Core.term,
    "type">:
      doc "The subterm's type (a binding's forall type at roots), via hydra.checking"
      Core.type_,
    "links">:
      doc "The outgoing links of the node" $
      T.list termLink]

termNodeId :: TypeDefinition
termNodeId = define "TermNodeId" $
  doc ("The id of a term-graph node: a root binding name plus a subterm path from that root. "
    <> "A root node's id has an empty path. A letBinding step appears only for an actual nested "
    <> "let inside a binding's term, never as a leading pseudo-step.") $
  T.record [
    "root">:
      doc "The name of the root binding"
      Core.name,
    "path">:
      doc "The subterm path from the root"
      subtermPath]

termReference :: TypeDefinition
termReference = define "TermReference" $
  doc "The target of a variable or primitive occurrence" $
  T.union [
    "lambda">:
      doc "A reference to a lambda-bound variable"
      lambdaVariableReference,
    "let">:
      doc "A reference to a let-bound variable"
      letVariableReference,
    "primitive">:
      doc "A reference to a primitive function"
      primitiveReference]

termReferenceLink :: TypeDefinition
termReferenceLink = define "TermReferenceLink" $
  doc "An outgoing link to a variable or primitive occurrence" $
  T.record [
    "step">:
      doc "The step by which the occurrence is reached"
      subtermStep,
    "target">:
      doc "The referenced binding, lambda parameter, or primitive"
      termReference]

typeAttributeLink :: TypeDefinition
typeAttributeLink = define "TypeAttributeLink" $
  doc "A non-type constituent of this node's type, with its value" $
  T.union [
    "annotatedAnnotation">:
      doc "The annotation of an annotated type (a term; not descended into)"
      Core.term,
    "forallParameter">:
      doc "The type-variable parameter of a universally quantified type"
      Core.name,
    "literal">:
      doc "The literal type of a literal type (a leaf)"
      Core.literalType]

typeGraph :: TypeDefinition
typeGraph = define "TypeGraph" $
  doc "A schema as a type graph: one root node per named type" $
  T.record [
    "roots">:
      doc "The root nodes, keyed by type name" $
      T.map Core.name typeNode]

typeLink :: TypeDefinition
typeLink = define "TypeLink" $
  doc "An outgoing link of a type node: an inline subtype, a type reference, or a non-type attribute" $
  T.union [
    "attribute">:
      doc "A non-type constituent of this node's type"
      typeAttributeLink,
    "reference">:
      doc "A named-type or type-variable occurrence"
      typeReferenceLink,
    "subtype">:
      doc "An immediate subtype, carried inline"
      subtypeLink]

typeNode :: TypeDefinition
typeNode = define "TypeNode" $
  doc "A node in a type graph: a subtype as written and its outgoing links" $
  T.record [
    "type">:
      doc "The subtype as written (needed for linkless leaves: literal types, unit, void)"
      Core.type_,
    "links">:
      doc "The outgoing links of the node" $
      T.list typeLink]

typeNodeId :: TypeDefinition
typeNodeId = define "TypeNodeId" $
  doc ("The id of a type-graph node: a root type name plus a subtype path from that root. "
    <> "A root node's id has an empty path. There is no enclosing-let analog for schemas.") $
  T.record [
    "root">:
      doc "The name of the root type"
      Core.name,
    "path">:
      doc "The subtype path from the root"
      subtypePath]

typeReference :: TypeDefinition
typeReference = define "TypeReference" $
  doc "The target of a named-type or type-variable occurrence" $
  T.union [
    "nominal">:
      doc "A reference to a named type of the schema"
      nominalTypeReference,
    "variable">:
      doc "A reference to a forall-bound type variable"
      typeVariableReference]

typeReferenceLink :: TypeDefinition
typeReferenceLink = define "TypeReferenceLink" $
  doc "An outgoing link to a named-type or type-variable occurrence" $
  T.record [
    "step">:
      doc "The step by which the occurrence is reached"
      subtypeStep,
    "target">:
      doc "The referenced named type or type variable"
      typeReference]

typeVariableReference :: TypeDefinition
typeVariableReference = define "TypeVariableReference" $
  doc "A reference to a forall-bound type variable: the node introducing it is the binder" $
  T.record [
    "variable">:
      doc "The name of the forall-bound type variable"
      Core.name,
    "boundByNode">:
      doc "The id of the node at the binding forall"
      typeNodeId]
