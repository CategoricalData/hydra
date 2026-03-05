-- | Meta-DSL for constructing graph-related terms (Comparison, Primitive, etc.)

module Hydra.Dsl.Meta.Graph where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Dsl.Meta.Lib.Sets as Sets

import qualified Data.Map as M
import qualified Data.Set as S


comparisonLessThan :: TTerm Comparison
comparisonLessThan = injectUnit _Comparison _Comparison_lessThan

comparisonEqualTo :: TTerm Comparison
comparisonEqualTo = injectUnit _Comparison _Comparison_equalTo

comparisonGreaterThan :: TTerm Comparison
comparisonGreaterThan = injectUnit _Comparison _Comparison_greaterThan

emptyGraph :: TTerm Graph
emptyGraph = graph
    Maps.empty
    Maps.empty
    Maps.empty
    Sets.empty
    Maps.empty
    Maps.empty
    Maps.empty
    Sets.empty

graph :: TTerm (M.Map Name Term)
    -> TTerm (M.Map Name TypeScheme)
    -> TTerm (M.Map Name TypeVariableMetadata)
    -> TTerm (S.Set Name)
    -> TTerm (M.Map Name Term)
    -> TTerm (M.Map Name Primitive)
    -> TTerm (M.Map Name TypeScheme)
    -> TTerm (S.Set Name)
    -> TTerm Graph
graph boundTerms boundTypes classConstraints lambdaVariables metadata primitives schemaTypes typeVariables = record _Graph [
    _Graph_boundTerms>>: boundTerms,
    _Graph_boundTypes>>: boundTypes,
    _Graph_classConstraints>>: classConstraints,
    _Graph_lambdaVariables>>: lambdaVariables,
    _Graph_metadata>>: metadata,
    _Graph_primitives>>: primitives,
    _Graph_schemaTypes>>: schemaTypes,
    _Graph_typeVariables>>: typeVariables]

graphBoundTerms :: TTerm Graph -> TTerm (M.Map Name Term)
graphBoundTerms g = project _Graph _Graph_boundTerms @@ g

graphBoundTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme)
graphBoundTypes g = project _Graph _Graph_boundTypes @@ g

graphClassConstraints :: TTerm Graph -> TTerm (M.Map Name TypeVariableMetadata)
graphClassConstraints g = project _Graph _Graph_classConstraints @@ g

graphLambdaVariables :: TTerm Graph -> TTerm (S.Set Name)
graphLambdaVariables g = project _Graph _Graph_lambdaVariables @@ g

graphMetadata :: TTerm Graph -> TTerm (M.Map Name Term)
graphMetadata g = project _Graph _Graph_metadata @@ g

graphPrimitives :: TTerm Graph -> TTerm (M.Map Name Primitive)
graphPrimitives g = project _Graph _Graph_primitives @@ g

graphSchemaTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme)
graphSchemaTypes g = project _Graph _Graph_schemaTypes @@ g

graphTypeVariables :: TTerm Graph -> TTerm (S.Set Name)
graphTypeVariables g = project _Graph _Graph_typeVariables @@ g

graphWithBoundTerms :: TTerm Graph -> TTerm (M.Map Name Term) -> TTerm Graph
graphWithBoundTerms g newBoundTerms = graph
    newBoundTerms
    (Hydra.Dsl.Meta.Graph.graphBoundTypes g)
    (Hydra.Dsl.Meta.Graph.graphClassConstraints g)
    (Hydra.Dsl.Meta.Graph.graphLambdaVariables g)
    (Hydra.Dsl.Meta.Graph.graphMetadata g)
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    (Hydra.Dsl.Meta.Graph.graphSchemaTypes g)
    (Hydra.Dsl.Meta.Graph.graphTypeVariables g)

graphWithBoundTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme) -> TTerm Graph
graphWithBoundTypes g newBoundTypes = graph
    (Hydra.Dsl.Meta.Graph.graphBoundTerms g)
    newBoundTypes
    (Hydra.Dsl.Meta.Graph.graphClassConstraints g)
    (Hydra.Dsl.Meta.Graph.graphLambdaVariables g)
    (Hydra.Dsl.Meta.Graph.graphMetadata g)
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    (Hydra.Dsl.Meta.Graph.graphSchemaTypes g)
    (Hydra.Dsl.Meta.Graph.graphTypeVariables g)

graphWithClassConstraints :: TTerm Graph -> TTerm (M.Map Name TypeVariableMetadata) -> TTerm Graph
graphWithClassConstraints g newCC = graph
    (Hydra.Dsl.Meta.Graph.graphBoundTerms g)
    (Hydra.Dsl.Meta.Graph.graphBoundTypes g)
    newCC
    (Hydra.Dsl.Meta.Graph.graphLambdaVariables g)
    (Hydra.Dsl.Meta.Graph.graphMetadata g)
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    (Hydra.Dsl.Meta.Graph.graphSchemaTypes g)
    (Hydra.Dsl.Meta.Graph.graphTypeVariables g)

graphWithLambdaVariables :: TTerm Graph -> TTerm (S.Set Name) -> TTerm Graph
graphWithLambdaVariables g newLV = graph
    (Hydra.Dsl.Meta.Graph.graphBoundTerms g)
    (Hydra.Dsl.Meta.Graph.graphBoundTypes g)
    (Hydra.Dsl.Meta.Graph.graphClassConstraints g)
    newLV
    (Hydra.Dsl.Meta.Graph.graphMetadata g)
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    (Hydra.Dsl.Meta.Graph.graphSchemaTypes g)
    (Hydra.Dsl.Meta.Graph.graphTypeVariables g)

graphWithMetadata :: TTerm Graph -> TTerm (M.Map Name Term) -> TTerm Graph
graphWithMetadata g newMeta = graph
    (Hydra.Dsl.Meta.Graph.graphBoundTerms g)
    (Hydra.Dsl.Meta.Graph.graphBoundTypes g)
    (Hydra.Dsl.Meta.Graph.graphClassConstraints g)
    (Hydra.Dsl.Meta.Graph.graphLambdaVariables g)
    newMeta
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    (Hydra.Dsl.Meta.Graph.graphSchemaTypes g)
    (Hydra.Dsl.Meta.Graph.graphTypeVariables g)

graphWithSchemaTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme) -> TTerm Graph
graphWithSchemaTypes g newSchemaTypes = graph
    (Hydra.Dsl.Meta.Graph.graphBoundTerms g)
    (Hydra.Dsl.Meta.Graph.graphBoundTypes g)
    (Hydra.Dsl.Meta.Graph.graphClassConstraints g)
    (Hydra.Dsl.Meta.Graph.graphLambdaVariables g)
    (Hydra.Dsl.Meta.Graph.graphMetadata g)
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    newSchemaTypes
    (Hydra.Dsl.Meta.Graph.graphTypeVariables g)

-- | Get primitive types as a Map Name TypeScheme from a Graph's primitives map
graphPrimitiveTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme)
graphPrimitiveTypes g = Maps.fromList (Lists.map
    ("_gpt_p" ~> pair (Hydra.Dsl.Meta.Graph.primitiveName $ var "_gpt_p") (Hydra.Dsl.Meta.Graph.primitiveType $ var "_gpt_p"))
    (Maps.elems $ Hydra.Dsl.Meta.Graph.graphPrimitives g))

graphWithTypeVariables :: TTerm Graph -> TTerm (S.Set Name) -> TTerm Graph
graphWithTypeVariables g newTV = graph
    (Hydra.Dsl.Meta.Graph.graphBoundTerms g)
    (Hydra.Dsl.Meta.Graph.graphBoundTypes g)
    (Hydra.Dsl.Meta.Graph.graphClassConstraints g)
    (Hydra.Dsl.Meta.Graph.graphLambdaVariables g)
    (Hydra.Dsl.Meta.Graph.graphMetadata g)
    (Hydra.Dsl.Meta.Graph.graphPrimitives g)
    (Hydra.Dsl.Meta.Graph.graphSchemaTypes g)
    newTV

primitive :: TTerm Name
    -> TTerm TypeScheme
    -> TTerm (Context -> Graph -> [Term] -> Either (InContext Error) Term)
    -> TTerm Primitive
primitive name typ implementation = record _Primitive [
    _Primitive_name>>: name,
    _Primitive_type>>: typ,
    _Primitive_implementation>>: implementation]

primitiveName :: TTerm Primitive -> TTerm Name
primitiveName p = project _Primitive _Primitive_name @@ p

primitiveType :: TTerm Primitive -> TTerm TypeScheme
primitiveType p = project _Primitive _Primitive_type @@ p

primitiveImplementation :: TTerm Primitive -> TTerm (Context -> Graph -> [Term] -> Either (InContext Error) Term)
primitiveImplementation p = project _Primitive _Primitive_implementation @@ p

primitiveWithType :: TTerm Primitive -> TTerm TypeScheme -> TTerm Primitive
primitiveWithType p newType = Hydra.Dsl.Meta.Graph.primitive
    (Hydra.Dsl.Meta.Graph.primitiveName p)
    newType
    (Hydra.Dsl.Meta.Graph.primitiveImplementation p)

typeClassEquality :: TTerm TypeClass
typeClassEquality = injectUnit _TypeClass _TypeClass_equality

typeClassOrdering :: TTerm TypeClass
typeClassOrdering = injectUnit _TypeClass _TypeClass_ordering
