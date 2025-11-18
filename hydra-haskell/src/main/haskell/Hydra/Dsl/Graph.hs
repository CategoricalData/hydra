module Hydra.Dsl.Graph where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Data.Map as M


comparisonLessThan :: TTerm Comparison
comparisonLessThan = injectUnit _Comparison _Comparison_lessThan

comparisonEqualTo :: TTerm Comparison
comparisonEqualTo = injectUnit _Comparison _Comparison_equalTo

comparisonGreaterThan :: TTerm Comparison
comparisonGreaterThan = injectUnit _Comparison _Comparison_greaterThan

graph :: TTerm (M.Map Name Binding)
    -> TTerm (M.Map Name (Maybe Term))
    -> TTerm (M.Map Name TypeScheme)
    -> TTerm Term
    -> TTerm (M.Map Name Primitive)
    -> TTerm (Maybe Graph)
    -> TTerm Graph
graph elements environment types body primitives schema = record _Graph [
    _Graph_elements>>: elements,
    _Graph_environment>>: environment,
    _Graph_types>>: types,
    _Graph_body>>: body,
    _Graph_primitives>>: primitives,
    _Graph_schema>>: schema]

graphElements :: TTerm Graph -> TTerm (M.Map Name Binding)
graphElements g = project _Graph _Graph_elements @@ g

graphEnvironment :: TTerm Graph -> TTerm (M.Map Name (Maybe Term))
graphEnvironment g = project _Graph _Graph_environment @@ g

graphTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme)
graphTypes g = project _Graph _Graph_types @@ g

graphBody :: TTerm Graph -> TTerm Term
graphBody g = project _Graph _Graph_body @@ g

graphPrimitives :: TTerm Graph -> TTerm (M.Map Name Primitive)
graphPrimitives g = project _Graph _Graph_primitives @@ g

graphSchema :: TTerm Graph -> TTerm (Maybe Graph)
graphSchema g = project _Graph _Graph_schema @@ g

graphWithElements :: TTerm Graph -> TTerm (M.Map Name Binding) -> TTerm Graph
graphWithElements g newElements = graph
    newElements
    (Hydra.Dsl.Graph.graphEnvironment g)
    (Hydra.Dsl.Graph.graphTypes g)
    (Hydra.Dsl.Graph.graphBody g)
    (Hydra.Dsl.Graph.graphPrimitives g)
    (Hydra.Dsl.Graph.graphSchema g)

graphWithEnvironment :: TTerm Graph -> TTerm (M.Map Name (Maybe Term)) -> TTerm Graph
graphWithEnvironment g newEnvironment = graph
    (Hydra.Dsl.Graph.graphElements g)
    newEnvironment
    (Hydra.Dsl.Graph.graphTypes g)
    (Hydra.Dsl.Graph.graphBody g)
    (Hydra.Dsl.Graph.graphPrimitives g)
    (Hydra.Dsl.Graph.graphSchema g)

graphWithTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme) -> TTerm Graph
graphWithTypes g newTypes = graph
    (Hydra.Dsl.Graph.graphElements g)
    (Hydra.Dsl.Graph.graphEnvironment g)
    newTypes
    (Hydra.Dsl.Graph.graphBody g)
    (Hydra.Dsl.Graph.graphPrimitives g)
    (Hydra.Dsl.Graph.graphSchema g)

graphWithBody :: TTerm Graph -> TTerm Term -> TTerm Graph
graphWithBody g newBody = graph
    (Hydra.Dsl.Graph.graphElements g)
    (Hydra.Dsl.Graph.graphEnvironment g)
    (Hydra.Dsl.Graph.graphTypes g)
    newBody
    (Hydra.Dsl.Graph.graphPrimitives g)
    (Hydra.Dsl.Graph.graphSchema g)

graphWithPrimitives :: TTerm Graph -> TTerm (M.Map Name Primitive) -> TTerm Graph
graphWithPrimitives g newPrimitives = graph
    (Hydra.Dsl.Graph.graphElements g)
    (Hydra.Dsl.Graph.graphEnvironment g)
    (Hydra.Dsl.Graph.graphTypes g)
    (Hydra.Dsl.Graph.graphBody g)
    newPrimitives
    (Hydra.Dsl.Graph.graphSchema g)

graphWithSchema :: TTerm Graph -> TTerm (Maybe Graph) -> TTerm Graph
graphWithSchema g newSchema = graph
    (Hydra.Dsl.Graph.graphElements g)
    (Hydra.Dsl.Graph.graphEnvironment g)
    (Hydra.Dsl.Graph.graphTypes g)
    (Hydra.Dsl.Graph.graphBody g)
    (Hydra.Dsl.Graph.graphPrimitives g)
    newSchema

primitive :: TTerm Name
    -> TTerm TypeScheme
    -> TTerm ([Term] -> Flow Graph Term)
    -> TTerm Primitive
primitive name typ implementation = record _Primitive [
    _Primitive_name>>: name,
    _Primitive_type>>: typ,
    _Primitive_implementation>>: implementation]

primitiveName :: TTerm Primitive -> TTerm Name
primitiveName p = project _Primitive _Primitive_name @@ p

primitiveType :: TTerm Primitive -> TTerm TypeScheme
primitiveType p = project _Primitive _Primitive_type @@ p

primitiveImplementation :: TTerm Primitive -> TTerm ([Term] -> Flow Graph Term)
primitiveImplementation p = project _Primitive _Primitive_implementation @@ p

primitiveWithType :: TTerm Primitive -> TTerm TypeScheme -> TTerm Primitive
primitiveWithType p newType = Hydra.Dsl.Graph.primitive
    (Hydra.Dsl.Graph.primitiveName p)
    newType
    (Hydra.Dsl.Graph.primitiveImplementation p)

typeClassEquality :: TTerm TypeClass
typeClassEquality = injectUnit _TypeClass _TypeClass_equality

typeClassOrdering :: TTerm TypeClass
typeClassOrdering = injectUnit _TypeClass _TypeClass_ordering
