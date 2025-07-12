module Hydra.Dsl.Graph where

import Hydra.Kernel
import Hydra.Dsl.Phantoms

import qualified Data.Map as M


comparisonLessThan :: TTerm Comparison
comparisonLessThan = unitVariant _Comparison _Comparison_lessThan

comparisonEqualTo :: TTerm Comparison
comparisonEqualTo = unitVariant _Comparison _Comparison_equalTo

comparisonGreaterThan :: TTerm Comparison
comparisonGreaterThan = unitVariant _Comparison _Comparison_greaterThan

element :: TTerm Name -> TTerm Term -> TTerm (Maybe TypeScheme) -> TTerm Element
element name term mtyp = record _Element [
  _Element_name>>: name,
  _Element_term>>: term,
  _Element_type>>: mtyp]

elementName :: TTerm Element -> TTerm Name
elementName el = project _Element _Element_name @@ el

elementTerm :: TTerm Element -> TTerm Term
elementTerm el = project _Element _Element_term @@ el

elementType :: TTerm Element -> TTerm (Maybe TypeScheme)
elementType el = project _Element _Element_type @@ el

graph :: TTerm (M.Map Name Element)
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

graphElements :: TTerm Graph -> TTerm (M.Map Name Element)
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

graphWithElements :: TTerm Graph -> TTerm (M.Map Name Element) -> TTerm Graph
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

primitiveName :: TTerm Primitive -> TTerm Name
primitiveName p = project _Primitive _Primitive_name @@ p

primitiveType :: TTerm Primitive -> TTerm TypeScheme
primitiveType p = project _Primitive _Primitive_type @@ p

primitiveImplementation :: TTerm Primitive -> TTerm ([Term] -> Flow Graph Term)
primitiveImplementation p = project _Primitive _Primitive_implementation @@ p

typeClassEquality :: TTerm TypeClass
typeClassEquality = unitVariant _TypeClass _TypeClass_equality

typeClassOrdering :: TTerm TypeClass
typeClassOrdering = unitVariant _TypeClass _TypeClass_ordering
