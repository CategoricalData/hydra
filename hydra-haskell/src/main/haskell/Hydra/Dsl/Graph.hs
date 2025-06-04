module Hydra.Dsl.Graph where

import Hydra.Kernel
import Hydra.Dsl.Phantoms

import qualified Data.Map as M


element :: TTerm Name -> TTerm Term -> TTerm (Maybe TypeScheme) -> TTerm Element
element name term mtyp = record _Element [
  _Element_name>>: name,
  _Element_term>>: term,
  _Element_type>>: mtyp]

elementName :: TTerm Element -> TTerm Name
elementName el = project _Element _Element_name @@ el

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

primitiveName :: TTerm Primitive -> TTerm Name
primitiveName p = project _Primitive _Primitive_name @@ p

primitiveType :: TTerm Primitive -> TTerm TypeScheme
primitiveType p = project _Primitive _Primitive_type @@ p

primitiveImplementation :: TTerm Primitive -> TTerm ([Term] -> Flow Graph Term)
primitiveImplementation p = project _Primitive _Primitive_type @@ p