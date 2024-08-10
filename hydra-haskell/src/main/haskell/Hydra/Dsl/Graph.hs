module Hydra.Dsl.Graph where

import Hydra.Kernel
import Hydra.Dsl.Base as Base

import qualified Data.Map as M


elementName :: TTerm (Element -> Name)
elementName = project _Element _Element_name

graph :: TTerm (M.Map Name Element)
    -> TTerm (M.Map Name (Maybe Term))
    -> TTerm (M.Map Name Type)
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

graphElements :: TTerm (Graph -> M.Map Name Element)
graphElements = project _Graph _Graph_elements

graphEnvironment :: TTerm (Graph -> M.Map Name (Maybe Term))
graphEnvironment = project _Graph _Graph_environment

graphTypes :: TTerm (Graph -> M.Map Name Type)
graphTypes = project _Graph _Graph_types

graphBody :: TTerm (Graph -> Term)
graphBody = project _Graph _Graph_body

graphPrimitives :: TTerm (Graph -> M.Map Name Primitive)
graphPrimitives = project _Graph _Graph_primitives

graphSchema :: TTerm (Graph -> Maybe Graph)
graphSchema = project _Graph _Graph_schema

primitiveName :: TTerm (Primitive -> Name)
primitiveName = project _Primitive _Primitive_name

primitiveType :: TTerm (Primitive -> TypeScheme)
primitiveType = project _Primitive _Primitive_type

primitiveImplementation :: TTerm (Primitive -> ([Term] -> Flow Graph Term))
primitiveImplementation = project _Primitive _Primitive_type
