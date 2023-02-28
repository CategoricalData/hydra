-- | The extension to graphs of Hydra's core type system (hydra/core)

module Hydra.Graph where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | A typeclass-like construct providing common functions for working with annotations
data AnnotationClass m = 
  AnnotationClass {
    annotationClassDefault :: m,
    annotationClassEqual :: (m -> m -> Bool),
    annotationClassCompare :: (m -> m -> Comparison),
    annotationClassShow :: (m -> String),
    annotationClassRead :: (String -> Maybe m),
    annotationClassTermAnnotation :: (Core.Term m -> m),
    annotationClassTypeAnnotation :: (Core.Type m -> m),
    annotationClassTermDescription :: (Core.Term m -> Compute.Flow (Graph m) (Maybe String)),
    annotationClassTypeDescription :: (Core.Type m -> Compute.Flow (Graph m) (Maybe String)),
    annotationClassTermType :: (Core.Term m -> Compute.Flow (Graph m) (Maybe (Core.Type m))),
    annotationClassSetTermDescription :: (Graph m -> Maybe String -> Core.Term m -> Core.Term m),
    annotationClassSetTermType :: (Graph m -> Maybe (Core.Type m) -> Core.Term m -> Core.Term m),
    annotationClassTypeOf :: (m -> Compute.Flow (Graph m) (Maybe (Core.Type m))),
    annotationClassSetTypeOf :: (Maybe (Core.Type m) -> m -> m)}

_AnnotationClass = (Core.Name "hydra/graph.AnnotationClass")

_AnnotationClass_default = (Core.FieldName "default")

_AnnotationClass_equal = (Core.FieldName "equal")

_AnnotationClass_compare = (Core.FieldName "compare")

_AnnotationClass_show = (Core.FieldName "show")

_AnnotationClass_read = (Core.FieldName "read")

_AnnotationClass_termAnnotation = (Core.FieldName "termAnnotation")

_AnnotationClass_typeAnnotation = (Core.FieldName "typeAnnotation")

_AnnotationClass_termDescription = (Core.FieldName "termDescription")

_AnnotationClass_typeDescription = (Core.FieldName "typeDescription")

_AnnotationClass_termType = (Core.FieldName "termType")

_AnnotationClass_setTermDescription = (Core.FieldName "setTermDescription")

_AnnotationClass_setTermType = (Core.FieldName "setTermType")

_AnnotationClass_typeOf = (Core.FieldName "typeOf")

_AnnotationClass_setTypeOf = (Core.FieldName "setTypeOf")

-- | An equality judgement: less than, equal to, or greater than
data Comparison = 
  ComparisonLessThan  |
  ComparisonEqualTo  |
  ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = (Core.Name "hydra/graph.Comparison")

_Comparison_lessThan = (Core.FieldName "lessThan")

_Comparison_equalTo = (Core.FieldName "equalTo")

_Comparison_greaterThan = (Core.FieldName "greaterThan")

-- | A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph
data Graph m = 
  Graph {
    -- | All of the elements in the graph
    graphElements :: (Map Core.Name (Element m)),
    -- | The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
    graphEnvironment :: (Map Core.Name (Maybe (Core.Term m))),
    -- | The body of the term which generated this context
    graphBody :: (Core.Term m),
    -- | All supported primitive constants and functions, by name
    graphPrimitives :: (Map Core.Name (Primitive m)),
    -- | The annotation class which is supported in this context
    graphAnnotations :: (AnnotationClass m),
    -- | The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph.
    graphSchema :: (Maybe (Graph m))}

_Graph = (Core.Name "hydra/graph.Graph")

_Graph_elements = (Core.FieldName "elements")

_Graph_environment = (Core.FieldName "environment")

_Graph_body = (Core.FieldName "body")

_Graph_primitives = (Core.FieldName "primitives")

_Graph_annotations = (Core.FieldName "annotations")

_Graph_schema = (Core.FieldName "schema")

-- | A graph element, having a name, data term (value), and schema term (type)
data Element m = 
  Element {
    elementName :: Core.Name,
    elementSchema :: (Core.Term m),
    elementData :: (Core.Term m)}
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/graph.Element")

_Element_name = (Core.FieldName "name")

_Element_schema = (Core.FieldName "schema")

_Element_data = (Core.FieldName "data")

-- | A built-in function
data Primitive m = 
  Primitive {
    -- | The unique name of the primitive function
    primitiveName :: Core.Name,
    -- | The type signature of the primitive function
    primitiveType :: (Core.Type m),
    -- | A concrete implementation of the primitive function
    primitiveImplementation :: ([Core.Term m] -> Compute.Flow () (Core.Term m))}

_Primitive = (Core.Name "hydra/graph.Primitive")

_Primitive_name = (Core.FieldName "name")

_Primitive_type = (Core.FieldName "type")

_Primitive_implementation = (Core.FieldName "implementation")

-- | A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
data TermCoder m a = 
  TermCoder {
    termCoderType :: (Core.Type m),
    termCoderCoder :: (Compute.Coder () () (Core.Term m) a)}

_TermCoder = (Core.Name "hydra/graph.TermCoder")

_TermCoder_type = (Core.FieldName "type")

_TermCoder_coder = (Core.FieldName "coder")