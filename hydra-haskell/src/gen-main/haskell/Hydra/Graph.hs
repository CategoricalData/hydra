-- | The extension to graphs of Hydra's core type system (hydra/core)

module Hydra.Graph where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | An equality judgement: less than, equal to, or greater than
data Comparison = 
  ComparisonLessThan  |
  ComparisonEqualTo  |
  ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = (Core.Name "hydra/graph.Comparison")

_Comparison_lessThan = (Core.Name "lessThan")

_Comparison_equalTo = (Core.Name "equalTo")

_Comparison_greaterThan = (Core.Name "greaterThan")

_Comparison_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/graph.Comparison"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessThan"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equalTo"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "greaterThan"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph
data Graph = 
  Graph {
    -- | All of the elements in the graph
    graphElements :: (Map Core.Name Element),
    -- | The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
    graphEnvironment :: (Map Core.Name (Maybe Core.Term)),
    -- | The typing environment of the graph
    graphTypes :: (Map Core.Name Core.Type),
    -- | The body of the term which generated this context
    graphBody :: Core.Term,
    -- | All supported primitive constants and functions, by name
    graphPrimitives :: (Map Core.Name Primitive),
    -- | The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph.
    graphSchema :: (Maybe Graph)}

_Graph = (Core.Name "hydra/graph.Graph")

_Graph_elements = (Core.Name "elements")

_Graph_environment = (Core.Name "environment")

_Graph_types = (Core.Name "types")

_Graph_body = (Core.Name "body")

_Graph_primitives = (Core.Name "primitives")

_Graph_schema = (Core.Name "schema")

_Graph_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/graph.Graph"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "elements"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = Core._Name_type_,
        Core.mapTypeValues = _Element_type_}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "environment"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = Core._Name_type_,
        Core.mapTypeValues = (Core.TypeOptional Core._Term_type_)}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "types"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = Core._Name_type_,
        Core.mapTypeValues = Core._Type_type_}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = Core._Term_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitives"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = Core._Name_type_,
        Core.mapTypeValues = _Primitive_type_}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "schema"),
      Core.fieldTypeType = (Core.TypeOptional _Graph_type_)}]}))

-- | A graph element, having a name, data term (value), and schema term (type)
data Element = 
  Element {
    elementName :: Core.Name,
    elementData :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/graph.Element")

_Element_name = (Core.Name "name")

_Element_data = (Core.Name "data")

_Element_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/graph.Element"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = Core._Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "data"),
      Core.fieldTypeType = Core._Term_type_}]}))

-- | A built-in function
data Primitive = 
  Primitive {
    -- | The unique name of the primitive function
    primitiveName :: Core.Name,
    -- | The type signature of the primitive function
    primitiveType :: Core.Type,
    -- | A concrete implementation of the primitive function
    primitiveImplementation :: ([Core.Term] -> Compute.Flow Graph Core.Term)}

_Primitive = (Core.Name "hydra/graph.Primitive")

_Primitive_name = (Core.Name "name")

_Primitive_type = (Core.Name "type")

_Primitive_implementation = (Core.Name "implementation")

_Primitive_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/graph.Primitive"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = Core._Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = Core._Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementation"),
      Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (Core.TypeList Core._Term_type_),
        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = Compute._Flow_type_,
            Core.applicationTypeArgument = _Graph_type_})),
          Core.applicationTypeArgument = Core._Term_type_}))}))}]}))

-- | A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
data TermCoder x = 
  TermCoder {
    termCoderType :: Core.Type,
    termCoderCoder :: (Compute.Coder Graph Graph Core.Term x)}

_TermCoder = (Core.Name "hydra/graph.TermCoder")

_TermCoder_type = (Core.Name "type")

_TermCoder_coder = (Core.Name "coder")

_TermCoder_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "x"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/graph.TermCoder"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "type"),
        Core.fieldTypeType = Core._Type_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "coder"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = Compute._Coder_type_,
                Core.applicationTypeArgument = _Graph_type_})),
              Core.applicationTypeArgument = _Graph_type_})),
            Core.applicationTypeArgument = Core._Term_type_})),
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "x"))}))}]}))}))

-- | Any of a small number of built-in type classes
data TypeClass = 
  TypeClassEquality  |
  TypeClassOrdering 
  deriving (Eq, Ord, Read, Show)

_TypeClass = (Core.Name "hydra/graph.TypeClass")

_TypeClass_equality = (Core.Name "equality")

_TypeClass_ordering = (Core.Name "ordering")

_TypeClass_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/graph.TypeClass"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equality"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ordering"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))