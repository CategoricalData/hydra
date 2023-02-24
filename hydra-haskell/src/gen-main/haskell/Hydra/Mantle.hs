-- | A set of types which supplement hydra/core with type variants, graphs, and elements

module Hydra.Mantle where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | An equality judgement: less than, equal to, or greater than
data Comparison = 
  ComparisonLessThan  |
  ComparisonEqualTo  |
  ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = (Core.Name "hydra/mantle.Comparison")

_Comparison_lessThan = (Core.FieldName "lessThan")

_Comparison_equalTo = (Core.FieldName "equalTo")

_Comparison_greaterThan = (Core.FieldName "greaterThan")

-- | A graph element, having a name, data term (value), and schema term (type)
data Element m = 
  Element {
    elementName :: Core.Name,
    elementSchema :: (Core.Term m),
    elementData :: (Core.Term m)}
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/mantle.Element")

_Element_name = (Core.FieldName "name")

_Element_schema = (Core.FieldName "schema")

_Element_data = (Core.FieldName "data")

-- | The identifier of an elimination constructor
data EliminationVariant = 
  EliminationVariantElement  |
  EliminationVariantList  |
  EliminationVariantWrapped  |
  EliminationVariantOptional  |
  EliminationVariantRecord  |
  EliminationVariantUnion 
  deriving (Eq, Ord, Read, Show)

_EliminationVariant = (Core.Name "hydra/mantle.EliminationVariant")

_EliminationVariant_element = (Core.FieldName "element")

_EliminationVariant_list = (Core.FieldName "list")

_EliminationVariant_wrapped = (Core.FieldName "wrapped")

_EliminationVariant_optional = (Core.FieldName "optional")

_EliminationVariant_record = (Core.FieldName "record")

_EliminationVariant_union = (Core.FieldName "union")

-- | The identifier of a function constructor
data FunctionVariant = 
  FunctionVariantElimination  |
  FunctionVariantLambda  |
  FunctionVariantPrimitive 
  deriving (Eq, Ord, Read, Show)

_FunctionVariant = (Core.Name "hydra/mantle.FunctionVariant")

_FunctionVariant_elimination = (Core.FieldName "elimination")

_FunctionVariant_lambda = (Core.FieldName "lambda")

_FunctionVariant_primitive = (Core.FieldName "primitive")

-- | A graph, or set of named terms, together with its schema graph
data Graph m = 
  Graph {
    -- | All of the elements in the graph
    graphElements :: (Map Core.Name (Element m)),
    -- | The schema graph to this graph. If omitted, the graph is its own schema graph.
    graphSchema :: (Maybe (Graph m))}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/mantle.Graph")

_Graph_elements = (Core.FieldName "elements")

_Graph_schema = (Core.FieldName "schema")

-- | The identifier of a literal constructor
data LiteralVariant = 
  LiteralVariantBinary  |
  LiteralVariantBoolean  |
  LiteralVariantFloat  |
  LiteralVariantInteger  |
  LiteralVariantString 
  deriving (Eq, Ord, Read, Show)

_LiteralVariant = (Core.Name "hydra/mantle.LiteralVariant")

_LiteralVariant_binary = (Core.FieldName "binary")

_LiteralVariant_boolean = (Core.FieldName "boolean")

_LiteralVariant_float = (Core.FieldName "float")

_LiteralVariant_integer = (Core.FieldName "integer")

_LiteralVariant_string = (Core.FieldName "string")

-- | Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision = 
  PrecisionArbitrary  |
  PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra/mantle.Precision")

_Precision_arbitrary = (Core.FieldName "arbitrary")

_Precision_bits = (Core.FieldName "bits")

-- | The identifier of a term expression constructor
data TermVariant = 
  TermVariantAnnotated  |
  TermVariantApplication  |
  TermVariantElement  |
  TermVariantFunction  |
  TermVariantLet  |
  TermVariantList  |
  TermVariantLiteral  |
  TermVariantMap  |
  TermVariantWrapped  |
  TermVariantOptional  |
  TermVariantProduct  |
  TermVariantRecord  |
  TermVariantSet  |
  TermVariantStream  |
  TermVariantSum  |
  TermVariantUnion  |
  TermVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TermVariant = (Core.Name "hydra/mantle.TermVariant")

_TermVariant_annotated = (Core.FieldName "annotated")

_TermVariant_application = (Core.FieldName "application")

_TermVariant_element = (Core.FieldName "element")

_TermVariant_function = (Core.FieldName "function")

_TermVariant_let = (Core.FieldName "let")

_TermVariant_list = (Core.FieldName "list")

_TermVariant_literal = (Core.FieldName "literal")

_TermVariant_map = (Core.FieldName "map")

_TermVariant_wrapped = (Core.FieldName "wrapped")

_TermVariant_optional = (Core.FieldName "optional")

_TermVariant_product = (Core.FieldName "product")

_TermVariant_record = (Core.FieldName "record")

_TermVariant_set = (Core.FieldName "set")

_TermVariant_stream = (Core.FieldName "stream")

_TermVariant_sum = (Core.FieldName "sum")

_TermVariant_union = (Core.FieldName "union")

_TermVariant_variable = (Core.FieldName "variable")

-- | A type expression together with free type variables occurring in the expression
data TypeScheme m = 
  TypeScheme {
    typeSchemeVariables :: [Core.VariableType],
    typeSchemeType :: (Core.Type m)}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = (Core.Name "hydra/mantle.TypeScheme")

_TypeScheme_variables = (Core.FieldName "variables")

_TypeScheme_type = (Core.FieldName "type")

-- | The identifier of a type constructor
data TypeVariant = 
  TypeVariantAnnotated  |
  TypeVariantApplication  |
  TypeVariantElement  |
  TypeVariantFunction  |
  TypeVariantLambda  |
  TypeVariantList  |
  TypeVariantLiteral  |
  TypeVariantMap  |
  TypeVariantWrapped  |
  TypeVariantOptional  |
  TypeVariantProduct  |
  TypeVariantRecord  |
  TypeVariantSet  |
  TypeVariantStream  |
  TypeVariantSum  |
  TypeVariantUnion  |
  TypeVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TypeVariant = (Core.Name "hydra/mantle.TypeVariant")

_TypeVariant_annotated = (Core.FieldName "annotated")

_TypeVariant_application = (Core.FieldName "application")

_TypeVariant_element = (Core.FieldName "element")

_TypeVariant_function = (Core.FieldName "function")

_TypeVariant_lambda = (Core.FieldName "lambda")

_TypeVariant_list = (Core.FieldName "list")

_TypeVariant_literal = (Core.FieldName "literal")

_TypeVariant_map = (Core.FieldName "map")

_TypeVariant_wrapped = (Core.FieldName "wrapped")

_TypeVariant_optional = (Core.FieldName "optional")

_TypeVariant_product = (Core.FieldName "product")

_TypeVariant_record = (Core.FieldName "record")

_TypeVariant_set = (Core.FieldName "set")

_TypeVariant_stream = (Core.FieldName "stream")

_TypeVariant_sum = (Core.FieldName "sum")

_TypeVariant_union = (Core.FieldName "union")

_TypeVariant_variable = (Core.FieldName "variable")

-- | A type together with an instance of the type
data TypedTerm m = 
  TypedTerm {
    typedTermType :: (Core.Type m),
    typedTermTerm :: (Core.Term m)}
  deriving (Eq, Ord, Read, Show)

_TypedTerm = (Core.Name "hydra/mantle.TypedTerm")

_TypedTerm_type = (Core.FieldName "type")

_TypedTerm_term = (Core.FieldName "term")