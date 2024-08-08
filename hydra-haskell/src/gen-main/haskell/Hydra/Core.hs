-- | Hydra's core data model, defining types, terms, and their dependencies

module Hydra.Core where

import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A term together with an annotation
data AnnotatedTerm = 
  AnnotatedTerm {
    annotatedTermSubject :: Term,
    annotatedTermAnnotation :: (Map String Term)}
  deriving (Eq, Ord, Read, Show)

_AnnotatedTerm = (Name "hydra/core.AnnotatedTerm")

_AnnotatedTerm_subject = (Name "subject")

_AnnotatedTerm_annotation = (Name "annotation")

_AnnotatedTerm_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.AnnotatedTerm"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "subject"),
      fieldTypeType = _Term_type_},
    FieldType {
      fieldTypeName = (Name "annotation"),
      fieldTypeType = (TypeMap (MapType {
        mapTypeKeys = (TypeLiteral LiteralTypeString),
        mapTypeValues = _Term_type_}))}]}))

-- | A type together with an annotation
data AnnotatedType = 
  AnnotatedType {
    annotatedTypeSubject :: Type,
    annotatedTypeAnnotation :: (Map String Term)}
  deriving (Eq, Ord, Read, Show)

_AnnotatedType = (Name "hydra/core.AnnotatedType")

_AnnotatedType_subject = (Name "subject")

_AnnotatedType_annotation = (Name "annotation")

_AnnotatedType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.AnnotatedType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "subject"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "annotation"),
      fieldTypeType = (TypeMap (MapType {
        mapTypeKeys = (TypeLiteral LiteralTypeString),
        mapTypeValues = _Term_type_}))}]}))

-- | A term which applies a function to an argument
data Application = 
  Application {
    -- | The left-hand side of the application
    applicationFunction :: Term,
    -- | The right-hand side of the application
    applicationArgument :: Term}
  deriving (Eq, Ord, Read, Show)

_Application = (Name "hydra/core.Application")

_Application_function = (Name "function")

_Application_argument = (Name "argument")

_Application_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Application"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "function"),
      fieldTypeType = _Term_type_},
    FieldType {
      fieldTypeName = (Name "argument"),
      fieldTypeType = _Term_type_}]}))

-- | The type-level analog of an application term
data ApplicationType = 
  ApplicationType {
    -- | The left-hand side of the application
    applicationTypeFunction :: Type,
    -- | The right-hand side of the application
    applicationTypeArgument :: Type}
  deriving (Eq, Ord, Read, Show)

_ApplicationType = (Name "hydra/core.ApplicationType")

_ApplicationType_function = (Name "function")

_ApplicationType_argument = (Name "argument")

_ApplicationType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.ApplicationType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "function"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "argument"),
      fieldTypeType = _Type_type_}]}))

-- | A union elimination; a case statement
data CaseStatement = 
  CaseStatement {
    caseStatementTypeName :: Name,
    caseStatementDefault :: (Maybe Term),
    caseStatementCases :: [Field]}
  deriving (Eq, Ord, Read, Show)

_CaseStatement = (Name "hydra/core.CaseStatement")

_CaseStatement_typeName = (Name "typeName")

_CaseStatement_default = (Name "default")

_CaseStatement_cases = (Name "cases")

_CaseStatement_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.CaseStatement"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "default"),
      fieldTypeType = (TypeOptional _Term_type_)},
    FieldType {
      fieldTypeName = (Name "cases"),
      fieldTypeType = (TypeList _Field_type_)}]}))

-- | A corresponding elimination for an introduction term
data Elimination = 
  -- | Eliminates a list using a fold function; this function has the signature b -> [a] -> b
  EliminationList Term |
  -- | Eliminates an optional term by matching over the two possible cases
  EliminationOptional OptionalCases |
  -- | Eliminates a tuple by projecting the component at a given 0-indexed offset
  EliminationProduct TupleProjection |
  -- | Eliminates a record by projecting a given field
  EliminationRecord Projection |
  -- | Eliminates a union term by matching over the fields of the union. This is a case statement.
  EliminationUnion CaseStatement |
  -- | Unwrap a wrapped term
  EliminationWrap Name
  deriving (Eq, Ord, Read, Show)

_Elimination = (Name "hydra/core.Elimination")

_Elimination_list = (Name "list")

_Elimination_optional = (Name "optional")

_Elimination_product = (Name "product")

_Elimination_record = (Name "record")

_Elimination_union = (Name "union")

_Elimination_wrap = (Name "wrap")

_Elimination_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.Elimination"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "list"),
      fieldTypeType = _Term_type_},
    FieldType {
      fieldTypeName = (Name "optional"),
      fieldTypeType = _OptionalCases_type_},
    FieldType {
      fieldTypeName = (Name "product"),
      fieldTypeType = _TupleProjection_type_},
    FieldType {
      fieldTypeName = (Name "record"),
      fieldTypeType = _Projection_type_},
    FieldType {
      fieldTypeName = (Name "union"),
      fieldTypeType = _CaseStatement_type_},
    FieldType {
      fieldTypeName = (Name "wrap"),
      fieldTypeType = _Name_type_}]}))

-- | A name/term pair
data Field = 
  Field {
    fieldName :: Name,
    fieldTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_Field = (Name "hydra/core.Field")

_Field_name = (Name "name")

_Field_term = (Name "term")

_Field_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Field"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "name"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "term"),
      fieldTypeType = _Term_type_}]}))

-- | A name/type pair
data FieldType = 
  FieldType {
    fieldTypeName :: Name,
    fieldTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_FieldType = (Name "hydra/core.FieldType")

_FieldType_name = (Name "name")

_FieldType_type = (Name "type")

_FieldType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.FieldType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "name"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "type"),
      fieldTypeType = _Type_type_}]}))

-- | A floating-point type
data FloatType = 
  FloatTypeBigfloat  |
  FloatTypeFloat32  |
  FloatTypeFloat64 
  deriving (Eq, Ord, Read, Show)

_FloatType = (Name "hydra/core.FloatType")

_FloatType_bigfloat = (Name "bigfloat")

_FloatType_float32 = (Name "float32")

_FloatType_float64 = (Name "float64")

_FloatType_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.FloatType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "bigfloat"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "float32"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "float64"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))}]}))

-- | A floating-point literal value
data FloatValue = 
  -- | An arbitrary-precision floating-point value
  FloatValueBigfloat Double |
  -- | A 32-bit floating-point value
  FloatValueFloat32 Float |
  -- | A 64-bit floating-point value
  FloatValueFloat64 Double
  deriving (Eq, Ord, Read, Show)

_FloatValue = (Name "hydra/core.FloatValue")

_FloatValue_bigfloat = (Name "bigfloat")

_FloatValue_float32 = (Name "float32")

_FloatValue_float64 = (Name "float64")

_FloatValue_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.FloatValue"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "bigfloat"),
      fieldTypeType = (TypeLiteral (LiteralTypeFloat FloatTypeBigfloat))},
    FieldType {
      fieldTypeName = (Name "float32"),
      fieldTypeType = (TypeLiteral (LiteralTypeFloat FloatTypeFloat32))},
    FieldType {
      fieldTypeName = (Name "float64"),
      fieldTypeType = (TypeLiteral (LiteralTypeFloat FloatTypeFloat64))}]}))

-- | A function
data Function = 
  -- | An elimination for any of a few term variants
  FunctionElimination Elimination |
  -- | A function abstraction (lambda)
  FunctionLambda Lambda |
  -- | A reference to a built-in (primitive) function
  FunctionPrimitive Name
  deriving (Eq, Ord, Read, Show)

_Function = (Name "hydra/core.Function")

_Function_elimination = (Name "elimination")

_Function_lambda = (Name "lambda")

_Function_primitive = (Name "primitive")

_Function_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.Function"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "elimination"),
      fieldTypeType = _Elimination_type_},
    FieldType {
      fieldTypeName = (Name "lambda"),
      fieldTypeType = _Lambda_type_},
    FieldType {
      fieldTypeName = (Name "primitive"),
      fieldTypeType = _Name_type_}]}))

-- | A function type, also known as an arrow type
data FunctionType = 
  FunctionType {
    functionTypeDomain :: Type,
    functionTypeCodomain :: Type}
  deriving (Eq, Ord, Read, Show)

_FunctionType = (Name "hydra/core.FunctionType")

_FunctionType_domain = (Name "domain")

_FunctionType_codomain = (Name "codomain")

_FunctionType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.FunctionType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "domain"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "codomain"),
      fieldTypeType = _Type_type_}]}))

-- | An instance of a union type; i.e. a string-indexed generalization of inl() or inr()
data Injection = 
  Injection {
    injectionTypeName :: Name,
    injectionField :: Field}
  deriving (Eq, Ord, Read, Show)

_Injection = (Name "hydra/core.Injection")

_Injection_typeName = (Name "typeName")

_Injection_field = (Name "field")

_Injection_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Injection"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "field"),
      fieldTypeType = _Field_type_}]}))

-- | An integer type
data IntegerType = 
  IntegerTypeBigint  |
  IntegerTypeInt8  |
  IntegerTypeInt16  |
  IntegerTypeInt32  |
  IntegerTypeInt64  |
  IntegerTypeUint8  |
  IntegerTypeUint16  |
  IntegerTypeUint32  |
  IntegerTypeUint64 
  deriving (Eq, Ord, Read, Show)

_IntegerType = (Name "hydra/core.IntegerType")

_IntegerType_bigint = (Name "bigint")

_IntegerType_int8 = (Name "int8")

_IntegerType_int16 = (Name "int16")

_IntegerType_int32 = (Name "int32")

_IntegerType_int64 = (Name "int64")

_IntegerType_uint8 = (Name "uint8")

_IntegerType_uint16 = (Name "uint16")

_IntegerType_uint32 = (Name "uint32")

_IntegerType_uint64 = (Name "uint64")

_IntegerType_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.IntegerType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "bigint"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "int8"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "int16"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "int32"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "int64"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "uint8"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "uint16"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "uint32"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "uint64"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))}]}))

-- | An integer literal value
data IntegerValue = 
  -- | An arbitrary-precision integer value
  IntegerValueBigint Integer |
  -- | An 8-bit signed integer value
  IntegerValueInt8 Int8 |
  -- | A 16-bit signed integer value (short value)
  IntegerValueInt16 Int16 |
  -- | A 32-bit signed integer value (int value)
  IntegerValueInt32 Int |
  -- | A 64-bit signed integer value (long value)
  IntegerValueInt64 Int64 |
  -- | An 8-bit unsigned integer value (byte)
  IntegerValueUint8 Int16 |
  -- | A 16-bit unsigned integer value
  IntegerValueUint16 Int |
  -- | A 32-bit unsigned integer value (unsigned int)
  IntegerValueUint32 Int64 |
  -- | A 64-bit unsigned integer value (unsigned long)
  IntegerValueUint64 Integer
  deriving (Eq, Ord, Read, Show)

_IntegerValue = (Name "hydra/core.IntegerValue")

_IntegerValue_bigint = (Name "bigint")

_IntegerValue_int8 = (Name "int8")

_IntegerValue_int16 = (Name "int16")

_IntegerValue_int32 = (Name "int32")

_IntegerValue_int64 = (Name "int64")

_IntegerValue_uint8 = (Name "uint8")

_IntegerValue_uint16 = (Name "uint16")

_IntegerValue_uint32 = (Name "uint32")

_IntegerValue_uint64 = (Name "uint64")

_IntegerValue_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.IntegerValue"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "bigint"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeBigint))},
    FieldType {
      fieldTypeName = (Name "int8"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt8))},
    FieldType {
      fieldTypeName = (Name "int16"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt16))},
    FieldType {
      fieldTypeName = (Name "int32"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))},
    FieldType {
      fieldTypeName = (Name "int64"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt64))},
    FieldType {
      fieldTypeName = (Name "uint8"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeUint8))},
    FieldType {
      fieldTypeName = (Name "uint16"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeUint16))},
    FieldType {
      fieldTypeName = (Name "uint32"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeUint32))},
    FieldType {
      fieldTypeName = (Name "uint64"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeUint64))}]}))

-- | A function abstraction (lambda)
data Lambda = 
  Lambda {
    -- | The parameter of the lambda
    lambdaParameter :: Name,
    -- | An optional domain type for the lambda
    lambdaDomain :: (Maybe Type),
    -- | The body of the lambda
    lambdaBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Lambda = (Name "hydra/core.Lambda")

_Lambda_parameter = (Name "parameter")

_Lambda_domain = (Name "domain")

_Lambda_body = (Name "body")

_Lambda_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Lambda"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "parameter"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "domain"),
      fieldTypeType = (TypeOptional _Type_type_)},
    FieldType {
      fieldTypeName = (Name "body"),
      fieldTypeType = _Term_type_}]}))

-- | A type abstraction; the type-level analog of a lambda term
data LambdaType = 
  LambdaType {
    -- | The variable which is bound by the lambda
    lambdaTypeParameter :: Name,
    -- | The body of the lambda
    lambdaTypeBody :: Type}
  deriving (Eq, Ord, Read, Show)

_LambdaType = (Name "hydra/core.LambdaType")

_LambdaType_parameter = (Name "parameter")

_LambdaType_body = (Name "body")

_LambdaType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.LambdaType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "parameter"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "body"),
      fieldTypeType = _Type_type_}]}))

-- | A set of (possibly recursive) 'let' bindings together with an environment in which they are bound
data Let = 
  Let {
    letBindings :: [LetBinding],
    letEnvironment :: Term}
  deriving (Eq, Ord, Read, Show)

_Let = (Name "hydra/core.Let")

_Let_bindings = (Name "bindings")

_Let_environment = (Name "environment")

_Let_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Let"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "bindings"),
      fieldTypeType = (TypeList _LetBinding_type_)},
    FieldType {
      fieldTypeName = (Name "environment"),
      fieldTypeType = _Term_type_}]}))

-- | A field with an optional type scheme, used to bind variables to terms in a 'let' expression
data LetBinding = 
  LetBinding {
    letBindingName :: Name,
    letBindingTerm :: Term,
    letBindingType :: (Maybe TypeScheme)}
  deriving (Eq, Ord, Read, Show)

_LetBinding = (Name "hydra/core.LetBinding")

_LetBinding_name = (Name "name")

_LetBinding_term = (Name "term")

_LetBinding_type = (Name "type")

_LetBinding_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.LetBinding"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "name"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "term"),
      fieldTypeType = _Term_type_},
    FieldType {
      fieldTypeName = (Name "type"),
      fieldTypeType = (TypeOptional _TypeScheme_type_)}]}))

-- | A term constant; an instance of a literal type
data Literal = 
  -- | A binary literal
  LiteralBinary String |
  -- | A boolean literal
  LiteralBoolean Bool |
  -- | A floating-point literal
  LiteralFloat FloatValue |
  -- | An integer literal
  LiteralInteger IntegerValue |
  -- | A string literal
  LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = (Name "hydra/core.Literal")

_Literal_binary = (Name "binary")

_Literal_boolean = (Name "boolean")

_Literal_float = (Name "float")

_Literal_integer = (Name "integer")

_Literal_string = (Name "string")

_Literal_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.Literal"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "binary"),
      fieldTypeType = (TypeLiteral LiteralTypeBinary)},
    FieldType {
      fieldTypeName = (Name "boolean"),
      fieldTypeType = (TypeLiteral LiteralTypeBoolean)},
    FieldType {
      fieldTypeName = (Name "float"),
      fieldTypeType = _FloatValue_type_},
    FieldType {
      fieldTypeName = (Name "integer"),
      fieldTypeType = _IntegerValue_type_},
    FieldType {
      fieldTypeName = (Name "string"),
      fieldTypeType = (TypeLiteral LiteralTypeString)}]}))

-- | Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants
data LiteralType = 
  LiteralTypeBinary  |
  LiteralTypeBoolean  |
  LiteralTypeFloat FloatType |
  LiteralTypeInteger IntegerType |
  LiteralTypeString 
  deriving (Eq, Ord, Read, Show)

_LiteralType = (Name "hydra/core.LiteralType")

_LiteralType_binary = (Name "binary")

_LiteralType_boolean = (Name "boolean")

_LiteralType_float = (Name "float")

_LiteralType_integer = (Name "integer")

_LiteralType_string = (Name "string")

_LiteralType_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.LiteralType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "binary"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "boolean"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))},
    FieldType {
      fieldTypeName = (Name "float"),
      fieldTypeType = _FloatType_type_},
    FieldType {
      fieldTypeName = (Name "integer"),
      fieldTypeType = _IntegerType_type_},
    FieldType {
      fieldTypeName = (Name "string"),
      fieldTypeType = (TypeRecord (RowType {
        rowTypeTypeName = (Name "hydra/core.Unit"),
        rowTypeExtends = Nothing,
        rowTypeFields = []}))}]}))

-- | A map type
data MapType = 
  MapType {
    mapTypeKeys :: Type,
    mapTypeValues :: Type}
  deriving (Eq, Ord, Read, Show)

_MapType = (Name "hydra/core.MapType")

_MapType_keys = (Name "keys")

_MapType_values = (Name "values")

_MapType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.MapType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "keys"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "values"),
      fieldTypeType = _Type_type_}]}))

-- | A symbol which stands for a term, type, or element
newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Name "hydra/core.Name")

_Name_type_ = (TypeWrap (WrappedType {
  wrappedTypeTypeName = (Name "hydra/core.Name"),
  wrappedTypeObject = (TypeLiteral LiteralTypeString)}))

-- | A term wrapped in a type name
data WrappedTerm = 
  WrappedTerm {
    wrappedTermTypeName :: Name,
    wrappedTermObject :: Term}
  deriving (Eq, Ord, Read, Show)

_WrappedTerm = (Name "hydra/core.WrappedTerm")

_WrappedTerm_typeName = (Name "typeName")

_WrappedTerm_object = (Name "object")

_WrappedTerm_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.WrappedTerm"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "object"),
      fieldTypeType = _Term_type_}]}))

-- | A type wrapped in a type name
data WrappedType = 
  WrappedType {
    wrappedTypeTypeName :: Name,
    wrappedTypeObject :: Type}
  deriving (Eq, Ord, Read, Show)

_WrappedType = (Name "hydra/core.WrappedType")

_WrappedType_typeName = (Name "typeName")

_WrappedType_object = (Name "object")

_WrappedType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.WrappedType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "object"),
      fieldTypeType = _Type_type_}]}))

-- | A case statement for matching optional terms
data OptionalCases = 
  OptionalCases {
    -- | A term provided if the optional value is nothing
    optionalCasesNothing :: Term,
    -- | A function which is applied if the optional value is non-nothing
    optionalCasesJust :: Term}
  deriving (Eq, Ord, Read, Show)

_OptionalCases = (Name "hydra/core.OptionalCases")

_OptionalCases_nothing = (Name "nothing")

_OptionalCases_just = (Name "just")

_OptionalCases_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.OptionalCases"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "nothing"),
      fieldTypeType = _Term_type_},
    FieldType {
      fieldTypeName = (Name "just"),
      fieldTypeType = _Term_type_}]}))

-- | A record elimination; a projection
data Projection = 
  Projection {
    -- | The name of the record type
    projectionTypeName :: Name,
    -- | The name of the projected field
    projectionField :: Name}
  deriving (Eq, Ord, Read, Show)

_Projection = (Name "hydra/core.Projection")

_Projection_typeName = (Name "typeName")

_Projection_field = (Name "field")

_Projection_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Projection"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "field"),
      fieldTypeType = _Name_type_}]}))

-- | A record, or labeled tuple; a map of field names to terms
data Record = 
  Record {
    recordTypeName :: Name,
    recordFields :: [Field]}
  deriving (Eq, Ord, Read, Show)

_Record = (Name "hydra/core.Record")

_Record_typeName = (Name "typeName")

_Record_fields = (Name "fields")

_Record_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Record"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "fields"),
      fieldTypeType = (TypeList _Field_type_)}]}))

-- | A labeled record or union type
data RowType = 
  RowType {
    -- | The name of the row type, which must correspond to the name of a Type element
    rowTypeTypeName :: Name,
    -- | Optionally, the name of another row type which this one extends. If/when field order is preserved, the inherited fields of the extended type precede those of the extension.
    rowTypeExtends :: (Maybe Name),
    -- | The fields of this row type, excluding any inherited fields
    rowTypeFields :: [FieldType]}
  deriving (Eq, Ord, Read, Show)

_RowType = (Name "hydra/core.RowType")

_RowType_typeName = (Name "typeName")

_RowType_extends = (Name "extends")

_RowType_fields = (Name "fields")

_RowType_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.RowType"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "typeName"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "extends"),
      fieldTypeType = (TypeOptional _Name_type_)},
    FieldType {
      fieldTypeName = (Name "fields"),
      fieldTypeType = (TypeList _FieldType_type_)}]}))

-- | The unlabeled equivalent of an Injection term
data Sum = 
  Sum {
    sumIndex :: Int,
    sumSize :: Int,
    sumTerm :: Term}
  deriving (Eq, Ord, Read, Show)

_Sum = (Name "hydra/core.Sum")

_Sum_index = (Name "index")

_Sum_size = (Name "size")

_Sum_term = (Name "term")

_Sum_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Sum"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "index"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))},
    FieldType {
      fieldTypeName = (Name "size"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))},
    FieldType {
      fieldTypeName = (Name "term"),
      fieldTypeType = _Term_type_}]}))

-- | A data term
data Term = 
  -- | A term annotated with metadata
  TermAnnotated AnnotatedTerm |
  -- | A function application
  TermApplication Application |
  -- | A function term
  TermFunction Function |
  TermLet Let |
  -- | A list
  TermList [Term] |
  -- | A literal value
  TermLiteral Literal |
  -- | A map of keys to values
  TermMap (Map Term Term) |
  -- | An optional value
  TermOptional (Maybe Term) |
  -- | A tuple
  TermProduct [Term] |
  -- | A record term
  TermRecord Record |
  -- | A set of values
  TermSet (Set Term) |
  -- | A variant tuple
  TermSum Sum |
  -- | A term annotated with its type
  TermTyped TypedTerm |
  -- | An injection; an instance of a union type
  TermUnion Injection |
  -- | A variable reference
  TermVariable Name |
  TermWrap WrappedTerm
  deriving (Eq, Ord, Read, Show)

_Term = (Name "hydra/core.Term")

_Term_annotated = (Name "annotated")

_Term_application = (Name "application")

_Term_function = (Name "function")

_Term_let = (Name "let")

_Term_list = (Name "list")

_Term_literal = (Name "literal")

_Term_map = (Name "map")

_Term_optional = (Name "optional")

_Term_product = (Name "product")

_Term_record = (Name "record")

_Term_set = (Name "set")

_Term_sum = (Name "sum")

_Term_typed = (Name "typed")

_Term_union = (Name "union")

_Term_variable = (Name "variable")

_Term_wrap = (Name "wrap")

_Term_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.Term"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "annotated"),
      fieldTypeType = _AnnotatedTerm_type_},
    FieldType {
      fieldTypeName = (Name "application"),
      fieldTypeType = _Application_type_},
    FieldType {
      fieldTypeName = (Name "function"),
      fieldTypeType = _Function_type_},
    FieldType {
      fieldTypeName = (Name "let"),
      fieldTypeType = _Let_type_},
    FieldType {
      fieldTypeName = (Name "list"),
      fieldTypeType = (TypeList _Term_type_)},
    FieldType {
      fieldTypeName = (Name "literal"),
      fieldTypeType = _Literal_type_},
    FieldType {
      fieldTypeName = (Name "map"),
      fieldTypeType = (TypeMap (MapType {
        mapTypeKeys = _Term_type_,
        mapTypeValues = _Term_type_}))},
    FieldType {
      fieldTypeName = (Name "optional"),
      fieldTypeType = (TypeOptional _Term_type_)},
    FieldType {
      fieldTypeName = (Name "product"),
      fieldTypeType = (TypeList _Term_type_)},
    FieldType {
      fieldTypeName = (Name "record"),
      fieldTypeType = _Record_type_},
    FieldType {
      fieldTypeName = (Name "set"),
      fieldTypeType = (TypeSet _Term_type_)},
    FieldType {
      fieldTypeName = (Name "sum"),
      fieldTypeType = _Sum_type_},
    FieldType {
      fieldTypeName = (Name "typed"),
      fieldTypeType = _TypedTerm_type_},
    FieldType {
      fieldTypeName = (Name "union"),
      fieldTypeType = _Injection_type_},
    FieldType {
      fieldTypeName = (Name "variable"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "wrap"),
      fieldTypeType = _WrappedTerm_type_}]}))

-- | A tuple elimination; a projection from an integer-indexed product
data TupleProjection = 
  TupleProjection {
    -- | The arity of the tuple
    tupleProjectionArity :: Int,
    -- | The 0-indexed offset from the beginning of the tuple
    tupleProjectionIndex :: Int}
  deriving (Eq, Ord, Read, Show)

_TupleProjection = (Name "hydra/core.TupleProjection")

_TupleProjection_arity = (Name "arity")

_TupleProjection_index = (Name "index")

_TupleProjection_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.TupleProjection"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "arity"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))},
    FieldType {
      fieldTypeName = (Name "index"),
      fieldTypeType = (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))}]}))

-- | A data type
data Type = 
  TypeAnnotated AnnotatedType |
  TypeApplication ApplicationType |
  TypeFunction FunctionType |
  TypeLambda LambdaType |
  TypeList Type |
  TypeLiteral LiteralType |
  TypeMap MapType |
  TypeOptional Type |
  TypeProduct [Type] |
  TypeRecord RowType |
  TypeSet Type |
  TypeSum [Type] |
  TypeUnion RowType |
  TypeVariable Name |
  TypeWrap WrappedType
  deriving (Eq, Ord, Read, Show)

_Type = (Name "hydra/core.Type")

_Type_annotated = (Name "annotated")

_Type_application = (Name "application")

_Type_function = (Name "function")

_Type_lambda = (Name "lambda")

_Type_list = (Name "list")

_Type_literal = (Name "literal")

_Type_map = (Name "map")

_Type_optional = (Name "optional")

_Type_product = (Name "product")

_Type_record = (Name "record")

_Type_set = (Name "set")

_Type_sum = (Name "sum")

_Type_union = (Name "union")

_Type_variable = (Name "variable")

_Type_wrap = (Name "wrap")

_Type_type_ = (TypeUnion (RowType {
  rowTypeTypeName = (Name "hydra/core.Type"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "annotated"),
      fieldTypeType = _AnnotatedType_type_},
    FieldType {
      fieldTypeName = (Name "application"),
      fieldTypeType = _ApplicationType_type_},
    FieldType {
      fieldTypeName = (Name "function"),
      fieldTypeType = _FunctionType_type_},
    FieldType {
      fieldTypeName = (Name "lambda"),
      fieldTypeType = _LambdaType_type_},
    FieldType {
      fieldTypeName = (Name "list"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "literal"),
      fieldTypeType = _LiteralType_type_},
    FieldType {
      fieldTypeName = (Name "map"),
      fieldTypeType = _MapType_type_},
    FieldType {
      fieldTypeName = (Name "optional"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "product"),
      fieldTypeType = (TypeList _Type_type_)},
    FieldType {
      fieldTypeName = (Name "record"),
      fieldTypeType = _RowType_type_},
    FieldType {
      fieldTypeName = (Name "set"),
      fieldTypeType = _Type_type_},
    FieldType {
      fieldTypeName = (Name "sum"),
      fieldTypeType = (TypeList _Type_type_)},
    FieldType {
      fieldTypeName = (Name "union"),
      fieldTypeType = _RowType_type_},
    FieldType {
      fieldTypeName = (Name "variable"),
      fieldTypeType = _Name_type_},
    FieldType {
      fieldTypeName = (Name "wrap"),
      fieldTypeType = _WrappedType_type_}]}))

-- | A type expression together with free type variables occurring in the expression
data TypeScheme = 
  TypeScheme {
    typeSchemeVariables :: [Name],
    typeSchemeType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = (Name "hydra/core.TypeScheme")

_TypeScheme_variables = (Name "variables")

_TypeScheme_type = (Name "type")

_TypeScheme_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.TypeScheme"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "variables"),
      fieldTypeType = (TypeList _Name_type_)},
    FieldType {
      fieldTypeName = (Name "type"),
      fieldTypeType = _Type_type_}]}))

-- | A term together with its type
data TypedTerm = 
  TypedTerm {
    typedTermTerm :: Term,
    typedTermType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypedTerm = (Name "hydra/core.TypedTerm")

_TypedTerm_term = (Name "term")

_TypedTerm_type = (Name "type")

_TypedTerm_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.TypedTerm"),
  rowTypeExtends = Nothing,
  rowTypeFields = [
    FieldType {
      fieldTypeName = (Name "term"),
      fieldTypeType = _Term_type_},
    FieldType {
      fieldTypeName = (Name "type"),
      fieldTypeType = _Type_type_}]}))

-- | An empty record as a canonical unit value
data Unit = 
  Unit {}
  deriving (Eq, Ord, Read, Show)

_Unit = (Name "hydra/core.Unit")

_Unit_type_ = (TypeRecord (RowType {
  rowTypeTypeName = (Name "hydra/core.Unit"),
  rowTypeExtends = Nothing,
  rowTypeFields = []}))