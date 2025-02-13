module Hydra.Dsl.ShorthandTypes where

import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Mantle
import Hydra.Module
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


-- Typeclass references
eqA = (M.fromList [(Name "a", S.fromList [TypeClassEquality])])
ordA = (M.fromList [(Name "a", S.fromList [TypeClassOrdering])])

-- Type constructors
tA = tVar "a"
tApply = Types.apply
tApplyN = Types.applyN
tB = tVar "b"
tBigfloat = Types.bigfloat
tBigint = Types.bigint
tBinary = Types.binary
tBoolean = Types.boolean
tC = tVar "c"
tD = tVar "d"
tE = tVar "e"
tFloat32 = Types.float32
tFloat64 = Types.float64
tFlow s x = tApplyN [flowT, s, x]
tFlowS1A = tFlow (tVar "s1") tA
tFlowS2A = tFlow (tVar "s2") tA
tFlowSA = tFlow tS tA
tFlowSS = tFlow tS tS
tFlowState s x = tApplyN [flowStateT, s, x]
tFun = Types.function
tFunN = Types.functionN
tInt8 = Types.int8
tInt16 = Types.int16
tInt32 = Types.int32
tInt64 = Types.int64
tList = Types.list
tMap = Types.map
tMono = Types.mono
tOpt = Types.optional
tPair = Types.pair
tS = tVar "s"
tSet = TypeSet
tString = Types.string
tSum = Types.sum
tUint8 = Types.uint8
tUint16 = Types.uint16
tUint32 = Types.uint32
tUint64 = Types.uint64
tUnit = Types.unit
tVar = Types.var
tX = tVar "x"

-- Type references
annotatedTermT = TypeVariable _AnnotatedTerm :: Type
annotatedTypeT = TypeVariable _AnnotatedType :: Type
applicationT = TypeVariable _Application :: Type
applicationTypeT = TypeVariable _ApplicationType :: Type
caseConventionT = TypeVariable _CaseConvention :: Type
caseStatementT = TypeVariable _CaseStatement :: Type
elementT = TypeVariable _Element :: Type
eliminationT = TypeVariable _Elimination :: Type
eliminationVariantT = TypeVariable _EliminationVariant :: Type
fieldT = TypeVariable _Field :: Type
fieldNameT = TypeVariable _Name :: Type
fieldTypeT = TypeVariable _FieldType :: Type
fileExtensionT = TypeVariable _FileExtension :: Type
floatTypeT = TypeVariable _FloatType :: Type
floatValueT = TypeVariable _FloatValue :: Type
flowStateT = TypeVariable _FlowState :: Type
flowT = TypeVariable _Flow :: Type
functionT = TypeVariable _Function :: Type
functionTypeT = TypeVariable _FunctionType :: Type
functionVariantT = TypeVariable _FunctionVariant :: Type
graphT = TypeVariable _Graph :: Type
injectionT = TypeVariable _Injection :: Type
integerTypeT = TypeVariable _IntegerType :: Type
integerValueT = TypeVariable _IntegerValue :: Type
lambdaT = TypeVariable _Lambda :: Type
lambdaTypeT = TypeVariable _LambdaType :: Type
languageT = TypeVariable _Language :: Type
letT = TypeVariable _Let :: Type
letBindingT = TypeVariable _LetBinding :: Type
literalT = TypeVariable _Literal :: Type
literalTypeT = TypeVariable _LiteralType :: Type
literalVariantT = TypeVariable _LiteralVariant :: Type
mapTypeT = TypeVariable _MapType :: Type
nameT = TypeVariable _Name
namespaceT = TypeVariable _Namespace
optionalCasesT = TypeVariable _OptionalCases :: Type
precisionT = TypeVariable _Precision :: Type
primitiveT = TypeVariable _Primitive :: Type
projectionT = TypeVariable _Projection :: Type
qualifiedNameT = TypeVariable _QualifiedName
recordT = TypeVariable _Record :: Type
rowTypeT = TypeVariable _RowType :: Type
sumT = TypeVariable _Sum :: Type
termT = TypeVariable _Term :: Type
termAccessorT = TypeVariable _TermAccessor :: Type
termVariantT = TypeVariable _TermVariant :: Type
traceT = TypeVariable _Trace
tupleProjectionT = TypeVariable _TupleProjection :: Type
typeT = TypeVariable _Type :: Type
typeAbstractionT = TypeVariable _TypeAbstraction :: Type
typeSchemeT = TypeVariable _TypeScheme :: Type
typedTermT = TypeVariable _TypedTerm :: Type
typeVariantT = TypeVariable _TypeVariant :: Type
wrappedTermT = TypeVariable _WrappedTerm :: Type
wrappedTypeT = TypeVariable _WrappedType :: Type
