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


eqA = (M.fromList [(Name "a", S.fromList [TypeClassEquality])])
ordA = (M.fromList [(Name "a", S.fromList [TypeClassOrdering])])

aT = Types.var "a" :: Type
annotatedTermT = TypeVariable _AnnotatedTerm :: Type
annotatedTypeT = TypeVariable _AnnotatedType :: Type
applicationT = TypeVariable _Application :: Type
applicationTypeT = TypeVariable _ApplicationType :: Type
bT = Types.var "b" :: Type
bigfloatT = Types.bigfloat
bigintT = Types.bigint
binaryT = Types.binary
booleanT = Types.boolean
cT = Types.var "c" :: Type
caseStatementT = TypeVariable _CaseStatement :: Type
dT = Types.var "d" :: Type
eT = Types.var "e" :: Type
elementT = TypeVariable _Element :: Type
eliminationT = TypeVariable _Elimination :: Type
eliminationVariantT = TypeVariable _EliminationVariant :: Type
fieldT = TypeVariable _Field :: Type
fieldNameT = TypeVariable _Name :: Type
fieldTypeT = TypeVariable _FieldType :: Type
fileExtensionT = TypeVariable _FileExtension :: Type
float32T = Types.float32
float64T = Types.float64
floatTypeT = TypeVariable _FloatType :: Type
floatValueT = TypeVariable _FloatValue :: Type
flowS1AT = flowT (Types.var "s1") aT :: Type
flowS2AT = flowT (Types.var "s2") aT :: Type
flowSAT = flowT sT aT :: Type
flowSST = flowT sT sT :: Type
flowStateT s x = Types.apply (Types.apply (TypeVariable _FlowState) s) x
flowT s x = Types.apply (Types.apply (TypeVariable _Flow) s) x
funT = Types.function
functionT = TypeVariable _Function :: Type
functionTypeT = TypeVariable _FunctionType :: Type
functionVariantT = TypeVariable _FunctionVariant :: Type
graphT = TypeVariable _Graph :: Type
injectionT = TypeVariable _Injection :: Type
int8T = Types.int8
int16T = Types.int16
int32T = Types.int32
int64T = Types.int64
integerTypeT = TypeVariable _IntegerType :: Type
integerValueT = TypeVariable _IntegerValue :: Type
kvT = mapT nameT termT :: Type
lambdaT = TypeVariable _Lambda :: Type
lambdaTypeT = TypeVariable _LambdaType :: Type
languageT = TypeVariable _Language :: Type
letT = TypeVariable _Let :: Type
letBindingT = TypeVariable _LetBinding :: Type
listT = Types.list
literalT = TypeVariable _Literal :: Type
literalTypeT = TypeVariable _LiteralType :: Type
literalVariantT = TypeVariable _LiteralVariant :: Type
mapT = Types.map
mapTypeT = TypeVariable _MapType :: Type
nameT = TypeVariable _Name
namespaceT = TypeVariable _Namespace
optionalT = Types.optional
optionalCasesT = TypeVariable _OptionalCases :: Type
pairT = Types.pair
precisionT = TypeVariable _Precision :: Type
primitiveT = TypeVariable _Primitive :: Type
projectionT = TypeVariable _Projection :: Type
qualifiedNameT = TypeVariable _QualifiedName
recordT = TypeVariable _Record :: Type
rowTypeT = TypeVariable _RowType :: Type
sT = Types.var "s" :: Type
setT = TypeSet
stringT = Types.string :: Type
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
uint8T = Types.uint8
uint16T = Types.uint16
uint32T = Types.uint32
uint64T = Types.uint64
unitT = Types.unit :: Type
wrappedTermT = TypeVariable _WrappedTerm :: Type
wrappedTypeT = TypeVariable _WrappedType :: Type
xT = Types.var "x" :: Type
