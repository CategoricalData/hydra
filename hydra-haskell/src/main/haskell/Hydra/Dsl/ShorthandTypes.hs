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
booleanT = Types.boolean
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
flowS1AT = flowT (Types.var "s1") aT :: Type
flowS2AT = flowT (Types.var "s2") aT :: Type
flowSAT = flowT sT aT :: Type
flowSST = flowT sT sT :: Type
flowStateT s x = Types.apply (Types.apply (TypeVariable _FlowState) s) x
flowT s x = Types.apply (Types.apply (TypeVariable _Flow) s) x
funT = Types.function
functionT = TypeVariable _Function :: Type
functionVariantT = TypeVariable _FunctionVariant :: Type
graphT = TypeVariable _Graph :: Type
injectionT = TypeVariable _Injection :: Type
int32T = Types.int32
integerTypeT = TypeVariable _IntegerType :: Type
integerValueT = TypeVariable _IntegerValue :: Type
kvT = mapT stringT termT :: Type
lambdaT = TypeVariable _Lambda :: Type
lambdaTypeT = TypeVariable _LambdaType :: Type
languageT = TypeVariable _Language :: Type
letT = TypeVariable _Let :: Type
listT = Types.list
literalT = TypeVariable _Literal :: Type
literalTypeT = TypeVariable _LiteralType :: Type
literalVariantT = TypeVariable _LiteralVariant :: Type
mapT = Types.map
mapTypeT = TypeVariable _MapType :: Type
nameT = TypeVariable _Name
namespaceT = TypeVariable _Namespace
optionalT = Types.optional
pairT = Types.pair
precisionT = TypeVariable _Precision :: Type
primitiveT = TypeVariable _Primitive :: Type
qualifiedNameT = TypeVariable _QualifiedName
recordT = TypeVariable _Record :: Type
rowTypeT = TypeVariable _RowType :: Type
sT = Types.var "s" :: Type
setT = TypeSet
stringT = Types.string :: Type
sumT = TypeVariable _Sum :: Type
termT = TypeVariable _Term :: Type
termVariantT = TypeVariable _TermVariant :: Type
traceT = TypeVariable _Trace
typeT = TypeVariable _Type :: Type
typeVariantT = TypeVariable _TypeVariant :: Type
unitT = Types.unit :: Type
wrappedTermT = TypeVariable _WrappedTerm :: Type
wrappedTypeT = TypeVariable _WrappedType :: Type
xT = Types.var "x" :: Type
