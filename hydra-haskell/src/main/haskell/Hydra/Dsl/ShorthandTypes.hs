module Hydra.Dsl.ShorthandTypes where

import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Module
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


eqA = (M.fromList [(Name "a", S.fromList [TypeClassEquality])])
ordA = (M.fromList [(Name "a", S.fromList [TypeClassOrdering])])

aT = Types.var "a" :: Type
annotatedTermT = Types.apply (TypeVariable _Annotated) termT :: Type
annotatedTypeT = Types.apply (TypeVariable _Annotated) typeT :: Type
applicationT = TypeVariable _Application :: Type
applicationTypeT = TypeVariable _ApplicationType :: Type
booleanT = Types.boolean
caseStatementT = TypeVariable _CaseStatement :: Type
elementT = TypeVariable _Element :: Type
eliminationT = TypeVariable _Elimination :: Type
fieldT = TypeVariable _Field :: Type
fieldTypeT = TypeVariable _FieldType :: Type
floatValueT = TypeVariable _FloatValue :: Type
flowS1AT = flowT (Types.var "s1") aT :: Type
flowS2AT = flowT (Types.var "s2") aT :: Type
flowSAT = flowT sT aT :: Type
flowSST = flowT sT sT :: Type
flowStateT s x = Types.apply (Types.apply (TypeVariable _FlowState) s) x
flowT s x = Types.apply (Types.apply (TypeVariable _Flow) s) x
functionT = Types.function
graphT = TypeVariable _Graph :: Type
injectionT = TypeVariable _Injection :: Type
integerValueT = TypeVariable _IntegerValue :: Type
kvT = TypeVariable _Kv :: Type
lambdaT = TypeVariable _Lambda :: Type
lambdaTypeT = TypeVariable _LambdaType :: Type
languageT = TypeVariable _Language :: Type
letT = TypeVariable _Let :: Type
listT = Types.list
mapT = Types.map
mapTypeT = TypeVariable _MapType :: Type
nameT = TypeVariable _Name
namespaceT = TypeVariable _Namespace
optionalT = Types.optional
pairT = Types.pair
primitiveT = TypeVariable _Primitive :: Type
qualifiedNameT = TypeVariable _QualifiedName
recordT = TypeVariable _Record :: Type
rowTypeT = TypeVariable _RowType :: Type
sT = Types.var "s" :: Type
setT = TypeSet
stringT = Types.string :: Type
sumT = TypeVariable _Sum :: Type
termT = TypeVariable _Term :: Type
termKV = TypeVariable _Term :: Type
traceT = TypeVariable _Trace
typeT = TypeVariable _Type :: Type
unitT = Types.unit :: Type
xT = Types.var "x" :: Type
