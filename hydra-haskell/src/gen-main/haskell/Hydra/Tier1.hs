-- | A module for miscellaneous tier-1 functions and constants.

module Hydra.Tier1 where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Strip as Strip
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Convert a floating-point value of any precision to a bigfloat
floatValueToBigfloat :: (Core.FloatValue -> Double)
floatValueToBigfloat x = case x of
  Core.FloatValueBigfloat v0 -> (Equality.identity v0)
  Core.FloatValueFloat32 v1 -> (Literals.float32ToBigfloat v1)
  Core.FloatValueFloat64 v2 -> (Literals.float64ToBigfloat v2)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v3 -> (Equality.identity v3)
  Core.IntegerValueInt8 v4 -> (Literals.int8ToBigint v4)
  Core.IntegerValueInt16 v5 -> (Literals.int16ToBigint v5)
  Core.IntegerValueInt32 v6 -> (Literals.int32ToBigint v6)
  Core.IntegerValueInt64 v7 -> (Literals.int64ToBigint v7)
  Core.IntegerValueUint8 v8 -> (Literals.uint8ToBigint v8)
  Core.IntegerValueUint16 v9 -> (Literals.uint16ToBigint v9)
  Core.IntegerValueUint32 v10 -> (Literals.uint32ToBigint v10)
  Core.IntegerValueUint64 v11 -> (Literals.uint64ToBigint v11)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v12 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v12)
  Core.TermLet v14 -> (isLambda (Core.letEnvironment v14))
  _ -> False) (Strip.fullyStripTerm term))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v15 -> (Strings.cat [
            Module.unNamespace v15,
            "."])) (Module.qualifiedNameNamespace qname))
  in (Core.Name (Strings.cat [
    prefix,
    (Module.qualifiedNameLocal qname)]))

-- | Fold over a term, traversing its subterms in the specified order
foldOverTerm :: (Coders.TraversalOrder -> (x -> Core.Term -> x) -> x -> Core.Term -> x)
foldOverTerm order fld b0 term = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverTerm order fld) (fld b0 term) (subterms term))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverTerm order fld) b0 (subterms term)) term)) order)

-- | Fold over a type, traversing its subtypes in the specified order
foldOverType :: (Coders.TraversalOrder -> (x -> Core.Type -> x) -> x -> Core.Type -> x)
foldOverType order fld b0 typ = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverType order fld) (fld b0 typ) (subtypes typ))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverType order fld) b0 (subtypes typ)) typ)) order)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: (Core.Term -> Set Core.Name)
freeVariablesInTerm term =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (subterms term))
  in ((\x -> case x of
    Core.TermFunction v20 -> ((\x -> case x of
      Core.FunctionLambda v21 -> (Sets.remove (Core.lambdaParameter v21) (freeVariablesInTerm (Core.lambdaBody v21)))
      _ -> dfltVars) v20)
    Core.TermVariable v22 -> (Sets.singleton v22)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v23 -> (Sets.remove (Core.lambdaTypeParameter v23) (freeVariablesInType (Core.lambdaTypeBody v23)))
    Core.TypeVariable v24 -> (Sets.singleton v24)
    _ -> dfltVars) typ)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v25 -> [
    Core.annotatedTermSubject v25]
  Core.TermApplication v26 -> [
    Core.applicationFunction v26,
    (Core.applicationArgument v26)]
  Core.TermFunction v27 -> ((\x -> case x of
    Core.FunctionElimination v28 -> ((\x -> case x of
      Core.EliminationList v29 -> [
        v29]
      Core.EliminationOptional v30 -> [
        Core.optionalCasesNothing v30,
        (Core.optionalCasesJust v30)]
      Core.EliminationUnion v31 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v32 -> [
          v32]) (Core.caseStatementDefault v31)) (Lists.map Core.fieldTerm (Core.caseStatementCases v31)))
      _ -> []) v28)
    Core.FunctionLambda v33 -> [
      Core.lambdaBody v33]
    _ -> []) v27)
  Core.TermLet v34 -> (Lists.cons (Core.letEnvironment v34) (Lists.map Core.letBindingTerm (Core.letBindings v34)))
  Core.TermList v35 -> v35
  Core.TermLiteral _ -> []
  Core.TermMap v37 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v37)))
  Core.TermOptional v38 -> ((\x -> case x of
    Nothing -> []
    Just v39 -> [
      v39]) v38)
  Core.TermProduct v40 -> v40
  Core.TermRecord v41 -> (Lists.map Core.fieldTerm (Core.recordFields v41))
  Core.TermSet v42 -> (Sets.toList v42)
  Core.TermSum v43 -> [
    Core.sumTerm v43]
  Core.TermTypeAbstraction v44 -> [
    Core.typeAbstractionBody v44]
  Core.TermTypeApplication v45 -> [
    Core.typedTermTerm v45]
  Core.TermTyped v46 -> [
    Core.typedTermTerm v46]
  Core.TermUnion v47 -> [
    Core.fieldTerm (Core.injectionField v47)]
  Core.TermVariable _ -> []
  Core.TermWrap v49 -> [
    Core.wrappedTermObject v49]

-- | Find the children of a given term
subtermsWithAccessors :: (Core.Term -> [(Mantle.TermAccessor, Core.Term)])
subtermsWithAccessors x = case x of
  Core.TermAnnotated v50 -> [
    (Mantle.TermAccessorAnnotatedSubject, (Core.annotatedTermSubject v50))]
  Core.TermApplication v51 -> [
    (Mantle.TermAccessorApplicationFunction, (Core.applicationFunction v51)),
    (Mantle.TermAccessorApplicationArgument, (Core.applicationArgument v51))]
  Core.TermFunction v52 -> ((\x -> case x of
    Core.FunctionElimination v53 -> ((\x -> case x of
      Core.EliminationList v54 -> [
        (Mantle.TermAccessorListFold, v54)]
      Core.EliminationOptional v55 -> [
        (Mantle.TermAccessorOptionalCasesNothing, (Core.optionalCasesNothing v55)),
        (Mantle.TermAccessorOptionalCasesJust, (Core.optionalCasesJust v55))]
      Core.EliminationUnion v56 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v57 -> [
          (Mantle.TermAccessorUnionCasesDefault, v57)]) (Core.caseStatementDefault v56)) (Lists.map (\f -> (Mantle.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v56)))
      _ -> []) v53)
    Core.FunctionLambda v58 -> [
      (Mantle.TermAccessorLambdaBody, (Core.lambdaBody v58))]
    _ -> []) v52)
  Core.TermLet v59 -> (Lists.cons (Mantle.TermAccessorLetEnvironment, (Core.letEnvironment v59)) (Lists.map (\b -> (Mantle.TermAccessorLetBinding (Core.letBindingName b), (Core.letBindingTerm b))) (Core.letBindings v59)))
  Core.TermList v60 -> (Lists.map (\e -> (Mantle.TermAccessorListElement 0, e)) v60)
  Core.TermLiteral _ -> []
  Core.TermMap v62 -> (Lists.concat (Lists.map (\p -> [
    (Mantle.TermAccessorMapKey 0, (fst p)),
    (Mantle.TermAccessorMapValue 0, (snd p))]) (Maps.toList v62)))
  Core.TermOptional v63 -> ((\x -> case x of
    Nothing -> []
    Just v64 -> [
      (Mantle.TermAccessorOptionalTerm, v64)]) v63)
  Core.TermProduct v65 -> (Lists.map (\e -> (Mantle.TermAccessorProductTerm 0, e)) v65)
  Core.TermRecord v66 -> (Lists.map (\f -> (Mantle.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v66))
  Core.TermSet v67 -> (Lists.map (\e -> (Mantle.TermAccessorListElement 0, e)) (Sets.toList v67))
  Core.TermSum v68 -> [
    (Mantle.TermAccessorSumTerm, (Core.sumTerm v68))]
  Core.TermTypeAbstraction v69 -> [
    (Mantle.TermAccessorTypeAbstractionBody, (Core.typeAbstractionBody v69))]
  Core.TermTypeApplication v70 -> [
    (Mantle.TermAccessorTypeApplicationTerm, (Core.typedTermTerm v70))]
  Core.TermTyped v71 -> [
    (Mantle.TermAccessorTypedTerm, (Core.typedTermTerm v71))]
  Core.TermUnion v72 -> [
    (Mantle.TermAccessorInjectionTerm, (Core.fieldTerm (Core.injectionField v72)))]
  Core.TermVariable _ -> []
  Core.TermWrap v74 -> [
    (Mantle.TermAccessorWrappedTerm, (Core.wrappedTermObject v74))]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v75 -> [
    Core.annotatedTypeSubject v75]
  Core.TypeApplication v76 -> [
    Core.applicationTypeFunction v76,
    (Core.applicationTypeArgument v76)]
  Core.TypeFunction v77 -> [
    Core.functionTypeDomain v77,
    (Core.functionTypeCodomain v77)]
  Core.TypeLambda v78 -> [
    Core.lambdaTypeBody v78]
  Core.TypeList v79 -> [
    v79]
  Core.TypeLiteral _ -> []
  Core.TypeMap v81 -> [
    Core.mapTypeKeys v81,
    (Core.mapTypeValues v81)]
  Core.TypeOptional v82 -> [
    v82]
  Core.TypeProduct v83 -> v83
  Core.TypeRecord v84 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v84))
  Core.TypeSet v85 -> [
    v85]
  Core.TypeSum v86 -> v86
  Core.TypeUnion v87 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v87))
  Core.TypeVariable _ -> []
  Core.TypeWrap v89 -> [
    Core.wrappedTypeObject v89]

emptyTrace :: Compute.Trace
emptyTrace = Compute.Trace {
  Compute.traceStack = [],
  Compute.traceMessages = [],
  Compute.traceOther = Maps.empty}

-- | Check whether a flow succeeds
flowSucceeds :: (s -> Compute.Flow s a -> Bool)
flowSucceeds cx f = (Optionals.isJust (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

-- | Get the value of a flow, or a default value if the flow fails
fromFlow :: (a -> s -> Compute.Flow s a -> a)
fromFlow def cx f = ((\x -> case x of
  Nothing -> def
  Just v90 -> v90) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

mutateTrace :: ((Compute.Trace -> Mantle.Either_ String Compute.Trace) -> (Compute.Trace -> Compute.Trace -> Compute.Trace) -> Compute.Flow s a -> Compute.Flow s a)
mutateTrace mutate restore f = (Compute.Flow (\s0 -> \t0 ->  
  let forLeft = (\msg -> Compute.FlowState {
          Compute.flowStateValue = Nothing,
          Compute.flowStateState = s0,
          Compute.flowStateTrace = (pushError msg t0)}) 
      forRight = (\t1 ->  
              let f2 = (Compute.unFlow f s0 t1)
              in Compute.FlowState {
                Compute.flowStateValue = (Compute.flowStateValue f2),
                Compute.flowStateState = (Compute.flowStateState f2),
                Compute.flowStateTrace = (restore t0 (Compute.flowStateTrace f2))})
  in ((\x -> case x of
    Mantle.EitherLeft v91 -> (forLeft v91)
    Mantle.EitherRight v92 -> (forRight v92)) (mutate t0))))

-- | Push an error message
pushError :: (String -> Compute.Trace -> Compute.Trace)
pushError msg t =  
  let errorMsg = (Strings.cat [
          "Error: ",
          msg,
          " (",
          Strings.intercalate " > " (Lists.reverse (Compute.traceStack t)),
          ")"])
  in Compute.Trace {
    Compute.traceStack = (Compute.traceStack t),
    Compute.traceMessages = (Lists.cons errorMsg (Compute.traceMessages t)),
    Compute.traceOther = (Compute.traceOther t)}

-- | Continue the current flow after adding a warning message
warn :: (String -> Compute.Flow s a -> Compute.Flow s a)
warn msg b = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow b s0 t0) 
      addMessage = (\t -> Compute.Trace {
              Compute.traceStack = (Compute.traceStack t),
              Compute.traceMessages = (Lists.cons (Strings.cat [
                "Warning: ",
                msg]) (Compute.traceMessages t)),
              Compute.traceOther = (Compute.traceOther t)})
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = (Compute.flowStateState f1),
    Compute.flowStateTrace = (addMessage (Compute.flowStateTrace f1))}))

-- | Continue the current flow after setting a flag
withFlag :: (Core.Name -> Compute.Flow s a -> Compute.Flow s a)
withFlag flag =  
  let mutate = (\t -> Mantle.EitherRight (Compute.Trace {
          Compute.traceStack = (Compute.traceStack t),
          Compute.traceMessages = (Compute.traceMessages t),
          Compute.traceOther = (Maps.insert flag (Core.TermLiteral (Core.LiteralBoolean True)) (Compute.traceOther t))})) 
      restore = (\ignored -> \t1 -> Compute.Trace {
              Compute.traceStack = (Compute.traceStack t1),
              Compute.traceMessages = (Compute.traceMessages t1),
              Compute.traceOther = (Maps.remove flag (Compute.traceOther t1))})
  in (mutateTrace mutate restore)

-- | Continue a flow using a given state
withState :: (s1 -> Compute.Flow s1 a -> Compute.Flow s2 a)
withState cx0 f = (Compute.Flow (\cx1 -> \t1 ->  
  let f1 = (Compute.unFlow f cx0 t1)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx1,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

-- | Continue the current flow after augmenting the trace
withTrace :: (String -> Compute.Flow s a -> Compute.Flow s a)
withTrace msg =  
  let mutate = (\t -> Logic.ifElse (Mantle.EitherLeft "maximum trace depth exceeded. This may indicate an infinite loop") (Mantle.EitherRight (Compute.Trace {
          Compute.traceStack = (Lists.cons msg (Compute.traceStack t)),
          Compute.traceMessages = (Compute.traceMessages t),
          Compute.traceOther = (Compute.traceOther t)})) (Equality.gteInt32 (Lists.length (Compute.traceStack t)) Constants.maxTraceDepth)) 
      restore = (\t0 -> \t1 -> Compute.Trace {
              Compute.traceStack = (Compute.traceStack t0),
              Compute.traceMessages = (Compute.traceMessages t1),
              Compute.traceOther = (Compute.traceOther t1)})
  in (mutateTrace mutate restore)