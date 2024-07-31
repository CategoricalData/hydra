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
  Core.FloatValueBigfloat v78 -> (Equality.identity v78)
  Core.FloatValueFloat32 v79 -> (Literals.float32ToBigfloat v79)
  Core.FloatValueFloat64 v80 -> (Literals.float64ToBigfloat v80)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v81 -> (Equality.identity v81)
  Core.IntegerValueInt8 v82 -> (Literals.int8ToBigint v82)
  Core.IntegerValueInt16 v83 -> (Literals.int16ToBigint v83)
  Core.IntegerValueInt32 v84 -> (Literals.int32ToBigint v84)
  Core.IntegerValueInt64 v85 -> (Literals.int64ToBigint v85)
  Core.IntegerValueUint8 v86 -> (Literals.uint8ToBigint v86)
  Core.IntegerValueUint16 v87 -> (Literals.uint16ToBigint v87)
  Core.IntegerValueUint32 v88 -> (Literals.uint32ToBigint v88)
  Core.IntegerValueUint64 v89 -> (Literals.uint64ToBigint v89)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v90 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v90)
  Core.TermLet v92 -> (isLambda (Core.letEnvironment v92))
  _ -> False) (Strip.fullyStripTerm term))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v93 -> (Strings.cat [
            Module.unNamespace v93,
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
    Core.TermFunction v98 -> ((\x -> case x of
      Core.FunctionLambda v99 -> (Sets.remove (Core.lambdaParameter v99) (freeVariablesInTerm (Core.lambdaBody v99)))
      _ -> dfltVars) v98)
    Core.TermVariable v100 -> (Sets.singleton v100)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v101 -> (Sets.remove (Core.lambdaTypeParameter v101) (freeVariablesInType (Core.lambdaTypeBody v101)))
    Core.TypeVariable v102 -> (Sets.singleton v102)
    _ -> dfltVars) typ)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v103 -> [
    Core.annotatedTermSubject v103]
  Core.TermApplication v104 -> [
    Core.applicationFunction v104,
    (Core.applicationArgument v104)]
  Core.TermFunction v105 -> ((\x -> case x of
    Core.FunctionElimination v106 -> ((\x -> case x of
      Core.EliminationList v107 -> [
        v107]
      Core.EliminationOptional v108 -> [
        Core.optionalCasesNothing v108,
        (Core.optionalCasesJust v108)]
      Core.EliminationUnion v109 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v110 -> [
          v110]) (Core.caseStatementDefault v109)) (Lists.map Core.fieldTerm (Core.caseStatementCases v109)))
      _ -> []) v106)
    Core.FunctionLambda v111 -> [
      Core.lambdaBody v111]
    _ -> []) v105)
  Core.TermLet v112 -> (Lists.cons (Core.letEnvironment v112) (Lists.map Core.letBindingTerm (Core.letBindings v112)))
  Core.TermList v113 -> v113
  Core.TermLiteral _ -> []
  Core.TermMap v115 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v115)))
  Core.TermOptional v116 -> ((\x -> case x of
    Nothing -> []
    Just v117 -> [
      v117]) v116)
  Core.TermProduct v118 -> v118
  Core.TermRecord v119 -> (Lists.map Core.fieldTerm (Core.recordFields v119))
  Core.TermSet v120 -> (Sets.toList v120)
  Core.TermStream _ -> []
  Core.TermSum v122 -> [
    Core.sumTerm v122]
  Core.TermTyped v123 -> [
    Core.typedTermTerm v123]
  Core.TermUnion v124 -> [
    Core.fieldTerm (Core.injectionField v124)]
  Core.TermVariable _ -> []
  Core.TermWrap v126 -> [
    Core.wrappedTermObject v126]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v127 -> [
    Core.annotatedTypeSubject v127]
  Core.TypeApplication v128 -> [
    Core.applicationTypeFunction v128,
    (Core.applicationTypeArgument v128)]
  Core.TypeFunction v129 -> [
    Core.functionTypeDomain v129,
    (Core.functionTypeCodomain v129)]
  Core.TypeLambda v130 -> [
    Core.lambdaTypeBody v130]
  Core.TypeList v131 -> [
    v131]
  Core.TypeLiteral _ -> []
  Core.TypeMap v133 -> [
    Core.mapTypeKeys v133,
    (Core.mapTypeValues v133)]
  Core.TypeOptional v134 -> [
    v134]
  Core.TypeProduct v135 -> v135
  Core.TypeRecord v136 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v136))
  Core.TypeSet v137 -> [
    v137]
  Core.TypeSum v138 -> v138
  Core.TypeUnion v139 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v139))
  Core.TypeVariable _ -> []
  Core.TypeWrap v141 -> [
    Core.wrappedTypeObject v141]

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
  Just v142 -> v142) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

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
    Mantle.EitherLeft v143 -> (forLeft v143)
    Mantle.EitherRight v144 -> (forRight v144)) (mutate t0))))

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
withFlag :: (String -> Compute.Flow s a -> Compute.Flow s a)
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