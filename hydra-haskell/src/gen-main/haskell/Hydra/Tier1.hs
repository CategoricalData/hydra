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
  Core.FloatValueBigfloat v76 -> (Equality.identity v76)
  Core.FloatValueFloat32 v77 -> (Literals.float32ToBigfloat v77)
  Core.FloatValueFloat64 v78 -> (Literals.float64ToBigfloat v78)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v79 -> (Equality.identity v79)
  Core.IntegerValueInt8 v80 -> (Literals.int8ToBigint v80)
  Core.IntegerValueInt16 v81 -> (Literals.int16ToBigint v81)
  Core.IntegerValueInt32 v82 -> (Literals.int32ToBigint v82)
  Core.IntegerValueInt64 v83 -> (Literals.int64ToBigint v83)
  Core.IntegerValueUint8 v84 -> (Literals.uint8ToBigint v84)
  Core.IntegerValueUint16 v85 -> (Literals.uint16ToBigint v85)
  Core.IntegerValueUint32 v86 -> (Literals.uint32ToBigint v86)
  Core.IntegerValueUint64 v87 -> (Literals.uint64ToBigint v87)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v88 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v88)
  Core.TermLet v90 -> (isLambda (Core.letEnvironment v90))
  _ -> False) (Strip.fullyStripTerm term))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v91 -> (Strings.cat [
            Module.unNamespace v91,
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
    Core.TermFunction v96 -> ((\x -> case x of
      Core.FunctionLambda v97 -> (Sets.remove (Core.lambdaParameter v97) (freeVariablesInTerm (Core.lambdaBody v97)))
      _ -> dfltVars) v96)
    Core.TermVariable v98 -> (Sets.singleton v98)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v99 -> (Sets.remove (Core.lambdaTypeParameter v99) (freeVariablesInType (Core.lambdaTypeBody v99)))
    Core.TypeVariable v100 -> (Sets.singleton v100)
    _ -> dfltVars) typ)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v101 -> [
    Core.annotatedTermSubject v101]
  Core.TermApplication v102 -> [
    Core.applicationFunction v102,
    (Core.applicationArgument v102)]
  Core.TermFunction v103 -> ((\x -> case x of
    Core.FunctionElimination v104 -> ((\x -> case x of
      Core.EliminationList v105 -> [
        v105]
      Core.EliminationOptional v106 -> [
        Core.optionalCasesNothing v106,
        (Core.optionalCasesJust v106)]
      Core.EliminationUnion v107 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v108 -> [
          v108]) (Core.caseStatementDefault v107)) (Lists.map Core.fieldTerm (Core.caseStatementCases v107)))
      _ -> []) v104)
    Core.FunctionLambda v109 -> [
      Core.lambdaBody v109]
    _ -> []) v103)
  Core.TermLet v110 -> (Lists.cons (Core.letEnvironment v110) (Lists.map snd (Maps.toList (Core.letBindings v110))))
  Core.TermList v111 -> v111
  Core.TermLiteral _ -> []
  Core.TermMap v113 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v113)))
  Core.TermOptional v114 -> ((\x -> case x of
    Nothing -> []
    Just v115 -> [
      v115]) v114)
  Core.TermProduct v116 -> v116
  Core.TermRecord v117 -> (Lists.map Core.fieldTerm (Core.recordFields v117))
  Core.TermSet v118 -> (Sets.toList v118)
  Core.TermStream _ -> []
  Core.TermSum v120 -> [
    Core.sumTerm v120]
  Core.TermTyped v121 -> [
    Core.typedTermTerm v121]
  Core.TermUnion v122 -> [
    Core.fieldTerm (Core.injectionField v122)]
  Core.TermVariable _ -> []
  Core.TermWrap v124 -> [
    Core.wrappedTermObject v124]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v125 -> [
    Core.annotatedTypeSubject v125]
  Core.TypeApplication v126 -> [
    Core.applicationTypeFunction v126,
    (Core.applicationTypeArgument v126)]
  Core.TypeFunction v127 -> [
    Core.functionTypeDomain v127,
    (Core.functionTypeCodomain v127)]
  Core.TypeLambda v128 -> [
    Core.lambdaTypeBody v128]
  Core.TypeList v129 -> [
    v129]
  Core.TypeLiteral _ -> []
  Core.TypeMap v131 -> [
    Core.mapTypeKeys v131,
    (Core.mapTypeValues v131)]
  Core.TypeOptional v132 -> [
    v132]
  Core.TypeProduct v133 -> v133
  Core.TypeRecord v134 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v134))
  Core.TypeSet v135 -> [
    v135]
  Core.TypeSum v136 -> v136
  Core.TypeUnion v137 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v137))
  Core.TypeVariable _ -> []
  Core.TypeWrap v139 -> [
    Core.wrappedTypeObject v139]

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
  Just v140 -> v140) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

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
    Mantle.EitherLeft v141 -> (forLeft v141)
    Mantle.EitherRight v142 -> (forRight v142)) (mutate t0))))

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
  let addMessage = (\t -> Compute.Trace {
          Compute.traceStack = (Compute.traceStack t),
          Compute.traceMessages = (Lists.cons (Strings.cat [
            "Warning: ",
            msg]) (Compute.traceMessages t)),
          Compute.traceOther = (Compute.traceOther t)}) 
      f1 = (Compute.unFlow b s0 t0)
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