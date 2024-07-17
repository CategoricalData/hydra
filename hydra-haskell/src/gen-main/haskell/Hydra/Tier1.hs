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
  Core.FloatValueBigfloat v75 -> (Equality.identity v75)
  Core.FloatValueFloat32 v76 -> (Literals.float32ToBigfloat v76)
  Core.FloatValueFloat64 v77 -> (Literals.float64ToBigfloat v77)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v78 -> (Equality.identity v78)
  Core.IntegerValueInt8 v79 -> (Literals.int8ToBigint v79)
  Core.IntegerValueInt16 v80 -> (Literals.int16ToBigint v80)
  Core.IntegerValueInt32 v81 -> (Literals.int32ToBigint v81)
  Core.IntegerValueInt64 v82 -> (Literals.int64ToBigint v82)
  Core.IntegerValueUint8 v83 -> (Literals.uint8ToBigint v83)
  Core.IntegerValueUint16 v84 -> (Literals.uint16ToBigint v84)
  Core.IntegerValueUint32 v85 -> (Literals.uint32ToBigint v85)
  Core.IntegerValueUint64 v86 -> (Literals.uint64ToBigint v86)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term Core.Kv -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v87 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v87)
  Core.TermLet v89 -> (isLambda (Core.letEnvironment v89))
  _ -> False) (Strip.stripTerm term))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v90 -> (Strings.cat [
            Module.unNamespace v90,
            "."])) (Module.qualifiedNameNamespace qname))
  in (Core.Name (Strings.cat [
    prefix,
    (Module.qualifiedNameLocal qname)]))

-- | Fold over a term, traversing its subterms in the specified order
foldOverTerm :: (Coders.TraversalOrder -> (x -> Core.Term Core.Kv -> x) -> x -> Core.Term Core.Kv -> x)
foldOverTerm order fld b0 term = ((\x -> case x of
  Coders.TraversalOrderPre -> (L.foldl (foldOverTerm order fld) (fld b0 term) (subterms term))
  Coders.TraversalOrderPost -> (fld (L.foldl (foldOverTerm order fld) b0 (subterms term)) term)) order)

-- | Fold over a type, traversing its subtypes in the specified order
foldOverType :: (Coders.TraversalOrder -> (x -> Core.Type Core.Kv -> x) -> x -> Core.Type Core.Kv -> x)
foldOverType order fld b0 typ = ((\x -> case x of
  Coders.TraversalOrderPre -> (L.foldl (foldOverType order fld) (fld b0 typ) (subtypes typ))
  Coders.TraversalOrderPost -> (fld (L.foldl (foldOverType order fld) b0 (subtypes typ)) typ)) order)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: (Core.Term Core.Kv -> Set Core.Name)
freeVariablesInTerm term =
  let dfltVars = (L.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (subterms term))
  in ((\x -> case x of
    Core.TermFunction v95 -> ((\x -> case x of
      Core.FunctionLambda v96 -> (Sets.remove (Core.lambdaParameter v96) (freeVariablesInTerm (Core.lambdaBody v96)))
      _ -> dfltVars) v95)
    Core.TermVariable v97 -> (Sets.singleton v97)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type Core.Kv -> Set Core.Name)
freeVariablesInType typ =
  let dfltVars = (L.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v98 -> (Sets.remove (Core.lambdaTypeParameter v98) (freeVariablesInType (Core.lambdaTypeBody v98)))
    Core.TypeVariable v99 -> (Sets.singleton v99)
    _ -> dfltVars) typ)

-- | Find the children of a given term
subterms :: (Core.Term Core.Kv -> [Core.Term Core.Kv])
subterms x = case x of
  Core.TermAnnotated v100 -> [
    Core.annotatedSubject v100]
  Core.TermApplication v101 -> [
    Core.applicationFunction v101,
    (Core.applicationArgument v101)]
  Core.TermFunction v102 -> ((\x -> case x of
    Core.FunctionElimination v103 -> ((\x -> case x of
      Core.EliminationList v104 -> [
        v104]
      Core.EliminationOptional v105 -> [
        Core.optionalCasesNothing v105,
        (Core.optionalCasesJust v105)]
      Core.EliminationUnion v106 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v107 -> [
          v107]) (Core.caseStatementDefault v106)) (Lists.map Core.fieldTerm (Core.caseStatementCases v106)))
      _ -> []) v103)
    Core.FunctionLambda v108 -> [
      Core.lambdaBody v108]
    _ -> []) v102)
  Core.TermLet v109 -> (Lists.cons (Core.letEnvironment v109) (Lists.map snd (Maps.toList (Core.letBindings v109))))
  Core.TermList v110 -> v110
  Core.TermLiteral _ -> []
  Core.TermMap v112 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v112)))
  Core.TermOptional v113 -> ((\x -> case x of
    Nothing -> []
    Just v114 -> [
      v114]) v113)
  Core.TermProduct v115 -> v115
  Core.TermRecord v116 -> (Lists.map Core.fieldTerm (Core.recordFields v116))
  Core.TermSet v117 -> (Sets.toList v117)
  Core.TermStream _ -> []
  Core.TermSum v119 -> [
    Core.sumTerm v119]
  Core.TermTyped t -> [Core.termWithTypeTerm t]
  Core.TermUnion v120 -> [
    Core.fieldTerm (Core.injectionField v120)]
  Core.TermVariable _ -> []
  Core.TermWrap v122 -> [
    Core.nominalObject v122]

-- | Find the children of a given type expression
subtypes :: (Core.Type Core.Kv -> [Core.Type Core.Kv])
subtypes x = case x of
  Core.TypeAnnotated v123 -> [
    Core.annotatedSubject v123]
  Core.TypeApplication v124 -> [
    Core.applicationTypeFunction v124,
    (Core.applicationTypeArgument v124)]
  Core.TypeFunction v125 -> [
    Core.functionTypeDomain v125,
    (Core.functionTypeCodomain v125)]
  Core.TypeLambda v126 -> [
    Core.lambdaTypeBody v126]
  Core.TypeList v127 -> [
    v127]
  Core.TypeLiteral _ -> []
  Core.TypeMap v129 -> [
    Core.mapTypeKeys v129,
    (Core.mapTypeValues v129)]
  Core.TypeOptional v130 -> [
    v130]
  Core.TypeProduct v131 -> v131
  Core.TypeRecord v132 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v132))
  Core.TypeSet v133 -> [
    v133]
  Core.TypeSum v134 -> v134
  Core.TypeUnion v135 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v135))
  Core.TypeVariable _ -> []
  Core.TypeWrap v137 -> [
    Core.nominalObject v137]

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
  Just v138 -> v138) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

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
    Mantle.EitherLeft v139 -> (forLeft v139)
    Mantle.EitherRight v140 -> (forRight v140)) (mutate t0))))

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
