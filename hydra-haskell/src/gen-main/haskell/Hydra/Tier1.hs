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
  Core.FloatValueBigfloat v113 -> (Equality.identity v113)
  Core.FloatValueFloat32 v114 -> (Literals.float32ToBigfloat v114)
  Core.FloatValueFloat64 v115 -> (Literals.float64ToBigfloat v115)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v116 -> (Equality.identity v116)
  Core.IntegerValueInt8 v117 -> (Literals.int8ToBigint v117)
  Core.IntegerValueInt16 v118 -> (Literals.int16ToBigint v118)
  Core.IntegerValueInt32 v119 -> (Literals.int32ToBigint v119)
  Core.IntegerValueInt64 v120 -> (Literals.int64ToBigint v120)
  Core.IntegerValueUint8 v121 -> (Literals.uint8ToBigint v121)
  Core.IntegerValueUint16 v122 -> (Literals.uint16ToBigint v122)
  Core.IntegerValueUint32 v123 -> (Literals.uint32ToBigint v123)
  Core.IntegerValueUint64 v124 -> (Literals.uint64ToBigint v124)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v125 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v125)
  Core.TermLet v127 -> (isLambda (Core.letEnvironment v127))
  _ -> False) (Strip.fullyStripTerm term))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v128 -> (Strings.cat [
            Module.unNamespace v128,
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
    Core.TermFunction v133 -> ((\x -> case x of
      Core.FunctionLambda v134 -> (Sets.remove (Core.lambdaParameter v134) (freeVariablesInTerm (Core.lambdaBody v134)))
      _ -> dfltVars) v133)
    Core.TermVariable v135 -> (Sets.singleton v135)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v136 -> (Sets.remove (Core.lambdaTypeParameter v136) (freeVariablesInType (Core.lambdaTypeBody v136)))
    Core.TypeVariable v137 -> (Sets.singleton v137)
    _ -> dfltVars) typ)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v138 -> [
    Core.annotatedTermSubject v138]
  Core.TermApplication v139 -> [
    Core.applicationFunction v139,
    (Core.applicationArgument v139)]
  Core.TermFunction v140 -> ((\x -> case x of
    Core.FunctionElimination v141 -> ((\x -> case x of
      Core.EliminationList v142 -> [
        v142]
      Core.EliminationOptional v143 -> [
        Core.optionalCasesNothing v143,
        (Core.optionalCasesJust v143)]
      Core.EliminationUnion v144 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v145 -> [
          v145]) (Core.caseStatementDefault v144)) (Lists.map Core.fieldTerm (Core.caseStatementCases v144)))
      _ -> []) v141)
    Core.FunctionLambda v146 -> [
      Core.lambdaBody v146]
    _ -> []) v140)
  Core.TermLet v147 -> (Lists.cons (Core.letEnvironment v147) (Lists.map Core.letBindingTerm (Core.letBindings v147)))
  Core.TermList v148 -> v148
  Core.TermLiteral _ -> []
  Core.TermMap v150 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v150)))
  Core.TermOptional v151 -> ((\x -> case x of
    Nothing -> []
    Just v152 -> [
      v152]) v151)
  Core.TermProduct v153 -> v153
  Core.TermRecord v154 -> (Lists.map Core.fieldTerm (Core.recordFields v154))
  Core.TermSet v155 -> (Sets.toList v155)
  Core.TermSum v156 -> [
    Core.sumTerm v156]
  Core.TermTyped v157 -> [
    Core.typedTermTerm v157]
  Core.TermUnion v158 -> [
    Core.fieldTerm (Core.injectionField v158)]
  Core.TermVariable _ -> []
  Core.TermWrap v160 -> [
    Core.wrappedTermObject v160]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v161 -> [
    Core.annotatedTypeSubject v161]
  Core.TypeApplication v162 -> [
    Core.applicationTypeFunction v162,
    (Core.applicationTypeArgument v162)]
  Core.TypeFunction v163 -> [
    Core.functionTypeDomain v163,
    (Core.functionTypeCodomain v163)]
  Core.TypeLambda v164 -> [
    Core.lambdaTypeBody v164]
  Core.TypeList v165 -> [
    v165]
  Core.TypeLiteral _ -> []
  Core.TypeMap v167 -> [
    Core.mapTypeKeys v167,
    (Core.mapTypeValues v167)]
  Core.TypeOptional v168 -> [
    v168]
  Core.TypeProduct v169 -> v169
  Core.TypeRecord v170 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v170))
  Core.TypeSet v171 -> [
    v171]
  Core.TypeSum v172 -> v172
  Core.TypeUnion v173 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v173))
  Core.TypeVariable _ -> []
  Core.TypeWrap v175 -> [
    Core.wrappedTypeObject v175]

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
  Just v176 -> v176) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

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
    Mantle.EitherLeft v177 -> (forLeft v177)
    Mantle.EitherRight v178 -> (forRight v178)) (mutate t0))))

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