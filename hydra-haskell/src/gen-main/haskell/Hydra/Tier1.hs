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
  Core.FloatValueBigfloat v116 -> (Equality.identity v116)
  Core.FloatValueFloat32 v117 -> (Literals.float32ToBigfloat v117)
  Core.FloatValueFloat64 v118 -> (Literals.float64ToBigfloat v118)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v119 -> (Equality.identity v119)
  Core.IntegerValueInt8 v120 -> (Literals.int8ToBigint v120)
  Core.IntegerValueInt16 v121 -> (Literals.int16ToBigint v121)
  Core.IntegerValueInt32 v122 -> (Literals.int32ToBigint v122)
  Core.IntegerValueInt64 v123 -> (Literals.int64ToBigint v123)
  Core.IntegerValueUint8 v124 -> (Literals.uint8ToBigint v124)
  Core.IntegerValueUint16 v125 -> (Literals.uint16ToBigint v125)
  Core.IntegerValueUint32 v126 -> (Literals.uint32ToBigint v126)
  Core.IntegerValueUint64 v127 -> (Literals.uint64ToBigint v127)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v128 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v128)
  Core.TermLet v130 -> (isLambda (Core.letEnvironment v130))
  _ -> False) (Strip.fullyStripTerm term))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v131 -> (Strings.cat [
            Module.unNamespace v131,
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
    Core.TermFunction v136 -> ((\x -> case x of
      Core.FunctionLambda v137 -> (Sets.remove (Core.lambdaParameter v137) (freeVariablesInTerm (Core.lambdaBody v137)))
      _ -> dfltVars) v136)
    Core.TermVariable v138 -> (Sets.singleton v138)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v139 -> (Sets.remove (Core.lambdaTypeParameter v139) (freeVariablesInType (Core.lambdaTypeBody v139)))
    Core.TypeVariable v140 -> (Sets.singleton v140)
    _ -> dfltVars) typ)

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v141 -> [
    Core.annotatedTermSubject v141]
  Core.TermApplication v142 -> [
    Core.applicationFunction v142,
    (Core.applicationArgument v142)]
  Core.TermFunction v143 -> ((\x -> case x of
    Core.FunctionElimination v144 -> ((\x -> case x of
      Core.EliminationList v145 -> [
        v145]
      Core.EliminationOptional v146 -> [
        Core.optionalCasesNothing v146,
        (Core.optionalCasesJust v146)]
      Core.EliminationUnion v147 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v148 -> [
          v148]) (Core.caseStatementDefault v147)) (Lists.map Core.fieldTerm (Core.caseStatementCases v147)))
      _ -> []) v144)
    Core.FunctionLambda v149 -> [
      Core.lambdaBody v149]
    _ -> []) v143)
  Core.TermLet v150 -> (Lists.cons (Core.letEnvironment v150) (Lists.map Core.letBindingTerm (Core.letBindings v150)))
  Core.TermList v151 -> v151
  Core.TermLiteral _ -> []
  Core.TermMap v153 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v153)))
  Core.TermOptional v154 -> ((\x -> case x of
    Nothing -> []
    Just v155 -> [
      v155]) v154)
  Core.TermProduct v156 -> v156
  Core.TermRecord v157 -> (Lists.map Core.fieldTerm (Core.recordFields v157))
  Core.TermSet v158 -> (Sets.toList v158)
  Core.TermSum v159 -> [
    Core.sumTerm v159]
  Core.TermTypeAbstraction v160 -> [
    Core.typeAbstractionBody v160]
  Core.TermTypeApplication v161 -> [
    Core.typedTermTerm v161]
  Core.TermTyped v162 -> [
    Core.typedTermTerm v162]
  Core.TermUnion v163 -> [
    Core.fieldTerm (Core.injectionField v163)]
  Core.TermVariable _ -> []
  Core.TermWrap v165 -> [
    Core.wrappedTermObject v165]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v166 -> [
    Core.annotatedTypeSubject v166]
  Core.TypeApplication v167 -> [
    Core.applicationTypeFunction v167,
    (Core.applicationTypeArgument v167)]
  Core.TypeFunction v168 -> [
    Core.functionTypeDomain v168,
    (Core.functionTypeCodomain v168)]
  Core.TypeLambda v169 -> [
    Core.lambdaTypeBody v169]
  Core.TypeList v170 -> [
    v170]
  Core.TypeLiteral _ -> []
  Core.TypeMap v172 -> [
    Core.mapTypeKeys v172,
    (Core.mapTypeValues v172)]
  Core.TypeOptional v173 -> [
    v173]
  Core.TypeProduct v174 -> v174
  Core.TypeRecord v175 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v175))
  Core.TypeSet v176 -> [
    v176]
  Core.TypeSum v177 -> v177
  Core.TypeUnion v178 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v178))
  Core.TypeVariable _ -> []
  Core.TypeWrap v180 -> [
    Core.wrappedTypeObject v180]

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
  Just v181 -> v181) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

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
    Mantle.EitherLeft v182 -> (forLeft v182)
    Mantle.EitherRight v183 -> (forRight v183)) (mutate t0))))

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