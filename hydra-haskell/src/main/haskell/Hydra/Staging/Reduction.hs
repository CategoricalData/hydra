-- | Functions for reducing terms and types, i.e. performing computations

module Hydra.Staging.Reduction where

import Hydra.Arity
import Hydra.Strip
import Hydra.Compute
import Hydra.Core
import Hydra.Staging.Schemas
import Hydra.Graph
import Hydra.Staging.Annotations
import Hydra.Staging.Lexical
import Hydra.Staging.Rewriting
import Hydra.Rewriting
import Hydra.Lexical
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


alphaConvert :: Name -> Term -> Term -> Term
alphaConvert vold tnew = rewriteTerm rewrite
  where
    rewrite recurse term = case term of
      TermFunction (FunctionLambda (Lambda v _ _)) -> if v == vold
        then term
        else recurse term
      TermVariable v -> if v == vold then tnew else TermVariable v
      _ -> recurse term

-- For demo purposes. This should be generalized to enable additional side effects of interest.
countPrimitiveInvocations :: Bool
countPrimitiveInvocations = True

-- A term evaluation function which is alternatively lazy or eager
reduceTerm :: Bool -> M.Map Name Term -> Term -> Flow Graph Term
reduceTerm eager env = rewriteTermM mapping
  where
    reduce eager = reduceTerm eager M.empty

    mapping recurse mid = do
      inner <- if doRecurse eager mid then recurse mid else pure mid
      applyIfNullary eager inner []

    doRecurse eager term = eager && case term of
      TermFunction (FunctionLambda _) -> False
      _ -> True

    -- Reduce an argument only if evaluation is lazy (i.e. the argument may not already have been reduced)
    reduceArg eager arg = if eager then pure arg else reduce False arg

    applyToArguments fun args = case args of
      [] -> fun
      (h:r) -> applyToArguments (Terms.apply fun h) r

    replaceFreeName toReplace replacement = rewriteTerm mapping
      where
        mapping recurse inner = case inner of
          TermFunction (FunctionLambda (Lambda param _ _)) -> if param == toReplace then inner else recurse inner
          TermVariable name -> if name == toReplace then replacement else inner
          _ -> recurse inner

    applyIfNullary eager original args = case stripTerm original of
      TermApplication (Application fun arg) -> applyIfNullary eager fun (arg:args)
      TermFunction fun -> case fun of
        FunctionElimination elm -> case args of
          [] -> pure original
          -- Reduce the argument prior to application, regardless of laziness
          (arg:remainingArgs) -> do
            reducedArg <- reduceArg eager $ stripTerm arg
            reducedResult <- applyElimination elm reducedArg >>= reduce eager
            applyIfNullary eager reducedResult remainingArgs
        FunctionLambda (Lambda param _ body) -> case args of
          [] -> pure original
          (arg:remainingArgs) -> do
            reducedArg <- reduce eager $ stripTerm arg
            reducedResult <- reduce eager $ replaceFreeName param reducedArg body
            applyIfNullary eager reducedResult remainingArgs
        FunctionPrimitive name -> do
          prim <- requirePrimitive name
          let arity = primitiveArity prim
          if arity > L.length args
            -- Not enough arguments available; back out
            then return $ applyToArguments original args
            else do
              let argList = L.take arity args
              let remainingArgs = L.drop arity args
              reducedArgs <- CM.mapM (reduceArg eager) argList
              reducedResult <- primitiveImplementation prim reducedArgs >>= reduce eager
              applyIfNullary eager reducedResult remainingArgs
      TermVariable _ -> pure $ applyToArguments original args -- TODO: dereference variables
      _ -> pure $ applyToArguments original args

    applyElimination elm reducedArg = case elm of
      EliminationList _ -> fail "list eliminations are unsupported"
      EliminationOptional _ -> fail "optional eliminations are unsupported"
      EliminationRecord proj -> do
        fields <- Expect.recordWithName (projectionTypeName proj) $ stripTerm reducedArg
        let matchingFields = L.filter (\f -> fieldName f == projectionField proj) fields
        if L.null matchingFields
          then fail $ "no such field: " ++ unName (projectionField proj) ++ " in " ++ unName (projectionTypeName proj) ++ " record"
          else pure $ fieldTerm $ L.head matchingFields
      EliminationUnion (CaseStatement name def fields) -> do
        field <- Expect.injectionWithName name reducedArg
        let matchingFields = L.filter (\f -> fieldName f == fieldName field) fields
        if L.null matchingFields
          then case def of
            Just d -> pure d
            Nothing -> fail $ "no such field " ++ unName (fieldName field) ++ " in " ++ unName name ++ " case statement"
          else pure $ Terms.apply (fieldTerm $ L.head matchingFields) (fieldTerm field)
      EliminationWrap name -> Expect.wrap name reducedArg

-- Note: this is eager beta reduction, in that we always descend into subtypes,
--       and always reduce the right-hand side of an application prior to substitution
betaReduceType :: Type -> Flow Graph Type
betaReduceType typ = rewriteTypeM mapExpr typ
  where
    mapExpr recurse t = do
        r <- recurse t
        case r of
          TypeApplication a -> reduceApp a
          t' -> pure t'
      where
        reduceApp (ApplicationType lhs rhs) = case lhs of
          TypeAnnotated (AnnotatedType t' ann) -> do
            a <- reduceApp $ ApplicationType t' rhs
            return $ TypeAnnotated $ AnnotatedType a ann
          TypeLambda (LambdaType v body) -> betaReduceType $ replaceFreeName v rhs body
          -- nominal types are transparent
          TypeVariable name -> do
            t' <- requireType name
            betaReduceType $ TypeApplication $ ApplicationType t' rhs

-- | Apply the special rules:
--     ((\x.e1) e2) == e1, where x does not appear free in e1
--   and
--     ((\x.e1) e2) = e1[x/e2]
--  These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it.
contractTerm :: Term -> Term
contractTerm = rewriteTerm rewrite
  where
    rewrite recurse term = case rec of
        TermApplication (Application lhs rhs) -> case fullyStripTerm lhs of
          TermFunction (FunctionLambda (Lambda v _ body)) -> if isFreeVariableInTerm v body
            then body
            else alphaConvert v rhs body
          _ -> rec
        _ -> rec
      where
        rec = recurse term

-- Note: unused / untested
etaReduceTerm :: Term -> Term
etaReduceTerm term = case term of
    TermAnnotated (AnnotatedTerm term1 ann) -> TermAnnotated (AnnotatedTerm (etaReduceTerm term1) ann)
    TermFunction (FunctionLambda l) -> reduceLambda l
    _ -> noChange
  where
    reduceLambda (Lambda v d body) = case etaReduceTerm body of
      TermAnnotated (AnnotatedTerm body1 ann) -> reduceLambda (Lambda v d body1)
      TermApplication a -> reduceApplication a
        where
          reduceApplication (Application lhs rhs) = case etaReduceTerm rhs of
            TermAnnotated (AnnotatedTerm rhs1 ann) -> reduceApplication (Application lhs rhs1)
            TermVariable v1 -> if v == v1 && isFreeVariableInTerm v lhs
              then etaReduceTerm lhs
              else noChange
            _ -> noChange
      _ -> noChange
    noChange = term

-- | Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x', in which the implicit
--   parameters of primitive functions and eliminations are made into explicit lambda parameters.
--   Variable references are not expanded.
--   This is useful for targets like Python with weaker support for currying than Hydra or Haskell.
--   Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references.
expandLambdas :: Graph -> Term -> Term
--expandLambdas g = contractTerm . unshadowVariables . expand
expandLambdas graph term = contractTerm $ rewriteTerm (rewrite []) term
  where
    rewrite args recurse term = case term of
        TermApplication (Application lhs rhs) -> rewrite (erhs:args) recurse lhs
          where
            erhs = rewrite [] recurse rhs
        _ -> afterRecursion $ recurse term
      where
        afterRecursion term = expand args (expansionArity graph term) term
    expand args arity term = pad is apps
      where
        apps = L.foldl (\lhs arg -> TermApplication $ Application lhs arg) term args
        is = if arity <= L.length args then [] else [1..(arity - L.length args)]
    pad is term = if L.null is
        then term
        else TermFunction $ FunctionLambda $
          Lambda var Nothing $ pad (tail is) (TermApplication $ Application term $ TermVariable var)
      where
        var = (Name $ "v" ++ show (head is))

expansionArity :: Graph -> Term -> Int
expansionArity graph term = case fullyStripTerm term of
  TermApplication (Application lhs rhs) -> expansionArity graph lhs - 1
  TermFunction f -> case f of
    FunctionElimination _ -> 1
    FunctionLambda _ -> 0 -- lambdas increase logical arity, but they do not need to be expanded
    FunctionPrimitive name -> primitiveArity $ Y.fromJust $ lookupPrimitive graph name
  TermVariable name -> case typeSchemeType <$> (lookupElement graph name >>= elementType) of
    Nothing -> 0
    Just typ -> typeArity typ
  _ -> 0

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term -> Bool
termIsClosed = S.null . freeVariablesInTerm

-- | Whether a term has been fully reduced to a "value"
termIsValue :: Graph -> Term -> Bool
termIsValue g term = case stripTerm term of
    TermApplication _ -> False
    TermLiteral _ -> True
    TermFunction f -> functionIsValue f
    TermList els -> forList els
    TermMap map -> L.foldl
      (\b (k, v) -> b && termIsValue g k && termIsValue g v)
      True $ M.toList map
    TermOptional m -> case m of
      Nothing -> True
      Just term -> termIsValue g term
    TermRecord (Record _ fields) -> checkFields fields
    TermSet els -> forList $ S.toList els
    TermUnion (Injection _ field) -> checkField field
    TermVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue g t) True els
    checkField = termIsValue g . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

    functionIsValue f = case f of
      FunctionElimination e -> case e of
        EliminationWrap _ -> True
        EliminationOptional (OptionalCases nothing just) -> termIsValue g nothing
          && termIsValue g just
        EliminationRecord _ -> True
        EliminationUnion (CaseStatement _ def cases) -> checkFields cases && (Y.maybe True (termIsValue g) def)
      FunctionLambda (Lambda _ _ body) -> termIsValue g body
      FunctionPrimitive _ -> True
