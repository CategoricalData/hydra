-- | Functions for reducing terms and types, i.e. performing computations

module Hydra.Reduction where

import Hydra.Core
import Hydra.Flows
import Hydra.Rewriting
import Hydra.Basics
import Hydra.Graph
import Hydra.Lexical
import Hydra.CoreDecoding
import Hydra.Kv
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


alphaConvert :: Ord a => Name -> Term a -> Term a -> Term a
alphaConvert vold tnew = rewriteTerm rewrite id
  where
    rewrite recurse term = case term of
      TermFunction (FunctionLambda (Lambda v body)) -> if v == vold
        then term
        else recurse term
      TermVariable v -> if v == vold then tnew else TermVariable v
      _ -> recurse term

-- For demo purposes. This should be generalized to enable additional side effects of interest.
countPrimitiveInvocations :: Bool
countPrimitiveInvocations = True

-- A term evaluation function which is alternatively lazy or eager
reduceTerm :: (Ord a, Show a) => Bool -> M.Map Name (Term a) -> Term a -> GraphFlow a (Term a)
reduceTerm eager env = rewriteTermM mapping pure
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

    replaceFreeName toReplace replacement = rewriteTerm mapping id
      where
        mapping recurse inner = case inner of
          TermFunction (FunctionLambda (Lambda param body)) -> if param == toReplace then inner else recurse inner
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
        FunctionLambda (Lambda param body) -> case args of
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
      EliminationElement -> fail "element eliminations are unsupported"
      EliminationList _ -> fail "list eliminations are unsupported"
      EliminationOptional _ -> fail "optional eliminations are unsupported"
      EliminationRecord proj -> do
        fields <- Expect.recordWithName (projectionTypeName proj) $ stripTerm reducedArg
        let matchingFields = L.filter (\f -> fieldName f == projectionField proj) fields
        if L.null matchingFields
          then fail $ "no such field: " ++ unFieldName (projectionField proj) ++ " in " ++ unName (projectionTypeName proj) ++ " record"
          else pure $ fieldTerm $ L.head matchingFields
      EliminationUnion (CaseStatement name def fields) -> do
        field <- Expect.injectionWithName name reducedArg
        let matchingFields = L.filter (\f -> fieldName f == fieldName field) fields
        if L.null matchingFields
          then case def of
            Just d -> pure d
            Nothing -> fail $ "no such field " ++ unFieldName (fieldName field) ++ " in " ++ unName name ++ " case statement"
          else pure $ Terms.apply (fieldTerm $ L.head matchingFields) (fieldTerm field)
      EliminationWrap name -> Expect.wrapWithName name reducedArg


-- -- | A beta reduction function which is designed for safety, not speed.
-- --   This function does not assume that term to be evaluated is in a normal form,
-- --   and will provide an informative error message if evaluation fails.
-- --   Type checking is assumed to have already occurred.
-- betaReduceTerm :: (Ord a, Show a) => Term a -> GraphFlow a (Term a)
-- betaReduceTerm = reduce M.empty
--   where
--     reduce bindings term = do
--       cx <- getState
--       case stripTerm term of
--         TermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
--         TermLiteral _ -> done
--         TermElement _ -> done
--         TermFunction f -> reduceFunction f
--         TermList terms -> TermList <$> CM.mapM reduceb terms
--         TermMap map -> TermMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
--           where
--             reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
--         TermWrap (Nominal name term') -> (\t -> TermWrap (Nominal name t)) <$> reduce bindings term'
--         TermOptional m -> TermOptional <$> CM.mapM reduceb m
--         TermRecord (Record n fields) -> TermRecord <$> (Record n <$> CM.mapM reduceField fields)
--         TermSet terms -> TermSet <$> fmap S.fromList (CM.mapM reduceb $ S.toList terms)
--         TermUnion (Injection n f) -> TermUnion <$> (Injection n <$> reduceField f)
--         TermVariable var@(Name v) -> case M.lookup var bindings of
--           Nothing -> fail $ "cannot reduce free variable " ++ v
--           Just t -> reduceb t
--       where
--         done = pure term
--         reduceb = reduce bindings
--         reduceField (Field n t) = Field n <$> reduceb t
--         reduceFunction f = case f of
--           FunctionElimination el -> case el of
--             EliminationElement -> done
--             EliminationOptional (OptionalCases nothing just) -> TermFunction . FunctionElimination . EliminationOptional <$>
--               (OptionalCases <$> reduceb nothing <*> reduceb just)
--             EliminationRecord _ -> done
--             EliminationUnion (CaseStatement n def cases) -> do
--                 rcases <- CM.mapM reduceField cases
--                 rdef <- case def of
--                   Nothing -> pure Nothing
--                   Just d -> Just <$> reduceb d
--                 return $ TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement n rdef rcases

--           FunctionLambda (Lambda v body) -> TermFunction . FunctionLambda . Lambda v <$> reduceb body
--           FunctionPrimitive name -> do
--             prim <- requirePrimitive name
--             if primitiveArity prim == 0
--               then primitiveImplementation prim []
--               else done

--         -- Assumes that the function is closed and fully reduced. The arguments may not be.
--         reduceApplication bindings args f = if L.null args then pure f else case stripTerm f of
--           TermApplication (Application func arg) -> reduce bindings func
--              >>= reduceApplication bindings (arg:args)

--           TermFunction f -> case f of
--             FunctionElimination e -> case e of
--               EliminationElement -> do
--                 arg <- reduce bindings $ L.head args
--                 case stripTerm arg of
--                   TermElement name -> dereferenceElement name
--                     >>= reduce bindings
--                     >>= reduceApplication bindings (L.tail args)
--                   _ -> fail "tried to apply data (delta) to a non- element reference"

--               EliminationOptional (OptionalCases nothing just) -> do
--                 arg <- (reduce bindings $ L.head args) >>= deref
--                 case stripTerm arg of
--                   TermOptional m -> case m of
--                     Nothing -> reduce bindings nothing
--                     Just t -> reduce bindings just >>= reduceApplication bindings (t:L.tail args)
--                   _ -> fail $ "tried to apply an optional case statement to a non-optional term: " ++ show arg

--               EliminationUnion (CaseStatement _ def cases) -> do
--                 arg <- (reduce bindings $ L.head args) >>= deref
--                 case stripTerm arg of
--                   TermUnion (Injection _ (Field fname t)) -> if L.null matching
--                       then case def of
--                         Nothing -> fail $ "no case for field named " ++ unFieldName fname
--                         Just d -> reduce bindings d
--                       else reduce bindings (fieldTerm $ L.head matching)
--                         >>= reduceApplication bindings (t:L.tail args)
--                     where
--                       matching = L.filter (\c -> fieldName c == fname) cases
--                   _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

--             FunctionPrimitive name -> do
--                 prim <- requirePrimitive name
--                 let arity = primitiveArity prim
--                 if L.length args >= arity
--                   then do
--                     rargs <- CM.mapM betaReduceTerm args
--                     if countPrimitiveInvocations
--                       then nextCount ("count_" ++ unName name)
--                       else pure 0
--                     pargs <- mapM (reduce bindings) $ L.take arity rargs
--                     result <- primitiveImplementation prim pargs
--                     reduce bindings result
--                       >>= reduceApplication bindings (L.drop arity rargs)
--                   else unwind args
--                 where
--                   unwind args = pure $ L.foldl (\l r -> TermApplication $ Application l r) (TermFunction f) args

--             FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
--               >>= reduceApplication bindings (L.tail args)

--             -- TODO: FunctionProjection

--           _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

-- Note: this is eager beta reduction, in that we always descend into subtypes,
--       and always reduce the right-hand side of an application prior to substitution
betaReduceType :: (Ord a, Show a) => Type a -> GraphFlow a (Type a)
betaReduceType typ = do
    g <- getState :: GraphFlow a (Graph a)
    return $ rewriteType (mapExpr g) id typ
  where
    mapExpr g recurse t = case recurse t of
        TypeApplication a -> reduceApp a
        t' -> t'
      where
        reduceApp (ApplicationType lhs rhs) = case lhs of
          TypeAnnotated (Annotated t' ann) -> TypeAnnotated (Annotated (reduceApp (ApplicationType t' rhs)) ann)
          TypeLambda (LambdaType v body) -> fromFlow g $ betaReduceType $ replaceFreeName v rhs body
          -- nominal types are transparent
          TypeWrap name -> fromFlow g $ betaReduceType $ TypeApplication $ ApplicationType t' rhs
            where
              t' = fromFlow g $ requireType name

-- | Apply the special rules:
--     ((\x.e1) e2) == e1, where x does not appear free in e1
--   and
--     ((\x.e1) e2) = e1[x/e2]
--  These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it.
contractTerm :: Ord a => Term a -> Term a
contractTerm = rewriteTerm rewrite id
  where
    rewrite recurse term = case rec of
        TermApplication (Application lhs rhs) -> case stripTerm lhs of
          TermFunction (FunctionLambda (Lambda v body)) -> if isFreeIn v body
            then body
            else alphaConvert v rhs body
          _ -> rec
        _ -> rec
      where
        rec = recurse term

-- Note: unused / untested
etaReduceTerm :: Term a -> Term a
etaReduceTerm term = case term of
    TermAnnotated (Annotated term1 ann) -> TermAnnotated (Annotated (etaReduceTerm term1) ann)
    TermFunction (FunctionLambda l) -> reduceLambda l
    _ -> noChange
  where
    reduceLambda (Lambda v body) = case etaReduceTerm body of
      TermAnnotated (Annotated body1 ann) -> reduceLambda (Lambda v body1)
      TermApplication a -> reduceApplication a
        where
          reduceApplication (Application lhs rhs) = case etaReduceTerm rhs of
            TermAnnotated (Annotated rhs1 ann) -> reduceApplication (Application lhs rhs1)
            TermVariable v1 -> if v == v1 && isFreeIn v lhs
              then etaReduceTerm lhs
              else noChange
            _ -> noChange
      _ -> noChange
    noChange = term

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term a -> Bool
termIsClosed = S.null . freeVariablesInTerm

-- | Whether a term has been fully reduced to a "value"
termIsValue :: Graph a -> Term a -> Bool
termIsValue g term = case stripTerm term of
    TermApplication _ -> False
    TermLiteral _ -> True
    TermElement _ -> True
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
        EliminationElement -> True
        EliminationWrap _ -> True
        EliminationOptional (OptionalCases nothing just) -> termIsValue g nothing
          && termIsValue g just
        EliminationRecord _ -> True
        EliminationUnion (CaseStatement _ def cases) -> checkFields cases && (Y.maybe True (termIsValue g) def)
      FunctionLambda (Lambda _ body) -> termIsValue g body
      FunctionPrimitive _ -> True
