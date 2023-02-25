-- | Functions for reducing terms and types, i.e. performing computations

module Hydra.Reduction where

import Hydra.Core
import Hydra.Monads
import Hydra.Compute
import Hydra.Rewriting
import Hydra.Basics
import Hydra.Lexical
import Hydra.Lexical
import Hydra.CoreDecoding
import Hydra.Kv

-- TODO: temp
import Hydra.Sources.Libraries

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


alphaConvert :: Ord m => Name -> Term m -> Term m -> Term m
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

-- | A beta reduction function which is designed for safety, not speed.
--   This function does not assume that term to be evaluated is in a normal form,
--   and will provide an informative error message if evaluation fails.
--   Type checking is assumed to have already occurred.
betaReduceTerm :: (Ord m, Show m) => Term m -> GraphFlow m (Term m)
betaReduceTerm = reduce M.empty
  where
    reduce bindings term = do
      cx <- getState
      case stripTerm term of
        TermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        TermLiteral _ -> done
        TermElement _ -> done
        TermFunction f -> reduceFunction f
        TermList terms -> TermList <$> CM.mapM reduceb terms
        TermMap map -> TermMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        TermWrap (Nominal name term') -> (\t -> TermWrap (Nominal name t)) <$> reduce bindings term'
        TermOptional m -> TermOptional <$> CM.mapM reduceb m
        TermRecord (Record n fields) -> TermRecord <$> (Record n <$> CM.mapM reduceField fields)
        TermSet terms -> TermSet <$> fmap S.fromList (CM.mapM reduceb $ S.toList terms)
        TermUnion (Injection n f) -> TermUnion <$> (Injection n <$> reduceField f)
        TermVariable var@(Name v) -> case M.lookup var bindings of
          Nothing -> fail $ "cannot reduce free variable " ++ v
          Just t -> reduceb t
      where
        done = pure term
        reduceb = reduce bindings
        reduceField (Field n t) = Field n <$> reduceb t
        reduceFunction f = case f of
          FunctionElimination el -> case el of
            EliminationElement -> done
            EliminationOptional (OptionalCases nothing just) -> TermFunction . FunctionElimination . EliminationOptional <$>
              (OptionalCases <$> reduceb nothing <*> reduceb just)
            EliminationRecord _ -> done
            EliminationUnion (CaseStatement n cases) ->
              TermFunction . FunctionElimination . EliminationUnion . CaseStatement n <$> CM.mapM reduceField cases
          FunctionLambda (Lambda v body) -> TermFunction . FunctionLambda . Lambda v <$> reduceb body
          FunctionPrimitive name -> do
            prim <- requirePrimitive name
            if primitiveFunctionArity prim == 0
              then primitiveImplementation prim []
              else done

        -- Assumes that the function is closed and fully reduced. The arguments may not be.
        reduceApplication bindings args f = if L.null args then pure f else case stripTerm f of
          TermApplication (Application func arg) -> reduce bindings func
             >>= reduceApplication bindings (arg:args)

          TermFunction f -> case f of
            FunctionElimination e -> case e of
              EliminationElement -> do
                arg <- reduce bindings $ L.head args
                case stripTerm arg of
                  TermElement name -> dereferenceElement name
                    >>= reduce bindings
                    >>= reduceApplication bindings (L.tail args)
                  _ -> fail "tried to apply data (delta) to a non- element reference"

              EliminationOptional (OptionalCases nothing just) -> do
                arg <- (reduce bindings $ L.head args) >>= deref
                case stripTerm arg of
                  TermOptional m -> case m of
                    Nothing -> reduce bindings nothing
                    Just t -> reduce bindings just >>= reduceApplication bindings (t:L.tail args)
                  _ -> fail $ "tried to apply an optional case statement to a non-optional term: " ++ show arg

              EliminationUnion (CaseStatement _ cases) -> do
                arg <- (reduce bindings $ L.head args) >>= deref
                case stripTerm arg of
                  TermUnion (Injection _ (Field fname t)) -> if L.null matching
                      then fail $ "no case for field named " ++ unFieldName fname
                      else reduce bindings (fieldTerm $ L.head matching)
                        >>= reduceApplication bindings (t:L.tail args)
                    where
                      matching = L.filter (\c -> fieldName c == fname) cases
                  _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

            FunctionPrimitive name -> do
                prim <- requirePrimitive name
                let arity = primitiveFunctionArity prim
                if L.length args >= arity
                  then do
                    rargs <- CM.mapM betaReduceTerm args
                    if countPrimitiveInvocations
                      then nextCount ("count_" ++ unName name)
                      else pure 0
                    (mapM (reduce bindings) $ L.take arity rargs)
                      >>= primitiveImplementation prim
                      >>= reduce bindings
                      >>= reduceApplication bindings (L.drop arity rargs)
                  else unwind args
                where
                  unwind args = pure $ L.foldl (\l r -> TermApplication $ Application l r) (TermFunction f) args

            FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
              >>= reduceApplication bindings (L.tail args)

            -- TODO: FunctionProjection

            _ -> fail $ "unsupported function variant: " ++ show (functionVariant f)

          _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

-- Note: this is eager beta reduction, in that we always descend into subtypes,
--       and always reduce the right-hand side of an application prior to substitution
betaReduceType :: (Ord m, Show m) => Type m -> GraphFlow m (Type m)
betaReduceType typ = do
    cx <- getState :: GraphFlow m (Context m)
    return $ rewriteType (mapExpr cx) id typ
  where
    mapExpr cx rec t = case rec t of
        TypeApplication a -> reduceApp a
        t' -> t'
      where
        reduceApp (ApplicationType lhs rhs) = case lhs of
          TypeAnnotated (Annotated t' ann) -> TypeAnnotated (Annotated (reduceApp (ApplicationType t' rhs)) ann)
          TypeLambda (LambdaType v body) -> fromFlow cx $ betaReduceType $ replaceFreeName v rhs body
          -- nominal types are transparent
          TypeWrap name -> fromFlow cx $ betaReduceType $ TypeApplication $ ApplicationType t' rhs
            where
              t' = fromFlow cx $ requireType name

-- | Apply the special rules:
--     ((\x.e1) e2) == e1, where x does not appear free in e1
--   and
--     ((\x.e1) e2) = e1[x/e2]
--  These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it.
contractTerm :: Ord m => Term m -> Term m
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
etaReduceTerm :: Term m -> Term m
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
termIsClosed :: Term m -> Bool
termIsClosed = S.null . freeVariablesInTerm

-- | Whether a term has been fully reduced to a "value"
termIsValue :: Context m -> Term m -> Bool
termIsValue cx term = case stripTerm term of
    TermApplication _ -> False
    TermLiteral _ -> True
    TermElement _ -> True
    TermFunction f -> functionIsValue f
    TermList els -> forList els
    TermMap map -> L.foldl
      (\b (k, v) -> b && termIsValue cx k && termIsValue cx v)
      True $ M.toList map
    TermOptional m -> case m of
      Nothing -> True
      Just term -> termIsValue cx term
    TermRecord (Record _ fields) -> checkFields fields
    TermSet els -> forList $ S.toList els
    TermUnion (Injection _ field) -> checkField field
    TermVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue cx t) True els
    checkField = termIsValue cx . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

    functionIsValue f = case f of
      FunctionElimination e -> case e of
        EliminationElement -> True
        EliminationWrap _ -> True
        EliminationOptional (OptionalCases nothing just) -> termIsValue cx nothing
          && termIsValue cx just
        EliminationRecord _ -> True
        EliminationUnion (CaseStatement _ cases) -> checkFields cases
      FunctionLambda (Lambda _ body) -> termIsValue cx body
      FunctionPrimitive _ -> True
