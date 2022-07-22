module Hydra.Reduction where

import Hydra.Core
import Hydra.Impl.Haskell.Extras
import Hydra.Evaluation
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Rewriting
import Hydra.Basics
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Primitives

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | A beta reduction function which is designed for safety, not speed.
--   This function does not assume that term to be evaluated is in a normal form,
--   and will provide an informative error message if evaluation fails.
--   Type checking is assumed to have already occurred.
betaReduceTerm :: (Ord a, Show a) => Context a -> Term a -> Result (Term a)
betaReduceTerm cx term = reduce M.empty term
  where
    reduce bindings term = if termIsOpaque (contextStrategy cx) term
      then pure term
      else case termExpr cx term of
        TermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        TermLiteral _ -> done
        TermElement _ -> done
        TermFunction f -> reduceFunction f
        TermList terms -> TermList <$> CM.mapM reduceb terms
        TermMap map -> TermMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        TermNominal (Named name term') -> (\t -> TermNominal (Named name t)) <$> reduce bindings term'
        TermOptional m -> TermOptional <$> CM.mapM reduceb m
        TermRecord fields -> TermRecord <$> CM.mapM reduceField fields
        TermSet terms -> TermSet <$> fmap S.fromList (CM.mapM reduceb $ S.toList terms)
        TermUnion f -> TermUnion <$> reduceField f
        TermVariable var@(Variable v) -> case M.lookup var bindings of
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
            EliminationUnion cases -> TermFunction . FunctionElimination . EliminationUnion <$> CM.mapM reduceField cases
          FunctionCompareTo other -> TermFunction . FunctionCompareTo <$> reduceb other
          FunctionLambda (Lambda v body) -> TermFunction . FunctionLambda . Lambda v <$> reduceb body
          FunctionPrimitive _ -> done

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication bindings args f = if L.null args then pure f else case termExpr cx f of
      TermApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      TermFunction f -> case f of
        FunctionElimination e -> case e of
          EliminationElement -> do
            arg <- reduce bindings $ L.head args
            case termExpr cx arg of
              TermElement name -> dereferenceElement cx name
                >>= reduce bindings
                >>= reduceApplication bindings (L.tail args)
              _ -> fail "tried to apply data (delta) to a non- element reference"

          EliminationOptional (OptionalCases nothing just) -> do
            arg <- (reduce bindings $ L.head args) >>= deref cx
            case termExpr cx arg of
              TermOptional m -> case m of
                Nothing -> reduce bindings nothing
                Just t -> reduce bindings just >>= reduceApplication bindings (t:L.tail args)
              _ -> fail $ "tried to apply an optional case statement to a non-optional term: " ++ show arg

          EliminationUnion cases -> do
            arg <- (reduce bindings $ L.head args) >>= deref cx
            case termExpr cx arg of
              TermUnion (Field fname t) -> if L.null matching
                  then fail $ "no case for field named " ++ unFieldName fname
                  else reduce bindings (fieldTerm $ L.head matching)
                    >>= reduceApplication bindings (t:L.tail args)
                where
                  matching = L.filter (\c -> fieldName c == fname) cases
              _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

        -- TODO: FunctionCompareTo

        FunctionPrimitive name -> do
             prim <- requirePrimitiveFunction cx name
             let arity = primitiveFunctionArity cx prim
             if L.length args >= arity
               then (mapM (reduce bindings) $ L.take arity args)
                 >>= primitiveFunctionImplementation prim
                 >>= reduce bindings
                 >>= reduceApplication bindings (L.drop arity args)
               else unwind
           where
             unwind = pure $ L.foldl apply (TermFunction f) args

        FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
          >>= reduceApplication bindings (L.tail args)

        -- TODO: FunctionProjection

        _ -> fail $ "unsupported function variant: " ++ show (functionVariant f)

      _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

betaReduceType :: (Ord m, Show m) => Bool -> Context m -> Type m -> Type m
betaReduceType eager cx = rewriteType mapExpr id
  where
    mapExpr _ t = case typeExpr cx t of
        TypeAnnotated (Annotated t' ann) -> TypeAnnotated $ Annotated (recurse t') ann
        TypeApplication (ApplicationType lhs rhs) -> case typeExpr cx lhs' of
            TypeLambda (LambdaType v body) -> recurse $ replaceFreeVariableType v rhs' body
            TypeNominal name -> recurse $ Types.apply t' rhs' -- nominal types are transparent
              where
                ResultSuccess t' = requireType cx name
            _ -> t
          where
            lhs' = recurse lhs
            rhs' = if eager then recurse rhs else rhs
        _ -> t
    recurse = betaReduceType eager cx

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term a -> Bool
termIsClosed = S.null . freeVariablesInTerm

-- | Whether a term is opaque to reduction, i.e. need not be reduced
termIsOpaque :: EvaluationStrategy -> Term a -> Bool
termIsOpaque strategy term = S.member (termVariant term) (evaluationStrategyOpaqueTermVariants strategy)

-- | Whether a term has been fully reduced to a "value"
termIsValue :: Context m -> EvaluationStrategy -> Term m -> Bool
termIsValue cx strategy term = termIsOpaque strategy term || case termExpr cx term of
    TermApplication _ -> False
    TermLiteral _ -> True
    TermElement _ -> True
    TermFunction f -> functionIsValue f
    TermList els -> forList els
    TermMap map -> L.foldl
      (\b (k, v) -> b && termIsValue cx strategy k && termIsValue cx strategy v)
      True $ M.toList map
    TermOptional m -> case m of
      Nothing -> True
      Just term -> termIsValue cx strategy term
    TermRecord fields -> checkFields fields
    TermSet els -> forList $ S.toList els
    TermUnion field -> checkField field
    TermVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue cx strategy t) True els
    checkField = termIsValue cx strategy . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

    functionIsValue f = case f of
      FunctionCompareTo other -> termIsValue cx strategy other
      FunctionElimination e -> case e of
        EliminationElement -> True
        EliminationNominal _ -> True
        EliminationOptional (OptionalCases nothing just) -> termIsValue cx strategy nothing
          && termIsValue cx strategy just
        EliminationRecord _ -> True
        EliminationUnion cases -> checkFields cases
      FunctionLambda (Lambda _ body) -> termIsValue cx strategy body
      FunctionPrimitive _ -> True
