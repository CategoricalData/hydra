module Hydra.Interpreter (
  evaluate,
  freeVariables,
  termIsClosed,
  termIsOpaque,
  termIsValue,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Basics
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.Primitives

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | A beta reduction function which is designed for safety, not speed.
--   This function does not assume that term to be evaluated is in a normal form,
--   and will provide an informative error message if evaluation fails.
--   Type checking is assumed to have already occurred.
evaluate :: (Ord a, Show a, Default a) => Context a -> Term a -> Result (Term a)
evaluate context term = reduce M.empty term
  where
    reduce bindings term = if termIsOpaque (contextStrategy context) term
      then pure term
      else case termExpr term of
        TermExprApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        TermExprLiteral _ -> done
        TermExprElement _ -> done
        TermExprFunction f -> reduceFunction f
        TermExprList terms -> defaultTerm . TermExprList <$> CM.mapM reduceb terms
        TermExprMap map -> defaultTerm . TermExprMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        TermExprNominal (Named name term') -> (\t -> defaultTerm $ TermExprNominal (Named name t)) <$> reduce bindings term'
        TermExprOptional m -> defaultTerm . TermExprOptional <$> CM.mapM reduceb m
        TermExprRecord fields -> defaultTerm  . TermExprRecord <$> CM.mapM reduceField fields
        TermExprSet terms -> defaultTerm . TermExprSet <$> fmap S.fromList (CM.mapM reduceb $ S.toList terms)
        TermExprUnion f -> defaultTerm . TermExprUnion <$> reduceField f
        TermExprVariable var@(Variable v) -> case M.lookup var bindings of
          Nothing -> fail $ "cannot reduce free variable " ++ v
          Just t -> reduceb t
      where
        done = pure term
        reduceb = reduce bindings
        reduceField (Field n t) = Field n <$> reduceb t
        reduceFunction f = case f of
          FunctionElimination el -> case el of
            EliminationElement -> done
            EliminationOptional (OptionalCases nothing just) -> defaultTerm . TermExprFunction . FunctionElimination . EliminationOptional <$>
              (OptionalCases <$> reduceb nothing <*> reduceb just)
            EliminationRecord _ -> done
            EliminationUnion cases -> defaultTerm . TermExprFunction . FunctionElimination . EliminationUnion <$> CM.mapM reduceField cases
          FunctionCompareTo other -> defaultTerm . TermExprFunction . FunctionCompareTo <$> reduceb other
          FunctionLambda (Lambda v body) -> defaultTerm . TermExprFunction . FunctionLambda . Lambda v <$> reduceb body
          FunctionPrimitive _ -> done

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication bindings args f = if L.null args then pure f else case termExpr f of
      TermExprApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      TermExprFunction f -> case f of
        FunctionElimination e -> case e of
          EliminationElement -> do
            arg <- reduce bindings $ L.head args
            case termExpr arg of
              TermExprElement name -> dereferenceElement context name
                >>= reduce bindings
                >>= reduceApplication bindings (L.tail args)
              _ -> fail "tried to apply data (delta) to a non- element reference"

          EliminationOptional (OptionalCases nothing just) -> do
            arg <- (reduce bindings $ L.head args) >>= deref context
            case termExpr arg of
              TermExprOptional m -> case m of
                Nothing -> reduce bindings nothing
                Just t -> reduce bindings just >>= reduceApplication bindings (t:L.tail args)
              _ -> fail $ "tried to apply an optional case statement to a non-optional term: " ++ show arg

          EliminationUnion cases -> do
            arg <- (reduce bindings $ L.head args) >>= deref context
            case termExpr arg of
              TermExprUnion (Field fname t) -> if L.null matching
                  then fail $ "no case for field named " ++ unFieldName fname
                  else reduce bindings (fieldTerm $ L.head matching)
                    >>= reduceApplication bindings (t:L.tail args)
                where
                  matching = L.filter (\c -> fieldName c == fname) cases
              _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

        -- TODO: FunctionCompareTo

        FunctionPrimitive name -> do
             prim <- requirePrimitiveFunction context name
             let arity = primitiveFunctionArity prim
             if L.length args >= arity
               then (mapM (reduce bindings) $ L.take arity args)
                 >>= primitiveFunctionImplementation prim
                 >>= reduce bindings
                 >>= reduceApplication bindings (L.drop arity args)
               else unwind
           where
             unwind = pure $ L.foldl apply (defaultTerm $ TermExprFunction f) args

        FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
          >>= reduceApplication bindings (L.tail args)

        -- TODO: FunctionProjection

        _ -> fail $ "unsupported function variant: " ++ show (functionVariant f)

      _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

freeVariables :: Term a -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case termExpr term of
        TermExprApplication (Application t1 t2) -> free bound t1 ++ free bound t2
        TermExprLiteral _ -> []
        TermExprElement _ -> []
        TermExprFunction f -> freeInFunction f
        TermExprList terms -> L.concatMap (free bound) terms
        TermExprMap map -> L.concatMap (\(k, v) -> free bound k ++ free bound v) $ M.toList map
        TermExprOptional m -> case m of
          Nothing -> []
          Just term -> free bound term
        TermExprRecord fields -> L.concatMap (free bound . fieldTerm) fields
        TermExprSet terms -> L.concatMap (free bound) terms
        TermExprUnion field -> free bound $ fieldTerm field
        TermExprVariable v -> [v | not (S.member v bound)]
      where
        freeInFunction f = case f of
          FunctionCompareTo term -> free bound term
          FunctionElimination e -> case e of
            EliminationElement -> []
            EliminationNominal _ -> []
            EliminationRecord _ -> []
            EliminationOptional (OptionalCases nothing just) -> free bound nothing ++ free bound just
            EliminationUnion cases -> L.concatMap (free bound . fieldTerm) cases
          FunctionLambda (Lambda v t) -> free (S.insert v bound) t
          FunctionPrimitive _ -> []

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term a -> Bool
termIsClosed = S.null . freeVariables

-- | Whether a term is opaque to reduction, i.e. need not be reduced
termIsOpaque :: EvaluationStrategy -> Term a -> Bool
termIsOpaque strategy term = S.member (termVariant term) (evaluationStrategyOpaqueTermVariants strategy)

-- | Whether a term has been fully reduced to a "value"
termIsValue :: EvaluationStrategy -> Term a -> Bool
termIsValue strategy term = termIsOpaque strategy term || case termExpr term of
      TermExprApplication _ -> False
      TermExprLiteral _ -> True
      TermExprElement _ -> True
      TermExprFunction f -> functionIsValue f
      TermExprList els -> forList els
      TermExprMap map -> L.foldl
        (\b (k, v) -> b && termIsValue strategy k && termIsValue strategy v)
        True $ M.toList map
      TermExprOptional m -> case m of
        Nothing -> True
        Just term -> termIsValue strategy term
      TermExprRecord fields -> checkFields fields
      TermExprSet els -> forList $ S.toList els
      TermExprUnion field -> checkField field
      TermExprVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue strategy t) True els
    checkField = termIsValue strategy . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

    functionIsValue f = case f of
      FunctionCompareTo other -> termIsValue strategy other
      FunctionElimination e -> case e of
        EliminationElement -> True
        EliminationNominal _ -> True
        EliminationOptional (OptionalCases nothing just) -> termIsValue strategy nothing && termIsValue strategy just
        EliminationRecord _ -> True
        EliminationUnion cases -> checkFields cases
      FunctionLambda (Lambda _ body) -> termIsValue strategy body
      FunctionPrimitive _ -> True
