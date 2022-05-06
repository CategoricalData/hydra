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
      else case termData term of
        ExpressionApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        ExpressionLiteral _ -> done
        ExpressionElement _ -> done
        ExpressionFunction f -> reduceFunction f
        ExpressionList terms -> defaultTerm . ExpressionList <$> CM.mapM reduceb terms
        ExpressionMap map -> defaultTerm . ExpressionMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        ExpressionNominal (NominalTerm name term') -> (\t -> defaultTerm $ ExpressionNominal (NominalTerm name t)) <$> reduce bindings term'
        ExpressionOptional m -> defaultTerm . ExpressionOptional <$> CM.mapM reduceb m
        ExpressionRecord fields -> defaultTerm  . ExpressionRecord <$> CM.mapM reduceField fields
        ExpressionSet terms -> defaultTerm . ExpressionSet <$> fmap S.fromList (CM.mapM reduceb $ S.toList terms)
        ExpressionUnion f -> defaultTerm . ExpressionUnion <$> reduceField f
        ExpressionVariable v -> case M.lookup v bindings of
          Nothing -> fail $ "cannot reduce free variable " ++ v
          Just t -> reduceb t
      where
        done = pure term
        reduceb = reduce bindings
        reduceField (Field n t) = Field n <$> reduceb t
        reduceFunction f = case f of
          FunctionCases cases -> defaultTerm . ExpressionFunction . FunctionCases <$> CM.mapM reduceField cases
          FunctionCompareTo other -> defaultTerm . ExpressionFunction . FunctionCompareTo <$> reduceb other
          FunctionData -> done
          FunctionLambda (Lambda v body) -> defaultTerm . ExpressionFunction . FunctionLambda . Lambda v <$> reduceb body
          FunctionOptionalCases (OptionalCases nothing just) -> defaultTerm . ExpressionFunction . FunctionOptionalCases <$>
            (OptionalCases <$> reduceb nothing <*> reduceb just)
          FunctionPrimitive _ -> done
          FunctionProjection _ -> done

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication bindings args f = if L.null args then pure f else case termData f of
      ExpressionApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      ExpressionFunction f -> case f of
        FunctionCases cases -> do
          arg <- (reduce bindings $ L.head args) >>= deref context
          case termData arg of
            ExpressionUnion (Field fname t) -> if L.null matching
                then fail $ "no case for field named " ++ fname
                else reduce bindings (fieldTerm $ L.head matching)
                  >>= reduceApplication bindings (t:L.tail args)
              where
                matching = L.filter (\c -> fieldName c == fname) cases
            _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

        -- TODO: FunctionCompareTo

        FunctionData -> do
          arg <- reduce bindings $ L.head args
          case termData arg of
            ExpressionElement name -> dereferenceElement context name
              >>= reduce bindings
              >>= reduceApplication bindings (L.tail args)
            _ -> fail "tried to apply data (delta) to a non- element reference"

        FunctionOptionalCases (OptionalCases nothing just) -> do
          arg <- (reduce bindings $ L.head args) >>= deref context
          case termData arg of
            ExpressionOptional m -> case m of
              Nothing -> reduce bindings nothing
              Just t -> reduce bindings just >>= reduceApplication bindings (t:L.tail args)
            _ -> fail $ "tried to apply an optional case statement to a non-optional term: " ++ show arg

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
             unwind = pure $ L.foldl apply (defaultTerm $ ExpressionFunction f) args

        FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
          >>= reduceApplication bindings (L.tail args)

        -- TODO: FunctionProjection

        _ -> fail $ "unsupported function variant: " ++ show (functionVariant f)

      _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

freeVariables :: Term a -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case termData term of
        ExpressionApplication (Application t1 t2) -> free bound t1 ++ free bound t2
        ExpressionLiteral _ -> []
        ExpressionElement _ -> []
        ExpressionFunction f -> freeInFunction f
        ExpressionList terms -> L.concatMap (free bound) terms
        ExpressionMap map -> L.concatMap (\(k, v) -> free bound k ++ free bound v) $ M.toList map
        ExpressionOptional m -> case m of
          Nothing -> []
          Just term -> free bound term
        ExpressionRecord fields -> L.concatMap (free bound . fieldTerm) fields
        ExpressionSet terms -> L.concatMap (free bound) terms
        ExpressionUnion field -> free bound $ fieldTerm field
        ExpressionVariable v -> [v | not (S.member v bound)]
      where
        freeInFunction f = case f of
          FunctionCases cases -> L.concatMap (free bound . fieldTerm) cases
          FunctionCompareTo term -> free bound term
          FunctionData -> []
          FunctionLambda (Lambda v t) -> free (S.insert v bound) t
          FunctionOptionalCases (OptionalCases nothing just) -> free bound nothing ++ free bound just
          FunctionPrimitive _ -> []
          FunctionProjection _ -> []

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term a -> Bool
termIsClosed = S.null . freeVariables

-- | Whether a term is opaque to reduction, i.e. need not be reduced
termIsOpaque :: EvaluationStrategy -> Term a -> Bool
termIsOpaque strategy term = S.member (termVariant term) (evaluationStrategyOpaqueTermVariants strategy)

-- | Whether a term has been fully reduced to a "value"
termIsValue :: EvaluationStrategy -> Term a -> Bool
termIsValue strategy term = termIsOpaque strategy term || case termData term of
      ExpressionApplication _ -> False
      ExpressionLiteral _ -> True
      ExpressionElement _ -> True
      ExpressionFunction f -> functionIsValue f
      ExpressionList els -> forList els
      ExpressionMap map -> L.foldl
        (\b (k, v) -> b && termIsValue strategy k && termIsValue strategy v)
        True $ M.toList map
      ExpressionOptional m -> case m of
        Nothing -> True
        Just term -> termIsValue strategy term
      ExpressionRecord fields -> checkFields fields
      ExpressionSet els -> forList $ S.toList els
      ExpressionUnion field -> checkField field
      ExpressionVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue strategy t) True els
    checkField = termIsValue strategy . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

    functionIsValue f = case f of
      FunctionCases cases -> checkFields cases
      FunctionCompareTo other -> termIsValue strategy other
      FunctionData -> True
      FunctionLambda (Lambda _ body) -> termIsValue strategy body
      FunctionOptionalCases (OptionalCases nothing just) -> termIsValue strategy nothing && termIsValue strategy just
      FunctionPrimitive _ -> True
      FunctionProjection _ -> True
