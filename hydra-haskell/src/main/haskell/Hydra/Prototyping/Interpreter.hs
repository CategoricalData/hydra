module Hydra.Prototyping.Interpreter (
  evaluate,
  freeVariables,
  termIsClosed,
  termIsOpaque,
  termIsValue,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.Primitives

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | A beta reduction function which is designed for safety, not speed.
--   This function does not assume that term to be evaluated is in a normal form,
--   and will provide an informative error message if evaluation fails.
--   Type checking is assumed to have already occurred.
evaluate :: Context -> Term -> Result Term
evaluate context term = reduce M.empty term
  where
    reduce :: M.Map Variable Term -> Term -> Result Term
    reduce bindings term = if termIsOpaque (contextStrategy context) term
      then pure term
      else case term of
        TermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        TermAtomic _ -> done
        TermElement _ -> done
        TermFunction f -> reduceFunction f
        TermList terms -> TermList <$> CM.mapM reduceb terms
        TermMap map -> TermMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        TermOptional m -> TermOptional <$> CM.mapM reduceb m
        TermRecord fields -> TermRecord <$> CM.mapM reduceField fields
        TermSet terms -> TermSet <$> (fmap S.fromList $ CM.mapM reduceb $ S.toList terms)
        TermUnion f -> TermUnion <$> reduceField f
        TermVariable v -> case M.lookup v bindings of
          Nothing -> fail $ "cannot reduce free variable " ++ v
          Just t -> reduceb t
      where
        done = pure term
        reduceb = reduce bindings
        reduceField (Field n t) = Field n <$> reduceb t
        reduceFunction f = case f of
          FunctionCases cases -> TermRecord <$> CM.mapM reduceField cases
          FunctionCompareTo other -> TermFunction . FunctionCompareTo <$> reduceb other
          FunctionData -> done
          FunctionLambda (Lambda v body) -> TermFunction . FunctionLambda . Lambda v <$> reduceb body
          FunctionPrimitive _ -> done
          FunctionProjection _ -> done

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication :: M.Map Variable Term -> [Term] -> Term -> Result Term
    reduceApplication bindings args f = if L.null args then pure f else case f of
      TermApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      TermFunction f -> case f of
        FunctionCases cases -> do
          arg <- reduce bindings $ L.head args
          case arg of
            TermUnion (Field fname t) -> if L.null matching
                then fail $ "no case for field named " ++ fname
                else reduce bindings (fieldTerm $ L.head matching)
                  >>= reduceApplication bindings (t:(L.tail args))
              where
                matching = L.filter (\c -> fieldName c == fname) cases
            _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

        -- TODO: FunctionCompareTo

        FunctionData -> do
            arg <- reduce bindings $ L.head args
            case arg of
              TermElement name -> dereferenceElement context name
                >>= reduce bindings
                >>= reduceApplication bindings (L.tail args)
              _ -> fail "tried to apply data (delta) to a non- element reference"

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
             unwind = pure $ L.foldl apply (TermFunction f) args

        FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
          >>= reduceApplication bindings (L.tail args)

        -- TODO: FunctionProjection

      _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

freeVariables :: Term -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case term of
        TermApplication (Application t1 t2) -> free bound t1 ++ free bound t2
        TermAtomic _ -> []
        TermElement _ -> []
        TermFunction f -> freeInFunction f
        TermList terms -> L.concatMap (free bound) terms
        TermMap map -> L.concatMap (\(k, v) -> free bound k ++ free bound v) $ M.toList map
        TermOptional m -> case m of
          Nothing -> []
          Just term -> free bound term
        TermRecord fields -> L.concatMap (free bound . fieldTerm) fields
        TermSet terms -> L.concatMap (free bound) terms
        TermUnion field -> free bound $ fieldTerm field
        TermVariable v -> [v | not (S.member v bound)]
      where
        freeInFunction f = case f of
          FunctionCases cases -> L.concatMap (free bound . fieldTerm) cases
          FunctionCompareTo term -> free bound term
          FunctionData -> []
          FunctionLambda (Lambda v t) -> free (S.insert v bound) t
          FunctionPrimitive _ -> []
          FunctionProjection _ -> []

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term -> Bool
termIsClosed = S.null . freeVariables

-- | Whether a term is opaque to reduction, i.e. need not be reduced
termIsOpaque :: EvaluationStrategy -> Term -> Bool
termIsOpaque strategy term = S.member (termVariant term) (evaluationStrategyOpaqueTermVariants strategy)

-- | Whether a term has been fully reduced to a "value"
termIsValue :: EvaluationStrategy -> Term -> Bool
termIsValue strategy term = if termIsOpaque strategy term
    then True
    else case term of
      TermApplication _ -> False
      TermAtomic _ -> True
      TermElement _ -> True
      TermFunction f -> functionIsValue f
      TermList els -> forList els
      TermMap map -> L.foldl
        (\b (k, v) -> b && termIsValue strategy k && termIsValue strategy v)
        True $ M.toList map
      TermOptional m -> case m of
        Nothing -> True
        Just term -> termIsValue strategy term
      TermRecord fields -> checkFields fields
      TermSet els -> forList $ S.toList els
      TermUnion field -> checkField field
      TermVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue strategy t) True els
    checkField = termIsValue strategy . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

    functionIsValue f = case f of
      FunctionCases cases -> checkFields cases
      FunctionCompareTo other -> termIsValue strategy other
      FunctionData -> True
      FunctionLambda (Lambda _ body) -> termIsValue strategy body
      FunctionPrimitive _ -> True
      FunctionProjection _ -> True
