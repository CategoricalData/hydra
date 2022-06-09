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
evaluate :: (Ord a, Show a, Default a) => Context a -> Data a -> Result (Data a)
evaluate context term = reduce M.empty term
  where
    reduce bindings term = if termIsOpaque (contextStrategy context) term
      then pure term
      else case dataTerm term of
        DataTermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        DataTermLiteral _ -> done
        DataTermElement _ -> done
        DataTermFunction f -> reduceFunction f
        DataTermList terms -> defaultData . DataTermList <$> CM.mapM reduceb terms
        DataTermMap map -> defaultData . DataTermMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        DataTermNominal (Named name term') -> (\t -> defaultData $ DataTermNominal (Named name t)) <$> reduce bindings term'
        DataTermOptional m -> defaultData . DataTermOptional <$> CM.mapM reduceb m
        DataTermRecord fields -> defaultData  . DataTermRecord <$> CM.mapM reduceField fields
        DataTermSet terms -> defaultData . DataTermSet <$> fmap S.fromList (CM.mapM reduceb $ S.toList terms)
        DataTermUnion f -> defaultData . DataTermUnion <$> reduceField f
        DataTermVariable var@(Variable v) -> case M.lookup var bindings of
          Nothing -> fail $ "cannot reduce free variable " ++ v
          Just t -> reduceb t
      where
        done = pure term
        reduceb = reduce bindings
        reduceField (Field n t) = Field n <$> reduceb t
        reduceFunction f = case f of
          FunctionElimination el -> case el of
            EliminationElement -> done
            EliminationOptional (OptionalCases nothing just) -> defaultData . DataTermFunction . FunctionElimination . EliminationOptional <$>
              (OptionalCases <$> reduceb nothing <*> reduceb just)
            EliminationRecord _ -> done
            EliminationUnion cases -> defaultData . DataTermFunction . FunctionElimination . EliminationUnion <$> CM.mapM reduceField cases
          FunctionCompareTo other -> defaultData . DataTermFunction . FunctionCompareTo <$> reduceb other
          FunctionLambda (Lambda v body) -> defaultData . DataTermFunction . FunctionLambda . Lambda v <$> reduceb body
          FunctionPrimitive _ -> done

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication bindings args f = if L.null args then pure f else case dataTerm f of
      DataTermApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      DataTermFunction f -> case f of
        FunctionElimination e -> case e of
          EliminationElement -> do
            arg <- reduce bindings $ L.head args
            case dataTerm arg of
              DataTermElement name -> dereferenceElement context name
                >>= reduce bindings
                >>= reduceApplication bindings (L.tail args)
              _ -> fail "tried to apply data (delta) to a non- element reference"

          EliminationOptional (OptionalCases nothing just) -> do
            arg <- (reduce bindings $ L.head args) >>= deref context
            case dataTerm arg of
              DataTermOptional m -> case m of
                Nothing -> reduce bindings nothing
                Just t -> reduce bindings just >>= reduceApplication bindings (t:L.tail args)
              _ -> fail $ "tried to apply an optional case statement to a non-optional term: " ++ show arg

          EliminationUnion cases -> do
            arg <- (reduce bindings $ L.head args) >>= deref context
            case dataTerm arg of
              DataTermUnion (Field fname t) -> if L.null matching
                  then fail $ "no case for field named " ++ unFieldName fname
                  else reduce bindings (fieldData $ L.head matching)
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
             unwind = pure $ L.foldl apply (defaultData $ DataTermFunction f) args

        FunctionLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
          >>= reduceApplication bindings (L.tail args)

        -- TODO: FunctionProjection

        _ -> fail $ "unsupported function variant: " ++ show (functionVariant f)

      _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

freeVariables :: Data a -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case dataTerm term of
        DataTermApplication (Application t1 t2) -> free bound t1 ++ free bound t2
        DataTermLiteral _ -> []
        DataTermElement _ -> []
        DataTermFunction f -> freeInFunction f
        DataTermList terms -> L.concatMap (free bound) terms
        DataTermMap map -> L.concatMap (\(k, v) -> free bound k ++ free bound v) $ M.toList map
        DataTermOptional m -> case m of
          Nothing -> []
          Just term -> free bound term
        DataTermRecord fields -> L.concatMap (free bound . fieldData) fields
        DataTermSet terms -> L.concatMap (free bound) terms
        DataTermUnion field -> free bound $ fieldData field
        DataTermVariable v -> [v | not (S.member v bound)]
      where
        freeInFunction f = case f of
          FunctionCompareTo term -> free bound term
          FunctionElimination e -> case e of
            EliminationElement -> []
            EliminationNominal _ -> []
            EliminationRecord _ -> []
            EliminationOptional (OptionalCases nothing just) -> free bound nothing ++ free bound just
            EliminationUnion cases -> L.concatMap (free bound . fieldData) cases
          FunctionLambda (Lambda v t) -> free (S.insert v bound) t
          FunctionPrimitive _ -> []

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Data a -> Bool
termIsClosed = S.null . freeVariables

-- | Whether a term is opaque to reduction, i.e. need not be reduced
termIsOpaque :: EvaluationStrategy -> Data a -> Bool
termIsOpaque strategy term = S.member (termVariant term) (evaluationStrategyOpaqueDataVariants strategy)

-- | Whether a term has been fully reduced to a "value"
termIsValue :: EvaluationStrategy -> Data a -> Bool
termIsValue strategy term = termIsOpaque strategy term || case dataTerm term of
      DataTermApplication _ -> False
      DataTermLiteral _ -> True
      DataTermElement _ -> True
      DataTermFunction f -> functionIsValue f
      DataTermList els -> forList els
      DataTermMap map -> L.foldl
        (\b (k, v) -> b && termIsValue strategy k && termIsValue strategy v)
        True $ M.toList map
      DataTermOptional m -> case m of
        Nothing -> True
        Just term -> termIsValue strategy term
      DataTermRecord fields -> checkFields fields
      DataTermSet els -> forList $ S.toList els
      DataTermUnion field -> checkField field
      DataTermVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue strategy t) True els
    checkField = termIsValue strategy . fieldData
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
