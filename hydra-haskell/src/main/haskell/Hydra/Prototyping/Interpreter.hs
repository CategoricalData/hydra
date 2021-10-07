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
import Hydra.Ext.Haskell.Dsl
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.Steps

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
    dereferenceElement :: Name -> Result Term
    dereferenceElement en = case M.lookup en (contextElements context) of
      Nothing -> fail $ "referenced element does not exist in graph: " ++ en
      Just e -> pure $ elementData e

    reduce :: M.Map Variable Term -> Term -> Result Term
    reduce bindings term = if termIsOpaque (contextStrategy context) term
      then pure term
      else case term of
        TermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        TermAtomic _ -> done
        TermCases cases -> TermRecord <$> CM.mapM reduceField cases
        TermCompareTo other -> TermCompareTo <$> reduceb other
        TermData -> done
        TermElement _ -> done
        TermFunction _ -> done
        TermLambda (Lambda v body) -> TermLambda . Lambda v <$> reduceb body
        TermList terms -> TermList <$> CM.mapM reduceb terms
        TermMap map -> TermMap <$> fmap M.fromList (CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        TermProjection _ -> done
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

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication :: M.Map Variable Term -> [Term] -> Term -> Result Term
    reduceApplication bindings args f = if L.null args then pure f else case f of
      TermApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      TermCases cases -> do
        arg <- reduce bindings $ L.head args
        case arg of
          TermUnion (Field fname t) -> if L.null matching
              then fail $ "no case for field named " ++ fname
              else reduce bindings (fieldTerm $ L.head matching)
                >>= reduceApplication bindings (t:(L.tail args))
            where
              matching = L.filter (\c -> fieldName c == fname) cases
          _ -> fail $ "tried to apply a case statement to a non- union term: " ++ show arg

      TermData -> do
          arg <- reduce bindings $ L.head args
          case arg of
            TermElement name -> dereferenceElement name
              >>= reduce bindings
              >>= reduceApplication bindings (L.tail args)
            _ -> fail "tried to apply data (delta) to a non- element reference"

      TermFunction name -> do
           prim <- requirePrimitiveFunction context name
           let arity = primitiveFunctionArity prim
           if L.length args >= arity
             then (mapM (reduce bindings) $ L.take arity args)
               >>= (reduce bindings . primitiveFunctionImplementation prim)
               >>= reduceApplication bindings (L.drop arity args)
             else unwind
         where
           unwind = pure $ L.foldl apply f args

      TermLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
        >>= reduceApplication bindings (L.tail args)

      _ -> fail $ "tried to apply a non-function: " ++ show (termVariant f)

freeVariables :: Term -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case term of
      TermApplication (Application t1 t2) -> free bound t1 ++ free bound t2
      TermAtomic _ -> []
      TermCases cases -> L.concatMap (free bound . fieldTerm) cases
      TermCompareTo term -> free bound term
      TermData -> []
      TermElement _ -> []
      TermFunction _ -> []
      TermLambda (Lambda v t) -> free (S.insert v bound) t
      TermList terms -> L.concatMap (free bound) terms
      TermMap map -> L.concatMap (\(k, v) -> free bound k ++ free bound v) $ M.toList map
      TermProjection _ -> []
      TermRecord fields -> L.concatMap (free bound . fieldTerm) fields
      TermSet terms -> L.concatMap (free bound) terms
      TermUnion field -> free bound $ fieldTerm field
      TermVariable v -> [v | not (S.member v bound)]

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
      TermCases cases -> checkFields cases
      TermCompareTo other -> termIsValue strategy other
      TermData -> True
      TermElement _ -> True
      TermFunction _ -> True
      TermLambda (Lambda _ body) -> termIsValue strategy body
      TermList els -> forList els
      TermMap map -> L.foldl
        (\b (k, v) -> b && termIsValue strategy k && termIsValue strategy v)
        True $ M.toList map
      TermProjection _ -> True
      TermRecord fields -> checkFields fields
      TermSet els -> forList $ S.toList els
      TermUnion field -> checkField field
      TermVariable _ -> False
  where
    forList els = L.foldl (\b t -> b && termIsValue strategy t) True els
    checkField = termIsValue strategy . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True
