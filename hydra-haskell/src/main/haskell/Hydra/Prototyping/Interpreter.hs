module Hydra.Prototyping.Interpreter (
  evaluate,
  primitiveFunctionArity,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.Primitives

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- | A beta reduction function which is designed for safety, not speed.
--   This function does not assume that term to be evaluated is in a normal form,
--   and will provide an informative error message if evaluation fails.
--   Type checking is assumed to have already occurred.
evaluate :: Context -> Term -> Either String Term
evaluate context term = reduce M.empty term
  where
    reduce :: (M.Map Variable Term) -> Term -> Either String Term
    reduce bindings term = if termIsOpaque (contextStrategy context) term
      then Right term
      else case term of
        TermApplication (Application func arg) -> reduceb func >>= reduceApplication bindings [arg]
        TermAtomic _ -> done
        TermCases cases -> TermRecord <$> CM.mapM reduceField cases
        TermCompareTo other -> TermCompareTo <$> reduceb other
        TermData -> done
        TermElement _ -> done
        TermFunction _ -> done
        TermLambda (Lambda v body) -> (TermLambda . Lambda v) <$> reduceb body
        TermList terms -> TermList <$> CM.mapM reduceb terms
        TermMap map -> TermMap <$> (fmap M.fromList $ CM.mapM reducePair $ M.toList map)
          where
            reducePair (k, v) = (,) <$> reduceb k <*> reduceb v
        TermProjection _ -> done
        TermRecord fields -> TermRecord <$> CM.mapM reduceField fields
        TermSet terms -> TermSet <$> (fmap S.fromList $ CM.mapM reduceb $ S.toList terms)
        TermUnion f -> TermUnion <$> reduceField f
        TermVariable v -> case M.lookup v bindings of
          Nothing -> Left $ "cannot reduce free variable " ++ v
          Just t -> reduceb t
      where
        done = Right term
        reduceb = reduce bindings
        reduceField (Field n t) = Field n <$> reduceb t

    -- Assumes that the function is closed and fully reduced. The arguments may not be.
    reduceApplication :: (M.Map Variable Term) -> [Term] -> Term -> Either String Term
    reduceApplication bindings args f = if L.null args then pure f else case f of
      TermApplication (Application func arg) -> reduce bindings func
         >>= reduceApplication bindings (arg:args)

      TermFunction name -> case lookupPrimitiveFunction context name of
        Nothing -> Left $ "no such primitive function: " ++ name
        Just prim -> if L.length args >= arity
            then (mapM (reduce bindings) $ L.take arity args)
              >>= (\args -> reduce bindings $ primitiveFunctionImplementation prim args)
              >>= (reduceApplication bindings (L.drop arity args))
            else unwind
          where
            arity = primitiveFunctionArity prim
            unwind = Right $ L.foldl apply f args

      TermLambda (Lambda v body) -> reduce (M.insert v (L.head args) bindings) body
        >>= reduceApplication bindings (L.tail args)

      _ -> Left $ "tried to apply a non-function: " ++ show (termVariant f)

primitiveFunctionArity :: PrimitiveFunction -> Int
primitiveFunctionArity = arity . primitiveFunctionType
  where
    arity (FunctionType dom cod) = 1 + case cod of
      TypeFunction ft -> arity ft
      _ -> 0
