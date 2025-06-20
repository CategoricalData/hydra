-- | Functions for reducing terms and types, i.e. performing computations

module Hydra.Staging.Reduction (
  module Hydra.Reduction,
  module Hydra.Staging.Reduction
) where

import Hydra.Arity
import Hydra.Strip
import Hydra.Compute
import Hydra.Core
import Hydra.Schemas
import Hydra.Graph
import Hydra.Annotations
import Hydra.Lexical
import Hydra.Reduction
import Hydra.Rewriting
import Hydra.Rewriting
import Hydra.Lexical
import Hydra.Lib.Io
import Hydra.Flows
import qualified Hydra.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


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
      EliminationRecord proj -> do
        fields <- Expect.record (projectionTypeName proj) $ stripTerm reducedArg
        let matchingFields = L.filter (\f -> fieldName f == projectionField proj) fields
        if L.null matchingFields
          then fail $ "no such field: " ++ unName (projectionField proj) ++ " in " ++ unName (projectionTypeName proj) ++ " record"
          else pure $ fieldTerm $ L.head matchingFields
      EliminationUnion (CaseStatement name def fields) -> do
        field <- Expect.injection name reducedArg
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
          TypeForall (ForallType v body) -> betaReduceType $ replaceFreeName v rhs body
          -- nominal types are transparent
          TypeVariable name -> do
            t' <- requireType name
            betaReduceType $ TypeApplication $ ApplicationType t' rhs
