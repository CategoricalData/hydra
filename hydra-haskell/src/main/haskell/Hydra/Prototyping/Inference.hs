module Hydra.Prototyping.Inference (
    checkType,
  ) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreEncoding

import qualified Data.List as L
import qualified Data.Map  as M


-- Check whether a term conforms to a type
checkType :: Context -> Type -> Term -> Bool
checkType context typ term = check M.empty typ term
  where
    check :: M.Map Variable Type -> Type -> Term -> Bool
    check bindings typ term = case term of
      -- TODO: nominal types

      TermApplication (Application f arg) -> case f of
--        TermApplication ... ->
        TermAtomic _ -> False
        TermElement _ -> False
        TermFunction f -> case f of
--          FunctionCases ... ->
--          FunctionCompareTo other ->
          FunctionData -> check bindings (TypeElement typ) arg
--          FunctionLambda ... ->
          FunctionPrimitive fn -> case lookupPrimitiveFunction context fn of
            Nothing -> False
            Just prim -> cod == typ && check bindings dom arg
              where
                FunctionType dom cod = primitiveFunctionType prim
--          FunctionProjection fn ->
        TermList _ -> False
        TermMap _ -> False
        TermRecord _ -> False
        TermSet _ -> False
        TermUnion _ -> False
        TermVariable v -> case M.lookup v bindings of
          Nothing -> False
          Just t -> case t of
            TypeFunction (FunctionType dom cod) -> cod == typ && check bindings dom arg
            _ -> False
                  
      TermAtomic v -> case typ of
        TypeAtomic at -> at == atomicValueType v
        _ -> False

      TermElement en -> case typ of
        TypeElement et -> case M.lookup en (contextElements context) of
          Nothing -> False -- TODO: this is not a term/type failure, but a data integrity failure
          Just e -> encodeType et == elementSchema e
        _ -> False

      TermList list -> case typ of
        TypeList lt -> and $ fmap (check bindings lt) list
        _ -> False

      TermRecord fields -> case typ of
        TypeRecord tfields -> sameLength tfields fields
            && (L.foldl (&&) True $ L.zipWith matches tfields fields)
          where
            matches tf f = fieldTypeName tf == fieldName f
              && check bindings (fieldTypeType tf) (fieldTerm f)
        _ -> False

      TermUnion field -> case typ of
        TypeUnion fields -> case matchingField (fieldName field) fields of
          Nothing -> False
          Just f -> check bindings (fieldTypeType f) $ fieldTerm field
        _ -> False

      TermVariable v -> case M.lookup v bindings of
        Nothing -> False
        Just t -> t == typ

      TermFunction f -> case f of
        FunctionCases cases -> case typ of
          TypeFunction (FunctionType dom cod) -> case dom of
            TypeUnion fields -> and $ fmap checkCase cases
              where
                checkCase (Field fn term) = case matchingField fn fields of
                  Nothing -> False
                  Just (FieldType _ ft) -> check bindings (TypeFunction $ FunctionType ft cod) term
          _ -> False

        FunctionCompareTo other -> case typ of
          TypeFunction (FunctionType dom cod) -> dom == cod && check bindings dom other

        FunctionData -> case typ of
          TypeFunction (FunctionType dom _) -> case dom of
            TypeElement t -> t == dom
            _ -> False
          _ -> False

        FunctionLambda (Lambda v b) -> case typ of
          TypeFunction (FunctionType dom cod) -> check bindings' cod b
            where
              bindings' = M.insert v dom bindings
          _ -> False

        FunctionPrimitive fn -> case lookupPrimitiveFunction context fn of
            Nothing -> False
            Just prim -> typ == TypeFunction (primitiveFunctionType prim)

        FunctionProjection fn -> case typ of
          TypeFunction (FunctionType dom cod) -> case dom of
            TypeRecord tfields -> case matchingField fn tfields of
              Nothing -> False
              Just f -> cod == fieldTypeType f
            _ -> False
          _ -> False

    sameLength l1 l2 = L.length l1 == L.length l2

    matchingField :: FieldName -> [FieldType] -> Maybe FieldType
    matchingField fn fields = if L.null matches then Nothing else Just (L.head matches)
      where
        matches = L.filter (\f -> fieldTypeName f == fn) fields
