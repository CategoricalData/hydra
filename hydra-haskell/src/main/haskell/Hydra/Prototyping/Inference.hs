module Hydra.Prototyping.Inference (
    checkType,
  ) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreEncoding
import Hydra.Prototyping.CoreDecoding
import Hydra.Impl.Haskell.Dsl

import qualified Data.List as L
import qualified Data.Map  as M


-- Check whether a term conforms to a type
checkType :: (Default a, Eq a) => Context a -> Type -> Term a -> Bool
checkType context typ term = check M.empty typ term
  where
    check bindings typ term = case termData term of
      -- TODO: nominal types

      ExpressionApplication (Application f arg) -> case termData f of
--        ExpressionApplication ... ->
        ExpressionLiteral _ -> False
        ExpressionElement _ -> False
        ExpressionFunction f -> case f of
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
        ExpressionList _ -> False
        ExpressionMap _ -> False
        ExpressionRecord _ -> False
        ExpressionSet _ -> False
        ExpressionUnion _ -> False
        ExpressionVariable v -> case M.lookup v bindings of
          Nothing -> False
          Just t -> case t of
            TypeFunction (FunctionType dom cod) -> cod == typ && check bindings dom arg
            _ -> False

      ExpressionLiteral v -> case typ of
        TypeLiteral at -> at == literalType v
        _ -> False

      ExpressionElement en -> case typ of
        TypeElement et -> case M.lookup en (contextElements context) of
          Nothing -> False -- TODO: this is not a term/type failure, but a data integrity failure
          Just e -> encodeType et == elementSchema e
        _ -> False

      ExpressionList list -> case typ of
        TypeList lt -> and $ fmap (check bindings lt) list
        _ -> False

      ExpressionOptional m -> case typ of
        TypeOptional ot -> case m of
          Nothing -> True
          Just term -> check bindings ot term
        _ -> False

      ExpressionRecord fields -> case typ of
        TypeRecord tfields -> sameLength tfields fields
            && (L.foldl (&&) True $ L.zipWith matches tfields fields)
          where
            matches tf f = fieldTypeName tf == fieldName f
              && check bindings (fieldTypeType tf) (fieldTerm f)
        _ -> False

      ExpressionUnion field -> case typ of
        TypeUnion fields -> case matchingField (fieldName field) fields of
          Nothing -> False
          Just f -> check bindings (fieldTypeType f) $ fieldTerm field
        _ -> False

      ExpressionVariable v -> case M.lookup v bindings of
        Nothing -> False
        Just t -> t == typ

      ExpressionFunction f -> case f of
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

inferType :: Context a -> Term a -> Result (Term (a, Type))
inferType context (Term expr meta) = case expr of
--  ExpressionApplication (Application fun arg) ->
    ExpressionLiteral av -> pure $ Term (ExpressionLiteral av) (meta, stringType)
--    ExpressionElement name -> do
--      el <- requireElement context name
--      scon <- schemaContext context
--      withType <$> decodeType scon $ elementSchema el
--  ExpressionFunction fun -> case fun of
----      FunctionVariantCases
----      FunctionVariantCompareTo
----      FunctionVariantData
----      FunctionVariantLambda
--      FunctionPrimitive name -> TypeFunction <$> (primitiveFunctionType <$> requirePrimitiveFunction context name)
--      FunctionProjection fn ->
--  ExpressionList els ->
--  ExpressionMap m ->
--  ExpressionOptional m ->
--  ExpressionRecord fields ->
--  ExpressionUnion field ->
--  ExpressionVariable v ->
--  where
--    withType typ = Term expr (meta, typ)
--applicationType :: Type -> Application -> (Type, Type)
--applicationType typ (Application fun arg) =
