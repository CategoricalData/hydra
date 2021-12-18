module Hydra.Ext.Yaml.Coder (
  yamlCoder,
  yamlLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.CoreLanguage
import Hydra.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Ext.Yaml.Model as YM

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


atomicCoder :: LiteralType -> Qualified (Step Literal YM.Scalar)
atomicCoder at = pure $ case at of
  LiteralTypeBoolean -> Step {
    stepOut = \(LiteralBoolean b) -> pure $ YM.ScalarBool $ case b of
      BooleanValueFalse -> False
      BooleanValueTrue -> True,
    stepIn = \s -> case s of
      YM.ScalarBool b -> pure $ LiteralBoolean $ if b then BooleanValueTrue else BooleanValueFalse
      _ -> unexpected "boolean" s}
  LiteralTypeFloat _ -> Step {
    stepOut = \(LiteralFloat (FloatValueBigfloat f)) -> pure $ YM.ScalarFloat f,
    stepIn = \s -> case s of
      YM.ScalarFloat f -> pure $ LiteralFloat $ FloatValueBigfloat f
      _ -> unexpected "floating-point value" s}
  LiteralTypeInteger _ -> Step {
    stepOut = \(LiteralInteger (IntegerValueBigint i)) -> pure $ YM.ScalarInt i,
    stepIn = \s -> case s of
      YM.ScalarInt i -> pure $ LiteralInteger $ IntegerValueBigint i
      _ -> unexpected "integer" s}
  LiteralTypeString -> Step {
    stepOut = \(LiteralString s) -> pure $ YM.ScalarStr s,
    stepIn = \s -> case s of
      YM.ScalarStr s' -> pure $ LiteralString s'
      _ -> unexpected "string" s}

recordCoder :: (Default a, Eq a, Ord a, Read a, Show a) => [FieldType] -> Qualified (Step (Term a) YM.Node)
recordCoder sfields = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder (fieldTypeType f)) sfields
    return $ Step (encode coders) (decode coders)
  where
    encode coders term = case termData term of
      ExpressionRecord fields -> YM.NodeMapping . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field fn fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _ , Term (ExpressionOptional Nothing) _) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (yamlString fn) <*> stepOut coder fv)
      _ -> unexpected "record" term
    decode coders n = case n of
      YM.NodeMapping m -> record <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored
        where
          decodeField m (FieldType fn ft, coder) = do
            v <- stepIn coder $ Y.fromMaybe yamlNull $ M.lookup (yamlString fn) m
            return $ Field fn v
      _ -> unexpected "mapping" n
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: (Default a, Eq a, Ord a, Read a, Show a) => Type -> Qualified (Step (Term a) YM.Node)
termCoder typ = case typ of
  TypeLiteral at -> do
    ac <- atomicCoder at
    return Step {
      stepOut = \(Term (ExpressionLiteral av) _) -> YM.NodeScalar <$> stepOut ac av,
      stepIn = \n -> case n of
        YM.NodeScalar s -> atomic <$> stepIn ac s
        _ -> unexpected "scalar node" n}
  TypeList lt -> do
    lc <- termCoder lt
    return Step {
      stepOut = \(Term (ExpressionList els) _) -> YM.NodeSequence <$> CM.mapM (stepOut lc) els,
      stepIn = \n -> case n of
        YM.NodeSequence nodes -> list <$> CM.mapM (stepIn lc) nodes
        _ -> unexpected "sequence" n}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Step {
      stepOut = \(Term (ExpressionOptional el) _) -> Y.maybe (pure yamlNull) (stepOut oc) el,
      stepIn = \n -> case n of
        YM.NodeScalar YM.ScalarNull -> pure $ optional Nothing
        _ -> optional . Just <$> stepIn oc n}
  TypeMap (MapType kt vt) -> do
    kc <- termCoder kt
    vc <- termCoder vt
    let encodeEntry (k, v) = (,) <$> stepOut kc k <*> stepOut vc v
    let decodeEntry (k, v) = (,) <$> stepIn kc k <*> stepIn vc v
    return Step {
      stepOut = \(Term (ExpressionMap m) _) -> YM.NodeMapping . M.fromList <$> CM.mapM encodeEntry (M.toList m),
      stepIn = \n -> case n of
        YM.NodeMapping m -> defaultTerm . ExpressionMap . M.fromList <$> CM.mapM decodeEntry (M.toList m)
        _ -> unexpected "mapping" n}
  TypeRecord sfields -> recordCoder sfields

yamlCoder :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Type -> Qualified (Step (Term a) YM.Node)
yamlCoder context typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeSteps (adapterStep adapter) coder
  where
    adContext = AdapterContext context hydraCoreLanguage yamlLanguage

yamlLanguage :: Language
yamlLanguage = Language "hydra/ext/yaml" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantLiteral, TypeVariantList, TypeVariantMap, TypeVariantOptional, TypeVariantRecord],
  languageConstraintsTypes = \typ -> case typ of
    TypeOptional (TypeOptional _) -> False
    _ -> True }

yamlNull :: YM.Node
yamlNull = YM.NodeScalar YM.ScalarNull

yamlString :: String -> YM.Node
yamlString = YM.NodeScalar . YM.ScalarStr
