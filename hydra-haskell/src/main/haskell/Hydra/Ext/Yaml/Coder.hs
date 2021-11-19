module Hydra.Ext.Yaml.Coder (
  yamlCoder,
  yamlLanguage,
) where

import Hydra.V2.Core
import Hydra.V2.Evaluation
import Hydra.V2.Adapter
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import qualified Hydra.Ext.Yaml.Model as YM

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


atomicCoder :: AtomicType -> Qualified (Step AtomicValue YM.Scalar)
atomicCoder at = pure $ case at of
  AtomicTypeBoolean -> Step {
    stepOut = \(AtomicValueBoolean b) -> pure $ YM.ScalarBool $ case b of
      BooleanValueFalse -> False
      BooleanValueTrue -> True,
    stepIn = \s -> case s of
      YM.ScalarBool b -> pure $ AtomicValueBoolean $ if b then BooleanValueTrue else BooleanValueFalse
      _ -> unexpected s "boolean"}
  AtomicTypeFloat _ -> Step {
    stepOut = \(AtomicValueFloat (FloatValueBigfloat f)) -> pure $ YM.ScalarFloat f,
    stepIn = \s -> case s of
      YM.ScalarFloat f -> pure $ AtomicValueFloat $ FloatValueBigfloat f
      _ -> unexpected s "floating-point value"}
  AtomicTypeInteger _ -> Step {
    stepOut = \(AtomicValueInteger (IntegerValueBigint i)) -> pure $ YM.ScalarInt i,
    stepIn = \s -> case s of
      YM.ScalarInt i -> pure $ AtomicValueInteger $ IntegerValueBigint i
      _ -> unexpected s "integer"}
  AtomicTypeString -> Step {
    stepOut = \(AtomicValueString s) -> pure $ YM.ScalarStr s,
    stepIn = \s -> case s of
      YM.ScalarStr s' -> pure $ AtomicValueString s'
      _ -> unexpected s "string"}

recordCoder :: [FieldType] -> Qualified (Step Term YM.Node)
recordCoder sfields = do
    coders <- CM.mapM (\f -> (,) <$> pure f <*> termCoder (fieldTypeType f)) sfields
    return $ Step (encode coders) (decode coders)
  where
    encode coders term = case term of
      ExpressionRecord fields -> YM.NodeMapping . M.fromList . Y.catMaybes <$> CM.zipWithM encodeField coders fields
        where
          encodeField (ft, coder) (Field fn fv) = case (fieldTypeType ft, fv) of
            (TypeOptional _ , ExpressionOptional Nothing) -> pure Nothing
            _ -> Just <$> ((,) <$> pure (yamlString fn) <*> stepOut coder fv)
      _ -> unexpected term "record"
    decode coders n = case n of
      YM.NodeMapping m -> ExpressionRecord <$> CM.mapM (decodeField m) coders -- Note: unknown fields are ignored 
        where
          decodeField m (FieldType fn ft, coder) = do
            v <- stepIn coder $ Y.fromMaybe yamlNull $ M.lookup (yamlString fn) m        
            return $ Field fn v
      _ -> unexpected n "mapping"
    getCoder coders fname = Y.maybe error pure $ M.lookup fname coders
      where
        error = fail $ "no such field: " ++ fname

termCoder :: Type -> Qualified (Step Term YM.Node)
termCoder typ = case typ of
  TypeAtomic at -> do
    ac <- atomicCoder at
    return Step {
      stepOut = \(ExpressionAtomic av) -> YM.NodeScalar <$> stepOut ac av,
      stepIn = \n -> case n of
        YM.NodeScalar s -> ExpressionAtomic <$> stepIn ac s
        _ -> unexpected n "scalar node"}
  TypeList lt -> do
    lc <- termCoder lt
    return Step {
      stepOut = \(ExpressionList els) -> YM.NodeSequence <$> CM.mapM (stepOut lc) els,
      stepIn = \n -> case n of
        YM.NodeSequence nodes -> ExpressionList <$> CM.mapM (stepIn lc) nodes
        _ -> unexpected n "sequence"}
  TypeOptional ot -> do
    oc <- termCoder ot
    return Step {
      stepOut = \(ExpressionOptional el) -> Y.maybe (pure yamlNull) (stepOut oc) el,
      stepIn = \n -> case n of
        YM.NodeScalar YM.ScalarNull -> pure $ ExpressionOptional Nothing
        _ -> ExpressionOptional . Just <$> stepIn oc n}
  TypeMap (MapType kt vt) -> do
    kc <- termCoder kt
    vc <- termCoder vt
    let encodeEntry (k, v) = (,) <$> stepOut kc k <*> stepOut vc v
    let decodeEntry (k, v) = (,) <$> stepIn kc k <*> stepIn vc v
    return Step {
      stepOut = \(ExpressionMap m) -> YM.NodeMapping . M.fromList <$> CM.mapM encodeEntry (M.toList m),
      stepIn = \n -> case n of
        YM.NodeMapping m -> ExpressionMap . M.fromList <$> CM.mapM decodeEntry (M.toList m)
        _ -> unexpected n "mapping"}
  TypeRecord sfields -> recordCoder sfields

unexpected :: Show v => v -> String -> Result a
unexpected x desc = fail $ "expected " ++ desc ++ ", found: " ++ show x

yamlCoder :: Context -> Type -> Qualified (Step Term YM.Node)
yamlCoder context typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeSteps (adapterStep adapter) coder
  where
    adContext = AdapterContext context hydraCoreLanguage yamlLanguage

yamlLanguage :: Language
yamlLanguage = Language "hydra/ext/yaml" $ Language_Constraints {
  languageConstraintsAtomicVariants = S.fromList [
    AtomicVariantBoolean, AtomicVariantFloat, AtomicVariantInteger, AtomicVariantString],
  languageConstraintsFloatVariants = S.fromList [FloatVariantBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerVariants = S.fromList [IntegerVariantBigint],
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAtomic, TypeVariantList, TypeVariantMap, TypeVariantOptional, TypeVariantRecord],
  languageConstraintsTypes = \typ -> case typ of
    TypeOptional (TypeOptional _) -> False
    _ -> True }

yamlNull :: YM.Node
yamlNull = YM.NodeScalar YM.ScalarNull

yamlString :: String -> YM.Node
yamlString = YM.NodeScalar . YM.ScalarStr
