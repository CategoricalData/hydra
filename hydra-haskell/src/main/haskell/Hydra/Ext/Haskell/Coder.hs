module Hydra.Ext.Haskell.Coder (
  haskellCoder,
  haskellLanguage,
) where

import Hydra.V1.Core
import Hydra.V1.Evaluation
import Hydra.V1.Adapter
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Control.Monad as CM
import qualified Data.Set as S


encodeAtomic :: AtomicValue -> Result H.Expression
encodeAtomic av = case av of
    AtomicValueBoolean b -> pure $ hsvar $ case b of
      BooleanValueTrue -> "True"
      _ -> "False"
    AtomicValueFloat fv -> case fv of
      FloatValueFloat32 f -> pure $ hslit $ H.LiteralFloat f
      FloatValueFloat64 f -> pure $ hslit $ H.LiteralDouble f
      _ -> unexpected "floating-point number" fv
    AtomicValueInteger iv -> case iv of
      IntegerValueBigint i -> pure $ hslit $ H.LiteralInteger i
      IntegerValueInt32 i -> pure $ hslit $ H.LiteralInt i
      _ -> unexpected "integer" iv
    AtomicValueString s -> pure $ hslit $ H.LiteralString s
    _ -> unexpected "atomic value" av
    
encodeFunction :: Function -> Result H.Expression
encodeFunction fun = case fun of
  FunctionCases fields -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
    where
      caseExpr :: Result H.Expression
      caseExpr = H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM toAlt fields)
      toAlt (Field fn fun) = do
        let var = "y"
        let pat = H.PatternApplication $ H.Pattern_Application (hsname fn) [H.PatternName $ hsname var]
        rhs <- hsapp <$> encodeTerm fun <*> pure (hsvar var)
        return $ H.Alternative pat rhs Nothing
  FunctionData -> pure $ hsvar "id"
  FunctionLambda (Lambda v body) -> hslambda v <$> encodeTerm body
  FunctionPrimitive name -> pure $ hsvar name
  FunctionProjection fname -> pure $ hsvar fname
  _ -> fail $ "unexpected function: " ++ show fun

encodeTerm :: Term -> Result H.Expression
encodeTerm term = case term of
  TermApplication (Application fun arg) -> hsapp <$> encodeTerm fun <*> encodeTerm arg
  TermAtomic av -> encodeAtomic av
  TermElement name -> pure $ hsvar name
  TermFunction f -> encodeFunction f
  TermList els -> H.ExpressionList <$> CM.mapM encodeTerm els
  TermOptional m -> case m of
    Nothing -> pure $ hsvar "Nothing"
    Just t -> hsapp (hsvar "Just") <$> encodeTerm t
  TermRecord fields -> case fields of
    [] -> pure $ H.ExpressionTuple []
    _ -> do
        let typeName = "Placeholder"
        updates <- CM.mapM toFieldUpdate fields
        return $ H.ExpressionConstructRecord $ H.Expression_ConstructRecord (hsname typeName) updates
      where
        toFieldUpdate (Field fn ft) = H.FieldUpdate (hsname fn) <$> encodeTerm ft
  TermUnion (Field fn ft) -> hsapp (hsvar fn) <$> encodeTerm ft
  TermVariable v -> pure $ hsvar v
  _ -> fail $ "unexpected term: " ++ show term

haskellCoder :: Context -> Type -> Qualified (Step Term H.Expression)
haskellCoder context typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeSteps (adapterStep adapter) coder
  where
    adContext = AdapterContext context hydraCoreLanguage haskellLanguage
    termCoder _ = pure $ unidirectionalStep encodeTerm

haskellLanguage :: Language
haskellLanguage = Language "hydra/ext/haskell" $ Language_Constraints {
  languageConstraintsAtomicVariants = S.fromList [
    AtomicVariantBoolean, AtomicVariantFloat, AtomicVariantInteger, AtomicVariantString],
  languageConstraintsFloatVariants = S.fromList [
    -- Bigfloat is excluded for now
    FloatVariantFloat32,
    FloatVariantFloat64],
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerVariants = S.fromList [IntegerVariantBigint, IntegerVariantInt32],
  languageConstraintsTermVariants = S.fromList [
    -- No native maps or sets
    TermVariantApplication,
    TermVariantAtomic,
    TermVariantElement,
    TermVariantFunction,
    TermVariantList,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantUnion,
    TermVariantVariable],
  languageConstraintsTypeVariants = S.fromList [
    -- No native maps or sets
    TypeVariantAtomic,
    TypeVariantElement,
    TypeVariantFunction,
    TypeVariantList,
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnion],
  languageConstraintsTypes = const True }

hsapp :: H.Expression -> H.Expression -> H.Expression
hsapp l r = H.ExpressionApplication $ H.Expression_Application l r

hslambda :: H.NamePart -> H.Expression -> H.Expression
hslambda v rhs = H.ExpressionLambda (H.Expression_Lambda [H.PatternName $ hsname v] rhs)

hslit :: H.Literal -> H.Expression
hslit = H.ExpressionLiteral

hsname :: H.NamePart -> H.Name
hsname s = H.NameNormal $ H.QualifiedName [] s

hsvar :: H.NamePart -> H.Expression
hsvar = H.ExpressionVariable . hsname

unexpected :: (MonadFail m, Show a1) => [Char] -> a1 -> m a2
unexpected cat obj = fail $ "unexpected " ++ cat ++ ": " ++ show obj
