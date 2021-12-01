module Hydra.Ext.Haskell.Coder (
  haskellCoder,
  haskellLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import Hydra.Util.Formatting
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Set as S
import qualified Data.Maybe as Y 


encodeAtomic :: Literal -> Result H.Expression
encodeAtomic av = case av of
    LiteralBoolean b -> pure $ hsvar $ case b of
      BooleanValueTrue -> "True"
      _ -> "False"
    LiteralFloat fv -> case fv of
      FloatValueFloat32 f -> pure $ hslit $ H.LiteralFloat f
      FloatValueFloat64 f -> pure $ hslit $ H.LiteralDouble f
      _ -> unexpected "floating-point number" fv
    LiteralInteger iv -> case iv of
      IntegerValueBigint i -> pure $ hslit $ H.LiteralInteger i
      IntegerValueInt32 i -> pure $ hslit $ H.LiteralInt i
      _ -> unexpected "integer" iv
    LiteralString s -> pure $ hslit $ H.LiteralString s
    _ -> unexpected "atomic value" av

encodeFunction :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> a -> Function a -> Result H.Expression
encodeFunction cx meta fun = case fun of
    FunctionCases fields -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
      where
        caseExpr :: Result H.Expression
        caseExpr = H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM toAlt fields)
        toAlt (Field fn fun) = do
          let var = "y"
          let pat = H.PatternApplication $ H.Pattern_Application (hsname fn) [H.PatternName $ hsname var]
          rhs <- hsapp <$> encodeTerm cx fun <*> pure (hsvar var)
          return $ H.Alternative pat rhs Nothing
    FunctionData -> pure $ hsvar "id"
    FunctionLambda (Lambda v body) -> hslambda v <$> encodeTerm cx body
    FunctionPrimitive name -> pure $ hsvar name
    FunctionProjection fname -> pure $ hsvar $ case domName of
      Just rname -> qualifyRecordFieldName fname rname
      Nothing -> fname
    _ -> fail $ "unexpected function: " ++ show fun
  where
    domName = case contextTypeOf cx meta of
      Just (TypeFunction (FunctionType (TypeNominal name) _)) -> Just name
      Nothing -> Nothing

encodeTerm :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Term a -> Result H.Expression
encodeTerm cx term@(Term expr meta) = case expr of
    ExpressionApplication (Application fun arg) -> hsapp <$> encodeTerm cx fun <*> encodeTerm cx arg
    ExpressionLiteral av -> encodeAtomic av
    ExpressionElement name -> pure $ hsvar name
    ExpressionFunction f -> encodeFunction cx (termMeta term) f
    ExpressionList els -> H.ExpressionList <$> CM.mapM (encodeTerm cx) els
    ExpressionNominal (NominalTerm _ term') -> encodeTerm cx term'
    ExpressionOptional m -> case m of
      Nothing -> pure $ hsvar "Nothing"
      Just t -> hsapp (hsvar "Just") <$> encodeTerm cx t
    ExpressionRecord fields -> case sname of
      Nothing -> 
        case fields of
          [] -> pure $ H.ExpressionTuple []
          _ -> fail $ "unexpected anonymous record: " ++ show term
      Just name -> do
          let typeName = typeNameForRecord name
          updates <- CM.mapM toFieldUpdate fields
          return $ H.ExpressionConstructRecord $ H.Expression_ConstructRecord (hsname typeName) updates
        where
          toFieldUpdate (Field fn ft) = H.FieldUpdate (hsname $ qualifyRecordFieldName name fn) <$> encodeTerm cx ft
    ExpressionUnion (Field fn ft) -> hsapp (hsvar $ Y.maybe fn (`qualifyUnionFieldName` fn) sname) <$> encodeTerm cx ft
    ExpressionVariable v -> pure $ hsvar v
    _ -> fail $ "unexpected term: " ++ show term
  where
    sname = case contextTypeOf cx meta of
      Just (TypeNominal name) -> Just name
      Nothing -> Nothing

haskellCoder :: (Default a, Ord a, Read a, Show a) => Context a -> Type -> Qualified (Step (Term a) H.Expression)
haskellCoder cx typ = do
    adapter <- termAdapter adContext typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeSteps (adapterStep adapter) coder
  where
    adContext = AdapterContext cx hydraCoreLanguage haskellLanguage
    termCoder _ = pure $ unidirectionalStep (encodeTerm cx)

haskellLanguage :: Language
haskellLanguage = Language "hydra/ext/haskell" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatVariants = S.fromList [
    -- Bigfloat is excluded for now
    FloatVariantFloat32,
    FloatVariantFloat64],
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerVariants = S.fromList [IntegerVariantBigint, IntegerVariantInt32],
  languageConstraintsTermVariants = S.fromList [
    -- No native maps or sets
    TermVariantApplication,
    TermVariantLiteral,
    TermVariantElement,
    TermVariantFunction,
    TermVariantList,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantUnion,
    TermVariantVariable],
  languageConstraintsTypeVariants = S.fromList [
    -- No native maps or sets
    TypeVariantLiteral,
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

qualifyRecordFieldName :: Name -> FieldName -> String
qualifyRecordFieldName sname fname = decapitalize (typeNameForRecord sname) ++ capitalize fname

qualifyUnionFieldName :: Name -> FieldName -> String
qualifyUnionFieldName sname fname = capitalize (typeNameForRecord sname) ++ capitalize fname

typeNameForRecord :: Name -> String
typeNameForRecord sname = L.last (LS.splitOn "." sname)

unexpected :: (MonadFail m, Show a1) => [Char] -> a1 -> m a2
unexpected cat obj = fail $ "unexpected " ++ cat ++ ": " ++ show obj
