module Hydra.Ext.Haskell.Coder (
  dataGraphToHaskellModule,
  haskellCoder,
  haskellLanguage,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Prototyping.Rewriting
import Hydra.Prototyping.Steps
import Hydra.Util.Formatting
import Hydra.Prototyping.Primitives
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


dataGraphToHaskellModule :: (Default a, Ord a, Read a, Show a) => Context a -> Graph a -> Qualified H.Module
dataGraphToHaskellModule cx g = do
    scx <- resultToQualified $ schemaContext cx
    pairs <- resultToQualified $ CM.mapM (elementAsTypedTerm scx) els
    coders <- codersFor $ L.nub (typedTermType <$> pairs)
    resultToQualified $ createModule coders $ L.zip els pairs
  where
    els = graphElements g

    codersFor types = do
      cdrs <- CM.mapM (haskellCoder cx) types
      return $ M.fromList $ L.zip types cdrs

    createDeclaration coders (el, TypedTerm typ term) = do
      let coder = Y.fromJust $ M.lookup typ coders
      rhs <- stepOut coder term
      let pat = H.PatternApplication $ H.Pattern_Application (H.NameNormal $ H.QualifiedName [] $ localNameOf el) []
      return $ H.DeclarationValueBinding $ H.ValueBindingSimple $
        rewriteValueBinding $ H.ValueBinding_Simple pat rhs Nothing

    createModule coders pairs = do
      decls <- CM.mapM (createDeclaration coders) pairs
      return $ H.Module (Just $ H.ModuleHead moduleName []) [] decls

    localNameOf el = L.last $ LS.splitOn "." $ elementName el
    
    moduleName = L.intercalate "." $ capitalize <$> LS.splitOn "/" modulePart
      where
        modulePart = L.head $ LS.splitOn "." $ graphName g
        
    rewriteValueBinding vb = case vb of
      H.ValueBinding_Simple (H.PatternApplication (H.Pattern_Application name args)) rhs bindings -> case rhs of
        H.ExpressionLambda (H.Expression_Lambda vars body) -> rewriteValueBinding $
          H.ValueBinding_Simple
            (H.PatternApplication (H.Pattern_Application name (args ++ vars))) body bindings
        _ -> vb

encodeFunction :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> a -> Function a -> Result H.Expression
encodeFunction cx meta fun = case fun of
    FunctionCases fields -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
      where
        caseExpr = H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM toAlt fields)
        toAlt (Field fn fun') = do
          let rhsTerm = simplifyTerm $ apply fun' (variable "v")
          let v = case termData rhsTerm of
                ExpressionFunction (FunctionLambda (Lambda v' _)) -> v'
                _ -> "_"
          let hn = Y.maybe fn (`qualifyUnionFieldName` fn) domName
          let lhs = H.PatternApplication $ H.Pattern_Application (hsname hn) [H.PatternName $ hsname v]
          rhs <- encodeTerm cx rhsTerm
          return $ H.Alternative lhs rhs Nothing
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

encodeLiteral :: Literal -> Result H.Expression
encodeLiteral av = case av of
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

encodeTerm :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Term a -> Result H.Expression
encodeTerm cx term@(Term expr meta) = case expr of
    ExpressionApplication (Application fun arg) -> case termData fun of
       ExpressionFunction FunctionData -> encodeTerm cx arg
       _ -> hsapp <$> encodeTerm cx fun <*> encodeTerm cx arg
    ExpressionLiteral av -> encodeLiteral av
    ExpressionElement name -> pure $ hsvar $ localNameOf name
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
    ExpressionUnion (Field fn ft) -> do
      let lhs = hsvar $ Y.maybe fn (`qualifyUnionFieldName` fn) sname
      case termData ft of
        ExpressionRecord [] -> pure lhs
        _ -> hsapp lhs <$> encodeTerm cx ft
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
