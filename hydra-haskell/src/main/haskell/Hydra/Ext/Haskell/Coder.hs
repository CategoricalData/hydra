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
import Hydra.Prototyping.CoreLanguage 
import Hydra.Basics
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
      let hname = simpleName $ localNameOf $ elementName el
      let pat = H.PatternApplication $ H.Pattern_Application hname []
      htype <- encodeType typ
      let decl = H.DeclarationTypedBinding $ H.TypedBinding
                  (H.TypeSignature hname htype)
                  (H.ValueBindingSimple $ rewriteValueBinding $ H.ValueBinding_Simple pat rhs Nothing)
      let comments = contextDescriptionOf cx $ termMeta term
      return $ H.DeclarationWithComments decl comments

    createModule coders pairs = do
      decls <- CM.mapM (createDeclaration coders) pairs
      return $ H.Module (Just $ H.ModuleHead (fst $ importName $ graphName g) []) imports decls

    imports = toImport <$> S.toList (dataGraphDependencies g)

    toImport name = H.Import False mname (Just alias) Nothing
      where
        (mname, alias) = importName name
        
    importName name = (L.intercalate "." parts, L.last parts)
      where
        parts = capitalize <$> LS.splitOn "/" name

    rewriteValueBinding vb = case vb of
      H.ValueBinding_Simple (H.PatternApplication (H.Pattern_Application name args)) rhs bindings -> case rhs of
        H.ExpressionLambda (H.Expression_Lambda vars body) -> rewriteValueBinding $
          H.ValueBinding_Simple
            (H.PatternApplication (H.Pattern_Application name (args ++ vars))) body bindings
        _ -> vb

dataGraphDependencies :: Show a => Graph a -> S.Set GraphName
dataGraphDependencies g = S.delete (graphName g) allDeps
  where
    allDeps = L.foldl (\s t -> S.union s $ depsOf t) S.empty $
      (elementData <$> graphElements g) ++ (elementSchema <$> graphElements g)
    depsOf term = foldOverTerm TraversalOrderPre addNames S.empty term
    addNames names term = case termData term of
      ExpressionElement name -> S.insert (graphNameOf name) names
      ExpressionFunction (FunctionPrimitive name) -> S.insert (graphNameOf name) names
      ExpressionNominal (NominalTerm name _) -> S.insert name names
      _ -> names
    graphNameOf = L.head . LS.splitOn "."

encodeFunction :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> a -> Function a -> Result H.Expression
encodeFunction cx meta fun = case fun of
    FunctionCases fields -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
      where
        caseExpr = do
          fieldMap <- fieldMapOf <$> findDomain
          H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM (toAlt fieldMap) fields)
        toAlt fieldMap (Field fn fun') = do
          let v0 = "v"
          let rhsTerm = simplifyTerm $ apply fun' (variable v0)
          let v1 = if S.member v0 $ freeVariablesInTerm rhsTerm then v0 else "_"
          let hn = Y.maybe fn (`qualifyUnionFieldName` fn) domName
          args <- case fieldMap >>= M.lookup fn of
                Just (FieldType _ (TypeRecord [])) -> pure []
                Just _ -> pure [H.PatternName $ hsname v1]
                Nothing -> fail $ "field " ++ show fn ++ " not found in " ++ show domName
--                  ++ ": " ++ show fieldMap
--                  ++ ". metadata: " ++ show meta
--                  ++ ". function: " ++ show fun
          let lhs = H.PatternApplication $ H.Pattern_Application (hsname hn) args
          rhs <- encodeTerm cx rhsTerm
          return $ H.Alternative lhs rhs Nothing
    FunctionData -> pure $ hsvar "id"
    FunctionLambda (Lambda v body) -> hslambda v <$> encodeTerm cx body
    FunctionOptionalCases (OptionalCases nothing just) -> do
      nothingRhs <- encodeTerm cx nothing
      let nothingAlt = H.Alternative (H.PatternName $ simpleName "Nothing") nothingRhs Nothing
      justAlt <- do
        -- Note: some of the following could be brought together with FunctionCases
        let v0 = "v"
        let rhsTerm = simplifyTerm $ apply just (variable v0)
        let v1 = if S.member v0 $ freeVariablesInTerm rhsTerm then v0 else "_"
        let lhs = H.PatternApplication $ H.Pattern_Application (hsname "Just") [H.PatternName $ hsname v1]
        rhs <- encodeTerm cx rhsTerm
        return $ H.Alternative lhs rhs Nothing
      return $ H.ExpressionCase $ H.Expression_Case (hsvar "x") [nothingAlt, justAlt]
    FunctionPrimitive name -> pure $ H.ExpressionVariable $ hsPrimitiveReference name
    FunctionProjection fname -> pure $ hsvar $ case domName of
      Just rname -> qualifyRecordFieldName rname fname
      Nothing -> fname
    _ -> fail $ "unexpected function: " ++ show fun
  where
    
    fieldMapOf typ = case typ of
      Just (TypeUnion tfields) -> Just $ M.fromList $ (\f -> (fieldTypeName f, f)) <$> tfields
      _ -> Nothing
    findDomain = case domName of
      Nothing -> pure Nothing
      Just name -> do
        scx <- schemaContext cx -- TODO: cache this
        typ <- requireType scx name
        return $ Just typ
    domName = case contextTypeOf cx meta of
        Just typ -> case typ of
          TypeFunction (FunctionType dom _) -> nomName dom
          _ -> Nothing
        Nothing -> Nothing
      where
        nomName typ = case typ of
          TypeNominal name -> Just name
          TypeUniversal (UniversalType _ body) -> nomName body
          _ -> Nothing

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
encodeTerm cx term@(Term expr meta) = do
   case expr of
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
      Nothing -> case fields of
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

encodeType :: Type -> Result H.Type
encodeType typ = case typ of
  TypeElement et -> encodeType et
  TypeFunction (FunctionType dom cod) -> H.TypeFunction <$> (H.Type_Function <$> encodeType dom <*> encodeType cod)
  TypeList lt -> H.TypeList <$> encodeType lt
  TypeLiteral lt -> H.TypeVariable . simpleName <$> case lt of
--    LiteralTypeBinary ->
    LiteralTypeBoolean -> pure "Bool"
    LiteralTypeFloat ft -> case ft of
--      FloatTypeBigfloat ->
      FloatTypeFloat32 -> pure "Float"
      FloatTypeFloat64 -> pure "Double"
      _ -> fail $ "unexpected floating-point type: " ++ show ft
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure "Integer"
--      IntegerTypeInt8 -> ""
--      IntegerTypeInt16 -> ""
      IntegerTypeInt32 -> pure "Int"
--      IntegerTypeInt64 -> ""
--      IntegerTypeUint8 -> ""
--      IntegerTypeUint16 -> ""
--      IntegerTypeUint32 -> ""
--      IntegerTypeUint64 -> ""
      _ -> fail $ "unexpected integer type: " ++ show it
    LiteralTypeString -> pure "String"
    _ -> fail $ "unexpected literal type: " ++ show lt
  TypeMap (MapType kt vt) -> toApplicationType <$> CM.sequence [
    pure $ H.TypeVariable $ simpleName "Map",
    encodeType kt,
    encodeType vt]
  TypeNominal name -> pure $ H.TypeVariable $ simpleName $ localNameOf name
  TypeOptional ot -> toApplicationType <$> CM.sequence [
    pure $ H.TypeVariable $ simpleName "Maybe",
    encodeType ot]
--  TypeRecord fields ->
  TypeSet st -> toApplicationType <$> CM.sequence [
    pure $ H.TypeVariable $ simpleName "Set",
    encodeType st]
--  TypeUnion fields ->
  TypeUniversal (UniversalType v body) -> toApplicationType <$> CM.sequence [
    pure $ H.TypeVariable $ simpleName v,
    encodeType body]
  TypeVariable v -> pure $ H.TypeVariable $ simpleName v
  _ -> fail $ "unexpected type: " ++ show typ

toApplicationType :: [H.Type] -> H.Type
toApplicationType types = app types
  where
    app l = case l of
      [e] -> e
      (h:r) -> H.TypeApplication $ H.Type_Application (app r) h

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
  languageConstraintsFloatTypes = S.fromList [
    -- Bigfloat is excluded for now
    FloatTypeFloat32,
    FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint, IntegerTypeInt32],
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
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = const True }

hsapp :: H.Expression -> H.Expression -> H.Expression
hsapp l r = H.ExpressionApplication $ H.Expression_Application l r

hslambda :: H.NamePart -> H.Expression -> H.Expression
hslambda v rhs = H.ExpressionLambda (H.Expression_Lambda [H.PatternName $ hsname v] rhs)

hslit :: H.Literal -> H.Expression
hslit = H.ExpressionLiteral

hsname :: H.NamePart -> H.Name
hsname s = H.NameNormal $ H.QualifiedName [] s

hsPrimitiveReference :: Name -> H.Name
hsPrimitiveReference name = H.NameNormal $ H.QualifiedName [prefix] local
  where
    (ns, local) = toQname name
    prefix = capitalize $ L.last $ LS.splitOn "/" ns

hsvar :: H.NamePart -> H.Expression
hsvar = H.ExpressionVariable . hsname

qualifyRecordFieldName :: Name -> FieldName -> String
qualifyRecordFieldName sname fname = decapitalize (typeNameForRecord sname) ++ capitalize fname

qualifyUnionFieldName :: Name -> FieldName -> String
qualifyUnionFieldName sname fname = capitalize (typeNameForRecord sname) ++ capitalize fname

simpleName :: String -> H.Name
simpleName n = H.NameNormal $ H.QualifiedName [] n

typeNameForRecord :: Name -> String
typeNameForRecord sname = L.last (LS.splitOn "." sname)

unexpected :: (MonadFail m, Show a1) => [Char] -> a1 -> m a2
unexpected cat obj = fail $ "unexpected " ++ cat ++ ": " ++ show obj
