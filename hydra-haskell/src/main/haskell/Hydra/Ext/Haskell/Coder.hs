module Hydra.Ext.Haskell.Coder (
  dataGraphToHaskellModule,
  haskellLanguage,
) where

import Hydra.Adapter
import Hydra.Adapters.Term
import Hydra.Basics
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreLanguage
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras
import Hydra.Primitives
import Hydra.Rewriting
import Hydra.Util.Coders
import Hydra.Util.Formatting
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


constructModule :: (Default m, Ord m, Read m, Show m)
  => Context m -> Graph m -> M.Map Type (Step (Term m) H.Expression) -> [(Element m, TypedTerm m)] -> Result H.Module
constructModule cx g coders pairs = do
    decls <- CM.mapM createDeclaration pairs
    return $ H.Module (Just $ H.ModuleHead (fst $ importName $ graphName g) []) imports decls
  where
    createDeclaration pair@(el, TypedTerm typ term) = if typ == TypeNominal _Type
      then createTypeDeclaration pair
      else createOtherDeclaration pair

    createTypeDeclaration (el, TypedTerm typ term) = do
        let lname = localNameOf $ elementName el
        let hname = simpleName lname
        t <- decodeType cx term
        let hd = H.DeclarationHeadSimple hname
        let deriv = simpleName <$> ["Eq", "Ord", "Read", "Show"]
        decl <- case t of
          TypeRecord fields -> do
            cons <- recordCons lname fields
            return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordData [] hd [cons] [deriv])
          TypeUnion fields -> do
            cons <- CM.mapM (unionCons lname) fields
            return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordData [] hd cons [deriv])
          _ -> do
            htype <- encodeAdaptedType cx t
            return $ H.DeclarationType (H.TypeDeclaration hd htype)
        let comments = contextDescriptionOf cx $ termMeta term
        return $ H.DeclarationWithComments decl comments
      where
        recordCons lname fields = do
            hFields <- CM.mapM toField fields
            return $ H.ConstructorRecord $ H.Constructor_Record (simpleName lname) hFields
          where
            toField (FieldType fname ftype) = do
              let hname = simpleName $ decapitalize lname ++ capitalize fname
              htype <- encodeAdaptedType cx ftype
              return $ H.Field hname htype

        unionCons lname (FieldType fname ftype) = do
          let nm = capitalize lname ++ capitalize fname
          typeList <- if ftype == unitType
            then pure []
            else do
              htype <- encodeAdaptedType cx ftype
              return [htype]
          return $ H.ConstructorOrdinary $ H.Constructor_Ordinary (simpleName nm) typeList

    createOtherDeclaration (el, TypedTerm typ term) = do
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

    toCons _ = H.Constructor_Record

    imports = toImport <$> S.toList (dataGraphDependencies True True True g)

    toImport name = H.Import False mname (Just alias) Nothing
      where
        (mname, alias) = importName name

    importName name = (L.intercalate "." parts, L.last parts)
      where
        parts = capitalize <$> Strings.splitOn "/" name

    rewriteValueBinding vb = case vb of
      H.ValueBinding_Simple (H.PatternApplication (H.Pattern_Application name args)) rhs bindings -> case rhs of
        H.ExpressionLambda (H.Expression_Lambda vars body) -> rewriteValueBinding $
          H.ValueBinding_Simple
            (H.PatternApplication (H.Pattern_Application name (args ++ vars))) body bindings
        _ -> vb

dataGraphToHaskellModule :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified H.Module
dataGraphToHaskellModule = dataGraphToExternalModule haskellLanguage encodeTerm constructModule

encodeFunction :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> m -> Function m -> Result H.Expression
encodeFunction cx meta fun = case fun of
    FunctionCases fields -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
      where
        caseExpr = do
          fieldMap <- fieldMapOf <$> findDomain
          H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM (toAlt fieldMap) fields)
        toAlt fieldMap (Field fn fun') = do
          let v0 = "v"
          let rhsTerm = simplifyTerm $ apply fun' (variable v0)
          let v1 = if isFreeIn v0 rhsTerm then "_" else v0
          let hn = Y.maybe fn (`qualifyUnionFieldName` fn) domName
          args <- case fieldMap >>= M.lookup fn of
                Just (FieldType _ (TypeRecord [])) -> pure []
                Just _ -> pure [H.PatternName $ hsname v1]
                Nothing -> fail $ "field " ++ show fn ++ " not found in " ++ show domName
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
    _ -> unexpected "literal value" av

encodeTerm :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Term m -> Result H.Expression
encodeTerm cx term@(Term expr meta) = do
   case expr of
    ExpressionApplication (Application fun arg) -> case termData fun of
       ExpressionFunction FunctionData -> encodeTerm cx arg
       _ -> hsapp <$> encodeTerm cx fun <*> encodeTerm cx arg
    ExpressionElement name -> pure $ hsvar $ localNameOf name
    ExpressionFunction f -> encodeFunction cx (termMeta term) f
    ExpressionList els -> H.ExpressionList <$> CM.mapM (encodeTerm cx) els
    ExpressionLiteral v -> encodeLiteral v
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
    LiteralTypeBoolean -> pure "Bool"
    LiteralTypeFloat ft -> case ft of
      FloatTypeFloat32 -> pure "Float"
      FloatTypeFloat64 -> pure "Double"
      _ -> fail $ "unexpected floating-point type: " ++ show ft
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure "Integer"
      IntegerTypeInt32 -> pure "Int"
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
  TypeSet st -> toApplicationType <$> CM.sequence [
    pure $ H.TypeVariable $ simpleName "Set",
    encodeType st]
  TypeUniversal (UniversalType v body) -> toApplicationType <$> CM.sequence [
    encodeType body,
    pure $ H.TypeVariable $ simpleName v]
  TypeVariable v -> pure $ H.TypeVariable $ simpleName v
  TypeRecord [] -> pure $ H.TypeTuple []
  _ -> fail $ "unexpected type: " ++ show typ

encodeAdaptedType :: (Default m, Ord m, Read m, Show m) => Context m -> Type -> Result H.Type
encodeAdaptedType cx typ = do
  let ac = AdapterContext cx hydraCoreLanguage haskellLanguage
  ad <- qualifiedToResult $ termAdapter ac typ
  encodeType $ adapterTarget ad

toApplicationType :: [H.Type] -> H.Type
toApplicationType = app . L.reverse
  where
    app l = case l of
      [e] -> e
      (h:r) -> H.TypeApplication $ H.Type_Application (app r) h

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
    TermVariantApplication,
    TermVariantElement,
    TermVariantFunction,
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantNominal,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantSet,
    TermVariantUnion,
    TermVariantVariable],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantElement,
    TypeVariantFunction,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantSet,
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
    prefix = capitalize $ L.last $ Strings.splitOn "/" ns

hsvar :: H.NamePart -> H.Expression
hsvar = H.ExpressionVariable . hsname

qualifyRecordFieldName :: Name -> FieldName -> String
qualifyRecordFieldName sname fname = decapitalize (typeNameForRecord sname) ++ capitalize fname

qualifyUnionFieldName :: Name -> FieldName -> String
qualifyUnionFieldName sname fname = capitalize (typeNameForRecord sname) ++ capitalize fname

simpleName :: String -> H.Name
simpleName n = H.NameNormal $ H.QualifiedName [] n

typeNameForRecord :: Name -> String
typeNameForRecord sname = L.last (Strings.splitOn "." sname)
