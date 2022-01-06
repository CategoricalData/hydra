module Hydra.Ext.Scala.Coder (
  dataGraphToScalaPackage,
  scalaLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Adapter
import Hydra.Basics
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Formatting
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Coders

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


dataGraphToScalaPackage :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified Scala.Pkg
dataGraphToScalaPackage = dataGraphToExternalModule scalaLanguage encodeTerm constructModule

constructModule :: Show m => Context m -> Graph m -> M.Map Type (Step (Term m) Scala.Term) -> [(Element m, TypedTerm m)]
  -> Result Scala.Pkg
constructModule cx g coders pairs = do
    let imports = toImport <$> S.toList (dataGraphDependencies g)
    defs <- CM.mapM toDef pairs
    let pname = toScalaName $ graphName g
    let pref = Scala.Term_RefName pname
    return $ Scala.Pkg pname pref (imports ++ defs)
  where
    toImport gname = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
      Scala.Importer (Scala.Term_RefName $ toScalaName gname) [Scala.ImporteeWildcard]]
    toScalaName name = Scala.Term_Name $ L.intercalate "." $ Strings.splitOn "/" name
    toDef (el, TypedTerm typ term) = do
        let coder = Y.fromJust $ M.lookup typ coders
        rhs <- stepOut coder term
        Scala.StatDefn <$> case rhs of
          Scala.TermApply _ -> toVal rhs
          Scala.TermFunctionTerm fun -> case typ of
            TypeFunction (FunctionType _ cod) -> toDef fun cod
            _ -> fail $ "expected function type, but found " ++ show typ
--          Scala.TermFunctionTerm _ -> toVal $ Scala.TermLit $ Scala.LitString $ show rhs -- TODO
          Scala.TermLit _ -> toVal rhs
          Scala.TermRef _ -> toVal rhs -- TODO
          _ -> fail $ "unexpected RHS: " ++ show rhs
      where
        lname = localNameOf $ elementName el

        toDef (Scala.Term_FunctionTermFunction (Scala.Term_Function params body)) cod = do
          let tparams = []
          let paramss = [params]
          scod <- encodeType cod
          return $ Scala.DefnDef $ Scala.Defn_Def [] (Scala.Term_Name lname) tparams paramss (Just scod) body

        toVal rhs = pure $ Scala.DefnVal $ Scala.Defn_Val [] [namePat] Nothing rhs
          where
            namePat = Scala.PatVar $ Scala.Pat_Var $ Scala.Term_Name lname

encodeFunction :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> m -> Function m -> Result Scala.Term
encodeFunction cx meta fun = case fun of
    FunctionLambda (Lambda v body) -> slambda v <$> encodeTerm cx body <*> sdom
    FunctionPrimitive name -> pure $ sprim name
    FunctionCases cases -> do
        let v = "x"
        let sn = case dom of
              Just (TypeNominal name) -> Just name
              _ -> Nothing
        scases <- CM.mapM (encodeCase sn cx) cases
        slambda v <$> pure (Scala.TermMatch $ Scala.Term_Match (sname v) scases) <*> sdom
      where
        encodeCase sn cx (Field fname fterm) = do
          let v = "y"
          -- Note: pattern extraction may or may not be the most appropriate constructor here
          let pat = Scala.PatExtract $ Scala.Pat_Extract (sname $ qualifyUnionFieldName sn fname) [svar v]
          let cond = Nothing
          body <- encodeTerm cx $ apply fterm (variable v)
          return $ Scala.Case pat cond body
    FunctionData -> pure $ sname "DATA" -- TODO
    FunctionProjection _ -> pure $ sname "PROJECTION" -- TODO
    _ -> fail $ "unexpected function: " ++ show fun
  where
    dom = fmap (\(TypeFunction (FunctionType d _)) -> d) $ contextTypeOf cx meta
    sdom = case dom of
      Nothing -> pure Nothing
      Just t -> Just <$> encodeType t

encodeLiteral :: Literal -> Result Scala.Lit
encodeLiteral av = case av of
    LiteralBoolean b -> pure $ Scala.LitBoolean $ case b of
      BooleanValueFalse -> False
      BooleanValueTrue -> True
    LiteralFloat fv -> case fv of
      FloatValueFloat32 f -> pure $ Scala.LitFloat f
      FloatValueFloat64 f -> pure $ Scala.LitDouble f
      _ -> unexpected "floating-point number" fv
    LiteralInteger iv -> case iv of
      IntegerValueInt16 i -> pure $ Scala.LitShort i
      IntegerValueInt32 i -> pure $ Scala.LitInt i
      IntegerValueInt64 i -> pure $ Scala.LitLong i
      IntegerValueUint8 i -> pure $ Scala.LitByte i
      _ -> unexpected "integer" iv
    LiteralString s -> pure $ Scala.LitString s
    _ -> unexpected "literal value" av

encodeTerm :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Term m -> Result Scala.Term
encodeTerm cx term@(Term expr meta) = do
   case expr of
    ExpressionApplication (Application fun arg) -> case termData fun of
       ExpressionFunction FunctionData -> encodeTerm cx arg
       _ -> case termData fun of
         ExpressionFunction (FunctionCases fields) -> do
             body <- encodeTerm cx arg
             cases <- CM.mapM toCase fields
             return $ Scala.TermMatch $ Scala.Term_Match body cases
           where
             toCase (Field fname fterm) = do
               let var = "v"
               -- Note: PatExtract has the right syntax, though this may or may not be the Scalameta-intended way to use it
               let pat = Scala.PatExtract $ Scala.Pat_Extract (sname fname) [svar var] -- TODO: qualify with type name
               body <- encodeTerm cx $ apply fterm $ variable var
               return $ Scala.Case pat Nothing body
         _ -> sapply <$> encodeTerm cx fun <*> ((: []) <$> encodeTerm cx arg)
    ExpressionElement name -> pure $ sname $ localNameOf name
    ExpressionFunction f -> encodeFunction cx (termMeta term) f
    ExpressionList els -> sapply (sname "Seq") <$> CM.mapM (encodeTerm cx) els
    ExpressionLiteral v -> Scala.TermLit <$> encodeLiteral v
    ExpressionMap m -> sapply (sname "Map") <$> CM.mapM toPair (M.toList m)
      where
        toPair (k, v) = sassign <$> encodeTerm cx k <*> encodeTerm cx v
    ExpressionNominal (NominalTerm _ term') -> encodeTerm cx term'
    ExpressionOptional m -> case m of
      Nothing -> pure $ sname "None"
      Just t -> (\s -> sapply (sname "Some") [s]) <$> encodeTerm cx t
    ExpressionRecord fields -> case schemaName of
      Nothing -> fail $ "unexpected anonymous record: " ++ show term
      Just name -> do
          let typeName = typeNameForRecord name
          args <- CM.mapM (encodeTerm cx) (fieldTerm <$> fields)
          return $ sapply (sname typeName) args
    ExpressionSet s -> sapply (sname "Set") <$> CM.mapM (encodeTerm cx) (S.toList s)
    ExpressionUnion (Field fn ft) -> do
      let lhs = sname $ qualifyUnionFieldName schemaName fn
      case termData ft of
        ExpressionRecord [] -> pure lhs
        _ -> do
          arg <- encodeTerm cx ft
          return $ sapply lhs [arg]
    ExpressionVariable v -> pure $ sname v
    _ -> fail $ "unexpected term: " ++ show term
  where
    schemaName = case contextTypeOf cx meta of
      Just (TypeNominal name) -> Just name
      Nothing -> Nothing

encodeType :: Type -> Result Scala.Type
encodeType t = case t of
--  TypeElement et ->
  TypeFunction (FunctionType dom cod) -> do
    sdom <- encodeType dom
    scod <- encodeType cod
    return $ Scala.TypeFunctionType $ Scala.Type_FunctionTypeFunction $ Scala.Type_Function [sdom] scod
  TypeList lt -> stapply <$> pure (stref "Seq") <*> encodeType lt
  TypeLiteral lt -> case lt of
--    TypeBinary ->
    LiteralTypeBoolean -> pure $ stref "Boolean"
    LiteralTypeFloat ft -> case ft of
--      FloatTypeBigfloat ->
      FloatTypeFloat32 -> pure $ stref "Float"
      FloatTypeFloat64 -> pure $ stref "Double"
    LiteralTypeInteger it -> case it of
--      IntegerTypeBigint ->
--      IntegerTypeInt8 ->
      IntegerTypeInt16 -> pure $ stref "Short"
      IntegerTypeInt32 -> pure $ stref "Int"
      IntegerTypeInt64 -> pure $ stref "Long"
      IntegerTypeUint8 -> pure $ stref "Byte"
--      IntegerTypeUint16 ->
--      IntegerTypeUint32 ->
--      IntegerTypeUint64 ->
    LiteralTypeString -> pure $ stref "String"
  TypeMap (MapType kt vt) -> stapply2 <$> pure (stref "Map") <*> encodeType kt <*> encodeType vt
  TypeNominal name -> pure $ stref $ localNameOf name
  TypeOptional ot -> stapply <$> pure (stref "Option") <*> encodeType ot
--  TypeRecord sfields ->
  TypeSet st -> stapply <$> pure (stref "Set") <*> encodeType st
--  TypeUnion sfields ->
  TypeUniversal (UniversalType v body) -> do
    sbody <- encodeType body
    return $ Scala.TypeLambda $ Scala.Type_Lambda [stparam v] sbody
  TypeVariable v -> pure $ Scala.TypeVar $ Scala.Type_Var $ Scala.Type_Name v
  _ -> fail $ "can't encode unsupported type: " ++ show t

qualifyUnionFieldName :: Y.Maybe Name -> FieldName -> String
qualifyUnionFieldName sname fname = (Y.maybe "" (\n -> localNameOf n ++ ".") sname) ++ fname

scalaLanguage :: Language
scalaLanguage = Language "hydra/ext/scala" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean,
    LiteralVariantFloat,
    LiteralVariantInteger,
    LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [
    -- Bigfloat is excluded for now
    FloatTypeFloat32,
    FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeBigint,
    IntegerTypeInt16,
    IntegerTypeInt32,
    IntegerTypeInt64,
    IntegerTypeUint8],
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

sapply :: Scala.Term -> [Scala.Term] -> Scala.Term
sapply fun args = Scala.TermApply $ Scala.Term_Apply fun args

sassign :: Scala.Term -> Scala.Term -> Scala.Term
sassign lhs rhs = Scala.TermAssign $ Scala.Term_Assign lhs rhs

slambda :: Variable -> Scala.Term -> Y.Maybe Scala.Type -> Scala.Term
slambda v body sdom = Scala.TermFunctionTerm $ Scala.Term_FunctionTermFunction
    $ Scala.Term_Function [Scala.Term_Param mods name sdom def] body
  where
    mods = []
    name = Scala.NameValue v
    def = Nothing

sname :: String -> Scala.Term
sname = Scala.TermRef . Scala.Term_RefName . Scala.Term_Name

sprim :: Name -> Scala.Term
sprim name = sname $ prefix ++ "." ++ local
  where
    (ns, local) = toQname name
    prefix = capitalize $ L.last $ Strings.splitOn "/" ns

stapply :: Scala.Type -> Scala.Type -> Scala.Type
stapply t1 t2 = Scala.TypeApply $ Scala.Type_Apply t1 [t2]

stapply2 :: Scala.Type -> Scala.Type -> Scala.Type -> Scala.Type
stapply2 t1 t2 t3 = Scala.TypeApply $ Scala.Type_Apply t1 [t2, t3]

stparam :: TypeVariable -> Scala.Type_Param
stparam v = Scala.Type_Param [] (Scala.NameValue v) [] [] [] []

stref :: String -> Scala.Type
stref = Scala.TypeRef . Scala.Type_RefName . Scala.Type_Name

svar :: Variable -> Scala.Pat
svar = Scala.PatVar . Scala.Pat_Var . Scala.Term_Name

typeNameForRecord :: Name -> String
typeNameForRecord sname = L.last (Strings.splitOn "." sname)
