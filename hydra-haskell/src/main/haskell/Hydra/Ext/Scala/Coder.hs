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
          Scala.TermFunctionTerm fun -> toDef fun
--          Scala.TermFunctionTerm _ -> toVal $ Scala.TermLit $ Scala.LitString $ show rhs -- TODO
          Scala.TermLit _ -> toVal rhs
          Scala.TermRef _ -> toVal rhs -- TODO
          _ -> fail $ "unexpected RHS: " ++ show rhs
      where
        lname = localNameOf $ elementName el

        toDef (Scala.Term_FunctionTermFunction (Scala.Term_Function params body)) = do
          let tparams = []
          let paramss = [params]
          return $ Scala.DefnDef $ Scala.Defn_Def [] (Scala.Term_Name lname) tparams paramss () body

        toVal rhs = pure $ Scala.DefnVal $ Scala.Defn_Val [] [namePat] Nothing rhs
          where
            namePat = Scala.PatVar $ Scala.Pat_Var $ Scala.Term_Name lname

encodeFunction :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> a -> Function a -> Result Scala.Term
encodeFunction cx meta fun = case fun of
--    FunctionLambda (Lambda v body) -> slambda v <$> encodeTerm cx body
    FunctionLambda _ -> pure $ sname $ show fun --TODO: temp
    FunctionPrimitive name -> pure $ sprim name
    FunctionCases _ -> pure $ sname "CASES" -- TODO
    FunctionData -> pure $ sname "DATA" -- TODO
    FunctionProjection _ -> pure $ sname "PROJECTION" -- TODO
    _ -> fail $ "unexpected function: " ++ show fun

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
--         ExpressionFunction (FunctionProjection fname) -> sapply <$> (pure $ sname fname) <*> encodeTerm cx arg
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
    ExpressionUnion (Field fn ft) -> pure $ sname "UNION" -- do  TODO
--      let lhs = hsvar $ Y.maybe fn (`qualifyUnionFieldName` fn) sname
--      case termData ft of
--        ExpressionRecord [] -> pure lhs
--        _ -> hsapp lhs <$> encodeTerm cx ft
    ExpressionVariable v -> pure $ sname v
    _ -> fail $ "unexpected term: " ++ show term
  where
    schemaName = case contextTypeOf cx meta of
      Just (TypeNominal name) -> Just name
      Nothing -> Nothing

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

slambda :: Variable -> Scala.Term -> Scala.Term
slambda v body = Scala.TermFunctionTerm $ Scala.Term_FunctionTermFunction
  $ Scala.Term_Function [Scala.Term_Param [] $ Scala.NameValue v] body

sname :: String -> Scala.Term
sname = Scala.TermRef . Scala.Term_RefName . Scala.Term_Name

sprim :: Name -> Scala.Term
sprim name = sname $ prefix ++ "." ++ local
  where
    (ns, local) = toQname name
    prefix = capitalize $ L.last $ Strings.splitOn "/" ns

svar :: Variable -> Scala.Pat
svar = Scala.PatVar . Scala.Pat_Var . Scala.Term_Name

typeNameForRecord :: Name -> String
typeNameForRecord sname = L.last (Strings.splitOn "." sname)
