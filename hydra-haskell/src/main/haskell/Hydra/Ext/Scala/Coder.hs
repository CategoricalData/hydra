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
import Hydra.Primitives
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Coders
import Hydra.Rewriting
import Hydra.Types.Inference
import Hydra.Types.Substitution

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


dataGraphToScalaPackage :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified Scala.Pkg
dataGraphToScalaPackage = dataGraphToExternalModule scalaLanguage encodeUntypedTerm constructModule

constructModule :: Show m => Context m -> Graph m -> M.Map Type (Step (Term m) Scala.Term) -> [(Element m, TypedTerm m)]
  -> Result Scala.Pkg
constructModule cx g coders pairs = do
    defs <- CM.mapM toDef pairs
    let pname = toScalaName $ graphName g
    let pref = Scala.Term_RefName pname
    return $ Scala.Pkg pname pref (imports ++ defs)
  where
    imports = (toElImport <$> (S.toList $ dataGraphDependencies True False True g))
        ++ (toPrimImport <$> (S.toList $ dataGraphDependencies False True False g))
      where
        toElImport gname = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Term_RefName $ toScalaName gname) [Scala.ImporteeWildcard]]
        toPrimImport gname = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Term_RefName $ toScalaName gname) []]
    toScalaName name = Scala.Term_Name $ L.intercalate "." $ Strings.splitOn "/" name
    toDef (el, TypedTerm typ term) = do
        let coder = Y.fromJust $ M.lookup typ coders
        rhs <- stepOut coder term
        Scala.StatDefn <$> case rhs of
          Scala.TermApply _ -> toVal rhs
          Scala.TermFunctionTerm fun -> case typ of
            TypeFunction (FunctionType _ cod) -> toDefn fun cod
            _ -> fail $ "expected function type, but found " ++ show typ
          Scala.TermLit _ -> toVal rhs
          Scala.TermRef _ -> toVal rhs -- TODO
          _ -> fail $ "unexpected RHS: " ++ show rhs
      where
        lname = localNameOf $ elementName el

        freeTypeVars = S.toList $ freeVariablesInType typ

        toDefn (Scala.Term_FunctionTermFunction (Scala.Term_Function params body)) cod = do
          let tparams = stparam <$> freeTypeVars
          scod <- encodeType cod
          return $ Scala.DefnDef $ Scala.Defn_Def [] (Scala.Term_Name lname) tparams [params] (Just scod) body

        toVal rhs = pure $ Scala.DefnVal $ Scala.Defn_Val [] [namePat] Nothing rhs
          where
            namePat = Scala.PatVar $ Scala.Pat_Var $ Scala.Term_Name lname

encodeFunction :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> m -> Function m -> Y.Maybe (Term m) -> Result Scala.Term
encodeFunction cx meta fun arg = case fun of
    FunctionLambda (Lambda v body) -> slambda v <$> encodeTerm cx body <*> (findSdom meta)
    FunctionPrimitive name -> pure $ sprim name
    FunctionCases cases -> do
        let v = "v"
        dom <- findDomain meta
        scx <- schemaContext cx
        ftypes <- fieldTypes scx dom
        let sn = nameOfType dom
        scases <- CM.mapM (encodeCase ftypes sn cx) cases
        case arg of
          Nothing -> slambda v <$> pure (Scala.TermMatch $ Scala.Term_Match (sname v) scases) <*> findSdom meta
          Just a -> do
            sa <- encodeTerm cx a
            return $ Scala.TermMatch $ Scala.Term_Match sa scases
      where
        encodeCase ftypes sn cx f@(Field fname fterm) = do
--            dom <- findDomain (termMeta fterm)           -- Option #1: use type inference
            let dom = Y.fromJust $ M.lookup fname ftypes -- Option #2: look up the union type
            let patArgs = if dom == unitType then [] else [svar v]
            -- Note: PatExtract has the right syntax, though this may or may not be the Scalameta-intended way to use it
            let pat = Scala.PatExtract $ Scala.Pat_Extract (sname $ qualifyUnionFieldName "MATCHED." sn fname) patArgs
            body <- encodeTerm cx $ applyVar fterm v
            return $ Scala.Case pat Nothing body
          where
            v = "y"
        applyVar fterm v = case termData fterm of
          ExpressionFunction (FunctionLambda (Lambda v1 body)) -> if isFreeIn v1 body
            then body
            else substituteVariable v1 v body
          _ -> apply fterm (variable v)
    FunctionData -> pure $ sname "DATA" -- TODO
    FunctionProjection fname -> fail $ "unapplied projection not yet supported"
    _ -> fail $ "unexpected function: " ++ show fun
  where
    findSdom meta = Just <$> (findDomain meta >>= encodeType)
    findDomain meta = do
        case contextTypeOf cx meta of
          Nothing -> fail $ "expected a typed term"
          Just t -> domainOf t
      where
        domainOf t = case t of
          TypeFunction (FunctionType dom _) -> pure dom
          TypeElement et -> domainOf et
          _ -> fail $ "expected a function type, but found " ++ show t

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
encodeTerm cx term@(Term expr meta) = case expr of
    ExpressionApplication (Application fun arg) -> case termData fun of
        ExpressionFunction f -> case f of
          FunctionCases _ -> encodeFunction cx (termMeta fun) f (Just arg)
          FunctionData -> encodeTerm cx arg
          FunctionProjection fname -> do
            sarg <- encodeTerm cx arg
            return $ Scala.TermRef $ Scala.Term_RefSelect $ Scala.Term_Select sarg (Scala.Term_Name fname)
          _ -> fallback
        _ -> fallback
      where
        fallback = sapply <$> encodeTerm cx fun <*> ((: []) <$> encodeTerm cx arg)
    ExpressionElement name -> pure $ sname $ localNameOf name
    ExpressionFunction f -> encodeFunction cx (termMeta term) f Nothing
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
          let n = typeName False name
          args <- CM.mapM (encodeTerm cx) (fieldTerm <$> fields)
          return $ sapply (sname n) args
    ExpressionSet s -> sapply (sname "Set") <$> CM.mapM (encodeTerm cx) (S.toList s)
    ExpressionUnion (Field fn ft) -> do
      let lhs = sname $ qualifyUnionFieldName "UNION." schemaName fn
      args <- case termData ft of
        ExpressionRecord [] -> pure []
        _ -> do
          arg <- encodeTerm cx ft
          return [arg]
      return $ sapply lhs args
    ExpressionVariable v -> pure $ sname v
    _ -> fail $ "unexpected term: " ++ show term
  where
    schemaName = contextTypeOf cx meta >>= nameOfType

encodeType :: Type -> Result Scala.Type
encodeType t = case t of
--  TypeElement et ->
  TypeFunction (FunctionType dom cod) -> do
    sdom <- encodeType dom
    scod <- encodeType cod
    return $ Scala.TypeFunctionType $ Scala.Type_FunctionTypeFunction $ Scala.Type_Function [sdom] scod
  TypeList lt -> stapply1 <$> pure (stref "Seq") <*> encodeType lt
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
  TypeNominal name -> pure $ stref $ typeName True name
  TypeOptional ot -> stapply1 <$> pure (stref "Option") <*> encodeType ot
--  TypeRecord sfields ->
  TypeSet st -> stapply1 <$> pure (stref "Set") <*> encodeType st
--  TypeUnion sfields ->
  TypeUniversal (UniversalType v body) -> do
    sbody <- encodeType body
    return $ Scala.TypeLambda $ Scala.Type_Lambda [stparam v] sbody
  TypeVariable v -> pure $ Scala.TypeVar $ Scala.Type_Var $ Scala.Type_Name v
  _ -> fail $ "can't encode unsupported type in Scala: " ++ show t

encodeUntypedTerm :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Term m -> Result Scala.Term
encodeUntypedTerm cx term = do
    (term1, _) <- inferType cx term
    let term2 = rewriteTermMeta annotType term1
    encodeTerm cx term2
  where
    annotType (m, t, _) = contextSetTypeOf cx (Just t) m

nameOfType :: Type -> Y.Maybe Name
nameOfType t = case t of
  TypeNominal name -> Just name
  TypeUniversal (UniversalType _ body) -> nameOfType body
  _ -> Nothing

qualifyUnionFieldName :: String -> Y.Maybe Name -> FieldName -> String
qualifyUnionFieldName dlft sname fname = (Y.maybe dlft (\n -> typeName True n ++ ".") sname) ++ fname

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

scalaReservedWords = S.fromList $ keywords ++ classNames
  where
    -- Classes in the Scala Standard Library 2.13.8
    -- Note: numbered class names like Function1, Product16, and the names of exception/error classes are omitted,
    --       as they are unlikely to occur by chance.
    classNames = [
      "Any", "AnyVal", "App", "Array", "Boolean", "Byte", "Char", "Console", "DelayedInit", "Double", "DummyExplicit",
      "Dynamic", "Enumeration", "Equals", "Float", "Function", "Int", "Long", "MatchError", "None",
      "Nothing", "Null", "Option", "PartialFunction", "Predef", "Product", "Proxy",
      "SerialVersionUID", "Short", "Singleton", "Some", "Specializable", "StringContext",
      "Symbol", "Unit", "ValueOf"]
    -- Not an official or comprehensive list; taken from https://www.geeksforgeeks.org/scala-keywords
    keywords = [
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for",
      "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private",
      "protected", "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type", "val", "var", "while",
      "with", "yield"]

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
    prefix = L.last $ Strings.splitOn "/" ns

stapply :: Scala.Type -> [Scala.Type] -> Scala.Type
stapply t args = Scala.TypeApply $ Scala.Type_Apply t args

stapply1 :: Scala.Type -> Scala.Type -> Scala.Type
stapply1 t1 t2 = stapply t1 [t2]

stapply2 :: Scala.Type -> Scala.Type -> Scala.Type -> Scala.Type
stapply2 t1 t2 t3 = stapply t1 [t2, t3]

stparam :: TypeVariable -> Scala.Type_Param
stparam v = Scala.Type_Param [] (Scala.NameValue v) [] [] [] []

stref :: String -> Scala.Type
stref = Scala.TypeRef . Scala.Type_RefName . Scala.Type_Name

svar :: Variable -> Scala.Pat
svar = Scala.PatVar . Scala.Pat_Var . Scala.Term_Name

typeName :: Bool -> Name -> String
typeName qualify sname = if qualify && S.member local scalaReservedWords
    then L.intercalate "." $ Strings.splitOn "/" sname
    else local
  where
    (ns, local) = toQname sname
