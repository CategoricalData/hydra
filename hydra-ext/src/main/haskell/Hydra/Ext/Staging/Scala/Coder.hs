module Hydra.Ext.Staging.Scala.Coder (moduleToScala) where

import Hydra.Kernel
import Hydra.Dsl.Terms
import Hydra.Ext.Staging.Scala.Language
import Hydra.Ext.Staging.Scala.Utils
import Hydra.Ext.Staging.Scala.Serde
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Lib.Strings as Strings

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found


-- | New simple adapter version that works with definitions directly
moduleToScala :: Module -> [Definition] -> Context -> Graph -> Result (M.Map FilePath String)
moduleToScala mod defs cx g = do
  pkg <- constructModule cx g mod defs
  let s = printExpr $ parenthesize $ writePkg pkg
  return $ M.fromList [(namespaceToFilePath CaseConventionCamel (FileExtension "scala") $ moduleNamespace mod, s)]

constructModule :: Context -> Graph -> Module -> [Definition] -> Result Scala.Pkg
constructModule cx g mod defs = do
    let (typeDefs, termDefs) = partitionDefinitions defs
    typeDeclStats <- CM.mapM (encodeTypeDefinition cx g) typeDefs
    termDeclStats <- CM.mapM (encodeTermDefinition cx g) termDefs
    let pname = toScalaName $ h $ moduleNamespace mod
    let pref = Scala.Data_RefName pname
    imports <- findImports cx g
    return $ Scala.Pkg pname pref (imports ++ typeDeclStats ++ termDeclStats)
  where
    h (Namespace n) = n
    findImports cx g = do
        elImps <- moduleDependencyNamespaces cx g False False True False mod
        primImps <- moduleDependencyNamespaces cx g False True False False mod
        return $ (toElImport <$> S.toList elImps) ++ (toPrimImport <$> S.toList primImps)
      where
        toElImport (Namespace ns) = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Data_RefName $ toScalaName ns) [Scala.ImporteeWildcard]]
        toPrimImport (Namespace ns) = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Data_RefName $ toScalaName ns) []]
    toScalaName name = Scala.Data_Name $ Scala.PredefString $ L.intercalate "." $ Strings.splitOn "." name

    encodeTypeDefinition cx g (TypeDefinition name typ) = do
      let lname = localNameOf name
      let tname = Scala.Type_Name lname
      let dname = Scala.Data_Name $ Scala.PredefString lname
      -- Only use unqualified names as type parameters (e.g., "a", "b")
      -- Qualified names like "hydra.accessors.AccessorNode" are type references, not parameters
      let freeVars = L.filter (not . isQualifiedName) $ S.toList $ freeVariablesInType typ
      let tparams = stparam <$> freeVars
      Scala.StatDefn <$> case deannotateType typ of
        TypeRecord (RowType _ fields) -> do
          params <- CM.mapM (fieldToParam cx g) fields
          return $ Scala.DefnClass $ Scala.Defn_Class
            [Scala.ModCase]
            tname
            tparams
            (Scala.Ctor_Primary [] (Scala.NameValue "") [params])
            emptyTemplate
        TypeUnion (RowType _ fields) -> do
          cases <- CM.mapM (fieldToEnumCase cx g lname tparams) fields
          return $ Scala.DefnEnum $ Scala.Defn_Enum
            []
            tname
            tparams
            (Scala.Ctor_Primary [] (Scala.NameValue "") [])
            (Scala.Template [] [] emptySelf cases)
        TypeWrap (WrappedType _ inner) -> do
          styp <- encodeType cx g inner
          return $ Scala.DefnType $ Scala.Defn_Type [] tname tparams styp
        _ -> do
          styp <- encodeType cx g typ
          return $ Scala.DefnType $ Scala.Defn_Type [] tname tparams styp

    fieldToParam :: Context -> Graph -> FieldType -> Result Scala.Data_Param
    fieldToParam cx g (FieldType (Name fname) ftyp) = do
      sftyp <- encodeType cx g ftyp
      return $ Scala.Data_Param [] (Scala.NameValue fname) (Just sftyp) Nothing

    fieldToEnumCase :: Context -> Graph -> String -> [Scala.Type_Param] -> FieldType -> Result Scala.Stat
    fieldToEnumCase cx g parentName tparams (FieldType (Name fname) ftyp) = do
      sftyp <- encodeType cx g ftyp
      let caseName = Scala.Data_Name $ Scala.PredefString fname
      let isUnit = case deannotateType ftyp of
            TypeUnit -> True
            TypeRecord (RowType _ []) -> True
            _ -> False
      let params = if isUnit then [] else [Scala.Data_Param [] (Scala.NameValue "value") (Just sftyp) Nothing]
      -- The extends clause for the enum case
      let parentType = if L.null tparams
            then stref parentName
            else stapply (stref parentName) (typeParamToTypeVar <$> tparams)
      return $ Scala.StatDefn $ Scala.DefnEnumCase $ Scala.Defn_EnumCase
        []
        caseName
        []
        (Scala.Ctor_Primary [] (Scala.NameValue "") [params])
        [Scala.Init parentType (Scala.NameValue "") []]

    typeParamToTypeVar :: Scala.Type_Param -> Scala.Type
    typeParamToTypeVar (Scala.Type_Param _ n _ _ _ _) = Scala.TypeVar $ Scala.Type_Var $ Scala.Type_Name $ nameToString n
      where
        nameToString (Scala.NameValue s) = s

    emptyTemplate :: Scala.Template
    emptyTemplate = Scala.Template [] [] emptySelf []

    emptySelf :: Scala.Self
    emptySelf = Scala.Self ()

    isQualifiedName :: Name -> Bool
    isQualifiedName (Name n) = '.' `L.elem` n

    encodeTermDefinition cx g (TermDefinition name term typ) = do
        rhs <- encodeTerm cx g term
        let lname = localNameOf name
        let typ' = typeSchemeType typ
        Scala.StatDefn <$> case rhs of
          Scala.DataApply _ -> toVal lname rhs
          Scala.DataFunctionData fun -> case deannotateType typ' of
            TypeFunction (FunctionType _ cod) -> toDefn cx g lname typ' fun cod
            _ -> toVal lname rhs
          Scala.DataLit _ -> toVal lname rhs
          Scala.DataRef _ -> toVal lname rhs
          _ -> toVal lname rhs
      where
        toDefn cx g lname typ0 (Scala.Data_FunctionDataFunction (Scala.Data_Function params body)) cod = do
          let freeTypeVars = S.toList $ freeVariablesInType typ0
          let tparams = stparam <$> freeTypeVars
          scod <- encodeType cx g cod
          return $ Scala.DefnDef $ Scala.Defn_Def []
            (Scala.Data_Name $ Scala.PredefString lname) tparams [params] (Just scod) body

        toVal lname rhs = pure $ Scala.DefnVal $ Scala.Defn_Val [] [namePat] Nothing rhs
          where
            namePat = Scala.PatVar $ Scala.Pat_Var $ Scala.Data_Name $ Scala.PredefString lname

encodeFunction :: Context -> Graph -> M.Map Name Term -> Function -> Y.Maybe Term -> Result Scala.Data
encodeFunction cx g meta fun arg = case fun of
    FunctionLambda (Lambda (Name v) _ body) -> slambda v <$> encodeTerm cx g body <*> findSdom
    FunctionPrimitive name -> pure $ sprim name
    FunctionElimination e -> case e of
      EliminationWrap name -> pure $ sname $ "ELIM-NOMINAL(" ++ show name ++ ")" -- TODO
      EliminationRecord p -> err cx "unapplied projection not yet supported"
      EliminationUnion (CaseStatement _ def cases) -> do
          let v = "v"
          dom <- findDomain
          ftypes <- fieldTypes cx g dom
          let sn = nameOfType g dom
          scases <- CM.mapM (encodeCase ftypes sn) cases
          -- TODO: default
          case arg of
            Nothing -> slambda v <$> pure (Scala.DataMatch $ Scala.Data_Match (sname v) scases) <*> findSdom
            Just a -> do
              sa <- encodeTerm cx g a
              return $ Scala.DataMatch $ Scala.Data_Match sa scases
        where
          encodeCase ftypes sn f@(Field fname fterm) = do
--            dom <- findDomain (termMeta fterm)           -- Option #1: use type inference
              let dom = Y.fromJust $ M.lookup fname ftypes -- Option #2: look up the union type
              let patArgs = if dom == Types.unit then [] else [svar v]
              -- Note: PatExtract has the right syntax, though this may or may not be the Scalameta-intended way to use it
              let pat = Scala.PatExtract $ Scala.Pat_Extract (sname $ qualifyUnionFieldName "MATCHED." sn fname) patArgs
              body <- encodeTerm cx g $ applyVar fterm v
              return $ Scala.Case pat Nothing body
            where
              v = Name "y"
          applyVar fterm avar@(Name v) = case deannotateTerm fterm of
            TermFunction (FunctionLambda (Lambda v1 _ body)) -> if isFreeVariableInTerm v1 body
              then body
              else substituteVariable v1 avar body
            _ -> apply fterm (var v)
  where
    findSdom = Just <$> (findDomain >>= encodeType cx g)
    findDomain = do
        r <- decodingErrorToOtherError cx $ getType g meta
        case r of
          Nothing -> err cx "expected a typed term"
          Just t -> domainOf t
      where
        domainOf t = case deannotateType t of
          TypeFunction (FunctionType dom _) -> pure dom
          _ -> err cx $ "expected a function type, but found " ++ show t

decodingErrorToOtherError :: Context -> Either DecodingError a -> Result a
decodingErrorToOtherError cx (Left (DecodingError msg)) = err cx msg
decodingErrorToOtherError _ (Right a) = Right a

encodeLiteral :: Context -> Graph -> Literal -> Result Scala.Lit
encodeLiteral cx g av = case av of
    LiteralBoolean b -> pure $ Scala.LitBoolean b
    LiteralFloat fv -> case fv of
      FloatValueFloat32 f -> pure $ Scala.LitFloat f
      FloatValueFloat64 f -> pure $ Scala.LitDouble f
      _ -> unexpectedE cx "floating-point number" $ show fv
    LiteralInteger iv -> case iv of
      IntegerValueInt16 i -> pure $ Scala.LitShort $ fromIntegral i
      IntegerValueInt32 i -> pure $ Scala.LitInt i
      IntegerValueInt64 i -> pure $ Scala.LitLong $ fromIntegral i
      IntegerValueUint8 i -> pure $ Scala.LitByte $ fromIntegral i
      _ -> unexpectedE cx "integer" $ show iv
    LiteralString s -> pure $ Scala.LitString s
    _ -> unexpectedE cx "literal value" $ show av

encodeTerm :: Context -> Graph -> Term -> Result Scala.Data
encodeTerm cx g term = case deannotateTerm term of
    TermApplication (Application fun arg) -> case deannotateTerm fun of
        TermFunction f -> case f of
          FunctionElimination e -> case e of
            EliminationWrap name -> fallback
            EliminationRecord (Projection _ (Name fname)) -> do
              sarg <- encodeTerm cx g arg
              return $ Scala.DataRef $ Scala.Data_RefSelect $ Scala.Data_Select sarg
                (Scala.Data_Name $ Scala.PredefString fname)
            EliminationUnion _ -> do
              encodeFunction cx g (termAnnotationInternal fun) f (Just arg)
          _ -> fallback
        _ -> fallback
      where
        fallback = sapply <$> encodeTerm cx g fun <*> ((: []) <$> encodeTerm cx g arg)
    TermFunction f -> do
      encodeFunction cx g (termAnnotationInternal term) f Nothing
    TermList els -> sapply (sname "Seq") <$> CM.mapM (encodeTerm cx g) els
    TermLiteral v -> Scala.DataLit <$> encodeLiteral cx g v
    TermMap m -> sapply (sname "Map") <$> CM.mapM toPair (M.toList m)
      where
        toPair (k, v) = sassign <$> encodeTerm cx g k <*> encodeTerm cx g v
    TermWrap (WrappedTerm _ term') -> encodeTerm cx g term'
    TermMaybe m -> case m of
      Nothing -> pure $ sname "None"
      Just t -> (\s -> sapply (sname "Some") [s]) <$> encodeTerm cx g t
    TermRecord (Record name fields) -> do
      let n = scalaTypeName False name
      args <- CM.mapM (encodeTerm cx g) (fieldTerm <$> fields)
      return $ sapply (sname n) args
    TermSet s -> sapply (sname "Set") <$> CM.mapM (encodeTerm cx g) (S.toList s)
    TermUnion (Injection sn (Field fn ft)) -> do
      let lhs = sname $ qualifyUnionFieldName "UNION." (Just sn) fn
      args <- case deannotateTerm ft of
        TermRecord (Record _ []) -> pure []
        _ -> do
          arg <- encodeTerm cx g ft
          return [arg]
      return $ sapply lhs args
    TermVariable (Name v) -> pure $ sname v
    _ -> err cx $ "unexpected term: " ++ show term


encodeType :: Context -> Graph -> Type -> Result Scala.Type
encodeType cx g t = case deannotateType t of
  TypeUnit -> pure $ stref "Unit"
  TypeEither (EitherType lt rt) -> stapply2 <$> pure (stref "Either") <*> encodeType cx g lt <*> encodeType cx g rt
  TypeFunction (FunctionType dom cod) -> do
    sdom <- encodeType cx g dom
    scod <- encodeType cx g cod
    return $ Scala.TypeFunctionType $ Scala.Type_FunctionTypeFunction $ Scala.Type_Function [sdom] scod
  TypeList lt -> stapply1 <$> pure (stref "Seq") <*> encodeType cx g lt
  TypeLiteral lt -> case lt of
    LiteralTypeBinary -> pure $ stapply (stref "Array") [stref "Byte"]
    LiteralTypeBoolean -> pure $ stref "Boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeBigfloat -> pure $ stref "BigDecimal"
      FloatTypeFloat32 -> pure $ stref "Float"
      FloatTypeFloat64 -> pure $ stref "Double"
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure $ stref "BigInt"
      IntegerTypeInt8 -> pure $ stref "Byte"
      IntegerTypeInt16 -> pure $ stref "Short"
      IntegerTypeInt32 -> pure $ stref "Int"
      IntegerTypeInt64 -> pure $ stref "Long"
      IntegerTypeUint8 -> pure $ stref "Byte"
      IntegerTypeUint16 -> pure $ stref "Int"
      IntegerTypeUint32 -> pure $ stref "Long"
      IntegerTypeUint64 -> pure $ stref "BigInt"
    LiteralTypeString -> pure $ stref "String"
  TypeMap (MapType kt vt) -> stapply2 <$> pure (stref "Map") <*> encodeType cx g kt <*> encodeType cx g vt
  TypeMaybe ot -> stapply1 <$> pure (stref "Option") <*> encodeType cx g ot
  TypePair (PairType ft st) -> stapply2 <$> pure (stref "Tuple2") <*> encodeType cx g ft <*> encodeType cx g st
  TypeRecord (RowType tname _) -> pure $ stref $ scalaTypeName True tname
  TypeSet st -> stapply1 <$> pure (stref "Set") <*> encodeType cx g st
  TypeUnion (RowType tname _) -> pure $ stref $ scalaTypeName True tname
  TypeWrap (WrappedType tname _) -> pure $ stref $ scalaTypeName True tname
  TypeForall (ForallType v body) -> do
    sbody <- encodeType cx g body
    return $ Scala.TypeLambda $ Scala.Type_Lambda [stparam v] sbody
--   TypeVariable name -> pure $ stref $ scalaTypeName True name
  TypeVariable (Name v) -> pure $ Scala.TypeVar $ Scala.Type_Var $ Scala.Type_Name v
  _ -> err cx $ "can't encode unsupported type in Scala: " ++ show t

encodeUntypeApplicationTerm :: Context -> Graph -> Term -> Result Scala.Data
encodeUntypeApplicationTerm cx g term = do
  result <- inferInGraphContext cx g term
  encodeTerm cx g (inferenceResultTerm result)
