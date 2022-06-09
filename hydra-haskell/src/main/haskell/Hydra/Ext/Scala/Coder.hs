module Hydra.Ext.Scala.Coder (
  moduleToScalaPackage,
  scalaLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Primitives
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Lib.Strings as Strings
import Hydra.Ext.Scala.Language
import Hydra.Ext.Scala.Utils
import Hydra.Util.Coders
import Hydra.Rewriting
import Hydra.Types.Inference
import Hydra.Types.Substitution

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


moduleToScalaPackage :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified Scala.Pkg
moduleToScalaPackage = dataGraphToExternalModule scalaLanguage encodeUntypedData constructModule

constructModule :: (Ord m, Show m) => Context m -> Graph m -> M.Map (Type m) (Step (Data m) Scala.Data) -> [(Element m, TypedData m)]
  -> Result Scala.Pkg
constructModule cx g coders pairs = do
    defs <- CM.mapM toDef pairs
    let pname = toScalaName $ h $ graphName g
    let pref = Scala.Data_RefName pname
    return $ Scala.Pkg pname pref (imports ++ defs)
  where
    h (GraphName n) = n
    imports = (toElImport <$> S.toList (dataGraphDependencies True False True g))
        ++ (toPrimImport <$> S.toList (dataGraphDependencies False True False g))
      where
        toElImport (GraphName gname) = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Data_RefName $ toScalaName gname) [Scala.ImporteeWildcard]]
        toPrimImport (GraphName gname) = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Data_RefName $ toScalaName gname) []]
    toScalaName name = Scala.Data_Name $ Scala.PredefString $ L.intercalate "." $ Strings.splitOn "/" name
    toDef (el, TypedData typ term) = do
        let coder = Y.fromJust $ M.lookup typ coders
        rhs <- stepOut coder term
        Scala.StatDefn <$> case rhs of
          Scala.DataApply _ -> toVal rhs
          Scala.DataFunctionData fun -> case typeTerm typ of
            TypeTermFunction (FunctionType _ cod) -> toDefn fun cod
            _ -> fail $ "expected function type, but found " ++ show typ
          Scala.DataLit _ -> toVal rhs
          Scala.DataRef _ -> toVal rhs -- TODO
          _ -> fail $ "unexpected RHS: " ++ show rhs
      where
        lname = localNameOf $ elementName el

        freeTypeVars = S.toList $ freeVariablesInType typ

        toDefn (Scala.Data_FunctionDataFunction (Scala.Data_Function params body)) cod = do
          let tparams = stparam <$> freeTypeVars
          scod <- encodeType cod
          return $ Scala.DefnDef $ Scala.Defn_Def []
            (Scala.Data_Name $ Scala.PredefString lname) tparams [params] (Just scod) body

        toVal rhs = pure $ Scala.DefnVal $ Scala.Defn_Val [] [namePat] Nothing rhs
          where
            namePat = Scala.PatVar $ Scala.Pat_Var $ Scala.Data_Name $ Scala.PredefString lname

encodeFunction :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> m -> Function m -> Y.Maybe (Data m) -> Result Scala.Data
encodeFunction cx meta fun arg = case fun of
    FunctionLambda (Lambda (Variable v) body) -> slambda v <$> encodeData cx body <*> (findSdom meta)
    FunctionPrimitive name -> pure $ sprim name
    FunctionElimination e -> case e of
      EliminationElement -> pure $ sname "DATA" -- TODO
      EliminationRecord fname -> fail $ "unapplied projection not yet supported"
      EliminationUnion cases -> do
          let v = "v"
          dom <- findDomain meta
          scx <- schemaContext cx
          ftypes <- fieldTypes scx dom
          let sn = nameOfType dom
          scases <- CM.mapM (encodeCase ftypes sn cx) cases
          case arg of
            Nothing -> slambda v <$> pure (Scala.DataMatch $ Scala.Data_Match (sname v) scases) <*> findSdom meta
            Just a -> do
              sa <- encodeData cx a
              return $ Scala.DataMatch $ Scala.Data_Match sa scases
        where
          encodeCase ftypes sn cx f@(Field fname fterm) = do
  --            dom <- findDomain (dataMeta fterm)           -- Option #1: use type inference
              let dom = Y.fromJust $ M.lookup fname ftypes -- Option #2: look up the union type
              let patArgs = if dom == Types.unit then [] else [svar v]
              -- Note: PatExtract has the right syntax, though this may or may not be the Scalameta-intended way to use it
              let pat = Scala.PatExtract $ Scala.Pat_Extract (sname $ qualifyUnionFieldName "MATCHED." sn fname) patArgs
              body <- encodeData cx $ applyVar fterm v
              return $ Scala.Case pat Nothing body
            where
              v = Variable "y"
          applyVar fterm var@(Variable v) = case dataTerm fterm of
            DataTermFunction (FunctionLambda (Lambda v1 body)) -> if isFreeIn v1 body
              then body
              else substituteVariable v1 var body
            _ -> apply fterm (variable v)
    _ -> fail $ "unexpected function: " ++ show fun
  where
    findSdom meta = Just <$> (findDomain meta >>= encodeType)
    findDomain meta = do
        r <- contextTypeOf cx meta
        case r of
          Nothing -> fail $ "expected a typed term"
          Just t -> domainOf t
      where
        domainOf t = case typeTerm t of
          TypeTermFunction (FunctionType dom _) -> pure dom
          TypeTermElement et -> domainOf et
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
      IntegerValueInt16 i -> pure $ Scala.LitShort $ fromIntegral i
      IntegerValueInt32 i -> pure $ Scala.LitInt i
      IntegerValueInt64 i -> pure $ Scala.LitLong $ fromIntegral i
      IntegerValueUint8 i -> pure $ Scala.LitByte $ fromIntegral i
      _ -> unexpected "integer" iv
    LiteralString s -> pure $ Scala.LitString s
    _ -> unexpected "literal value" av

encodeData :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Data m -> Result Scala.Data
encodeData cx term@(Data expr meta) = case expr of
    DataTermApplication (Application fun arg) -> case dataTerm fun of
        DataTermFunction f -> case f of
          FunctionElimination e -> case e of
            EliminationElement -> encodeData cx arg
            EliminationRecord (FieldName fname) -> do
              sarg <- encodeData cx arg
              return $ Scala.DataRef $ Scala.Data_RefSelect $ Scala.Data_Select sarg
                (Scala.Data_Name $ Scala.PredefString fname)
            EliminationUnion _ -> encodeFunction cx (dataMeta fun) f (Just arg)
          _ -> fallback
        _ -> fallback
      where
        fallback = sapply <$> encodeData cx fun <*> ((: []) <$> encodeData cx arg)
    DataTermElement name -> pure $ sname $ localNameOf name
    DataTermFunction f -> encodeFunction cx (dataMeta term) f Nothing
    DataTermList els -> sapply (sname "Seq") <$> CM.mapM (encodeData cx) els
    DataTermLiteral v -> Scala.DataLit <$> encodeLiteral v
    DataTermMap m -> sapply (sname "Map") <$> CM.mapM toPair (M.toList m)
      where
        toPair (k, v) = sassign <$> encodeData cx k <*> encodeData cx v
    DataTermNominal (Named _ term') -> encodeData cx term'
    DataTermOptional m -> case m of
      Nothing -> pure $ sname "None"
      Just t -> (\s -> sapply (sname "Some") [s]) <$> encodeData cx t
    DataTermRecord fields -> do
      sn <- schemaName
      case sn of
        Nothing -> fail $ "unexpected anonymous record: " ++ show term
        Just name -> do
          let n = scalaTypeName False name
          args <- CM.mapM (encodeData cx) (fieldData <$> fields)
          return $ sapply (sname n) args
    DataTermSet s -> sapply (sname "Set") <$> CM.mapM (encodeData cx) (S.toList s)
    DataTermUnion (Field fn ft) -> do
      sn <- schemaName
      let lhs = sname $ qualifyUnionFieldName "UNION." sn fn
      args <- case dataTerm ft of
        DataTermRecord [] -> pure []
        _ -> do
          arg <- encodeData cx ft
          return [arg]
      return $ sapply lhs args
    DataTermVariable (Variable v) -> pure $ sname v
    _ -> fail $ "unexpected term: " ++ show term
  where
    schemaName = do
      r <- contextTypeOf cx meta
      pure $ r >>= nameOfType

encodeType :: Show m => Type m -> Result Scala.Type
encodeType t = case typeTerm t of
--  TypeTermElement et ->
  TypeTermFunction (FunctionType dom cod) -> do
    sdom <- encodeType dom
    scod <- encodeType cod
    return $ Scala.TypeFunctionType $ Scala.Type_FunctionTypeFunction $ Scala.Type_Function [sdom] scod
  TypeTermList lt -> stapply1 <$> pure (stref "Seq") <*> encodeType lt
  TypeTermLiteral lt -> case lt of
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
  TypeTermMap (MapType kt vt) -> stapply2 <$> pure (stref "Map") <*> encodeType kt <*> encodeType vt
  TypeTermNominal name -> pure $ stref $ scalaTypeName True name
  TypeTermOptional ot -> stapply1 <$> pure (stref "Option") <*> encodeType ot
--  TypeTermRecord sfields ->
  TypeTermSet st -> stapply1 <$> pure (stref "Set") <*> encodeType st
--  TypeTermUnion sfields ->
  TypeTermUniversal (UniversalType v body) -> do
    sbody <- encodeType body
    return $ Scala.TypeLambda $ Scala.Type_Lambda [stparam v] sbody
  TypeTermVariable (TypeVariable v) -> pure $ Scala.TypeVar $ Scala.Type_Var $ Scala.Type_Name v
  _ -> fail $ "can't encode unsupported type in Scala: " ++ show t

encodeUntypedData :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Data m -> Result Scala.Data
encodeUntypedData cx term = do
    (term1, _) <- inferType cx term
    let term2 = rewriteDataMeta annotType term1
    encodeData cx term2
  where
    annotType (m, t, _) = contextSetTypeOf cx (Just t) m
