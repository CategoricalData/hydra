module Hydra.Ext.Scala.Coder (printGraph) where

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
import Hydra.Util.Codetree.Script
import Hydra.Ext.Scala.Serde

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


printGraph :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified (M.Map FilePath String)
printGraph cx g = do
  pkg <- moduleToScalaPackage cx g
  let s = printExpr $ parenthesize $ writePkg pkg
  return $ M.fromList [(toFilePath False (FileExtension "scala") $ graphName g, s)]

moduleToScalaPackage :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified Scala.Pkg
moduleToScalaPackage = graphToExternalModule language encodeUntypedTerm constructModule

constructModule :: (Ord m, Show m) => Context m -> Graph m -> M.Map (Type m) (Step (Term m) Scala.Data) -> [(Element m, TypedTerm m)]
  -> Result Scala.Pkg
constructModule cx g coders pairs = do
    defs <- CM.mapM toDef pairs
    let pname = toScalaName $ h $ graphName g
    let pref = Scala.Data_RefName pname
    return $ Scala.Pkg pname pref (imports ++ defs)
  where
    h (GraphName n) = n
    imports = (toElImport <$> S.toList (graphDependencies True False True g))
        ++ (toPrimImport <$> S.toList (graphDependencies False True False g))
      where
        toElImport (GraphName gname) = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Data_RefName $ toScalaName gname) [Scala.ImporteeWildcard]]
        toPrimImport (GraphName gname) = Scala.StatImportExport $ Scala.ImportExportStatImport $ Scala.Import [
          Scala.Importer (Scala.Data_RefName $ toScalaName gname) []]
    toScalaName name = Scala.Data_Name $ Scala.PredefString $ L.intercalate "." $ Strings.splitOn "/" name
    toDef (el, TypedTerm typ term) = do
        let coder = Y.fromJust $ M.lookup typ coders
        rhs <- stepOut coder term
        Scala.StatDefn <$> case rhs of
          Scala.DataApply _ -> toVal rhs
          Scala.DataFunctionData fun -> case typeExpr typ of
            TypeExprFunction (FunctionType _ cod) -> toDefn fun cod
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

encodeFunction :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> m -> Function m -> Y.Maybe (Term m) -> Result Scala.Data
encodeFunction cx meta fun arg = case fun of
    FunctionLambda (Lambda (Variable v) body) -> slambda v <$> encodeTerm cx body <*> (findSdom meta)
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
              sa <- encodeTerm cx a
              return $ Scala.DataMatch $ Scala.Data_Match sa scases
        where
          encodeCase ftypes sn cx f@(Field fname fterm) = do
  --            dom <- findDomain (termMeta fterm)           -- Option #1: use type inference
              let dom = Y.fromJust $ M.lookup fname ftypes -- Option #2: look up the union type
              let patArgs = if dom == Types.unit then [] else [svar v]
              -- Note: PatExtract has the right syntax, though this may or may not be the Scalameta-intended way to use it
              let pat = Scala.PatExtract $ Scala.Pat_Extract (sname $ qualifyUnionFieldName "MATCHED." sn fname) patArgs
              body <- encodeTerm cx $ applyVar fterm v
              return $ Scala.Case pat Nothing body
            where
              v = Variable "y"
          applyVar fterm var@(Variable v) = case termExpr fterm of
            TermExprFunction (FunctionLambda (Lambda v1 body)) -> if isFreeIn v1 body
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
        domainOf t = case typeExpr t of
          TypeExprFunction (FunctionType dom _) -> pure dom
          TypeExprElement et -> domainOf et
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

encodeTerm :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Term m -> Result Scala.Data
encodeTerm cx term@(Term expr meta) = case expr of
    TermExprApplication (Application fun arg) -> case termExpr fun of
        TermExprFunction f -> case f of
          FunctionElimination e -> case e of
            EliminationElement -> encodeTerm cx arg
            EliminationRecord (FieldName fname) -> do
              sarg <- encodeTerm cx arg
              return $ Scala.DataRef $ Scala.Data_RefSelect $ Scala.Data_Select sarg
                (Scala.Data_Name $ Scala.PredefString fname)
            EliminationUnion _ -> encodeFunction cx (termMeta fun) f (Just arg)
          _ -> fallback
        _ -> fallback
      where
        fallback = sapply <$> encodeTerm cx fun <*> ((: []) <$> encodeTerm cx arg)
    TermExprElement name -> pure $ sname $ localNameOf name
    TermExprFunction f -> encodeFunction cx (termMeta term) f Nothing
    TermExprList els -> sapply (sname "Seq") <$> CM.mapM (encodeTerm cx) els
    TermExprLiteral v -> Scala.DataLit <$> encodeLiteral v
    TermExprMap m -> sapply (sname "Map") <$> CM.mapM toPair (M.toList m)
      where
        toPair (k, v) = sassign <$> encodeTerm cx k <*> encodeTerm cx v
    TermExprNominal (Named _ term') -> encodeTerm cx term'
    TermExprOptional m -> case m of
      Nothing -> pure $ sname "None"
      Just t -> (\s -> sapply (sname "Some") [s]) <$> encodeTerm cx t
    TermExprRecord fields -> do
      sn <- schemaName
      case sn of
        Nothing -> fail $ "unexpected anonymous record: " ++ show term
        Just name -> do
          let n = scalaTypeName False name
          args <- CM.mapM (encodeTerm cx) (fieldTerm <$> fields)
          return $ sapply (sname n) args
    TermExprSet s -> sapply (sname "Set") <$> CM.mapM (encodeTerm cx) (S.toList s)
    TermExprUnion (Field fn ft) -> do
      sn <- schemaName
      let lhs = sname $ qualifyUnionFieldName "UNION." sn fn
      args <- case termExpr ft of
        TermExprRecord [] -> pure []
        _ -> do
          arg <- encodeTerm cx ft
          return [arg]
      return $ sapply lhs args
    TermExprVariable (Variable v) -> pure $ sname v
    _ -> fail $ "unexpected term: " ++ show term
  where
    schemaName = do
      r <- contextTypeOf cx meta
      pure $ r >>= nameOfType

encodeType :: Show m => Type m -> Result Scala.Type
encodeType t = case typeExpr t of
--  TypeExprElement et ->
  TypeExprFunction (FunctionType dom cod) -> do
    sdom <- encodeType dom
    scod <- encodeType cod
    return $ Scala.TypeFunctionType $ Scala.Type_FunctionTypeFunction $ Scala.Type_Function [sdom] scod
  TypeExprList lt -> stapply1 <$> pure (stref "Seq") <*> encodeType lt
  TypeExprLiteral lt -> case lt of
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
  TypeExprMap (MapType kt vt) -> stapply2 <$> pure (stref "Map") <*> encodeType kt <*> encodeType vt
  TypeExprNominal name -> pure $ stref $ scalaTypeName True name
  TypeExprOptional ot -> stapply1 <$> pure (stref "Option") <*> encodeType ot
--  TypeExprRecord sfields ->
  TypeExprSet st -> stapply1 <$> pure (stref "Set") <*> encodeType st
--  TypeExprUnion sfields ->
  TypeExprUniversal (UniversalType v body) -> do
    sbody <- encodeType body
    return $ Scala.TypeLambda $ Scala.Type_Lambda [stparam v] sbody
  TypeExprVariable (TypeVariable v) -> pure $ Scala.TypeVar $ Scala.Type_Var $ Scala.Type_Name v
  _ -> fail $ "can't encode unsupported type in Scala: " ++ show t

encodeUntypedTerm :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Term m -> Result Scala.Data
encodeUntypedTerm cx term = do
    (term1, _) <- inferType cx term
    let term2 = rewriteTermMeta annotType term1
    encodeTerm cx term2
  where
    annotType (m, t, _) = contextSetTypeOf cx (Just t) m
