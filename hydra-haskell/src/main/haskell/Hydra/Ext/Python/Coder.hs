module Hydra.Ext.Python.Coder (moduleToPython) where

import Hydra.Kernel
import Hydra.Staging.Adapters
import Hydra.Ext.Python.Language
import Hydra.Dsl.Terms
import Hydra.Staging.Serialization
import qualified Hydra.Ext.Python.Syntax as Py
import Hydra.Ext.Python.Names
import Hydra.Ext.Python.Utils
import qualified Hydra.Ext.Python.Serde as PySer
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes
import Hydra.Lib.Io
import Hydra.Staging.Formatting
import qualified Hydra.Decode as Decode

import qualified Control.Monad as CM
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y


-- | Temporary metadata which is used to create the header section of a Python file
data PythonModuleMetadata = PythonModuleMetadata {
  pythonModuleMetadataTypeVariables :: S.Set Name,
  pythonModuleMetadataUsesAnnotated :: Bool,
  pythonModuleMetadataUsesCallable :: Bool,
  pythonModuleMetadataUsesDataclass :: Bool,
  pythonModuleMetadataUsesEnum :: Bool,
  pythonModuleMetadataUsesFrozenDict :: Bool,
  pythonModuleMetadataUsesFrozenList :: Bool,
  pythonModuleMetadataUsesGeneric :: Bool,
  pythonModuleMetadataUsesTypeVar :: Bool,
  pythonModuleMetadataUsesNode :: Bool}

encodeApplication :: PythonEnvironment -> Application -> Flow Graph Py.Expression
encodeApplication env (Application fun arg) = case fullyStripTerm fun of
    TermFunction f -> case f of
      FunctionElimination elm -> case elm of
--        EliminationList ...
        EliminationOptional (OptionalCases nothing just) -> do
          return $ stringToPyExpression Py.QuoteStyleDouble "optional match expressions not yet supported"
--        EliminationProduct ...
        EliminationRecord (Projection _ fname) -> do
          parg <- encodeTerm env arg
          return $ projectFromExpression parg $ encodeFieldName env fname
        EliminationUnion (CaseStatement tname mdef cases) -> do
          return $ stringToPyExpression Py.QuoteStyleDouble "inline match expressions are unsupported"
        EliminationWrap _ -> do
          parg <- encodeTerm env arg
          return $ projectFromExpression parg $ Py.Name "value"
        _ -> fail $ "elimination variant is not yet supported in applications: " ++ show (eliminationVariant elm)
      FunctionPrimitive name -> do
        parg <- encodeTerm env arg
        return $ functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) [parg]
      _ -> def
    TermVariable name -> do -- Special-casing variables prevents quoting; forward references are allowed for function calls
      parg <- encodeTerm env arg
      return $ functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) [parg]
    _ -> def
  where
    def = do
      pfun <- encodeTerm env fun
      parg <- encodeTerm env arg
      return $ functionCall (pyExpressionToPyPrimary pfun) [parg]

encodeApplicationType :: PythonEnvironment -> ApplicationType -> Flow Graph Py.Expression
encodeApplicationType env at = do
    pyBody <- encodeType env body
    pyArgs <- CM.mapM (encodeType env) args
    return $ primaryAndParams (pyExpressionToPyPrimary pyBody) pyArgs
  where
    (body, args) = gatherParams (TypeApplication at) []
    gatherParams t ps = case stripType t of
      TypeApplication (ApplicationType lhs rhs) -> gatherParams lhs (rhs:ps)
      _ -> (t, ps)

encodeDefinition :: PythonEnvironment -> Definition -> Flow Graph [Py.Statement]
encodeDefinition env def = case def of
  DefinitionTerm name term typ -> withTrace ("data element " ++ unName name) $ do
    comment <- fmap normalizeComment <$> getTermDescription term
    g <- getState
    encodeTermAssignment env name (fullyStripTerm $ expandLambdas g term) typ comment
  DefinitionType name typ -> withTrace ("type element " ++ unName name) $ do
    comment <- fmap normalizeComment <$> getTypeDescription typ
    encodeTypeAssignment env name typ comment

encodeField :: PythonEnvironment -> Field -> Flow Graph (Py.Name, Py.Expression)
encodeField env (Field fname fterm) = do
  pterm <- encodeTerm env fterm
  return (encodeFieldName env fname, pterm)

encodeFieldType :: PythonEnvironment -> FieldType -> Flow Graph Py.Statement
encodeFieldType env (FieldType fname ftype) = do
  comment <- getTypeDescription ftype
  let pyName = Py.SingleTargetName $ encodeFieldName env fname
  pyType <- annotatedExpression comment <$> encodeType env ftype
  return $ pyAssignmentToPyStatement $ Py.AssignmentTyped $ Py.TypedAssignment pyName pyType Nothing

encodeFunctionType :: PythonEnvironment -> FunctionType -> Flow Graph Py.Expression
encodeFunctionType env ft = do
    pydoms <- CM.mapM encode doms
    pycod <- encode cod
    return $ pyPrimaryToPyExpression $ primaryWithSlices (pyNameToPyPrimary $ Py.Name "Callable")
      (pyPrimaryToPySlice $ Py.PrimarySimple $ Py.AtomList $ pyList pydoms)
      [Py.SliceOrStarredExpressionSlice $ pyExpressionToPySlice pycod]
  where
    encode = encodeType env
    (doms, cod) = gatherParams [] ft
    gatherParams rdoms (FunctionType dom cod) = case stripType cod of
      TypeFunction ft2 -> gatherParams (dom:rdoms) ft2
      _ -> (L.reverse (dom:rdoms), cod)

encodeFloatValue :: FloatValue -> Flow s Py.Expression
encodeFloatValue fv = case fv of
  FloatValueBigfloat f -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberFloat f
  -- TODO: remove these variants; the fact that the float32 type is appearing here is a bug
  FloatValueFloat32 f -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberFloat $ realToFrac f
  FloatValueFloat64 f -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberFloat $ realToFrac f
--  _ -> fail $ "unsupported floating point type: " ++ show (floatValueType fv)

encodeFunction :: PythonEnvironment -> Function -> Flow Graph Py.Expression
encodeFunction env f = case f of
  FunctionLambda (Lambda var _ body) -> do
    pbody <- encodeTerm env body
    return $ Py.ExpressionLambda $ Py.Lambda (Py.LambdaParameters Nothing [] [] $
      Just $ Py.LambdaStarEtcParamNoDefault $ Py.LambdaParamNoDefault $ encodeNameQualified env var) pbody
  FunctionPrimitive name -> pure $ pyNameToPyExpression $ encodeName True CaseConventionLowerSnake env name -- Only nullary primitives should appear here.
  _ -> fail $ "unexpected function variant: " ++ show (functionVariant f)

encodeFunctionDefinition :: PythonEnvironment -> Name -> [Name] -> Term -> [Type] -> Type -> Maybe String -> [Py.Statement] -> Flow Graph Py.Statement
encodeFunctionDefinition env name args body doms cod comment prefixes = do
    pyArgs <- CM.zipWithM toParam args doms
    let params = Py.ParametersParamNoDefault $ Py.ParamNoDefaultParameters pyArgs [] Nothing
    stmts <- encodeTopLevelTerm env body
    let block = indentedBlock comment [prefixes ++ stmts]
    returnType <- encodeType env cod
    return $ Py.StatementCompound $ Py.CompoundStatementFunction $ Py.FunctionDefinition Nothing
      $ Py.FunctionDefRaw False (encodeName False CaseConventionLowerSnake env name) [] (Just params) (Just returnType) Nothing block
  where
    toParam name typ = do
      pyTyp <- encodeType env typ
      return $ Py.ParamNoDefault (Py.Param (encodeName False CaseConventionLowerSnake env name) $ Just $ Py.Annotation pyTyp) Nothing

encodeIntegerValue :: IntegerValue -> Flow s Py.Expression
encodeIntegerValue iv = case iv of
  IntegerValueBigint i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger i
  -- TODO: remove these variants; the fact that the int32 type is appearing here is a bug
  IntegerValueInt8 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueInt16 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueInt32 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueInt64 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueUint8 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueUint16 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueUint32 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
  IntegerValueUint64 i -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberInteger $ fromIntegral i
--  _ -> fail $ "unsupported integer type: " ++ show (integerValueType iv)

encodeLambdaType :: PythonEnvironment -> LambdaType -> Flow Graph Py.Expression
encodeLambdaType env lt = do
    pyBody <- encodeType env body
    return $ primaryAndParams (pyExpressionToPyPrimary pyBody) (pyNameToPyExpression . Py.Name . unName <$> params)
  where
    (body, params) = gatherParams (TypeLambda lt) []
    gatherParams t ps = case stripType t of
      TypeLambda (LambdaType name body) -> gatherParams body (name:ps)
      _ -> (t, L.reverse ps)

encodeLiteral :: Literal -> Flow s Py.Expression
encodeLiteral lit = case lit of
  LiteralBoolean b -> pure $ pyAtomToPyExpression $ if b then Py.AtomTrue else Py.AtomFalse
  LiteralFloat f -> encodeFloatValue f
  LiteralInteger i -> encodeIntegerValue i
  LiteralString s -> pure $ stringToPyExpression Py.QuoteStyleDouble s
  _ -> fail $ "unsupported literal variant: " ++ show (literalVariant lit)

encodeLiteralType :: LiteralType -> Flow Graph Py.Expression
encodeLiteralType lt = do
    name <- Py.Name <$> findName
    return $ pyNameToPyExpression name
  where
    findName = case lt of
      LiteralTypeBinary -> pure "bytes"
      LiteralTypeBoolean -> pure "bool"
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat64 -> pure "float"
        _ -> fail $ "unsupported floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> pure "int"
--        _ -> fail $ "unsupported integer type: " ++ show it
        _ -> pure "int" -- TODO: restore the failure behavior; the fact that the int32 type is appearing here is a bug
      LiteralTypeString -> pure "str"

encodeModule :: Module -> Flow Graph Py.Module
encodeModule mod = do
    defs <- adaptedModuleDefinitions pythonLanguage mod
    let namespaces = namespacesForDefinitions encodeNamespace (moduleNamespace mod) defs
    let env = PythonEnvironment {
              pythonEnvironmentNamespaces = namespaces,
              pythonEnvironmentBoundTypeVariables = ([], M.empty)}
    defStmts <- L.concat <$> (CM.mapM (encodeDefinition env) defs)
    let meta = gatherMetadata defs
    let tvars = pythonModuleMetadataTypeVariables meta
    let importStmts = imports namespaces meta
    let tvarStmts = tvarStmt . encodeTypeVariable <$> S.toList tvars
    let mc = tripleQuotedString . normalizeComment <$> moduleDescription mod
    let commentStmts = case normalizeComment <$> moduleDescription mod of
                       Nothing -> []
                       Just c -> [commentStatement c]
    let body = L.filter (not . L.null) [commentStmts, importStmts, tvarStmts] ++ (singleton <$> defStmts)
    return $ Py.Module body
  where
    singleton s = [s]
    tvarStmt name = assignmentToExpression name $ functionCall (pyNameToPyPrimary $ Py.Name "TypeVar")
      [doubleQuotedString $ Py.unName name]
    imports namespaces meta = pySimpleStatementToPyStatement . Py.SimpleStatementImport <$> (standardImports ++ domainImports)
      where
        domainImports = toImport <$> names
          where
            names = L.sort $ M.elems $ namespacesMapping namespaces
            toImport ns = Py.ImportStatementName $ Py.ImportName [Py.DottedAsName ns Nothing]
        standardImports = toImport <$> (Y.catMaybes (simplifyPair <$> pairs))
          where
            simplifyPair (a, symbols) = if L.null rem then Nothing else Just (a, rem)
              where
                rem = Y.catMaybes symbols
            pairs = [
                ("__future__", [if _useFutureAnnotations_ then Just "annotations" else Nothing]),
                ("collections.abc", [
                  cond "Callable" $ pythonModuleMetadataUsesCallable meta]),
                ("dataclasses", [
                  cond "dataclass" $ pythonModuleMetadataUsesDataclass meta]),
                ("enum", [
                  cond "Enum" $ pythonModuleMetadataUsesEnum meta]),
                ("hydra.dsl.python", [
                  cond "FrozenDict" $ pythonModuleMetadataUsesFrozenDict meta,
                  cond "frozenlist" $ pythonModuleMetadataUsesFrozenList meta,
                  cond "Node" $ pythonModuleMetadataUsesNode meta]),
                ("typing", [
                  cond "Annotated" $ pythonModuleMetadataUsesAnnotated meta,
                  cond "Generic" $ pythonModuleMetadataUsesGeneric meta,
                  cond "TypeVar" $ pythonModuleMetadataUsesTypeVar meta])]
              where
                cond name b = if b then Just name else Nothing
            toImport (modName, symbols) = Py.ImportStatementFrom $
                Py.ImportFrom [] (Just $ Py.DottedName [Py.Name modName]) $
                  Py.ImportFromTargetsSimple (forSymbol <$> symbols)
              where
                forSymbol s = Py.ImportFromAsName (Py.Name s) Nothing

encodeRecordType :: PythonEnvironment -> Name -> RowType -> Maybe String -> Flow Graph Py.Statement
encodeRecordType env name (RowType _ tfields) comment = do
    pyFields <- CM.mapM (encodeFieldType env) tfields
    let body = indentedBlock comment [pyFields]
    return $ pyClassDefinitionToPyStatement $
      Py.ClassDefinition (Just decs) (encodeName False CaseConventionPascal env name) [] args body
  where
    (tparamList, tparamMap) = pythonEnvironmentBoundTypeVariables env
    args = fmap (\a -> pyExpressionsToPyArgs [a]) $ genericArg tparamList
    decs = Py.Decorators [pyNameToPyNamedExpression $ Py.Name "dataclass"]

encodeTerm :: PythonEnvironment -> Term -> Flow Graph Py.Expression
encodeTerm env term = case fullyStripTerm term of
    TermApplication a -> encodeApplication env a
    TermFunction f -> encodeFunction env f
    TermLet _ -> pure $ stringToPyExpression Py.QuoteStyleDouble "let terms are not supported here"
    TermList els -> do
      pl <- pyAtomToPyExpression . Py.AtomList . pyList <$> CM.mapM encode els
      return $ functionCall (pyNameToPyPrimary $ Py.Name "tuple") [pl]
    TermLiteral lit -> encodeLiteral lit
    TermMap m -> do
        pairs <- CM.mapM encodePair $ M.toList m
        return $ functionCall (pyNameToPyPrimary $ Py.Name "FrozenDict")
          [pyAtomToPyExpression $ Py.AtomDict $ Py.Dict pairs]
      where
        encodePair (k, v) = do
          pyK <- encode k
          pyV <- encode v
          return $ Py.DoubleStarredKvpairPair $ Py.Kvpair pyK pyV
    TermOptional mt -> case mt of
      Nothing -> pure $ pyNameToPyExpression pyNone
      Just term1 -> encode term1
    TermProduct terms -> do
      pyExprs <- CM.mapM encode terms
      return $ pyAtomToPyExpression $ Py.AtomTuple $ Py.Tuple (pyExpressionToPyStarNamedExpression <$> pyExprs)
    TermRecord (Record tname fields) -> do
      pargs <- CM.mapM (encode . fieldTerm) fields
      return $ functionCall (pyNameToPyPrimary $ encodeNameQualified env tname) pargs
    TermSet s -> do
      pyEls <- CM.mapM encode $ S.toList s
      return $ functionCall (pyNameToPyPrimary $ Py.Name "frozenset")
        [pyAtomToPyExpression $ Py.AtomSet $ Py.Set (pyExpressionToPyStarNamedExpression <$> pyEls)]
    TermUnion (Injection tname field) -> do
      rt <- requireUnionType tname
      if isEnumType rt
        then return $ projectFromExpression (pyNameToPyExpression $ encodeNameQualified env tname)
          $ encodeEnumValue env $ fieldName field
        else do
          parg <- encode $ fieldTerm field
          return $ functionCall (pyNameToPyPrimary $ variantName True env tname (fieldName field)) [parg]
    TermVariable name -> pure $ termVariableReference env name
    TermWrap (WrappedTerm tname term1) -> do
      parg <- encode term1
      return $ functionCall (pyNameToPyPrimary $ encodeNameQualified env tname) [parg]
    t -> fail $ "unsupported term variant: " ++ show (termVariant t) ++ " in " ++ show term
  where
    encode = encodeTerm env

encodeTermAssignment :: PythonEnvironment -> Name -> Term -> Type -> Maybe String -> Flow Graph [Py.Statement]
encodeTermAssignment env name term typ comment = if L.null args && L.null bindings
    -- If there are no arguments or let bindings, use a simple a = b assignment.
    then do
      bodyExpr <- encodeTerm env body
      return [annotatedStatement comment $ assignmentToExpression (encodeName False CaseConventionLowerSnake env name) bodyExpr]
    -- If there are either arguments or let bindings, then only a function definition will work.
    else do
        -- TODO: topological sort of bindings
        bindingStmts <- L.concat <$> CM.mapM encodeBinding bindings
        bodyStmt <- encodeFunctionDefinition env name args body doms cod comment bindingStmts
        return [bodyStmt]
  where
    encodeBinding (LetBinding name1 term1 mts) = do
      comment <- fmap normalizeComment <$> getTermDescription term1
      typ1 <- case mts of
        Nothing -> fail $ "missing type for let binding " ++ unName name1 ++ " in " ++ unName name
        Just ts -> return $ typeSchemeType ts
      encodeTermAssignment env name1 term1 typ1 comment
    (args, bindings, body, doms, cod) = gatherArgsAndBindings [] [] [] term typ
    gatherArgsAndBindings prevArgs prevBindings prevDoms term typ = case fullyStripTerm term of
      TermFunction (FunctionLambda (Lambda var _ body)) -> case stripType typ of
        TypeFunction (FunctionType dom cod) -> gatherArgsAndBindings (var:prevArgs) prevBindings (dom:prevDoms) body cod
      TermLet (Let bindings body) -> gatherArgsAndBindings prevArgs (bindings ++ prevBindings) prevDoms body typ
      t -> (L.reverse prevArgs, L.reverse prevBindings, t, L.reverse prevDoms, typ)

encodeTopLevelTerm :: PythonEnvironment -> Term -> Flow Graph [Py.Statement]
encodeTopLevelTerm env term = if L.length args == 1
    then withArg body (L.head args)
    else dflt
  where
    (args, body) = gatherArgs [] term
    gatherArgs rest term = case fullyStripTerm term of
      TermApplication (Application l r) -> gatherArgs (r:rest) l
      t -> (rest, t)
    dflt = do
      expr <- encodeTerm env term
      return [returnSingle expr]
    withArg body arg = case fullyStripTerm body of
      TermFunction (FunctionElimination (EliminationUnion (CaseStatement tname dflt cases))) -> do
          rt <- requireUnionType tname
          let isEnum = isEnumType rt
          let isFull = L.length cases >= L.length (rowTypeFields rt)
          pyArg <- encodeTerm env arg
          pyCases <- CM.mapM (toCaseBlock isEnum) cases
          pyDflt <- toDefault isFull dflt
          let subj = Py.SubjectExpressionSimple $ Py.NamedExpressionSimple pyArg
          return [Py.StatementCompound $ Py.CompoundStatementMatch $ Py.MatchStatement subj $ pyCases ++ pyDflt]
        where
          toDefault isFull dflt = if isFull
            then pure []
            else do
              stmt <- case dflt of
                Nothing -> pure $ raiseTypeError $ "Unsupported " ++ localNameOf tname
                Just d -> returnSingle <$> encodeTerm env d
              let patterns = pyClosedPatternToPyPatterns Py.ClosedPatternWildcard
              let body = indentedBlock Nothing [[stmt]]
              return [Py.CaseBlock patterns Nothing body]
          toCaseBlock isEnum (Field fname fterm) = case fullyStripTerm fterm of
            TermFunction (FunctionLambda (Lambda v _ body)) -> do
                pyReturn <- encodeTerm env body
                let body = indentedBlock Nothing [[returnSingle pyReturn]]
                return $ Py.CaseBlock (pyClosedPatternToPyPatterns pattern) Nothing body
              where
                pattern = if isEnum
                    then Py.ClosedPatternValue $ Py.ValuePattern $ Py.Attribute [
                      encodeName True CaseConventionPascal env tname,
                      encodeEnumValue env fname]
                    else Py.ClosedPatternClass
                      $ Py.ClassPattern pyVarName (Just $ Py.PositionalPatterns [argPattern]) Nothing
                  where
                    pyVarName = Py.NameOrAttribute [variantName True env tname fname]
                    argPattern = Py.PatternOr $ Py.OrPattern [
                      Py.ClosedPatternCapture $ Py.CapturePattern
                        $ Py.PatternCaptureTarget (encodeName False CaseConventionLowerSnake env v)]
            _ -> fail "unsupported case"
      _ -> dflt

encodeType :: PythonEnvironment -> Type -> Flow Graph Py.Expression
encodeType env typ = case stripType typ of
    TypeApplication at -> encodeApplicationType env at
    TypeFunction ft -> encodeFunctionType env ft
    TypeLambda lt -> encodeLambdaType env lt
    TypeList et -> nameAndParams (Py.Name "frozenlist") . L.singleton <$> encode et
    TypeMap (MapType kt vt) -> do
      pykt <- encode kt
      pyvt <- encode vt
      return $ nameAndParams (Py.Name "FrozenDict") [pykt, pyvt]
    TypeLiteral lt -> encodeLiteralType lt
    TypeOptional et -> orNull . pyExpressionToPyPrimary <$> encode et
    TypeRecord rt -> pure $ if isUnitType (TypeRecord rt)
      then pyNameToPyExpression pyNone
      else typeVariableReference env $ rowTypeTypeName rt
    TypeSet et -> nameAndParams (Py.Name "frozenset") . L.singleton <$> encode et
    TypeUnion rt -> pure $ typeVariableReference env $ rowTypeTypeName rt
    TypeVariable name -> pure $ typeVariableReference env name
    TypeWrap (WrappedType name _) -> pure $ typeVariableReference env name
    _ -> dflt
  where
    encode = encodeType env
    dflt = pure $ doubleQuotedString $ "type = " ++ show (stripType typ)

encodeTypeAssignment :: PythonEnvironment -> Name -> Type -> Maybe String -> Flow Graph [Py.Statement]
encodeTypeAssignment env name typ comment = encode env typ
  where
    encode env typ = case stripType typ of
      TypeLambda (LambdaType var body) -> encode newEnv body
        where
          (tparamList, tparamMap) = pythonEnvironmentBoundTypeVariables env
          newEnv = env {pythonEnvironmentBoundTypeVariables = (tparamList ++ [var], M.insert var (encodeTypeVariable var) tparamMap)}
      TypeRecord rt -> single <$> encodeRecordType env name rt comment
      TypeUnion rt -> encodeUnionType env name rt comment
      TypeWrap (WrappedType _ t) -> single <$> encodeWrappedType env name t comment
      t -> singleTypedef env <$> encodeType env t
    single st = [st]
    singleTypedef env e = single $ typeAliasStatement (encodeName False CaseConventionPascal env name) tparams comment e
      where
        tparams = environmentTypeParameters env

encodeTypeQuoted :: PythonEnvironment -> Type -> Flow Graph Py.Expression
encodeTypeQuoted env typ = do
  pytype <- encodeType env typ
  return $ if S.null (freeVariablesInType typ)
    then pytype
    else doubleQuotedString $ printExpr $ PySer.encodeExpression pytype

encodeUnionType :: PythonEnvironment -> Name -> RowType -> Maybe String -> Flow Graph [Py.Statement]
encodeUnionType env name rt@(RowType _ tfields) comment = if isEnumType rt then asEnum else asUnion
  where
    asEnum = do
        vals <- CM.mapM toVal tfields
        let body = indentedBlock comment vals
        return [pyClassDefinitionToPyStatement $ Py.ClassDefinition Nothing (encodeName False CaseConventionPascal env name) [] args body]
      where
        args = Just $ pyExpressionsToPyArgs [pyNameToPyExpression $ Py.Name "Enum"]
        toVal (FieldType fname ftype) = do
          fcomment <- fmap normalizeComment <$> getTypeDescription ftype
          return $ Y.catMaybes [
            Just $ assignmentToExpression (encodeEnumValue env fname) (doubleQuotedString $ unName fname),
            pyExpressionToPyStatement . tripleQuotedString <$> fcomment]
    asUnion = do
      fieldStmts <- CM.mapM toFieldStmt tfields
      return $ fieldStmts ++ [unionStmt]
      where
        toFieldStmt (FieldType fname ftype) = do
            comment <- fmap normalizeComment <$> getTypeDescription ftype
            ptypeQuoted <- encodeTypeQuoted env ftype
            let body = indentedBlock comment []
            return $ pyClassDefinitionToPyStatement $
              Py.ClassDefinition
                Nothing
                (variantName False env name fname)
                (pyNameToPyTypeParameter <$> fieldParams ftype)
                (Just $ variantArgs ptypeQuoted [])
                body
        unionStmt = typeAliasStatement
            (encodeName False CaseConventionPascal env name)
            tparams
            comment
            (orExpression (alt <$> tfields))
          where
            tparams = environmentTypeParameters env
            alt (FieldType fname ftype) = if L.null tparams
                then namePrim
                else primaryWithExpressionSlices namePrim (pyNameToPyExpression <$> tparams)
              where
                tparams = fieldParams ftype
                namePrim = pyNameToPyPrimary $ variantName False env name fname
        fieldParams ftype = encodeTypeVariable <$> findTypeParams env ftype

encodeWrappedType :: PythonEnvironment -> Name -> Type -> Maybe String -> Flow Graph Py.Statement
encodeWrappedType env name typ comment = do
    ptypeQuoted <- encodeTypeQuoted env typ
    let body = indentedBlock comment []
    return $ pyClassDefinitionToPyStatement $
      Py.ClassDefinition
        Nothing
        (encodeName False CaseConventionPascal env name)
        (pyNameToPyTypeParameter . encodeTypeVariable <$> findTypeParams env typ)
        (Just $ variantArgs ptypeQuoted tparamList)
        body
  where
    (tparamList, tparamMap) = pythonEnvironmentBoundTypeVariables env

environmentTypeParameters :: PythonEnvironment -> [Py.TypeParameter]
environmentTypeParameters env = pyNameToPyTypeParameter . encodeTypeVariable <$> (fst $ pythonEnvironmentBoundTypeVariables env)

findTypeParams :: PythonEnvironment -> Type -> [Name]
findTypeParams env typ = L.filter isBound $ S.toList $ freeVariablesInType typ
  where
    isBound v = Y.isJust $ M.lookup v $ snd $ pythonEnvironmentBoundTypeVariables env

gatherMetadata :: [Definition] -> PythonModuleMetadata
gatherMetadata defs = checkTvars $ L.foldl addDef start defs
  where
    checkTvars meta = meta {pythonModuleMetadataUsesTypeVar = not $ S.null $ pythonModuleMetadataTypeVariables meta}
    start = PythonModuleMetadata {
      pythonModuleMetadataTypeVariables = S.empty,
      pythonModuleMetadataUsesAnnotated = False,
      pythonModuleMetadataUsesCallable = False,
      pythonModuleMetadataUsesDataclass = False,
      pythonModuleMetadataUsesEnum = False,
      pythonModuleMetadataUsesFrozenDict = False,
      pythonModuleMetadataUsesFrozenList = False,
      pythonModuleMetadataUsesGeneric = False,
      pythonModuleMetadataUsesTypeVar = False,
      pythonModuleMetadataUsesNode = False}
    addDef meta def = case def of
      DefinitionTerm _ term _ -> foldOverTerm TraversalOrderPre extendMeta meta term
        where
          extendMeta meta t = case t of
            TermMap _ -> meta {pythonModuleMetadataUsesFrozenDict = True}
            _ -> meta
      DefinitionType _ typ -> foldOverType TraversalOrderPre extendMeta meta3 typ
        where
          tvars = pythonModuleMetadataTypeVariables meta
          meta3 = digForWrap typ
            where
              digForWrap typ = case stripType typ of
                TypeLambda (LambdaType _ body) -> digForWrap body
                TypeWrap _ -> meta2 {pythonModuleMetadataUsesNode = True}
                _ -> meta2
          meta2 = meta {pythonModuleMetadataTypeVariables = newTvars tvars typ}
            where
              newTvars s t = case stripType t of
                TypeLambda (LambdaType v body) -> newTvars (S.insert v s) body
                _ -> s
          extendMeta meta t = case t of
            TypeFunction _ -> meta {pythonModuleMetadataUsesCallable = True}
            TypeLambda (LambdaType _ body) -> case baseType body of
                TypeRecord _ -> meta {pythonModuleMetadataUsesGeneric = True}
                _ -> meta
              where
                baseType t = case stripType t of
                  TypeLambda (LambdaType _ body2) -> baseType body2
                  t2 -> t2
            TypeList _ -> meta {pythonModuleMetadataUsesFrozenList = True}
            TypeMap _ -> meta {pythonModuleMetadataUsesFrozenDict = True}
            TypeRecord (RowType _ fields) -> meta {
                pythonModuleMetadataUsesAnnotated = L.foldl checkForAnnotated (pythonModuleMetadataUsesAnnotated meta) fields,
                pythonModuleMetadataUsesDataclass = pythonModuleMetadataUsesDataclass meta || not (L.null fields)}
              where
                checkForAnnotated b (FieldType _ ft) = b || hasTypeDescription ft
            TypeUnion rt@(RowType _ fields) -> if isEnumType rt
                then meta {pythonModuleMetadataUsesEnum = True}
                else meta {
                  pythonModuleMetadataUsesNode = pythonModuleMetadataUsesNode meta || (not $ L.null fields)}
              where
                checkForLiteral b (FieldType _ ft) = b || isUnitType (stripType ft)
                checkForNewType b (FieldType _ ft) = b || not (isUnitType (stripType ft))
            _ -> meta

genericArg :: [Name] -> Y.Maybe Py.Expression
genericArg tparamList = if L.null tparamList
  then Nothing
  else Just $ pyPrimaryToPyExpression $ primaryWithExpressionSlices (pyNameToPyPrimary $ Py.Name "Generic")
    (pyNameToPyExpression . encodeTypeVariable <$> tparamList)

moduleToPython :: Module -> Flow Graph (M.Map FilePath String)
moduleToPython mod = do
  file <- encodeModule mod
  let s = printExpr $ parenthesize $ PySer.encodeModule file
  let path = namespaceToFilePath CaseConventionLowerSnake (FileExtension "py") $ moduleNamespace mod
  return $ M.fromList [(path, s)]

variantArgs :: Py.Expression -> [Name] -> Py.Args
variantArgs ptype tparams = pyExpressionsToPyArgs $ Y.catMaybes [
    Just $ pyPrimaryToPyExpression $
      primaryWithExpressionSlices (pyNameToPyPrimary $ Py.Name "Node") [ptype],
    genericArg tparams]
