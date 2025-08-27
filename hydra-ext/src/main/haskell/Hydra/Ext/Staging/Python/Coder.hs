module Hydra.Ext.Staging.Python.Coder (moduleToPython) where

import Hydra.Kernel
import Hydra.Ext.Python.Language
import Hydra.Dsl.Terms
import qualified Hydra.Ext.Python.Syntax as Py
import Hydra.Ext.Staging.Python.Names
import Hydra.Ext.Staging.Python.Utils
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Ext.Staging.Python.Serde as PySer
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Lib.Literals as Literals
import Hydra.Dsl.ShorthandTypes
import Hydra.Formatting

import qualified Control.Monad as CM
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y
import qualified Text.Read  as TR


-- | Temporary metadata which is used to create the header section of a Python file
data PythonModuleMetadata = PythonModuleMetadata {
  pythonModuleMetadataTypeVariables :: S.Set Name,
  pythonModuleMetadataUsesAnnotated :: Bool,
  pythonModuleMetadataUsesCallable :: Bool,
  pythonModuleMetadataUsesDataclass :: Bool,
  pythonModuleMetadataUsesDecimal :: Bool,
  pythonModuleMetadataUsesEnum :: Bool,
  pythonModuleMetadataUsesFrozenDict :: Bool,
  pythonModuleMetadataUsesFrozenList :: Bool,
  pythonModuleMetadataUsesGeneric :: Bool,
  pythonModuleMetadataUsesName :: Bool,
  pythonModuleMetadataUsesNode :: Bool,
  pythonModuleMetadataUsesTuple :: Bool,
  pythonModuleMetadataUsesTypeVar :: Bool}

argsAndBindings :: TypeContext -> Term -> Type -> Flow s ([Name], [Binding], Term, [Type], Type)
argsAndBindings tcontext term _ = gather tcontext [] [] [] term
  where
    gather tcontext prevArgs prevBindings prevDoms term = case deannotateTerm term of
      TermFunction (FunctionLambda (Lambda var (Just dom) body)) -> gather tcontext2 (var:prevArgs) prevBindings (dom:prevDoms) body
        where
          tcontext2 = tcontext {typeContextTypes = M.insert var dom (typeContextTypes tcontext)}
      TermLet (Let bindings body) -> gather tcontext2 prevArgs (L.reverse bindings ++ prevBindings) prevDoms body
        where
          tcontext2 = tcontext {typeContextTypes = M.union
            (typeContextTypes tcontext)
            (M.fromList $ fmap (\b -> (bindingName b, typeSchemeToFType $ Y.fromJust $ bindingType b)) bindings)}
      t -> do
        typ <- typeOf tcontext t
        return (L.reverse prevArgs, L.reverse prevBindings, t, L.reverse prevDoms, typ)

-- | Rewrite case statements in which the top-level lambda variables are re-used, e.g.
--   cases _Type Nothing [_Type_list>>: "t" ~> ..., _Type_set>>: "t" ~> ...].
--   Such case statements are legal in Hydra, but may lead to variable name collision in languages like Python.
deduplicateCaseVariables :: [Field] -> [Field]
deduplicateCaseVariables cases = L.reverse $ snd $ L.foldl rewriteCase (M.empty, []) cases
  where
    rewriteCase (countByName, done) (Field fname fterm) = case fterm of
      -- Note: does not yet take annotations into account
      TermFunction (FunctionLambda (Lambda v mt body)) -> case M.lookup v countByName of
        Nothing -> (M.insert v 1 countByName, (Field fname fterm):done)
        Just count -> (M.insert v count2 countByName, (Field fname rewritten):done)
          where
            count2 = count + 1
            v2 = Name (unName v ++ Literals.showInt32 count2)
            rewritten = TermFunction $ FunctionLambda $ Lambda v2 mt (alphaConvert v v2 body)
      _ -> (countByName, (Field fname fterm):done)

encodeApplication :: PythonEnvironment -> Application -> Flow Graph Py.Expression
encodeApplication env app = do
    g <- getState
    let arity = expansionArity g fun
    pargs <- CM.mapM (encodeTerm env) args
    let hargs = L.take arity pargs
    let rargs = L.drop arity pargs
    lhs <- applyArgs hargs
    return $ L.foldl (\t a -> functionCall (pyExpressionToPyPrimary t) [a]) lhs rargs
  where
    (fun, args) = gatherArgs (TermApplication app) []
    gatherArgs term args = case deannotateTerm term of
      TermApplication (Application lhs rhs) -> gatherArgs lhs (rhs:args)
      _ -> (term, args)
    applyArgs hargs = case fun of
        TermFunction f -> case f of
          FunctionElimination elm -> case elm of
    --        EliminationProduct ...
            EliminationRecord (Projection _ fname) -> do
              return $ projectFromExpression parg $ encodeFieldName env fname
            EliminationUnion (CaseStatement tname mdef cases) -> do
              return $ stringToPyExpression Py.QuoteStyleDouble "inline match expressions are unsupported"
            EliminationWrap _ -> do
              return $ projectFromExpression parg $ Py.Name "value"
            _ -> fail $ "elimination variant is not yet supported in applications: " ++ show (eliminationVariant elm)
          FunctionPrimitive name -> do
            return $ functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) hargs
          _ -> def
        TermVariable name -> do -- Special-casing variables prevents quoting; forward references are allowed for function calls
          return $ functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) hargs
        _ -> def
      where
        parg = L.head hargs
        def = do
          pfun <- encodeTerm env fun
          return $ functionCall (pyExpressionToPyPrimary pfun) hargs

encodeApplicationType :: PythonEnvironment -> ApplicationType -> Flow Graph Py.Expression
encodeApplicationType env at = do
    pyBody <- encodeType env body
    pyArgs <- CM.mapM (encodeType env) args
    return $ primaryAndParams (pyExpressionToPyPrimary pyBody) pyArgs
  where
    (body, args) = gatherParams (TypeApplication at) []
    gatherParams t ps = case deannotateType t of
      TypeApplication (ApplicationType lhs rhs) -> gatherParams lhs (rhs:ps)
      _ -> (t, ps)

encodeDefinition :: PythonEnvironment -> Definition -> Flow Graph [[Py.Statement]]
encodeDefinition env def = case def of
  DefinitionTerm (TermDefinition name term typ) -> withTrace ("data element " ++ unName name) $ do
    comment <- fmap normalizeComment <$> getTermDescription term
    g <- getState
    stmts <- encodeTermAssignment env name term typ comment
    return [stmts]
  DefinitionType (TypeDefinition name typ) -> withTrace ("type element " ++ unName name) $ do
--  DefinitionType (TypeDefinition name typ) -> withTrace ("type element " ++ unName name ++ ": " ++ ShowCore.type_ typ) $ do
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

encodeFloatValue :: FloatValue -> Flow s Py.Expression
encodeFloatValue fv = case fv of
  FloatValueBigfloat f -> pure $ functionCall (pyNameToPyPrimary $ Py.Name "Decimal") [singleQuotedString $ show f]
  FloatValueFloat64 f -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberFloat $ realToFrac f
  _ -> fail $ "unsupported floating point type: " ++ show (floatValueType fv)

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
    returnType <- getType cod
    return $ Py.StatementCompound $ Py.CompoundStatementFunction $ Py.FunctionDefinition Nothing
      $ Py.FunctionDefRaw False (encodeName False CaseConventionLowerSnake env name) [] (Just params) (Just returnType) Nothing block
  where
    toParam name typ = do
      pyTyp <- getType typ
      return $ Py.ParamNoDefault (Py.Param (encodeName False CaseConventionLowerSnake env name) $ Just $ Py.Annotation pyTyp) Nothing -- TODO
    getType typ = case deannotateType typ of
      TypeVariable v -> case M.lookup v (typeContextTypes $ pythonEnvironmentTypeContext env) of
        Nothing -> encodeType env $ TypeVariable v
        Just t -> encodeType env t
      t -> encodeType env t

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
    gatherParams rdoms (FunctionType dom cod) = case deannotateType cod of
      TypeFunction ft2 -> gatherParams (dom:rdoms) ft2
      _ -> (L.reverse (dom:rdoms), cod)

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

encodeForallType :: PythonEnvironment -> ForallType -> Flow Graph Py.Expression
encodeForallType env lt = do
    pyBody <- encodeType env body
    return $ primaryAndParams (pyExpressionToPyPrimary pyBody) (pyNameToPyExpression . Py.Name . unName <$> params)
  where
    (body, params) = gatherParams (TypeForall lt) []
    gatherParams t ps = case deannotateType t of
      TypeForall (ForallType name body) -> gatherParams body (name:ps)
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
        FloatTypeBigfloat -> pure "Decimal"
        FloatTypeFloat64 -> pure "float"
        _ -> fail $ "unsupported floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> pure "int"
        _ -> fail $ "unsupported integer type: " ++ show it
      LiteralTypeString -> pure "str"

encodeModule :: Module -> [Definition] -> Flow Graph Py.Module
encodeModule mod defs0 = do
    let defs = reorderDefs defs0
    let meta = gatherMetadata defs
    let namespaces = findNamespaces defs meta
    let tvars = pythonModuleMetadataTypeVariables meta
    let importStmts = imports namespaces meta
    let tvarStmts = tvarStmt . encodeTypeVariable <$> S.toList tvars
    let mc = tripleQuotedString . normalizeComment <$> moduleDescription mod
    let commentStmts = case normalizeComment <$> moduleDescription mod of
                       Nothing -> []
                       Just c -> [commentStatement c]
    g <- getState
    tcontext <- initialTypeContext g
    let env = PythonEnvironment {
                pythonEnvironmentNamespaces = namespaces,
                pythonEnvironmentBoundTypeVariables = ([], M.empty),
                pythonEnvironmentTypeContext = tcontext}
    defStmts <- L.concat <$> (CM.mapM (encodeDefinition env) defs)

    let body = L.filter (not . L.null) $ [commentStmts, importStmts, tvarStmts] ++ defStmts
    return $ Py.Module body
  where
    findNamespaces defs meta = if fst (namespacesFocus namespaces) == coreNs
        then namespaces
        else namespaces {namespacesMapping = M.insert coreNs (encodeNamespace coreNs) $ namespacesMapping namespaces}
      where
        coreNs = Namespace "hydra.core"
        namespaces = namespacesForDefinitions encodeNamespace (moduleNamespace mod) defs
    reorderDefs defs = fst p ++ snd p
      where
        p = L.partition isNameDef defs
        isNameDef d = case d of
          DefinitionType (TypeDefinition name _) -> name == _Name
          _ -> False

    tvarStmt name = assignmentStatement name $ functionCall (pyNameToPyPrimary $ Py.Name "TypeVar")
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
                ("decimal", [
                  cond "Decimal" $ pythonModuleMetadataUsesDecimal meta]),
                ("enum", [
                  cond "Enum" $ pythonModuleMetadataUsesEnum meta]),
                ("hydra.dsl.python", [
                  cond "FrozenDict" $ pythonModuleMetadataUsesFrozenDict meta,
                  cond "frozenlist" $ pythonModuleMetadataUsesFrozenList meta,
                  cond "Node" $ pythonModuleMetadataUsesNode meta]),
                ("typing", [
                  cond "Annotated" $ pythonModuleMetadataUsesAnnotated meta,
                  cond "Generic" $ pythonModuleMetadataUsesGeneric meta,
                  cond "Tuple" $ pythonModuleMetadataUsesTuple meta,
                  cond "TypeVar" $ pythonModuleMetadataUsesTypeVar meta])]
              where
                cond name b = if b then Just name else Nothing
            toImport (modName, symbols) = Py.ImportStatementFrom $
                Py.ImportFrom [] (Just $ Py.DottedName [Py.Name modName]) $
                  Py.ImportFromTargetsSimple (forSymbol <$> symbols)
              where
                forSymbol s = Py.ImportFromAsName (Py.Name s) Nothing

encodeNameConstants :: PythonEnvironment -> Name -> Type -> [Py.Statement]
encodeNameConstants env name typ = toStmt <$> (namePair:(fieldPairs typ))
  where
    toStmt (pname, hname) = assignmentStatement pname $
--      doubleQuotedString $ unName hname -- TODO

      functionCall (pyNameToPyPrimary $ encodeName True CaseConventionPascal env _Name)
        [doubleQuotedString $ unName hname]


    namePair = (encodeConstantForTypeName env name, name)
    fieldPair field = (encodeConstantForFieldName env name $ fieldTypeName field, fieldTypeName field)
    fieldPairs typ = case deannotateType typ of
      TypeForall (ForallType _ body) -> fieldPairs body
      TypeRecord rt -> fieldPair <$> rowTypeFields rt
      TypeUnion rt -> fieldPair <$> rowTypeFields rt
      _ -> []

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
encodeTerm env term = case deannotateTerm term of
    TermApplication a -> encodeApplication env a
    TermFunction f -> encodeFunction env f
    TermLet _ -> pure $ stringToPyExpression Py.QuoteStyleDouble "let terms are not supported here"
    TermList terms -> do
      pyExprs <- CM.mapM encode terms
      return $ pyAtomToPyExpression $ Py.AtomTuple $ Py.Tuple (pyExpressionToPyStarNamedExpression <$> pyExprs)
--      return $ functionCall (pyNameToPyPrimary $ Py.Name "tuple") [pl]
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
    TermTypeLambda (TypeLambda _ term1) -> encode term1
    TermTypeApplication (TypedTerm term1 _) -> encode term1
    TermUnion (Injection tname field) -> do
      rt <- requireUnionType tname
      if isEnumRowType rt
        then return $ projectFromExpression (pyNameToPyExpression $ encodeNameQualified env tname)
          $ encodeEnumValue env $ fieldName field
        else do
          parg <- encode $ fieldTerm field
          return $ functionCall (pyNameToPyPrimary $ variantName True env tname (fieldName field)) [parg]
    TermUnit -> return $ pyNameToPyExpression pyNone
    TermVariable name -> do
      g <- getState
      return $ case lookupElement g name of
        -- Lambda-bound variables
        Nothing -> termVariableReference env name
        -- Let-bound variables
        Just el -> if isNullaryFunction el
          then functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) []
          else termVariableReference env name
    TermWrap (WrappedTerm tname term1) -> do
      parg <- encode term1
      return $ functionCall (pyNameToPyPrimary $ encodeNameQualified env tname) [parg]
    t -> fail $ "unsupported term variant: " ++ show (termVariant t) ++ " in " ++ show term
  where
    encode = encodeTerm env

encodeTermAssignment :: PythonEnvironment -> Name -> Term -> Type -> Maybe String -> Flow Graph [Py.Statement]
encodeTermAssignment env name term typ comment = do
    (args, bindings, body, doms, cod) <- argsAndBindings (pythonEnvironmentTypeContext env) term typ -- TODO: also modify TypeContext
    if L.null args && L.null bindings
    -- If there are no arguments or let bindings, use a simple a = b assignment.
    then do
      bodyExpr <- encodeTerm env body
      return [annotatedStatement comment $ assignmentStatement (encodeName False CaseConventionLowerSnake env name) bodyExpr]
    -- If there are either arguments or let bindings, then only a function definition will work.
    else do
        -- TODO: topological sort of bindings
        bindingStmts <- L.concat <$> CM.mapM encodeBinding bindings
        g <- getState
        withState (extendGraphWithBindings bindings g) $ do
          bodyStmt <- encodeFunctionDefinition env name args body doms cod comment bindingStmts
          return [bodyStmt]
  where
    encodeBinding (Binding name1 term1 mts) = do
        comment <- fmap normalizeComment <$> getTermDescription term1
        typ1 <- case mts of
          Nothing -> fail $ "missing type for let binding " ++ unName name1 ++ " in " ++ unName name
          Just ts -> return $ typeSchemeType ts
        encodeTermAssignment env name1 term1 typ1 comment

encodeTopLevelTerm :: PythonEnvironment -> Term -> Flow Graph [Py.Statement]
encodeTopLevelTerm env term = if L.length args == 1
    then withArg body (L.head args)
    else dflt
  where
    (args, body) = gatherArgs [] term
    gatherArgs rest term = case deannotateTerm term of
      TermApplication (Application l r) -> gatherArgs (r:rest) l
      t -> (rest, t)
    dflt = do
      expr <- encodeTerm env term
      return [returnSingle expr]
    withArg body arg = case deannotateTerm body of
      TermFunction (FunctionElimination (EliminationUnion (CaseStatement tname dflt cases))) -> do
          rt <- requireUnionType tname
          let isEnum = isEnumRowType rt
          let isFull = L.length cases >= L.length (rowTypeFields rt)
          pyArg <- encodeTerm env arg
          pyCases <- CM.mapM (toCaseBlock isEnum) $ deduplicateCaseVariables cases
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
          toCaseBlock isEnum (Field fname fterm) = case deannotateTerm fterm of
            TermFunction (FunctionLambda (Lambda v _ body)) -> do
                pyReturn <- encodeTerm env body
                let body = indentedBlock Nothing [[returnSingle pyReturn]]
                return $ Py.CaseBlock (pyClosedPatternToPyPatterns pattern) Nothing body
              where
                pattern = if isEnum
                    then Py.ClosedPatternValue $ Py.ValuePattern $ Py.Attribute [
                      encodeName True CaseConventionPascal env tname,
                      encodeEnumValue env fname]
                    else if isFreeVariableInTerm v body
                    then Py.ClosedPatternClass $
                      Py.ClassPattern pyVarName Nothing Nothing
                    else Py.ClosedPatternClass $
                      Py.ClassPattern pyVarName Nothing (Just $ Py.KeywordPatterns [argPattern])
                  where
                    pyVarName = Py.NameOrAttribute [variantName True env tname fname]
                    argPattern = Py.KeywordPattern (Py.Name "value") $ Py.PatternOr $ Py.OrPattern [
                      Py.ClosedPatternCapture $ Py.CapturePattern
                        $ Py.PatternCaptureTarget (encodeName False CaseConventionLowerSnake env v)]
            _ -> fail "unsupported case"
      _ -> dflt

encodeType :: PythonEnvironment -> Type -> Flow Graph Py.Expression
encodeType env typ = case deannotateType typ of
--encodeType env typ = withTrace ("encode type: " <> ShowCore.type_ typ) $ case deannotateType typ of
    TypeApplication at -> encodeApplicationType env at
    TypeFunction ft -> encodeFunctionType env ft
    TypeForall lt -> encodeForallType env lt
    TypeList et -> nameAndParams (Py.Name "frozenlist") . L.singleton <$> encode et
    TypeMap (MapType kt vt) -> do
      pykt <- encode kt
      pyvt <- encode vt
      return $ nameAndParams (Py.Name "FrozenDict") [pykt, pyvt]
    TypeLiteral lt -> encodeLiteralType lt
    TypeOptional et -> orNull . pyExpressionToPyPrimary <$> encode et
    TypeProduct types -> nameAndParams (Py.Name "Tuple") <$> (CM.mapM encode types)
    TypeRecord rt -> pure $ typeVariableReference env $ rowTypeTypeName rt
    TypeSet et -> nameAndParams (Py.Name "frozenset") . L.singleton <$> encode et
    TypeUnion rt -> pure $ typeVariableReference env $ rowTypeTypeName rt
    TypeUnit -> pure $ pyNameToPyExpression pyNone
    TypeVariable name -> pure $ typeVariableReference env name
    TypeWrap (WrappedType name _) -> pure $ typeVariableReference env name
    _ -> dflt
  where
    encode = encodeType env
    dflt = pure $ doubleQuotedString $ "type = " ++ ShowCore.type_ (deannotateType typ)

encodeTypeAssignment :: PythonEnvironment -> Name -> Type -> Maybe String -> Flow Graph [[Py.Statement]]
encodeTypeAssignment env name typ comment = do
    defStmts <- encode env typ
    let constStmts = encodeNameConstants env name typ
    return $ (pure <$> defStmts) ++ [constStmts]
  where
    encode env typ = case deannotateType typ of
      TypeForall (ForallType var body) -> encode newEnv body
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
encodeUnionType env name rt@(RowType _ tfields) comment = if isEnumRowType rt then asEnum else asUnion
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
            Just $ assignmentStatement (encodeEnumValue env fname) (doubleQuotedString $ unName fname),
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
      pythonModuleMetadataUsesDecimal = False,
      pythonModuleMetadataUsesEnum = False,
      pythonModuleMetadataUsesFrozenDict = False,
      pythonModuleMetadataUsesFrozenList = False,
      pythonModuleMetadataUsesGeneric = False,
      pythonModuleMetadataUsesName = False,
      pythonModuleMetadataUsesNode = False,
      pythonModuleMetadataUsesTuple = False,
      pythonModuleMetadataUsesTypeVar = False}
    addDef meta def = case def of
      DefinitionTerm (TermDefinition _ term typ) -> foldOverTerm TraversalOrderPre extendMetaForTerm (extendMetaForType True meta typ) term
      DefinitionType (TypeDefinition _ typ) -> foldOverType TraversalOrderPre (extendMetaForType False) meta2 typ
        where
          meta2 = meta {pythonModuleMetadataUsesName = True}
    extendMetaForTerm meta t = case t of
      TermLet (Let bindings _) -> L.foldl forBinding meta bindings
        where
          forBinding meta (Binding _ _ mts) = case mts of
            Nothing -> meta
            Just ts -> extendMetaForType True meta $ typeSchemeType ts
      TermLiteral l -> case l of
        LiteralFloat fv -> case fv of
          FloatValueBigfloat _ -> meta {pythonModuleMetadataUsesDecimal = True}
          _ -> meta
        _ -> meta
      TermMap _ -> meta {pythonModuleMetadataUsesFrozenDict = True}
      _ -> meta
    extendMetaForType isTermAnnot meta typ = extendFor meta3 typ
      where
        tvars = pythonModuleMetadataTypeVariables meta
        meta3 = digForWrap typ
          where
            digForWrap typ = case deannotateType typ of
              TypeForall (ForallType _ body) -> digForWrap body
              TypeWrap _ -> if isTermAnnot
                -- No need to import Node for instantiations of a Node type, e.g.
                --   placeholder_name = hydra.core.Name("Placeholder")
                then meta2
                else meta2 {pythonModuleMetadataUsesNode = True}
              _ -> meta2
        meta2 = meta {pythonModuleMetadataTypeVariables = newTvars tvars typ}
          where
            newTvars s t = case deannotateType t of
              TypeForall (ForallType v body) -> newTvars (S.insert v s) body
              _ -> s
        extendFor meta t = case t of
          TypeFunction _ -> if isTermAnnot
            -- If a particular term has a function type, don't import Callable; Python has special "def" syntax for functions.
            then meta
            -- If this is a type-level definition, or an *argument* to a function is a function, then we need Callable.
            else meta {pythonModuleMetadataUsesCallable = True}
          TypeForall (ForallType _ body) -> case baseType body of
              TypeRecord _ -> meta {pythonModuleMetadataUsesGeneric = True}
              _ -> meta
            where
              baseType t = case deannotateType t of
                TypeForall (ForallType _ body2) -> baseType body2
                t2 -> t2
          TypeList _ -> meta {pythonModuleMetadataUsesFrozenList = True}
          TypeLiteral lt -> case lt of
            LiteralTypeFloat ft -> case ft of
              FloatTypeBigfloat -> meta {pythonModuleMetadataUsesDecimal = True}
              _ -> meta
            _ -> meta
          TypeMap _ -> meta {pythonModuleMetadataUsesFrozenDict = True}
          TypeProduct _ -> meta {pythonModuleMetadataUsesTuple = True}
          TypeRecord (RowType _ fields) -> meta {
              pythonModuleMetadataUsesAnnotated = L.foldl checkForAnnotated (pythonModuleMetadataUsesAnnotated meta) fields,
              pythonModuleMetadataUsesDataclass = pythonModuleMetadataUsesDataclass meta || not (L.null fields)}
            where
              checkForAnnotated b (FieldType _ ft) = b || hasTypeDescription ft
          TypeUnion rt@(RowType _ fields) -> if isEnumRowType rt
              then meta {pythonModuleMetadataUsesEnum = True}
              else meta {
                pythonModuleMetadataUsesNode = pythonModuleMetadataUsesNode meta || (not $ L.null fields)}
            where
              checkForLiteral b (FieldType _ ft) = b || EncodeCore.isUnitType (deannotateType ft)
              checkForNewType b (FieldType _ ft) = b || not (EncodeCore.isUnitType (deannotateType ft))
          _ -> meta

genericArg :: [Name] -> Y.Maybe Py.Expression
genericArg tparamList = if L.null tparamList
  then Nothing
  else Just $ pyPrimaryToPyExpression $ primaryWithExpressionSlices (pyNameToPyPrimary $ Py.Name "Generic")
    (pyNameToPyExpression . encodeTypeVariable <$> tparamList)

isNullaryFunction :: Binding -> Bool
isNullaryFunction el = False -- TODO: determine whether distinguishing between "nullary functions" and constants is actually necessary

moduleToPython :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToPython mod defs = do
  file <- encodeModule mod defs
  let s = printExpr $ parenthesize $ PySer.encodeModule file
  let path = namespaceToFilePath CaseConventionLowerSnake (FileExtension "py") $ moduleNamespace mod
  return $ M.fromList [(path, s)]

variantArgs :: Py.Expression -> [Name] -> Py.Args
variantArgs ptype tparams = pyExpressionsToPyArgs $ Y.catMaybes [
    Just $ pyPrimaryToPyExpression $
      primaryWithExpressionSlices (pyNameToPyPrimary $ Py.Name "Node") [ptype],
    genericArg tparams]
