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
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Sorting as Sorting
import Hydra.Dsl.ShorthandTypes
import Hydra.Formatting
import Hydra.Sources.Libraries

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
  pythonModuleMetadataUsesCast :: Bool,
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

data PyGraph = PyGraph {
  pyGraphGraph :: Graph,
  pyGraphMetadata :: PythonModuleMetadata}

-- | Rewrite case statements in which the top-level lambda variables are re-used, e.g.
--   cases _Type Nothing [_Type_list>>: "t" ~> ..., _Type_set>>: "t" ~> ...].
--   Such case statements are legal in Hydra, but may lead to variable name collision in languages like Python.
deduplicateCaseVariables :: [Field] -> [Field]
deduplicateCaseVariables cases = L.reverse $ snd $ L.foldl rewriteCase (M.empty, []) cases
  where
    rewriteCase (countByName, done) (Field fname fterm) = case fterm of
      -- Note: does not yet take annotations into account
      TermFunction (FunctionLambda (Lambda v (Just dom) body)) -> case M.lookup v countByName of
        Nothing -> (M.insert v 1 countByName, (Field fname fterm):done)
        Just count -> (M.insert v count2 countByName, (Field fname rewritten):done)
          where
            count2 = count + 1
            v2 = Name (unName v ++ Literals.showInt32 count2)
            rewritten = TermFunction $ FunctionLambda $ Lambda v2 (Just dom) (alphaConvert v v2 body)
      _ -> (countByName, (Field fname fterm):done)

encodeApplication :: PythonEnvironment -> Application -> Flow PyGraph Py.Expression
encodeApplication env app = do
    PyGraph g _ <- getState
    arity <- typeArity <$> (typeOf (pythonEnvironmentTypeContext env) [] fun)
    let term = TermApplication app
    typ <- typeOf (pythonEnvironmentTypeContext env) [] term
    pargs <- CM.mapM (encodeTermInline env) args
    let hargs = L.take arity pargs
    let rargs = L.drop arity pargs
    lhs <- applyArgs hargs
    let pyapp = L.foldl (\t a -> functionCall (pyExpressionToPyPrimary t) [a]) lhs rargs
    if needsCast term typ then do
      updateMeta $ \m -> extendMetaForType True True typ $ m { pythonModuleMetadataUsesCast = True }
      pytyp <- encodeType env typ
      return $ functionCall (pyNameToPyPrimary $ Py.Name "cast") [pytyp, pyapp]
    else
      return pyapp
  where
    -- Note: this extreme special case is to be expanded as needed
    needsCast term typ = case typ of
      TypeMap _ -> case deannotateAndDetypeTerm term of
        TermMap _ -> False
        t -> t == TermFunction (FunctionPrimitive _maps_fromList)
          where
            f = fst $ gatherArgs t []
      _ -> False
    (fun, args) = gatherArgs (TermApplication app) []
    applyArgs hargs = case fun of
        TermFunction f -> case f of
          FunctionElimination elm -> case elm of
            EliminationProduct (TupleProjection arity idx _) -> do
              pyIdx <- encodeIntegerValue $ IntegerValueInt32 idx
              return $ pyPrimaryToPyExpression $ primaryWithExpressionSlices (pyExpressionToPyPrimary parg) [pyIdx]
            EliminationRecord (Projection _ fname) -> do
              return $ projectFromExpression parg $ encodeFieldName env fname
            EliminationUnion (CaseStatement tname mdef cases) -> do
              return $ stringToPyExpression Py.QuoteStyleDouble "inline match expressions are unsupported"
            EliminationWrap _ -> do
              return $ projectFromExpression parg $ Py.Name "value"
          FunctionPrimitive name -> do
            return $ functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) hargs
          _ -> def
        -- Special-casing variables prevents quoting; forward references are allowed for function calls
        TermVariable name -> return $ if L.null hargs
          then termVariableReference env name
          else functionCall (pyNameToPyPrimary $ encodeName True CaseConventionLowerSnake env name) hargs
        _ -> def
      where
        parg = L.head hargs
        def = do
          pfun <- encodeTermInline env fun
          return $ functionCall (pyExpressionToPyPrimary pfun) hargs

encodeApplicationType :: PythonEnvironment -> ApplicationType -> Flow PyGraph Py.Expression
encodeApplicationType env at = do
    pyBody <- encodeType env body
    pyArgs <- CM.mapM (encodeType env) args
    return $ primaryAndParams (pyExpressionToPyPrimary pyBody) pyArgs
  where
    (body, args) = gatherParams (TypeApplication at) []
    gatherParams t ps = case deannotateType t of
      TypeApplication (ApplicationType lhs rhs) -> gatherParams lhs (rhs:ps)
      _ -> (t, ps)

encodeBinding :: PythonEnvironment -> Binding -> Flow PyGraph Py.Statement
encodeBinding env (Binding name1 term1 mts) = do
  comment <- fmap normalizeComment <$> (inGraphContext $ getTermDescription term1)
  encodeTermAssignment env name1 term1 comment

-- TODO: topological sort of bindings
encodeBindings :: PythonEnvironment -> [Binding] -> Flow PyGraph [Py.Statement]
encodeBindings env bindings = CM.mapM (encodeBinding env) bindings

encodeDefinition :: PythonEnvironment -> Definition -> Flow PyGraph [[Py.Statement]]
encodeDefinition env def = case def of
  DefinitionTerm (TermDefinition name term _) -> withTrace ("data element " ++ unName name) $ do
--  DefinitionTerm (TermDefinition name term _) -> withTrace ("data element " ++ unName name
--    ++ ": " ++ ShowCore.term term) $ do

--    if name == Name "hydra.show.core.fields"
--    then fail $ "fields: " ++ ShowCore.term term
--    else return ()

    comment <- fmap normalizeComment <$> (inGraphContext $ getTermDescription term)
    stmt <- encodeTermAssignment env name term comment
    return [[stmt]]
  DefinitionType (TypeDefinition name typ) -> withTrace ("type element " ++ unName name) $ do
--  DefinitionType (TypeDefinition name typ) -> withTrace ("type element " ++ unName name ++ ": " ++ ShowCore.type_ typ) $ do
    comment <- fmap normalizeComment <$> (inGraphContext $ getTypeDescription typ)
    encodeTypeAssignment env name typ comment

encodeField :: PythonEnvironment -> Field -> Flow PyGraph (Py.Name, Py.Expression)
encodeField env (Field fname fterm) = do
  pterm <- encodeTermInline env fterm
  return (encodeFieldName env fname, pterm)

encodeFieldType :: PythonEnvironment -> FieldType -> Flow PyGraph Py.Statement
encodeFieldType env (FieldType fname ftype) = do
  comment <- inGraphContext $ getTypeDescription ftype
  let pyName = Py.SingleTargetName $ encodeFieldName env fname
  pyType <- annotatedExpression comment <$> encodeType env ftype
  return $ pyAssignmentToPyStatement $ Py.AssignmentTyped $ Py.TypedAssignment pyName pyType Nothing

encodeFloatValue :: FloatValue -> Flow s Py.Expression
encodeFloatValue fv = case fv of
  FloatValueBigfloat f -> pure $ functionCall (pyNameToPyPrimary $ Py.Name "Decimal") [singleQuotedString $ show f]
  FloatValueFloat64 f -> pure $ pyAtomToPyExpression $ Py.AtomNumber $ Py.NumberFloat $ realToFrac f
  _ -> fail $ "unsupported floating point type: " ++ show (floatValueType fv)

encodeFunction :: PythonEnvironment -> Function -> Flow PyGraph Py.Expression
encodeFunction env f = case f of
  FunctionLambda lam@(Lambda var (Just dom) body) -> do
      pbody <- encodeTermInline env2 body
      return $ Py.ExpressionLambda $ Py.Lambda (Py.LambdaParameters Nothing [] [] $
        Just $ Py.LambdaStarEtcParamNoDefault $ Py.LambdaParamNoDefault $ encodeNameQualified env2 var) pbody
    where
      env2 = extendEnvironmentForLambda env lam
  FunctionPrimitive name -> pure $ pyNameToPyExpression $ encodeName True CaseConventionLowerSnake env name -- Only nullary primitives should appear here.
  _ -> fail $ "unexpected function variant: " ++ show (functionVariant f)

encodeFunctionDefinition :: PythonEnvironment -> Name -> [Name] -> [Name] -> Term -> [Type] -> Type -> Maybe String -> [Py.Statement] -> Flow PyGraph Py.Statement
encodeFunctionDefinition env name tparams args body doms cod comment prefixes = do
    pyArgs <- CM.zipWithM toParam args doms
    let params = Py.ParametersParamNoDefault $ Py.ParamNoDefaultParameters pyArgs [] Nothing
    stmts <- encodeTermMultiline env body
    let block = indentedBlock comment [prefixes ++ stmts]
    returnType <- getType cod
    let pyTparams = fmap (pyNameToPyTypeParameter . encodeTypeVariable) tparams

    updateMeta $ extendMetaForTypes (cod:doms)

--    (PyGraph _ metax) <- getState
--    if name == Name "hydra.annotations.aggregateAnnotations"
--    then fail $ "body: " ++ ShowCore.term body
--      ++ "\ntparams: " ++ L.intercalate ", " (fmap unName tparams)
--      ++ "\nargs: " ++ L.intercalate ", " (fmap unName args)
--      ++ "\ndoms: " ++ L.intercalate ", " (fmap ShowCore.type_ doms)
--      ++ "\ncod: " ++ ShowCore.type_ cod
--      ++ "\nuses callable: " ++ show (pythonModuleMetadataUsesCallable metax)
--    else return ()

    return $ Py.StatementCompound $ Py.CompoundStatementFunction $ Py.FunctionDefinition Nothing $
      Py.FunctionDefRaw False (encodeName False CaseConventionLowerSnake env name) pyTparams (Just params) (Just returnType) Nothing block
  where
    toParam name typ = do
      pyTyp <- getType typ
      return $ Py.ParamNoDefault (Py.Param (encodeName False CaseConventionLowerSnake env name) $ Just $ Py.Annotation pyTyp) Nothing -- TODO
    getType typ = case deannotateType typ of
      TypeVariable v -> case M.lookup v (typeContextTypes $ pythonEnvironmentTypeContext env) of
        Nothing -> encodeType env $ TypeVariable v
        Just t -> encodeType env t
      t -> encodeType env t

encodeFunctionType :: PythonEnvironment -> FunctionType -> Flow PyGraph Py.Expression
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

encodeForallType :: PythonEnvironment -> ForallType -> Flow PyGraph Py.Expression
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

encodeLiteralType :: LiteralType -> Flow PyGraph Py.Expression
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
    let meta0 = gatherMetadata defs
    g <- getState
    withState (PyGraph g meta0) $ do
      let namespaces = findNamespaces defs meta0
      let mc = tripleQuotedString . normalizeComment <$> moduleDescription mod
      tcontext <- initialTypeContext g
      let env = PythonEnvironment {
                  pythonEnvironmentNamespaces = namespaces,
                  pythonEnvironmentBoundTypeVariables = ([], M.empty),
                  pythonEnvironmentTypeContext = tcontext}
      defStmts <- L.concat <$> (CM.mapM (encodeDefinition env) defs)
      PyGraph _ meta <- getState -- get metadata after definitions, which may have altered it
--      fail $ "uses callable: " ++ show (pythonModuleMetadataUsesCallable meta)

      let commentStmts = case normalizeComment <$> moduleDescription mod of
                         Nothing -> []
                         Just c -> [commentStatement c]
      let importStmts = imports namespaces meta
      let tvars = pythonModuleMetadataTypeVariables meta
      let tvarStmts = tvarStmt . encodeTypeVariable <$> S.toList tvars
      let body = L.filter (not . L.null) $ [commentStmts, importStmts, tvarStmts] ++ defStmts
      return $ Py.Module body
  where
    findNamespaces defs meta = if fst (namespacesFocus namespaces) == coreNs
        then namespaces
        else namespaces {namespacesMapping = M.insert coreNs (encodeNamespace coreNs) $ namespacesMapping namespaces}
      where
        coreNs = Namespace "hydra.core"
        namespaces = namespacesForDefinitions encodeNamespace (moduleNamespace mod) defs
    reorderDefs defs = sortedTypeDefs ++ sortedTermDefs
      where
        p1 = L.partition isTypeDef defs
          where
            isTypeDef d = case d of
              DefinitionType _ -> True
              _ -> False
        sortedTypeDefs = fst p2 ++ snd p2
          where
            p2 = L.partition isNameDef (fst p1)
            isNameDef d = case d of
              DefinitionType (TypeDefinition name _) -> name == _Name
              _ -> False
        sortedTermDefs = L.concat $ Sorting.topologicalSortNodes getKey getAdj $ snd p1
          where
            getKey def = case def of
              DefinitionTerm (TermDefinition name _ _) -> name
--              DefinitionType (TypeDefinition name _) -> name
            getAdj def = case def of
              DefinitionTerm (TermDefinition _ term _) -> S.toList $ Rewriting.freeVariablesInTerm term
--              DefinitionType (TypeDefinition _ typ) -> S.toList $ Rewriting.freeVariablesInType typ

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
                  cond "TypeVar" $ pythonModuleMetadataUsesTypeVar meta,
                  cond "cast" $ pythonModuleMetadataUsesCast meta])]
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
      functionCall (pyNameToPyPrimary $ encodeName True CaseConventionPascal env _Name)
        [doubleQuotedString $ unName hname]

    namePair = (encodeConstantForTypeName env name, name)
    fieldPair field = (encodeConstantForFieldName env name $ fieldTypeName field, fieldTypeName field)
    fieldPairs typ = case deannotateType typ of
      TypeForall (ForallType _ body) -> fieldPairs body
      TypeRecord rt -> fieldPair <$> rowTypeFields rt
      TypeUnion rt -> fieldPair <$> rowTypeFields rt
      _ -> []

encodeRecordType :: PythonEnvironment -> Name -> RowType -> Maybe String -> Flow PyGraph Py.Statement
encodeRecordType env name (RowType _ tfields) comment = do
    pyFields <- CM.mapM (encodeFieldType env) tfields
    let body = indentedBlock comment [pyFields]
    return $ pyClassDefinitionToPyStatement $
      Py.ClassDefinition (Just decs) (encodeName False CaseConventionPascal env name) [] args body
  where
    (tparamList, tparamMap) = pythonEnvironmentBoundTypeVariables env
    args = fmap (\a -> pyExpressionsToPyArgs [a]) $ genericArg tparamList
    decs = Py.Decorators [pyNameToPyNamedExpression $ Py.Name "dataclass"]

encodeTermAssignment :: PythonEnvironment -> Name -> Term -> Maybe String -> Flow PyGraph Py.Statement
encodeTermAssignment env name term comment = do

--  if name == Name "hydra.extract.coreDebug.nArgs"
----  if name == Name "hydra.extract.coreDebug.unexpected"
--  then fail $ "term: " ++ ShowCore.term term
--  else pure ()

  (tparams, params, bindings, body, doms, cod, env2) <- withTrace "gather for term assignment" $
    gatherBindingsAndParams env term

  -- If there are no arguments or let bindings, and if we are not dealing with a case statement,
  -- we can use a simple a = b assignment.
  if isSimpleAssignment term then do
    bodyExpr <- encodeTermInline env2 body
    return $ annotatedStatement comment $ assignmentStatement (encodeName False CaseConventionLowerSnake env2 name) bodyExpr
  -- Otherwise, only a function definition will work.
  else do

--    if name == Name "hydra.annotations.aggregateAnnotations"
--    then fail $ "term: " ++ ShowCore.term term
--      ++ "\nterm (raw): " ++ show term
--      ++ "\ntparams: " ++ L.intercalate ", " (fmap unName tparams)
--      ++ "\nparams: " ++ L.intercalate ", " (fmap unName params)
--      ++ "\nbindings: " ++ L.intercalate ", " (fmap ShowCore.binding bindings)
--      ++ "\nbody: " ++ ShowCore.term body
--      ++ "\ndoms: " ++ L.intercalate ", " (fmap ShowCore.type_ doms)
--      ++ "\ncod: " ++ ShowCore.type_ cod
--    else return ()

    -- TODO: consider moving this into the updated environment, along with the body
    bindingStmts <- encodeBindings env2 bindings

    withBindings bindings $
      encodeFunctionDefinition env2 name tparams params body doms cod comment bindingStmts

-- | Encode a term to an inline Python expression
encodeTermInline :: PythonEnvironment -> Term -> Flow PyGraph Py.Expression
encodeTermInline env term = case deannotateTerm term of
    TermApplication a -> encodeApplication env a
    TermFunction f -> encodeFunction env f
    TermLet _ -> pure $ stringToPyExpression Py.QuoteStyleDouble "let terms are not supported here"
    TermList terms -> do
      pyExprs <- CM.mapM encode terms
      return $ pyAtomToPyExpression $ Py.AtomTuple $ Py.Tuple (pyExpressionToPyStarNamedExpression <$> pyExprs)
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
    TermTypeLambda tl@(TypeLambda _ term1) -> encodeTermInline env2 term1
      where
        env2 = extendEnvironmentForTypeLambda env tl
    TermTypeApplication (TypeApplicationTerm term1 _) -> encode term1
    TermUnion (Injection tname field) -> do
      rt <- inGraphContext $ requireUnionType tname
      if isEnumRowType rt
        then return $ projectFromExpression (pyNameToPyExpression $ encodeNameQualified env tname)
          $ encodeEnumValue env $ fieldName field
        else do
          parg <- encode $ fieldTerm field
          return $ functionCall (pyNameToPyPrimary $ variantName True env tname (fieldName field)) [parg]
    TermUnit -> return $ pyNameToPyExpression pyNone
    TermVariable name -> do
      PyGraph g _ <- getState
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
    encode = encodeTermInline env

-- | Encode a term to a list of statements, with the last statement as the return value.
encodeTermMultiline :: PythonEnvironment -> Term -> Flow PyGraph [Py.Statement]
encodeTermMultiline env term = withTrace ("encodeTermMultiline: " ++ ShowCore.term term) $ if L.length args == 1
    then withArg body (L.head args)
    else dflt
  where
    (args, body) = gatherArgs [] term
    gatherArgs rest term = case deannotateTerm term of
      TermApplication (Application l r) -> gatherArgs (r:rest) l
      t -> (rest, t)
    dflt = do
      (tparams, params, bindings, body, doms, cod, env2) <- withTrace "gather for multiline" $
        gatherBindingsAndParams env term
      if (L.length params > 0)
        then fail $ "Functions currently unsupported in this context: " ++ ShowCore.term term
        else pure ()
      if (L.null bindings) then do
        expr <- encodeTermInline env term
        return [returnSingle expr]
      else do
        -- TODO: consider putting this inside of the updated environment, along with the body
        bindingStmts <- encodeBindings env2 bindings

        withBindings bindings $ do
          stmts <- encodeTermMultiline env2 body
          return $ bindingStmts ++ stmts
    withArg body arg = case deannotateTerm body of
      -- Case statements are special.
      TermFunction (FunctionElimination (EliminationUnion (CaseStatement tname dflt cases))) -> do
          rt <- inGraphContext $ requireUnionType tname
          let isEnum = isEnumRowType rt
          let isFull = L.length cases >= L.length (rowTypeFields rt)
          pyArg <- encodeTermInline env arg
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
                Just d -> returnSingle <$> encodeTermInline env d
              let patterns = pyClosedPatternToPyPatterns Py.ClosedPatternWildcard
              let body = indentedBlock Nothing [[stmt]]
              return [Py.CaseBlock patterns Nothing body]
          toCaseBlock isEnum (Field fname fterm) = case deannotateTerm fterm of
            TermFunction (FunctionLambda lam@(Lambda v _ body)) -> do
                stmts <- encodeTermMultiline env2 body
                let pyBody = indentedBlock Nothing [stmts]
                return $ Py.CaseBlock (pyClosedPatternToPyPatterns pattern) Nothing pyBody
              where
                env2 = extendEnvironmentForLambda env lam
                pattern = if isEnum
                    then Py.ClosedPatternValue $ Py.ValuePattern $ Py.Attribute [
                      encodeName True CaseConventionPascal env tname,
                      encodeEnumValue env2 fname]
                    else if isFreeVariableInTerm v body
                    then Py.ClosedPatternClass $
                      Py.ClassPattern pyVarName Nothing Nothing
                    else Py.ClosedPatternClass $
                      Py.ClassPattern pyVarName Nothing (Just $ Py.KeywordPatterns [argPattern])
                  where
                    pyVarName = Py.NameOrAttribute [variantName True env2 tname fname]
                    argPattern = Py.KeywordPattern (Py.Name "value") $ Py.PatternOr $ Py.OrPattern [
                      Py.ClosedPatternCapture $ Py.CapturePattern
                        $ Py.PatternCaptureTarget (encodeName False CaseConventionLowerSnake env2 v)]
            _ -> fail $ "unsupported case: " ++ ShowCore.term fterm
      _ -> dflt

encodeType :: PythonEnvironment -> Type -> Flow PyGraph Py.Expression
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

encodeTypeAssignment :: PythonEnvironment -> Name -> Type -> Maybe String -> Flow PyGraph [[Py.Statement]]
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

encodeTypeQuoted :: PythonEnvironment -> Type -> Flow PyGraph Py.Expression
encodeTypeQuoted env typ = do
  pytype <- encodeType env typ
  return $ if S.null (freeVariablesInType typ)
    then pytype
    else doubleQuotedString $ printExpr $ PySer.encodeExpression pytype

encodeUnionType :: PythonEnvironment -> Name -> RowType -> Maybe String -> Flow PyGraph [Py.Statement]
encodeUnionType env name rt@(RowType _ tfields) comment = if isEnumRowType rt then asEnum else asUnion
  where
    asEnum = do
        vals <- CM.mapM toVal tfields
        let body = indentedBlock comment vals
        return [pyClassDefinitionToPyStatement $ Py.ClassDefinition Nothing (encodeName False CaseConventionPascal env name) [] args body]
      where
        args = Just $ pyExpressionsToPyArgs [pyNameToPyExpression $ Py.Name "Enum"]
        toVal (FieldType fname ftype) = do
          fcomment <- fmap normalizeComment <$> (inGraphContext $ getTypeDescription ftype)
          return $ Y.catMaybes [
            Just $ assignmentStatement (encodeEnumValue env fname) (doubleQuotedString $ unName fname),
            pyExpressionToPyStatement . tripleQuotedString <$> fcomment]
    asUnion = do
      fieldStmts <- CM.mapM toFieldStmt tfields
      return $ fieldStmts ++ [unionStmt]
      where
        toFieldStmt (FieldType fname ftype) = do
            comment <- fmap normalizeComment <$> (inGraphContext $ getTypeDescription ftype)
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

encodeWrappedType :: PythonEnvironment -> Name -> Type -> Maybe String -> Flow PyGraph Py.Statement
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

extendEnvironmentForLambda :: PythonEnvironment -> Lambda -> PythonEnvironment
extendEnvironmentForLambda env lam = env {
  pythonEnvironmentTypeContext = extendTypeContextForLambda (pythonEnvironmentTypeContext env) lam}

extendEnvironmentForLet :: PythonEnvironment -> Let -> PythonEnvironment
extendEnvironmentForLet env letrec = env {
  pythonEnvironmentTypeContext = extendTypeContextForLet (pythonEnvironmentTypeContext env) letrec}

extendEnvironmentForTypeLambda :: PythonEnvironment -> TypeLambda -> PythonEnvironment
extendEnvironmentForTypeLambda env tlam = env {
  pythonEnvironmentTypeContext = extendTypeContextForTypeLambda (pythonEnvironmentTypeContext env) tlam}

extendMetaForTerm :: Bool -> PythonModuleMetadata -> Term -> PythonModuleMetadata
extendMetaForTerm topLevel meta t = case t of
    TermFunction f -> case f of
      FunctionLambda (Lambda _ (Just dom) body) -> if topLevel
          then extendMetaForType True False dom meta2
          else meta2
        where
          meta2 = extendMetaForTerm topLevel meta body
      _ -> meta
    TermLet (Let bindings body) -> L.foldl forBinding (extendMetaForTerm False meta body) bindings
      where
        forBinding meta (Binding _ term1 (Just ts)) = if isSimpleAssignment term1
          then meta
          else extendMetaForType True True (typeSchemeType ts) (extendMetaForTerm True meta term1)
    TermLiteral l -> case l of
      LiteralFloat fv -> case fv of
        FloatValueBigfloat _ -> meta {pythonModuleMetadataUsesDecimal = True}
        _ -> meta
      _ -> meta
    TermMap _ -> meta {pythonModuleMetadataUsesFrozenDict = True}
    _ -> meta2
  where
    meta2 = L.foldl (extendMetaForTerm False) meta $ subterms t

extendMetaForType :: Bool -> Bool -> Type -> PythonModuleMetadata -> PythonModuleMetadata
--extendMetaForType foo bar typ meta = extendFor meta typ
--extendMetaForType topLevel isTermAnnot typ meta = extendFor meta typ
--extendMetaForType topLevel isTermAnnot typ meta = case typ of
--  TypeFunction (FunctionType _ _) -> meta {pythonModuleMetadataUsesCallable = True}
--  _ -> meta

extendMetaForType topLevel isTermAnnot typ meta = extendFor meta3 typ
--extendMetaForType foo bar typ meta = extendFor meta3 typ
  where
--    topLevel = True
--    isTermAnnot = False

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
      TypeFunction (FunctionType dom cod) -> if isTermAnnot && topLevel
          -- If a particular term has a function type, don't import Callable; Python has special "def" syntax for functions.
          then meta3
          -- If this is a type-level definition, or an *argument* to a function is a function, then we need Callable.
          else meta3 {pythonModuleMetadataUsesCallable = True}
        where
          meta2 = extendMetaForType topLevel isTermAnnot cod meta
          meta3 = extendMetaForType False isTermAnnot dom meta2
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
      t -> L.foldl (\m t -> extendMetaForType False isTermAnnot t m) meta $ subtypes t

extendMetaForTypes :: [Type] -> PythonModuleMetadata -> PythonModuleMetadata
extendMetaForTypes types meta = L.foldl (\m t -> extendMetaForType True False t m) meta types

findTypeParams :: PythonEnvironment -> Type -> [Name]
findTypeParams env typ = L.filter isBound $ S.toList $ freeVariablesInType typ
  where
    isBound v = Y.isJust $ M.lookup v $ snd $ pythonEnvironmentBoundTypeVariables env

gatherArgs :: Term -> [Term] -> (Term, [Term])
gatherArgs term args = case deannotateTerm term of
  TermApplication (Application lhs rhs) -> gatherArgs lhs (rhs:args)
  TermTypeLambda (TypeLambda _ body) -> gatherArgs body args
  TermTypeApplication (TypeApplicationTerm t _) -> gatherArgs t args
  _ -> (term, args)

gatherBindingsAndParams :: PythonEnvironment -> Term -> Flow s ([Name], [Name], [Binding], Term, [Type], Type, PythonEnvironment)
gatherBindingsAndParams env term = withTrace ("gather for " ++ ShowCore.term term) $ gather True env [] [] [] [] [] term
  where
    gather argMode env tparams args bindings doms tapps term = case deannotateTerm term of
        TermFunction (FunctionLambda lam@(Lambda var (Just dom) body)) -> if argMode
            then gather argMode env2 tparams (var:args) bindings (dom:doms) tapps body
            else finish term
          where
            env2 = extendEnvironmentForLambda env lam
        TermLet lt@(Let bindings2 body) -> gather False env2 tparams args (bindings ++ bindings2) doms tapps body
          where
            env2 = extendEnvironmentForLet env lt
        TermTypeApplication (TypeApplicationTerm e t) -> gather argMode env tparams args bindings doms (t:tapps) e
        TermTypeLambda tlam@(TypeLambda tvar body) -> gather argMode env2 (tvar:tparams) args bindings doms tapps body
          where
            env2 = extendEnvironmentForTypeLambda env tlam
        t -> finish t
      where
        finish t = do
            typ <- typeOf (pythonEnvironmentTypeContext env) [] t2
            return (L.reverse tparams, L.reverse args, bindings, t2, L.reverse doms, typ, env)
          where
            t2 = L.foldl (\trm typ -> TermTypeApplication $ TypeApplicationTerm trm typ) t tapps

gatherMetadata :: [Definition] -> PythonModuleMetadata
gatherMetadata defs = checkTvars $ L.foldl addDef start defs
  where
    checkTvars meta = meta {pythonModuleMetadataUsesTypeVar = not $ S.null $ pythonModuleMetadataTypeVariables meta}
    start = PythonModuleMetadata {
      pythonModuleMetadataTypeVariables = S.empty,
      pythonModuleMetadataUsesAnnotated = False,
      pythonModuleMetadataUsesCallable = False,
      pythonModuleMetadataUsesCast = False,
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
      DefinitionTerm (TermDefinition _ term typ) -> extendMetaForTerm True meta2 term
        where
          meta2 = extendMetaForType True True typ meta
      DefinitionType (TypeDefinition _ typ) -> foldOverType TraversalOrderPre (\m t -> extendMetaForType True False t m) meta2 typ
        where
          meta2 = meta {pythonModuleMetadataUsesName = True}

genericArg :: [Name] -> Y.Maybe Py.Expression
genericArg tparamList = if L.null tparamList
  then Nothing
  else Just $ pyPrimaryToPyExpression $ primaryWithExpressionSlices (pyNameToPyPrimary $ Py.Name "Generic")
    (pyNameToPyExpression . encodeTypeVariable <$> tparamList)

isNullaryFunction :: Binding -> Bool
isNullaryFunction (Binding _ term (Just ts)) = isCaseStatement || (typeArity (typeSchemeType ts) == 0 && isLet)
  where
    term1 = deannotateAndDetypeTerm term
    isLet = case term1 of
      TermLet _ -> True
      _ -> False
    isCaseStatement = case fst (gatherArgs term1 []) of
      TermFunction (FunctionElimination (EliminationUnion _)) -> True
      _ -> False

isSimpleAssignment :: Term -> Bool
isSimpleAssignment term = case deannotateAndDetypeTerm term of
  TermFunction (FunctionLambda _) -> False
  TermLet _ -> False
  t -> case (fst (gatherArgs t [])) of
    TermFunction (FunctionElimination (EliminationUnion _)) -> False
    _ -> True

moduleToPython :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToPython mod defs = do
  file <- encodeModule mod defs
  let s = printExpr $ parenthesize $ PySer.encodeModule file
  let path = namespaceToFilePath CaseConventionLowerSnake (FileExtension "py") $ moduleNamespace mod
  return $ M.fromList [(path, s)]

--updateGraph :: (Graph -> Graph) -> Flow PyGraph ()
--updateGraph f = do
--  PyGraph g meta <- getState
--  putState (PyGraph (f g) meta)

updateMeta :: (PythonModuleMetadata -> PythonModuleMetadata) -> Flow PyGraph ()
updateMeta f = do
  PyGraph g meta <- getState
  putState $ PyGraph g (f meta)

variantArgs :: Py.Expression -> [Name] -> Py.Args
variantArgs ptype tparams = pyExpressionsToPyArgs $ Y.catMaybes [
    Just $ pyPrimaryToPyExpression $
      primaryWithExpressionSlices (pyNameToPyPrimary $ Py.Name "Node") [ptype],
    genericArg tparams]

withBindings :: [Binding] -> Flow PyGraph a -> Flow PyGraph a
withBindings bindings = withUpdatedGraph (extendGraphWithBindings bindings)

-- Note: this does not use withState, as we want the rest of the environment (including metadata) to be
--       mutable throughout the flow, even though we update the graph temporarily.
withUpdatedGraph :: (Graph -> Graph) -> Flow PyGraph a -> Flow PyGraph a
withUpdatedGraph f flow = do
  PyGraph g meta <- getState
  putState $ PyGraph (f g) meta
  r <- flow
  PyGraph _ meta2 <- getState
  putState $ PyGraph g meta2
  return r

inGraphContext :: Flow Graph a -> Flow PyGraph a
inGraphContext f = do
  (PyGraph g meta) <- getState
  (ret, g2) <- withState g $ do
    ret <- f
    g2 <- getState
    return (ret, g2)
  putState $ PyGraph g2 meta
  return ret
