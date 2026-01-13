module Hydra.Ext.Staging.Java.Coder (
  JavaFeatures(..),
  java8Features,
  moduleToJava,
) where

import Hydra.Kernel
import Hydra.Typing
import Hydra.Ext.Staging.CoderUtils
import Hydra.Ext.Staging.Java.Utils
import Hydra.Ext.Java.Language
import Hydra.Ext.Staging.Java.Names
import Hydra.Adapt.Modules
import Hydra.Ext.Staging.Java.Serde
import Hydra.Ext.Staging.Java.Settings
import Hydra.Adapt.Utils
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Monads as Monads
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Util as Util

import qualified Control.Monad as CM
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.String (String)


data JavaSymbolClass = JavaSymbolClassConstant | JavaSymbolClassNullaryFunction | JavaSymbolClassUnaryFunction | JavaSymbolLocalVariable

data JavaEnvironment = JavaEnvironment {
  javaEnvironmentAliases :: Aliases,
  javaEnvironmentTypeContext :: TypeContext
}

data JavaFeatures = JavaFeatures {
  supportsDiamondOperator :: Bool
}

java8Features = JavaFeatures {
  supportsDiamondOperator = False
}

java11Features = JavaFeatures {
  supportsDiamondOperator = True
}

-- For now, the supported features are hard-coded to those of Java 11, rather than being configurable.
javaFeatures = java11Features

-- | New simple adapter version that works with definitions directly
moduleToJava :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToJava mod defs = withTrace ("encode module: " ++ unNamespace (moduleNamespace mod)) $ do
    units <- encodeDefinitions mod defs
    return $ M.fromList $ forPair <$> M.toList units
  where
    forPair (name, unit) = (bindingNameToFilePath name, printExpr $ parenthesize $ writeCompilationUnit unit)

addComment :: Java.ClassBodyDeclaration -> FieldType -> Flow Graph Java.ClassBodyDeclarationWithComments
addComment decl field = Java.ClassBodyDeclarationWithComments decl <$> commentsFromFieldType field

analyzeJavaFunction :: JavaEnvironment -> Term -> Flow Graph (FunctionStructure JavaEnvironment)
analyzeJavaFunction env = analyzeFunctionTerm javaEnvironmentTypeContext (\tc e -> e { javaEnvironmentTypeContext = tc }) env

bindingNameToFilePath :: Name -> FilePath
bindingNameToFilePath name = nameToFilePath CaseConventionCamel CaseConventionPascal (FileExtension "java")
    $ unqualifyName $ QualifiedName ns (sanitizeJavaName local)
  where
    QualifiedName ns local = qualifyName name

boundTypeVariables :: Type -> [Name]
boundTypeVariables typ = case typ of
  TypeAnnotated (AnnotatedType typ1 _) -> boundTypeVariables typ1
  TypeForall (ForallType v body) -> v:(boundTypeVariables body)
  _ -> []

classModsPublic :: [Java.ClassModifier]
classModsPublic = [Java.ClassModifierPublic]

classifyDataReference :: Name -> Flow Graph JavaSymbolClass
classifyDataReference name = do
  mel <- dereferenceElement name
  case mel of
    Nothing -> return JavaSymbolLocalVariable
    Just el -> do
      g <- getState
      tc <- graphToTypeContext g
      ts <- case bindingType el of
        Nothing -> fail $ "no type scheme for element " ++ unName (bindingName el)
        Just ts -> pure ts
      return $ classifyDataTerm ts $ bindingTerm el

classifyDataTerm :: TypeScheme -> Term -> JavaSymbolClass
classifyDataTerm ts term = if isLambda term
    then JavaSymbolClassUnaryFunction
    else if hasTypeParameters || isUnsupportedVariant
      then JavaSymbolClassNullaryFunction
      else JavaSymbolClassConstant
  where
    hasTypeParameters = not $ L.null $ typeSchemeVariables ts
    isUnsupportedVariant = case deannotateTerm term of
      TermLet _ -> True
      _ -> False

commentsFromElement :: Binding -> Flow Graph (Maybe String)
commentsFromElement = getTermDescription . bindingTerm

commentsFromFieldType :: FieldType -> Flow Graph (Maybe String)
commentsFromFieldType = getTypeDescription . fieldTypeType

constantDecl :: String -> Aliases -> Name -> Flow Graph Java.ClassBodyDeclarationWithComments
constantDecl javaName aliases name = do
  g <- getState
  tc <- graphToTypeContext g
  let env = JavaEnvironment aliases tc
  jt <- encodeType aliases S.empty $ TypeVariable _Name
  arg <- encodeTerm env $ Terms.string $ unName name
  let init = Java.VariableInitializerExpression $ javaConstructorCall (javaConstructorName nameName Nothing) [arg] Nothing
  let var = javaVariableDeclarator (Java.Identifier javaName) (Just init)
  return $ noComment $ javaMemberField mods jt var
  where
    mods = [Java.FieldModifierPublic, Java.FieldModifierStatic, Java.FieldModifierFinal]
    nameName = nameToJavaName aliases _Name

constantDeclForFieldType :: Aliases -> FieldType -> Flow Graph Java.ClassBodyDeclarationWithComments
constantDeclForFieldType aliases ftyp = constantDecl javaName aliases name
  where
    name = fieldTypeName ftyp
    javaName = "FIELD_NAME_" ++ nonAlnumToUnderscores (convertCase CaseConventionCamel CaseConventionUpperSnake $ unName name)

constantDeclForTypeName :: Aliases -> Name -> Flow Graph Java.ClassBodyDeclarationWithComments
constantDeclForTypeName = constantDecl "TYPE_NAME"

constructElementsInterface :: Module -> [Java.InterfaceMemberDeclaration] -> (Name, Java.CompilationUnit)
constructElementsInterface mod members = (elName, cu)
  where
    cu = Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) [] [decl]
    pkg = javaPackageDeclaration $ moduleNamespace mod
    mods = [Java.InterfaceModifierPublic]
    className = elementsClassName $ moduleNamespace mod
    elName = unqualifyName $ QualifiedName (Just $ moduleNamespace mod) className
    body = Java.InterfaceBody members
    itf = Java.TypeDeclarationInterface $ Java.InterfaceDeclarationNormalInterface $
      Java.NormalInterfaceDeclaration mods (javaTypeIdentifier className) [] [] body
    decl = Java.TypeDeclarationWithComments itf $ moduleDescription mod

declarationForRecordType :: Bool -> Bool -> Aliases -> [Java.TypeParameter] -> Name
  -> [FieldType] -> Flow Graph Java.ClassDeclaration
declarationForRecordType isInner isSer aliases tparams elName fields = do

--    if (elName == Name "hydra.relational.ColumnSchema")
--      then fail $ "fields: [" ++ (L.intercalate ", " $ fmap (ShowCore.fieldType) fields) ++ "], tparams: " ++ show tparams
--      else pure ()

    memberVars <- CM.mapM toMemberVar fields
    memberVars' <- CM.zipWithM addComment memberVars fields
    withMethods <- if L.length fields > 1
      then CM.mapM toWithMethod fields
      else pure []
    cons <- constructor
    tn <- if isInner then pure [] else do
      d <- constantDeclForTypeName aliases elName
      dfields <- CM.mapM (constantDeclForFieldType aliases) fields
      return (d:dfields)
    let bodyDecls = tn ++ memberVars' ++ (noComment <$> [cons, equalsMethod, hashCodeMethod] ++ withMethods)
    return $ javaClassDeclaration aliases tparams elName classModsPublic Nothing (interfaceTypes isSer) bodyDecls
  where
    constructor = do
      params <- CM.mapM (fieldTypeToFormalParam aliases) fields
      let nullCheckStmts = fieldToNullCheckStatement <$> fields
      let assignStmts = fieldToAssignStatement <$> fields
      return $ makeConstructor aliases elName False params $ nullCheckStmts ++ assignStmts

    fieldToAssignStatement = Java.BlockStatementStatement . toAssignStmt . fieldTypeName

    fieldArgs = fieldNameToJavaExpression . fieldTypeName <$> fields

    toMemberVar (FieldType fname ft) = do
      let mods = [Java.FieldModifierPublic, Java.FieldModifierFinal]
      jt <- encodeType aliases S.empty ft
      let var = fieldNameToJavaVariableDeclarator fname
      return $ javaMemberField mods jt var

    toWithMethod field = do
        param <- fieldTypeToFormalParam aliases field
        return $ methodDeclaration mods [] anns methodName [param] result (Just [nullCheck, returnStmt])
      where
        anns = [] -- TODO
        mods = [Java.MethodModifierPublic]
        methodName = "with" ++ nonAlnumToUnderscores (capitalize (unName $ fieldTypeName field))
        nullCheck = fieldToNullCheckStatement field
        result = referenceTypeToResult $ nameToJavaReferenceType aliases False [] elName Nothing
        consId = Java.Identifier $ sanitizeJavaName $ localNameOf elName
        returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $
          javaConstructorCall (javaConstructorName consId Nothing) fieldArgs Nothing

    equalsMethod = methodDeclaration mods [] anns equalsMethodName [param] result $
        Just [instanceOfStmt,
          castStmt,
          returnAllFieldsEqual]
      where
        anns = [overrideAnnotation]
        mods = [Java.MethodModifierPublic]
        param = javaTypeToJavaFormalParameter (javaRefType [] Nothing "Object") (Name otherInstanceName)
        result = javaTypeToJavaResult javaBooleanType
        tmpName = "o"

        instanceOfStmt = Java.BlockStatementStatement $ Java.StatementIfThen $
            Java.IfThenStatement cond returnFalse
          where
            cond = javaUnaryExpressionToJavaExpression $
                Java.UnaryExpressionOther $
                Java.UnaryExpressionNotPlusMinusNot $
                javaRelationalExpressionToJavaUnaryExpression $
                javaInstanceOf other parent
              where
                other = javaIdentifierToJavaRelationalExpression $ javaIdentifier otherInstanceName
                parent = nameToJavaReferenceType aliases False [] elName Nothing

            returnFalse = javaReturnStatement $ Just $ javaBooleanExpression False

        castStmt = variableDeclarationStatement aliases jtype id rhs
          where
            jtype = javaTypeFromTypeName aliases elName
            id = javaIdentifier tmpName
            rhs = javaCastExpressionToJavaExpression $ javaCastExpression rt var
            var = javaIdentifierToJavaUnaryExpression $ Java.Identifier $ sanitizeJavaName otherInstanceName
            rt = nameToJavaReferenceType aliases False [] elName Nothing

        returnAllFieldsEqual = Java.BlockStatementStatement $ javaReturnStatement $ Just $ if L.null fields
            then javaBooleanExpression True
            else javaConditionalAndExpressionToJavaExpression $
              Java.ConditionalAndExpression (eqClause . fieldTypeName <$> fields)
          where
            eqClause (Name fname) = javaPostfixExpressionToJavaInclusiveOrExpression $
                javaMethodInvocationToJavaPostfixExpression $ Java.MethodInvocation header [arg]
              where
                arg = javaExpressionNameToJavaExpression $
                  fieldExpression (javaIdentifier tmpName) (javaIdentifier fname)
                header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex var [] (Java.Identifier equalsMethodName)
                var = Java.MethodInvocation_VariantExpression $ Java.ExpressionName Nothing $ Java.Identifier $
                  sanitizeJavaName fname

    hashCodeMethod = methodDeclaration mods [] anns hashCodeMethodName [] result $ Just [returnSum]
      where
        anns = [overrideAnnotation]
        mods = [Java.MethodModifierPublic]
        result = javaTypeToJavaResult javaIntType

        returnSum = Java.BlockStatementStatement $ if L.null fields
          then returnZero
          else javaReturnStatement $ Just $
            javaAdditiveExpressionToJavaExpression $ addExpressions $
              L.zipWith multPair multipliers (fieldTypeName <$> fields)
          where
            returnZero = javaReturnStatement $ Just $ javaIntExpression 0

            multPair :: Int -> Name -> Java.MultiplicativeExpression
            multPair i (Name fname) = Java.MultiplicativeExpressionTimes $
                Java.MultiplicativeExpression_Binary lhs rhs
              where
                lhs = Java.MultiplicativeExpressionUnary $ javaPrimaryToJavaUnaryExpression $
                  javaLiteralToJavaPrimary $ javaInt i
                rhs = javaPostfixExpressionToJavaUnaryExpression $
                  javaMethodInvocationToJavaPostfixExpression $
                  methodInvocationStatic (javaIdentifier fname) (Java.Identifier hashCodeMethodName) []

            multipliers = L.cycle first20Primes
              where
                first20Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

declarationForType :: Bool -> Aliases -> (Binding, TypeApplicationTerm) -> Flow Graph Java.TypeDeclarationWithComments
declarationForType isSer aliases (el, TypeApplicationTerm term _) = withTrace ("element " ++ unName (bindingName el)) $ do
    g <- Monads.getState
    t <- Monads.eitherToFlow Util.unDecodingError (DecodeCore.type_ g term) >>= adaptTypeToLanguage javaLanguage
    cd <- toClassDecl False isSer aliases [] (bindingName el) t
    comments <- commentsFromElement el
    return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass cd) comments

declarationForUnionType :: Bool -> Aliases
  -> [Java.TypeParameter] -> Name -> [FieldType] -> Flow Graph Java.ClassDeclaration
declarationForUnionType isSer aliases tparams elName fields = do
    variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
    let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
    variantDecls' <- CM.zipWithM addComment variantDecls fields
    let otherDecls = noComment <$> [privateConstructor, toAcceptMethod True tparams, visitor, partialVisitor]
    tn <- do
      d <- constantDeclForTypeName aliases elName
      dfields <- CM.mapM (constantDeclForFieldType aliases) fields
      return (d:dfields)
    let bodyDecls = tn ++ otherDecls ++ variantDecls'
    let mods = classModsPublic ++ [Java.ClassModifierAbstract]
    return $ javaClassDeclaration aliases tparams elName mods Nothing (interfaceTypes isSer) bodyDecls
  where
    privateConstructor = makeConstructor aliases elName True [] []
    unionFieldClass (FieldType fname ftype) = do
      let rtype = Types.record $ if Schemas.isUnitType ftype then [] else [FieldType (Name valueFieldName) $ deannotateType ftype]
      toClassDecl True isSer aliases [] (variantClassName False elName fname) rtype
    augmentVariantClass (Java.ClassDeclarationNormal cd) = Java.ClassDeclarationNormal $ cd {
        Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic, Java.ClassModifierStatic, Java.ClassModifierFinal],
        Java.normalClassDeclarationExtends = Just $ nameToJavaClassType aliases True args elName Nothing,
        Java.normalClassDeclarationParameters = tparams,
        Java.normalClassDeclarationBody = newBody (Java.normalClassDeclarationBody cd)}
      where
        newBody (Java.ClassBody decls) = Java.ClassBody $ decls ++ [noComment $ toAcceptMethod False tparams]
        args = typeParameterToTypeArgument <$> tparams

    visitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration mods ti vtparams extends body
      where
        mods = [Java.InterfaceModifierPublic]
        ti = Java.TypeIdentifier $ Java.Identifier visitorName
        vtparams = tparams ++ [javaTypeParameter visitorReturnParameter]
        extends = []
        body = Java.InterfaceBody (toVisitMethod . fieldTypeName <$> fields)
          where
            toVisitMethod fname = interfaceMethodDeclaration [] [] visitMethodName [variantInstanceParam fname] resultR Nothing

    partialVisitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration {
            Java.normalInterfaceDeclarationModifiers = [Java.InterfaceModifierPublic],
            Java.normalInterfaceDeclarationIdentifier = Java.TypeIdentifier $ Java.Identifier partialVisitorName,
            Java.normalInterfaceDeclarationParameters = tparams ++ [javaTypeParameter visitorReturnParameter],
            Java.normalInterfaceDeclarationExtends =
              [Java.InterfaceType $ javaClassType ((typeParameterToReferenceType <$> tparams) ++ [visitorTypeVariable]) Nothing visitorName],
            Java.normalInterfaceDeclarationBody = Java.InterfaceBody $ otherwise:(toVisitMethod . fieldTypeName <$> fields)}
      where
        otherwise = interfaceMethodDeclaration defaultMod [] otherwiseMethodName [mainInstanceParam] resultR $ Just [throw]
          where
            typeArgs = typeParameterToTypeArgument <$> tparams
            throw = Java.BlockStatementStatement $ javaThrowIllegalStateException args
              where
                args = [javaAdditiveExpressionToJavaExpression $ addExpressions [
                  javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                  Java.MultiplicativeExpressionUnary $ javaIdentifierToJavaUnaryExpression $ Java.Identifier "instance"]]

        toVisitMethod fname = interfaceMethodDeclaration defaultMod [] visitMethodName [variantInstanceParam fname] resultR $
            Just [returnOtherwise]
          where
            returnOtherwise = Java.BlockStatementStatement $ javaReturnStatement $ Just $
              javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray_ $ Java.PrimaryNoNewArrayMethodInvocation $
              methodInvocation Nothing (Java.Identifier otherwiseMethodName) [javaIdentifierToJavaExpression $ Java.Identifier "instance"]

    defaultMod = [Java.InterfaceMethodModifierDefault]

    resultR = javaTypeToJavaResult $ Java.TypeReference visitorTypeVariable

    typeArgs = typeParameterToTypeArgument <$> tparams

    mainInstanceParam = javaTypeToJavaFormalParameter classRef $ Name instanceName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False typeArgs elName Nothing

    variantInstanceParam fname = javaTypeToJavaFormalParameter classRef $ Name instanceName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False typeArgs (variantClassName False elName fname) Nothing

elementJavaIdentifier :: Bool -> Bool -> Aliases -> Name -> Java.Identifier
elementJavaIdentifier isPrim isMethod aliases name = Java.Identifier $ if isPrim
    then (qualify $ capitalize local) ++ "." ++ applyMethodName
    else case ns of
      Nothing -> sanitizeJavaName local
      Just n -> (qualify $ elementsClassName n) ++ sep ++ sanitizeJavaName local
  where
    sep = if isMethod then "::" else "."
    qualify s = Java.unIdentifier $ nameToJavaName aliases $ unqualifyName $ QualifiedName ns s
    QualifiedName ns local = qualifyName name

elementsClassName :: Namespace -> String
elementsClassName (Namespace ns) = capitalize $ L.last $ LS.splitOn "." ns

encodeApplication :: JavaEnvironment -> Application -> Flow Graph Java.Expression
encodeApplication env app@(Application lhs rhs) = do
    -- Get the function's arity from its type
    funTyp <- withTrace "debug b" $ typeOf tc [] fun
    let arity = typeArity funTyp
    -- Split arguments based on arity
    let hargs = L.take arity args  -- Head arguments: pass directly
        rargs = L.drop arity args  -- Remaining arguments: apply via .apply()
    -- Generate the initial call with head arguments
    -- gatherArgs already stripped type applications, just remove any remaining annotations
    initialCall <- case deannotateTerm fun of
      TermFunction f -> case f of
        FunctionPrimitive name -> functionCall env True name hargs
        _ -> fallback
      TermVariable name ->
        -- Check if this is a recursive let-bound variable
        -- Recursive variables need curried construction with .get()
        if isRecursiveVariable aliases name
          then fallback  -- Use curried construction for recursive bindings
          else functionCall env False name hargs  -- Direct call for library functions
      _ -> fallback
    -- Apply remaining arguments via .apply() for partial application
    applyRemaining initialCall rargs
  where
    aliases = javaEnvironmentAliases env
    tc = javaEnvironmentTypeContext env
    encode = encodeTerm env
    (fun, args) = gatherArgs (TermApplication app) []

    -- Apply remaining arguments one at a time using .apply()
    applyRemaining exp remArgs = case remArgs of
      [] -> pure exp
      (h:r) -> do
        jarg <- encode h
        applyRemaining (apply exp jarg) r

    fallback = withTrace "fallback" $ do
--        if Y.isNothing (getTermType lhs)
--          -- then fail $ "app: " ++ ShowCore.term (TermApplication app)
--          then fail $ "lhs: " ++ ShowCore.term lhs
--          else pure ()

        t <- withTrace "debug c" $ typeOf tc [] lhs
        (dom, cod) <- case deannotateTypeParameters $ deannotateType t of
            TypeFunction (FunctionType dom cod) -> pure (dom, cod)
            t' -> fail $ "expected a function type on function " ++ show lhs ++ ", but found " ++ show t'
        case deannotateTerm lhs of
          TermFunction f -> case f of
            FunctionElimination e -> do
                jarg <- encode rhs
                encodeElimination env (Just jarg) dom cod e
            _ -> defaultExpression
          _ -> defaultExpression
      where
        defaultExpression = do
          -- Note: the domain type will not be used, so we just substitute the unit type
          jfun <- encode lhs
          jarg <- encode rhs
          let prim = javaExpressionToJavaPrimary jfun
          return $ apply jfun jarg
    apply exp jarg = javaMethodInvocationToJavaExpression $
      methodInvocation (Just $ Right $ javaExpressionToJavaPrimary exp) (Java.Identifier applyMethodName) [jarg]

-- | Convert definitions directly to Java compilation units (simple adapter approach)
encodeDefinitions :: Module -> [Definition] -> Flow Graph (M.Map Name Java.CompilationUnit)
encodeDefinitions mod defs = do
    g <- getState
    tc <- initialTypeContext g
    let env = JavaEnvironment {
                javaEnvironmentAliases = aliases,
                javaEnvironmentTypeContext = tc}
    let pkg = javaPackageDeclaration $ moduleNamespace mod
    typeUnits <- CM.mapM (encodeTypeDefinition pkg aliases) typeDefs
    termUnits <- if L.null termDefs
      then return []
      else do
        dataMembers <- CM.mapM (encodeTermDefinition env) termDefs
        return [constructElementsInterface mod dataMembers]
    let units = typeUnits ++ termUnits
    return $ M.fromList units
  where
    (typeDefs, termDefs) = partitionDefinitions defs
    aliases = importAliasesForModule mod

encodeElimination :: JavaEnvironment -> Maybe Java.Expression -> Type -> Type -> Elimination -> Flow Graph Java.Expression
encodeElimination env marg dom cod elm = case elm of
    EliminationRecord (Projection _ fname) -> do
      jdomr <- encodeType aliases S.empty dom >>= javaTypeToJavaReferenceType
      jexp <- case marg of
        Nothing -> pure $ javaLambda var jbody
          where
            var = Name "rec"
            jbody = javaExpressionNameToJavaExpression $
              fieldExpression (variableToJavaIdentifier var) (javaIdentifier $ unName fname)
        Just jarg -> pure $ javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier $ unName fname)
          where
            qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
      return jexp
    EliminationUnion (CaseStatement tname def fields) -> do
       case marg of
        Nothing -> do
          g <- getState
          let lhs = TermFunction $ FunctionElimination elm
          let var = "u"
          encodeTerm env $ Terms.lambda var $ Terms.apply lhs (Terms.var var)
          -- TODO: default value
        Just jarg -> applyElimination jarg
      where
        applyElimination jarg = do
            let prim = javaExpressionToJavaPrimary jarg
            let consId = innerClassRef aliases tname $ case def of
                  Nothing -> visitorName
                  Just _ -> partialVisitorName
            jcod <- encodeType aliases S.empty cod
            rt <- javaTypeToJavaReferenceType jcod
            let targs = typeArgsOrDiamond $ javaTypeArgumentsForType dom ++ [Java.TypeArgumentReference rt]
            otherwiseBranches <- case def of
              Nothing -> pure []
              Just d -> do
                b <- otherwiseBranch jcod d
                return [b]
            visitBranches <- CM.mapM (visitBranch jcod) fields
            let body = Java.ClassBody $ otherwiseBranches ++ visitBranches
            let visitor = javaConstructorCall (javaConstructorName consId $ Just targs) [] (Just body)
            return $ javaMethodInvocationToJavaExpression $
              methodInvocation (Just $ Right prim) (Java.Identifier acceptMethodName) [visitor]
          where
            otherwiseBranch jcod d = do
              targs <- javaTypeArgumentsForNamedType tname
              let jdom = Java.TypeReference $ nameToJavaReferenceType aliases True targs tname Nothing
              let mods = [Java.MethodModifierPublic]
              let anns = [overrideAnnotation]
              let param = javaTypeToJavaFormalParameter jdom $ Name instanceName
              let result = Java.ResultType $ Java.UnannType jcod
              -- Analyze the term for bindings (handles nested lets)
              fs <- analyzeJavaFunction env d
              let bindings = functionStructureBindings fs
                  innerBody = functionStructureBody fs
                  env2 = functionStructureEnvironment fs
              -- Convert bindings to Java block statements
              (bindingStmts, env3) <- bindingsToStatements env2 bindings
              -- Encode the body (now without nested lets)
              jret <- encodeTerm env3 innerBody
              let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret
              let allStmts = bindingStmts ++ [returnStmt]
              return $ noComment $ methodDeclaration mods [] anns otherwiseMethodName [param] result (Just allStmts)

            visitBranch jcod field = do
              targs <- javaTypeArgumentsForNamedType tname
              let jdom = Java.TypeReference $ nameToJavaReferenceType aliases True targs tname (Just $ capitalize $ unName $ fieldName field)
              let mods = [Java.MethodModifierPublic]
              let anns = [overrideAnnotation]
              let result = Java.ResultType $ Java.UnannType jcod

              -- Field terms are lambdas; apply to special var that encodes to instance.value
              case deannotateTerm (fieldTerm field) of
                TermFunction (FunctionLambda lam@(Lambda lambdaParam mdom body)) -> withLambda env lam $ \env2 -> do
                  let env3 = insertBranchVar lambdaParam env2

                  fs <- analyzeJavaFunction env3 body
                  let bindings = functionStructureBindings fs
                      innerBody = functionStructureBody fs
                      env4 = functionStructureEnvironment fs

                  (bindingStmts, env5) <- bindingsToStatements env4 bindings
                  jret <- encodeTerm env5 innerBody

                  let param = javaTypeToJavaFormalParameter jdom lambdaParam
                  let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret
                  let allStmts = bindingStmts ++ [returnStmt]
                  return $ noComment $ methodDeclaration mods [] anns visitMethodName [param] result (Just allStmts)
                _ -> fail $ "visitBranch: field term is not a lambda: " ++ ShowCore.term (fieldTerm field)
    EliminationWrap name -> pure $ case marg of
        Nothing -> javaLambda var jbody
          where
            var = Name "wrapped"
            arg = javaIdentifierToJavaExpression $ variableToJavaIdentifier var
            jbody = withArg arg
        Just jarg -> withArg jarg
      where
        withArg jarg = javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier valueFieldName)
          where
            qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
  where
    aliases = javaEnvironmentAliases env

encodeFunction :: JavaEnvironment -> Type -> Type -> Function -> Flow Graph Java.Expression
encodeFunction env dom cod fun = case fun of
    FunctionElimination elm -> withTrace ("elimination (" ++ show (eliminationVariant elm) ++ ")") $ do
      encodeElimination env Nothing dom cod elm
    FunctionLambda lam@(Lambda var _ body) -> withTrace ("lambda " ++ unName var) $ withLambda env lam $ \env2 -> do
      -- For curried functions in Java, we need to check if body is also a lambda
      -- If it is, encode it recursively to create nested Java lambdas
      case deannotateTerm body of
        TermFunction (FunctionLambda innerLam) -> do
          -- Body is another lambda - recursively encode it
          -- The type of body is a function type, so we need to extract dom' and cod'
          bodyTyp <- typeOf (javaEnvironmentTypeContext env2) [] body
          case deannotateType bodyTyp of
            TypeFunction (FunctionType dom' cod') -> do
              innerJavaLambda <- encodeFunction env2 dom' cod' (FunctionLambda innerLam)
              let lam' = javaLambda var innerJavaLambda
              -- Apply cast
              jtype <- encodeType aliases S.empty (TypeFunction $ FunctionType dom cod)
              rt <- javaTypeToJavaReferenceType jtype
              return $ javaCastExpressionToJavaExpression $
                javaCastExpression rt (javaExpressionToJavaUnaryExpression lam')
            _ -> fail $ "expected function type for lambda body, but got: " ++ show bodyTyp
        _ -> do
          -- Body is not a lambda - analyze and encode normally
          fs <- withTrace "analyze function body" $ analyzeJavaFunction env2 body
          let bindings = functionStructureBindings fs
              innerBody = functionStructureBody fs
              env3 = functionStructureEnvironment fs

          (bindingStmts, env4) <- bindingsToStatements env3 bindings
          jbody <- encodeTerm env4 innerBody

          lam' <- if L.null bindings
            then return $ javaLambda var jbody
            else do
              let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
              return $ javaLambdaFromBlock var $ Java.Block $ bindingStmts ++ [returnSt]

          -- Apply cast
          jtype <- encodeType aliases S.empty (TypeFunction $ FunctionType dom cod)
          rt <- javaTypeToJavaReferenceType jtype
          return $ javaCastExpressionToJavaExpression $
            javaCastExpression rt (javaExpressionToJavaUnaryExpression lam')
    FunctionPrimitive name -> do
      -- For function primitives, generate a method reference like ClassName::apply
      let Java.Identifier classWithApply = elementJavaIdentifier True False aliases name
      -- elementJavaIdentifier with isPrim=True adds ".apply", but we want "::apply" for method references
      let suffix = "." ++ applyMethodName
      let className = take (length classWithApply - length suffix) classWithApply
      return $ javaIdentifierToJavaExpression $ Java.Identifier $ className ++ "::" ++ applyMethodName
    _ -> pure $ encodeLiteral $ LiteralString $
      "Unimplemented function variant: " ++ show (functionVariant fun)
  where
    aliases = javaEnvironmentAliases env

encodeLiteral :: Literal -> Java.Expression
encodeLiteral lit = case lit of
    LiteralBinary bs -> javaArrayCreation javaBytePrimitiveType (Just byteValues)
      where
        byteValues = javaArrayInitializer $ fmap toByteExpr $ B.unpack bs
        toByteExpr w = javaLiteralToJavaExpression $ Java.LiteralInteger $
          Java.IntegerLiteral $ fromIntegral w
    LiteralBoolean b -> litExp $ javaBoolean b
    LiteralFloat f -> case f of
        FloatValueBigfloat v -> javaConstructorCall
          (javaConstructorName (Java.Identifier "java.math.BigDecimal") Nothing)
          [encodeLiteral $ LiteralString $ "\"" <> Literals.showBigfloat v ++ "\""]
          Nothing
        FloatValueFloat32 v -> litExp $ Java.LiteralFloatingPoint $ Java.FloatingPointLiteral $ realToFrac v
        FloatValueFloat64 v -> litExp $ Java.LiteralFloatingPoint $ Java.FloatingPointLiteral v
    LiteralInteger i -> case i of
        IntegerValueBigint v -> javaConstructorCall
          (javaConstructorName (Java.Identifier "java.math.BigInteger") Nothing)
          [encodeLiteral $ LiteralString $ "\"" <> Literals.showBigint v ++ "\""]
          Nothing
        IntegerValueInt8 v -> litExp $ integer $ fromIntegral v -- byte
        IntegerValueInt16 v -> litExp $ integer $ fromIntegral v -- short
        IntegerValueInt32 v -> litExp $ integer $ fromIntegral v -- int
        IntegerValueInt64 v -> litExp $ integer $ fromIntegral v -- long
        IntegerValueUint16 v -> litExp $ Java.LiteralCharacter $ fromIntegral v -- char
      where
        integer = Java.LiteralInteger . Java.IntegerLiteral
    LiteralString s -> litExp $ javaString s
  where
    litExp = javaLiteralToJavaExpression

-- Note: we use Java object types everywhere, rather than primitive types, as the latter cannot be used
--       to build function types, parameterized types, etc.
encodeLiteralType :: LiteralType -> Flow Graph Java.Type
encodeLiteralType lt = case lt of
    LiteralTypeBinary -> pure $ Java.TypeReference $ Java.ReferenceTypeArray $
      Java.ArrayType (Java.Dims [[]]) $ Java.ArrayType_VariantPrimitive javaBytePrimitiveType
    LiteralTypeBoolean -> simple "Boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeBigfloat -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigDecimal"
      FloatTypeFloat32 -> simple "Float"
      FloatTypeFloat64 -> simple "Double"
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigInteger"
      IntegerTypeInt8 -> simple "Byte"
      IntegerTypeInt16 -> simple "Short"
      IntegerTypeInt32 -> simple "Integer"
      IntegerTypeInt64 -> simple "Long"
      IntegerTypeUint16 -> simple "Character"
      _ -> fail $ "unexpected integer type: " ++ show it
    LiteralTypeString -> simple "String"
    _ -> fail $ "unexpected literal type: " ++ show lt
  where
    simple n = pure $ javaRefType [] Nothing n

encodeNullaryConstant :: JavaEnvironment -> Type -> Function -> Flow Graph Java.Expression
encodeNullaryConstant env typ fun = case fun of
  FunctionPrimitive name -> functionCall env True name []
  _ -> unexpected "nullary function" $ show fun

encodeTerm :: JavaEnvironment -> Term -> Flow Graph Java.Expression
encodeTerm env term0 = encodeInternal [] [] term0
  where
    aliases = javaEnvironmentAliases env
    tc = javaEnvironmentTypeContext env
    encode = encodeTerm env
    failAsLiteral msg = pure $ encodeLiteral $ LiteralString msg
    encodeInternal anns tyapps term = case term of
        TermAnnotated (AnnotatedTerm term' ann) -> encodeInternal (ann:anns) tyapps term'

        TermApplication app -> withTrace "encode application" $ encodeApplication env app

        TermEither et -> do
          targs <- takeTypeArgs "either" 2
          case et of
            Left term1 -> do
              expr <- encode term1
              return $ javaMethodInvocationToJavaExpression $
                methodInvocationStaticWithTypeArgs (Java.Identifier "hydra.util.Either") (Java.Identifier "left") targs [expr]
            Right term1 -> do
              expr <- encode term1
              return $ javaMethodInvocationToJavaExpression $
                methodInvocationStaticWithTypeArgs (Java.Identifier "hydra.util.Either") (Java.Identifier "right") targs [expr]

        TermFunction f -> withTrace ("encode function (" ++ show (functionVariant f) ++ ")") $ do
          t <- withTrace "debug d" $ typeOf tc [] term0
          case deannotateType t of
            TypeFunction (FunctionType dom cod) -> do
              encodeFunction env dom cod f
            _ -> encodeNullaryConstant env t f

        TermLet _ -> fail $ "nested let is unsupported for Java: " ++ ShowCore.term term

        TermList els -> do
          jels <- CM.mapM encode els
          targs <- if L.null jels
            then takeTypeArgs "list" 1
            else pure []
          return $ javaMethodInvocationToJavaExpression $
            methodInvocationStaticWithTypeArgs (Java.Identifier "java.util.List") (Java.Identifier "of") targs jels

        TermLiteral l -> pure $ encodeLiteral l

        TermMap m -> do
          jkeys <- CM.mapM encode $ M.keys m
          jvals <- CM.mapM encode $ M.elems m
          let pairs = L.zip jkeys jvals
          let pairExprs = (\(k, v) -> javaMethodInvocationToJavaExpression $
                methodInvocationStatic (Java.Identifier "java.util.Map") (Java.Identifier "entry") [k, v]) <$> pairs
          return $ javaMethodInvocationToJavaExpression $
            methodInvocationStatic (Java.Identifier "java.util.Map") (Java.Identifier "ofEntries") pairExprs

        TermMaybe mt -> case mt of
          Nothing -> do
            targs <- takeTypeArgs "maybe" 1
            return $ javaMethodInvocationToJavaExpression $
              methodInvocationStaticWithTypeArgs (Java.Identifier "hydra.util.Maybe") (Java.Identifier "nothing") targs []
          Just term1 -> do
            expr <- encode term1
            return $ javaMethodInvocationToJavaExpression $
              methodInvocationStatic (Java.Identifier "hydra.util.Maybe") (Java.Identifier "just") [expr]

        TermPair (t1, t2) -> do
          jterm1 <- encode t1
          jterm2 <- encode t2
          let tupleTypeName = "hydra.util.Tuple.Tuple2"
          return $ javaConstructorCall (javaConstructorName (Java.Identifier tupleTypeName) Nothing) [jterm1, jterm2] Nothing

        TermRecord (Record name fields) -> do
          fieldExprs <- CM.mapM encode (fieldTerm <$> fields)
          let consId = nameToJavaName aliases name
          return $ javaConstructorCall (javaConstructorName consId Nothing) fieldExprs Nothing

        TermSet s -> do
          jels <- CM.mapM encode $ S.toList s
          let prim = javaMethodInvocationToJavaPrimary $
                     methodInvocationStatic (Java.Identifier "java.util.Stream") (Java.Identifier "of") jels
          let coll = javaMethodInvocationToJavaExpression $
                     methodInvocationStatic (Java.Identifier "java.util.stream.Collectors") (Java.Identifier "toSet") []
          return $ javaMethodInvocationToJavaExpression $
            methodInvocation (Just $ Right prim) (Java.Identifier "collect") [coll]

        TermTypeApplication (TypeApplicationTerm body atyp) -> do
          -- Type applications in Java require casting to the appropriate type
          -- We encode the body (stripping type applications) and cast it to the inferred type
          typ <- withTrace "debug e" $ typeOf tc [] term0
          jtype <- encodeType aliases S.empty typ
          jatyp <- encodeType aliases S.empty atyp
          jbody <- encodeInternal anns (jatyp:tyapps) body
          rt <- javaTypeToJavaReferenceType jtype
          return $ javaCastExpressionToJavaExpression $
            javaCastExpression rt (javaExpressionToJavaUnaryExpression jbody)

        TermTypeLambda tl@(TypeLambda _ body) -> withTypeLambda env tl $ \env2 -> encodeTerm env2 body

        TermUnion (Injection name (Field (Name fname) v)) -> do
          let (Java.Identifier typeId) = nameToJavaName aliases name
          let consId = Java.Identifier $ typeId ++ "." ++ sanitizeJavaName (capitalize fname)
          args <- if Schemas.isUnitTerm v
            then return []
            else do
              ex <- encode v
              return [ex]
          return $ javaConstructorCall (javaConstructorName consId Nothing) args Nothing

        TermVariable name -> encodeVariable env name

        TermWrap (WrappedTerm tname arg) -> do
          jarg <- encode arg
          return $ javaConstructorCall (javaConstructorName (nameToJavaName aliases tname) Nothing) [jarg] Nothing

        _ -> failAsLiteral $ "Unimplemented term variant: " ++ show (termVariant term)
      where
        takeTypeArgs label n = if L.length tyapps < n
          then fail $ "needed " ++ show n ++ " type arguments for " ++ label ++ ", found " ++ show (L.length tyapps)
          else do
            rt <- CM.mapM javaTypeToJavaReferenceType $ L.take n tyapps
            return $ fmap Java.TypeArgumentReference rt

-- | Convert a list of bindings to Java block statements, handling recursive bindings
-- and performing topological sorting for correct declaration order.
bindingsToStatements :: JavaEnvironment -> [Binding] -> Flow Graph ([Java.BlockStatement], JavaEnvironment)
bindingsToStatements env bindings = if L.null bindings
    then return ([], envExtended)
    else do
      groups <- CM.mapM toDeclStatements sorted
      return (L.concat groups, envExtended)
  where
    aliases = javaEnvironmentAliases env
    tc = javaEnvironmentTypeContext env

    -- Flatten nested lets: if a binding's value is a TermLet, expand it into multiple bindings
    flattenedBindings = L.concatMap flattenOne bindings
      where
        flattenOne (Binding name term mts) = case deannotateTerm term of
          TermLet (Let innerBindings body) -> flattenBindings innerBindings ++ [Binding name body mts]
          _ -> [Binding name term mts]
        flattenBindings bs = L.concatMap flattenOne bs

    -- Extend TypeContext with flattened bindings so they can reference each other
    tcExtended = extendTypeContextForLet bindingMetadata tc (Let flattenedBindings (TermVariable $ Name "dummy"))

    bindingVars = S.fromList (bindingName <$> flattenedBindings)

    -- Identify recursive bindings
    recursiveVars = S.fromList $ L.concat (ifRec <$> sorted)
      where
        ifRec names = case names of
          [name] -> case M.lookup name allDeps of
            Nothing -> []
            Just deps -> if S.member name deps then [name] else []
          _ -> names  -- Mutually recursive group

    -- Create environment with recursive vars marked in aliases
    aliasesWithRecursive = aliases { aliasesRecursiveVars = S.union (aliasesRecursiveVars aliases) recursiveVars }
    envExtended = env {
      javaEnvironmentTypeContext = tcExtended,
      javaEnvironmentAliases = aliasesWithRecursive
    }

    -- Build dependency graph
    allDeps = M.fromList (toDeps <$> flattenedBindings)
      where
        toDeps (Binding key value _) = (key, S.filter (\n -> S.member n bindingVars) $ freeVariablesInTerm value)

    -- Topological sort for correct declaration order
    sorted = topologicalSortComponents (toDeps <$> M.toList allDeps)
      where
        toDeps (key, deps) = (key, S.toList deps)

    -- Convert a group of bindings to statements
    toDeclStatements names = do
      inits <- Y.catMaybes <$> CM.mapM toDeclInit names
      impls <- CM.mapM toDeclStatement names
      return $ inits ++ impls

    -- Initialize recursive bindings with AtomicReference
    toDeclInit name = if S.member name recursiveVars
      then do
        let value = bindingTerm $ L.head $ L.filter (\b -> bindingName b == name) flattenedBindings
        typ <- withTrace "debug f" $ typeOf tcExtended [] value
        jtype <- encodeType aliasesWithRecursive S.empty typ
        let id = variableToJavaIdentifier name
        let pkg = javaPackageName ["java", "util", "concurrent", "atomic"]
        let arid = Java.Identifier "java.util.concurrent.atomic.AtomicReference"
        let aid = Java.AnnotatedIdentifier [] arid
        rt <- javaTypeToJavaReferenceType jtype
        let targs = typeArgsOrDiamond [Java.TypeArgumentReference rt]
        let ci = Java.ClassOrInterfaceTypeToInstantiate [aid] (Just targs)
        let body = javaConstructorCall ci [] Nothing
        let artype = javaRefType [rt] (Just pkg) "AtomicReference"
        return $ Just $ variableDeclarationStatement aliasesWithRecursive artype id body
      else pure Nothing

    -- Declare or set binding value
    toDeclStatement name = do
      let value = bindingTerm $ L.head $ L.filter (\b -> bindingName b == name) flattenedBindings
      typ <- withTrace "debug g" $ typeOf tcExtended [] value
      jtype <- encodeType aliasesWithRecursive S.empty typ
      let id = variableToJavaIdentifier name
      rhs <- encodeTerm envExtended value
      return $ if S.member name recursiveVars
        then Java.BlockStatementStatement $ javaMethodInvocationToJavaStatement $
          methodInvocation (Just $ Left $ Java.ExpressionName Nothing id) (Java.Identifier setMethodName) [rhs]
        else variableDeclarationStatement aliasesWithRecursive jtype id rhs

-- Lambdas cannot (in general) be turned into top-level constants, as there is no way of declaring type parameters for constants
-- These functions must be capable of handling various combinations of let and lambda terms:
-- * Plain lambdas such as \x y -> x + y + 42
-- * Lambdas with nested let terms, such as \x y -> let z = x + y in z + 42
-- * Let terms with nested lambdas, such as let z = 42 in \x y -> x + y + z
encodeTermDefinition :: JavaEnvironment -> TermDefinition -> Flow Graph Java.InterfaceMemberDeclaration
encodeTermDefinition env (TermDefinition name term _) = withTrace ("encode term definition \"" ++ unName name ++ "\"") $ do

    fs <- withTrace "analyze function term for term assignment" $ analyzeJavaFunction env term
    let tparams = functionStructureTypeParams fs
        params = functionStructureParams fs
        bindings = functionStructureBindings fs
        body = functionStructureBody fs
        doms = functionStructureDomains fs
        env2 = functionStructureEnvironment fs
    cod <- case functionStructureCodomain fs of
      Just c -> return c
      Nothing -> fail "Java requires a return type annotation, but type inference failed for this term"
    let jparams = fmap toParam tparams
        aliases2 = javaEnvironmentAliases env2

    -- Convert bindings to Java block statements
    (bindingStmts, env3) <- bindingsToStatements env2 bindings

    if (L.null tparams && L.null params && L.null bindings)
      then do -- Special case: constant field
        jtype <- Java.UnannType <$> encodeType aliases2 S.empty cod
        jbody <- encodeTerm env3 body
        let mods = []
        let var = javaVariableDeclarator (javaVariableName name) $ Just $ Java.VariableInitializerExpression jbody
        return $ Java.InterfaceMemberDeclarationConstant $ Java.ConstantDeclaration mods jtype [var]
      else do -- General case: method (possibly nullary)
        jformalParams <- mapM (\(dom, param) -> do
            jdom <- encodeType aliases2 S.empty dom
            return $ javaTypeToJavaFormalParameter jdom (Name $ unName param)
          ) (L.zip doms params)
        result <- javaTypeToJavaResult <$> encodeType aliases2 S.empty cod
        jbody <- encodeTerm env3 body
        let mods = [Java.InterfaceMethodModifierStatic]
        let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
        return $ interfaceMethodDeclaration mods jparams jname jformalParams result (Just $ bindingStmts ++ [returnSt])
  where
    tc = javaEnvironmentTypeContext env
    jname = sanitizeJavaName $ decapitalize $ localNameOf name
    toParam (Name v) = Java.TypeParameter [] (javaTypeIdentifier $ capitalize v) Nothing

encodeType :: Aliases -> S.Set Name -> Type -> Flow Graph Java.Type
encodeType aliases boundVars t =  case deannotateType t of
    TypeApplication (ApplicationType lhs rhs) -> do
      jlhs <- encode lhs
      jrhs <- encode rhs >>= javaTypeToJavaReferenceType
      addJavaTypeParameter jrhs jlhs
    TypeFunction (FunctionType dom cod) -> do
      jdom <- encode dom >>= javaTypeToJavaReferenceType
      jcod <- encode cod >>= javaTypeToJavaReferenceType
      return $ javaRefType [jdom, jcod] javaUtilFunctionPackageName "Function"
    TypeForall (ForallType v body) -> do
      jbody <- encodeType aliases (S.insert v boundVars) body
      addJavaTypeParameter (javaTypeVariable $ unName v) jbody
    TypeList et -> do
      jet <- encode et
      if listsAsArrays
        then toJavaArrayType jet
        else do
          rt <- javaTypeToJavaReferenceType jet
          return $ javaRefType [rt] javaUtilPackageName "List"
    TypeLiteral lt -> encodeLiteralType lt
    TypeEither (EitherType lt rt) -> do
      jlt <- encode lt >>= javaTypeToJavaReferenceType
      jrt <- encode rt >>= javaTypeToJavaReferenceType
      return $ javaRefType [jlt, jrt] hydraUtilPackageName "Either"
    TypeMap (MapType kt vt) -> do
      jkt <- encode kt >>= javaTypeToJavaReferenceType
      jvt <- encode vt >>= javaTypeToJavaReferenceType
      return $ javaRefType [jkt, jvt] javaUtilPackageName "Map"
    TypePair (PairType first second) -> do
      jfirst <- encode first >>= javaTypeToJavaReferenceType
      jsecond <- encode second >>= javaTypeToJavaReferenceType
      return $ javaRefType [jfirst, jsecond] hydraUtilPackageName "Tuple.Tuple2"
    TypeRecord (RowType _Unit []) -> unit
    TypeRecord (RowType name _) -> pure $
      Java.TypeReference $ nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) name Nothing
    TypeMaybe ot -> do
      jot <- encode ot >>= javaTypeToJavaReferenceType
      return $ javaRefType [jot] hydraUtilPackageName "Maybe"
    TypeSet st -> do
      jst <- encode st >>= javaTypeToJavaReferenceType
      return $ javaRefType [jst] javaUtilPackageName "Set"
    TypeUnion (RowType name _) -> pure $
      Java.TypeReference $ nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) name Nothing
    TypeVariable name -> pure $ forReference name
    TypeWrap (WrappedType name _) -> pure $ forReference name
    _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    forReference name = if isLambdaBoundVariable name
        then variableReference name
        else nameReference name
    nameReference name = Java.TypeReference $ nameToJavaReferenceType aliases True [] name Nothing
    variableReference name = Java.TypeReference $ javaTypeVariable $ unName name
    encode = encodeType aliases boundVars
    unit = return $ javaRefType [] javaLangPackageName "Void"

encodeTypeDefinition :: Java.PackageDeclaration -> Aliases -> TypeDefinition -> Flow Graph (Name, Java.CompilationUnit)
encodeTypeDefinition pkg aliases (TypeDefinition name typ) = do
    decl <- typeDefToClassDecl aliases name typ
    return (name, Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) imports [decl])
  where
    imports = if serializable
      then [Java.ImportDeclarationSingleType $ Java.SingleTypeImportDeclaration $ javaTypeName $ Java.Identifier "java.io.Serializable"]
      else []
    typeDefToClassDecl aliases name typ = do
      decl <- toClassDecl False serializable aliases [] name typ
      comment <- getTypeDescription typ
      return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass decl) comment
    serializable = isSerializableType typ
    isSerializableType typ = case deannotateType typ of
      TypeRecord _ -> True
      TypeUnion _ -> True
      TypeWrap _ -> True
      _ -> False

encodeVariable :: JavaEnvironment -> Name -> Flow Graph Java.Expression
encodeVariable env name =
    if S.member name (aliasesBranchVars aliases)
    then return $ javaFieldAccessToJavaExpression $ Java.FieldAccess
      (Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary $ javaIdentifierToJavaExpression jid)
      (javaIdentifier valueFieldName)
    -- Check if this is the special visitor instance value variable (instance_value)
    else if name == Name (instanceName ++ "_" ++ valueFieldName) && isRecursiveVariable aliases name
    then do
      -- Emit instance.value field access
      let instanceExpr = javaIdentifierToJavaExpression $ javaIdentifier instanceName
      return $ javaFieldAccessToJavaExpression $ Java.FieldAccess
        (Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary instanceExpr)
        (javaIdentifier valueFieldName)
    else if isRecursiveVariable aliases name
    then return $ javaMethodInvocationToJavaExpression $
      methodInvocation (Just $ Left $ Java.ExpressionName Nothing jid) (Java.Identifier getMethodName) []
    else do
      cls <- classifyDataReference name
      return $ case cls of
        JavaSymbolLocalVariable -> javaIdentifierToJavaExpression $ elementJavaIdentifier False False aliases name
        JavaSymbolClassConstant -> javaIdentifierToJavaExpression $ elementJavaIdentifier False False aliases name
        JavaSymbolClassNullaryFunction -> javaMethodInvocationToJavaExpression $
          methodInvocation Nothing (elementJavaIdentifier False False aliases name) []
        JavaSymbolClassUnaryFunction -> javaIdentifierToJavaExpression $ elementJavaIdentifier False True aliases name
  where
    aliases = javaEnvironmentAliases env
    jid = javaIdentifier $ unName name

fieldToNullCheckStatement :: FieldType -> Java.BlockStatement
fieldToNullCheckStatement field = Java.BlockStatementStatement $ javaMethodInvocationToJavaStatement $ Java.MethodInvocation header [arg]
  where
    arg = javaIdentifierToJavaExpression $ fieldNameToJavaIdentifier $ fieldTypeName field
    header = Java.MethodInvocation_HeaderSimple $ Java.MethodName $
      Java.Identifier "java.util.Objects.requireNonNull"

fieldTypeToFormalParam aliases (FieldType fname ft) = do
  jt <- encodeType aliases S.empty ft
  return $ javaTypeToJavaFormalParameter jt fname

functionCall :: JavaEnvironment -> Bool -> Name -> [Term] -> Flow Graph Java.Expression
functionCall env isPrim name args = do
--    if name == Name "hydra.lib.maybes.maybe"
--    then fail $ "args: " ++ L.intercalate ", " (fmap ShowCore.term args)
--    else pure ()

    -- When there are no arguments and it's a primitive, use a method reference instead of calling .apply()
    if isPrim && L.null args
    then do
      -- Generate method reference like ClassName::apply
      let Java.Identifier classWithApply = elementJavaIdentifier True False aliases name
      let suffix = "." ++ applyMethodName
      let className = take (length classWithApply - length suffix) classWithApply
      return $ javaIdentifierToJavaExpression $ Java.Identifier $ className ++ "::" ++ applyMethodName
    else do
      jargs <- CM.mapM (encodeTerm env) args
      if isLocalVariable name
        then do
          prim <- javaExpressionToJavaPrimary <$> encodeVariable env name
          return $ javaMethodInvocationToJavaExpression $
            methodInvocation (Just $ Right prim) (Java.Identifier applyMethodName) jargs
        else do
          let header = Java.MethodInvocation_HeaderSimple $ Java.MethodName $ elementJavaIdentifier isPrim False aliases name
          return $ javaMethodInvocationToJavaExpression $ Java.MethodInvocation header jargs
  where
    aliases = javaEnvironmentAliases env

getCodomain :: M.Map Name Term -> Flow Graph Type
getCodomain ann = functionTypeCodomain <$> getFunctionType ann

getFunctionType :: M.Map Name Term -> Flow Graph FunctionType
getFunctionType ann = do
  mt <- getType ann
  case mt of
    Nothing -> fail "type annotation is required for function and elimination terms in Java"
    Just t -> case t of
      TypeFunction ft -> return ft
      _ -> unexpected "function type (3)" $ show t

innerClassRef :: Aliases -> Name -> String -> Java.Identifier
innerClassRef aliases name local = Java.Identifier $ id ++ "." ++ local
  where
    Java.Identifier id = nameToJavaName aliases name

insertBranchVar :: Name -> JavaEnvironment -> JavaEnvironment
insertBranchVar name env = env {javaEnvironmentAliases = aliases {aliasesBranchVars = S.insert name (aliasesBranchVars aliases)}}
  where
    aliases = javaEnvironmentAliases env

interfaceTypes :: Bool -> [Java.InterfaceType]
interfaceTypes isSer = if isSer then [javaSerializableType] else []
  where
    javaSerializableType = Java.InterfaceType $
      Java.ClassType [] Java.ClassTypeQualifierNone (javaTypeIdentifier "Serializable") []

isLambdaBoundVariable :: Name -> Bool
isLambdaBoundVariable (Name v) = L.length v <= 4

isLocalVariable :: Name -> Bool
isLocalVariable name = Y.isNothing $ qualifiedNameNamespace $ qualifyName name

isRecursiveVariable :: Aliases -> Name -> Bool
isRecursiveVariable aliases name = S.member name (aliasesRecursiveVars aliases)

javaTypeArgumentsForNamedType :: Name -> Flow Graph [Java.TypeArgument]
javaTypeArgumentsForNamedType tname = do
    params <- javaTypeParametersForType <$> requireType tname
    return $ typeParameterToTypeArgument <$> params

javaTypeArgumentsForType :: Type -> [Java.TypeArgument]
javaTypeArgumentsForType typ = L.reverse (typeParameterToTypeArgument <$> javaTypeParametersForType typ)

-- Note: this is somewhat of a hack; it compensates for the irregular way in which type parameters are currently used.
--       When this irregularity is resolved, a better approach will be to simply pick up type parameters from type applications.
javaTypeParametersForType :: Type -> [Java.TypeParameter]
javaTypeParametersForType typ = toParam <$> vars
  where
    toParam (Name v) = Java.TypeParameter [] (javaTypeIdentifier $ capitalize v) Nothing
    vars = L.nub $ boundVars typ ++ freeVars
    boundVars t = case deannotateType t of
      TypeForall (ForallType v body) -> v:(boundVars body)
      _ -> []
    freeVars = L.filter isLambdaBoundVariable $ S.toList $ freeVariablesInType typ

noComment :: Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments
noComment decl = Java.ClassBodyDeclarationWithComments decl Nothing

reannotate mtyp anns term = base
  where
    base = reann anns term
    reann anns term = case anns of
      [] -> term
      (h:r) -> reann r $ TermAnnotated (AnnotatedTerm term h)

toClassDecl :: Bool -> Bool -> Aliases -> [Java.TypeParameter]
  -> Name -> Type -> Flow Graph Java.ClassDeclaration
toClassDecl isInner isSer aliases tparams elName t = case deannotateType t of
    TypeRecord rt -> declarationForRecordType isInner isSer aliases tparams elName $ rowTypeFields rt
    TypeUnion rt -> declarationForUnionType isSer aliases tparams elName $ rowTypeFields rt
    TypeForall (ForallType (Name v) body) -> toClassDecl False isSer aliases (tparams ++ [param]) elName body
      where
        param = javaTypeParameter $ capitalize v
    TypeWrap (WrappedType tname wt) -> declarationForRecordType isInner isSer aliases tparams elName
      [FieldType (Name "value") wt]
    -- Other types are not supported as class declarations, so we wrap them as record types.
    _ -> wrap t -- TODO: wrap and unwrap the corresponding terms as record terms.
  where
    wrap t' = declarationForRecordType isInner isSer aliases tparams elName [Types.field valueFieldName $ deannotateType t']

toDataDeclaration :: Aliases -> (a, TypeApplicationTerm) -> Flow Graph a
toDataDeclaration aliases (el, TypeApplicationTerm term typ) = do
  fail "not implemented" -- TODO

typeArgsOrDiamond :: [Java.TypeArgument] -> Java.TypeArgumentsOrDiamond
typeArgsOrDiamond args = if supportsDiamondOperator javaFeatures
  then Java.TypeArgumentsOrDiamondDiamond
  else Java.TypeArgumentsOrDiamondArguments args

withLambda :: JavaEnvironment -> Lambda -> (JavaEnvironment -> Flow s a) -> Flow s a
withLambda = withLambdaContext javaEnvironmentTypeContext (\tc e -> e { javaEnvironmentTypeContext = tc })

withTypeLambda :: JavaEnvironment -> TypeLambda -> (JavaEnvironment -> Flow s a) -> Flow s a
withTypeLambda = withTypeLambdaContext javaEnvironmentTypeContext (\tc e -> e { javaEnvironmentTypeContext = tc })
