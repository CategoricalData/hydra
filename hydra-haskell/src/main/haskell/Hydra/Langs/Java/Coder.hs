module Hydra.Langs.Java.Coder (printModule) where

import Hydra.Kernel
import Hydra.Reduction
import Hydra.Langs.Java.Utils
import Hydra.Langs.Java.Language
import Hydra.Langs.Java.Names
import Hydra.Adapters
import Hydra.Tools.Serialization
import Hydra.Langs.Java.Serde
import Hydra.Langs.Java.Settings
import Hydra.AdapterUtils
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Langs.Java.Syntax as Java

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.String (String)


type Aliases = M.Map Namespace Java.PackageName

printModule :: (Ord a, Read a, Show a) => Module a -> GraphFlow a (M.Map FilePath String)
printModule mod = do
    withTrace "encode in Java" $ do
      units <- moduleToJavaCompilationUnit mod
      return $ M.fromList $ forPair <$> M.toList units
  where
    forPair (name, unit) = (
      elementNameToFilePath name,
      printExpr $ parenthesize $ writeCompilationUnit unit)

boundTypeVariables :: Type a -> [Name]
boundTypeVariables typ = case typ of
  TypeAnnotated (Annotated typ1 _) -> boundTypeVariables typ1
  TypeLambda (LambdaType v body) -> v:(boundTypeVariables body)
  _ -> []

commentsFromElement :: Element a -> GraphFlow a (Maybe String)
commentsFromElement el = do
  g <- getState
  annotationClassTermDescription (graphAnnotations g) (elementData el)

commentsFromFieldType :: FieldType a -> GraphFlow a (Maybe String)
commentsFromFieldType (FieldType _ t) = do
  g <- getState
  annotationClassTypeDescription (graphAnnotations g) t

addComment :: Java.ClassBodyDeclaration -> FieldType a -> GraphFlow a Java.ClassBodyDeclarationWithComments
addComment decl field = Java.ClassBodyDeclarationWithComments decl <$> commentsFromFieldType field

noComment :: Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments
noComment decl = Java.ClassBodyDeclarationWithComments decl Nothing

elementNameToFilePath :: Name -> FilePath
elementNameToFilePath name = nameToFilePath False (FileExtension "java") $ fromQname ns (sanitizeJavaName local)
  where
    (ns, local) = toQnameEager name

moduleToJavaCompilationUnit :: (Ord a, Read a, Show a) => Module a -> GraphFlow a (M.Map Name Java.CompilationUnit)
moduleToJavaCompilationUnit mod = transformModule javaLanguage encode constructModule mod
  where
    aliases = importAliasesForModule mod
    encode = encodeTerm aliases . contractTerm

classModsPublic :: [Java.ClassModifier]
classModsPublic = [Java.ClassModifierPublic]

constructModule :: (Ord a, Read a, Show a)
  => Module a
  -> M.Map (Type a) (Coder (Graph a) (Graph a) (Term a) Java.Expression)
  -> [(Element a, TypedTerm a)]
  -> GraphFlow a (M.Map Name Java.CompilationUnit)
constructModule mod coders pairs = do
    let isTypePair = isType . typedTermType . snd
    let typePairs = L.filter isTypePair pairs
    let dataPairs = L.filter (not . isTypePair) pairs
    typeUnits <- CM.mapM typeToClass typePairs
    dataMembers <- CM.mapM (termToInterfaceMember coders) dataPairs
    return $ M.fromList $ typeUnits ++ ([constructElementsInterface mod dataMembers | not (L.null dataMembers)])
  where
    pkg = javaPackageDeclaration $ moduleNamespace mod
    aliases = importAliasesForModule mod

    typeToClass pair@(el, _) = do
      let imports = []
      decl <- declarationForType aliases pair
      return (elementName el,
        Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) imports [decl])

    termToInterfaceMember coders pair = do
        withTrace ("element " ++ unName (elementName el)) $ do
          expanded <- contractTerm <$> (expandLambdas $ typedTermTerm $ snd pair)
          if isLambda expanded
            then termToMethod coders el (typedTermType $ snd pair) expanded
            else termToConstant coders el (typedTermType $ snd pair) expanded
      where
        el = fst pair
        isLambda t = case stripTerm t of
          TermFunction (FunctionLambda _) -> True
          TermLet (Let _ env) -> isLambda env
          _ -> False

    termToConstant coders el typ term = do
      jtype <- Java.UnannType <$> encodeType aliases typ
      jterm <- coderEncode (Y.fromJust $ M.lookup typ coders) term
      let mods = []
      let var = javaVariableDeclarator (javaVariableName $ elementName el) $ Just $ Java.VariableInitializerExpression jterm
      return $ Java.InterfaceMemberDeclarationConstant $ Java.ConstantDeclaration mods jtype [var]

    -- Lambdas cannot (in general) be turned into top-level constants, as there is no way of declaring type parameters for constants
    termToMethod coders el typ term = case stripType typ of
      TypeFunction (FunctionType dom cod) -> maybeLet aliases term $ \tm stmts -> case stripTerm tm of
        TermFunction (FunctionLambda (Lambda v body)) -> do
          jdom <- encodeType aliases dom
          jcod <- encodeType aliases cod
          let mods = [Java.InterfaceMethodModifierStatic]
          let anns = []
          let mname = sanitizeJavaName $ decapitalize $ localNameOfEager $ elementName el
          let param = javaTypeToJavaFormalParameter jdom (FieldName $ unName v)
          let result = javaTypeToJavaResult jcod
          jbody <- encodeTerm aliases body
          -- TODO: use coders
--           jbody <- coderEncode (Y.fromJust $ M.lookup typ coders) body
          let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
          let tparams = javaTypeParametersForType typ
          return $ interfaceMethodDeclaration mods tparams mname [param] result (Just $ stmts ++ [returnSt])
        _ -> unexpected "function term" tm
      _ -> unexpected "function type (1)" typ

constructElementsInterface :: Module a -> [Java.InterfaceMemberDeclaration] -> (Name, Java.CompilationUnit)
constructElementsInterface mod members = (elName, cu)
  where
    cu = Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) [] [decl]
    pkg = javaPackageDeclaration $ moduleNamespace mod
    mods = [Java.InterfaceModifierPublic]
    className = elementsClassName $ moduleNamespace mod
    elName = fromQname (moduleNamespace mod) className
    body = Java.InterfaceBody members
    itf = Java.TypeDeclarationInterface $ Java.InterfaceDeclarationNormalInterface $
      Java.NormalInterfaceDeclaration mods (javaTypeIdentifier className) [] [] body
    decl = Java.TypeDeclarationWithComments itf $ moduleDescription mod

declarationForLambdaType :: (Eq a, Ord a, Read a, Show a) => Aliases
  -> [Java.TypeParameter] -> Name -> LambdaType a -> GraphFlow a Java.ClassDeclaration
declarationForLambdaType aliases tparams elName (LambdaType (Name v) body) =
    toClassDecl False aliases (tparams ++ [param]) elName body
  where
    param = javaTypeParameter $ capitalize v

declarationForRecordType :: (Ord a, Read a, Show a) => Bool -> Aliases -> [Java.TypeParameter] -> Name
  -> [FieldType a] -> GraphFlow a Java.ClassDeclaration
declarationForRecordType isInner aliases tparams elName fields = do
    memberVars <- CM.mapM toMemberVar fields
    memberVars' <- CM.zipWithM addComment memberVars fields
    withMethods <- if L.length fields > 1
      then CM.mapM toWithMethod fields
      else pure []
    cons <- constructor
    tn <- if isInner then pure [] else do
      d <- typeNameDecl aliases elName
      return [d]
    let bodyDecls = tn ++ memberVars' ++ (noComment <$> [cons, equalsMethod, hashCodeMethod] ++ withMethods)
    return $ javaClassDeclaration aliases tparams elName classModsPublic Nothing bodyDecls
  where
    constructor = do
      params <- CM.mapM (fieldTypeToFormalParam aliases) fields
      let stmts = Java.BlockStatementStatement . toAssignStmt . fieldTypeName <$> fields
      return $ makeConstructor aliases elName False params stmts

    fieldArgs = fieldNameToJavaExpression . fieldTypeName <$> fields

    toMemberVar (FieldType fname ft) = do
      let mods = [Java.FieldModifierPublic, Java.FieldModifierFinal]
      jt <- encodeType aliases ft
      let var = fieldNameToJavaVariableDeclarator fname
      return $ javaMemberField mods jt var

    toWithMethod field = do
      let mods = [Java.MethodModifierPublic]
      let methodName = "with" ++ capitalize (unFieldName $ fieldTypeName field)
      param <- fieldTypeToFormalParam aliases field
      let anns = [] -- TODO
      let result = referenceTypeToResult $ nameToJavaReferenceType aliases False [] elName Nothing
      let consId = Java.Identifier $ sanitizeJavaName $ localNameOfEager elName
      let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $
            javaConstructorCall (javaConstructorName consId Nothing) fieldArgs Nothing
      return $ methodDeclaration mods [] anns methodName [param] result (Just [returnStmt])

    equalsMethod = methodDeclaration mods [] anns equalsMethodName [param] result $
        Just [instanceOfStmt,
          castStmt,
          returnAllFieldsEqual]
      where
        anns = [overrideAnnotation]
        mods = [Java.MethodModifierPublic]
        param = javaTypeToJavaFormalParameter (javaRefType [] Nothing "Object") (FieldName otherInstanceName)
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
            rhs = javaCastExpressionToJavaExpression $ javaCastExpression aliases rt var
            var = javaIdentifierToJavaUnaryExpression $ Java.Identifier $ sanitizeJavaName otherInstanceName
            rt = nameToJavaReferenceType aliases False [] elName Nothing

        returnAllFieldsEqual = Java.BlockStatementStatement $ javaReturnStatement $ Just $ if L.null fields
            then javaBooleanExpression True
            else javaConditionalAndExpressionToJavaExpression $
              Java.ConditionalAndExpression (eqClause . fieldTypeName <$> fields)
          where
            eqClause (FieldName fname) = javaPostfixExpressionToJavaInclusiveOrExpression $
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

            multPair :: Int -> FieldName -> Java.MultiplicativeExpression
            multPair i (FieldName fname) = Java.MultiplicativeExpressionTimes $
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

declarationForType :: (Ord a, Read a, Show a)
  => Aliases -> (Element a, TypedTerm a) -> GraphFlow a Java.TypeDeclarationWithComments
declarationForType aliases (el, TypedTerm _ term) = do
    t <- epsilonDecodeType term >>= adaptType javaLanguage
    cd <- toClassDecl False aliases [] (elementName el) t
    comments <- commentsFromElement el
    return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass cd) comments

declarationForUnionType :: (Eq a, Ord a, Read a, Show a)
  => Aliases
  -> [Java.TypeParameter] -> Name -> [FieldType a] -> GraphFlow a Java.ClassDeclaration
declarationForUnionType aliases tparams elName fields = do
    variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
    let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
    variantDecls' <- CM.zipWithM addComment variantDecls fields
    let otherDecls = noComment <$> [privateConstructor, toAcceptMethod True tparams, visitor, partialVisitor]
    tn <- typeNameDecl aliases elName
    let bodyDecls = [tn] ++ otherDecls ++ variantDecls'
    let mods = classModsPublic ++ [Java.ClassModifierAbstract]
    return $ javaClassDeclaration aliases tparams elName mods Nothing bodyDecls
  where
    privateConstructor = makeConstructor aliases elName True [] []
    unionFieldClass (FieldType fname ftype) = do
      let rtype = Types.record $ if isUnitType ftype then [] else [FieldType (FieldName valueFieldName) ftype]
      toClassDecl True aliases [] (variantClassName False elName fname) rtype
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
            throw = Java.BlockStatementStatement $ Java.StatementWithoutTrailing $
                Java.StatementWithoutTrailingSubstatementThrow $ Java.ThrowStatement $
                javaConstructorCall (javaConstructorName (Java.Identifier "IllegalStateException") Nothing) args Nothing
              where
                args = [javaAdditiveExpressionToJavaExpression $ addExpressions [
                  javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                  Java.MultiplicativeExpressionUnary $ javaIdentifierToJavaUnaryExpression $ Java.Identifier "instance"]]

        toVisitMethod fname = interfaceMethodDeclaration defaultMod [] visitMethodName [variantInstanceParam fname] resultR $
            Just [returnOtherwise]
          where
            returnOtherwise = Java.BlockStatementStatement $ javaReturnStatement $ Just $
              javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray $ Java.PrimaryNoNewArrayMethodInvocation $
              methodInvocation Nothing (Java.Identifier otherwiseMethodName) [javaIdentifierToJavaExpression $ Java.Identifier "instance"]

    defaultMod = [Java.InterfaceMethodModifierDefault]

    resultR = javaTypeToJavaResult $ Java.TypeReference visitorTypeVariable

    typeArgs = typeParameterToTypeArgument <$> tparams

    mainInstanceParam = javaTypeToJavaFormalParameter classRef $ FieldName instanceName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False typeArgs elName Nothing

    variantInstanceParam fname = javaTypeToJavaFormalParameter classRef $ FieldName instanceName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False typeArgs (variantClassName False elName fname) Nothing

elementJavaIdentifier :: Bool -> Aliases -> Name -> Java.Identifier
elementJavaIdentifier isPrim aliases name = Java.Identifier $ if isPrim
    then (Java.unIdentifier $ nameToJavaName aliases $ fromQname gname $ capitalize local) ++ ".apply"
    else (Java.unIdentifier $ nameToJavaName aliases $ fromQname gname $ elementsClassName gname) ++ "." ++ local
  where
    (gname, local) = toQnameEager name

elementsClassName :: Namespace -> String
elementsClassName (Namespace ns) = capitalize $ L.last $ LS.splitOn "/" ns

encodeApplication :: (Eq a, Ord a, Read a, Show a)
  => Aliases -> Application a -> GraphFlow a Java.Expression
encodeApplication aliases app@(Application lhs rhs) = case stripTerm fun of
    TermFunction f -> case f of
      FunctionPrimitive name -> forNamedFunction aliases True name args
      FunctionElimination EliminationElement -> if L.length args > 0
        then case stripTerm (L.head args) of
          TermElement name -> do
            forNamedFunction aliases False name (L.tail args)
          _ -> fallback
        else fallback
      _ -> fallback
    _ -> fallback
  where
    (fun, args) = uncurry [] lhs rhs
      where
       uncurry args lhs rhs = case stripTerm lhs of
         TermApplication (Application lhs' rhs') -> uncurry (rhs:args) lhs' rhs'
         _ -> (lhs, (rhs:args))

    fallback = do
        t <- requireTypeAnnotation lhs
        (dom, cod) <- case stripType t of
            TypeFunction (FunctionType dom cod) -> pure (dom, cod)
            t' -> fail $ "expected a function type on function " ++ show lhs ++ ", but found " ++ show t'
        case stripTerm lhs of
          TermFunction f -> case f of
            FunctionElimination e -> case e of
              EliminationElement -> encodeTerm aliases rhs
              _ -> do
                jarg <- encodeTerm aliases rhs
                encodeElimination aliases (Just jarg) dom cod e
            _ -> defaultExpression
          _ -> defaultExpression
      where
        defaultExpression = do
          -- Note: the domain type will not be used, so we just substitute the unit type
          jfun <- encodeTerm aliases lhs
          jarg <- encodeTerm aliases rhs
          let prim = javaExpressionToJavaPrimary jfun
          return $ javaMethodInvocationToJavaExpression $ methodInvocation (Just $ Right prim) (Java.Identifier "apply") [jarg]

encodeElimination :: (Eq a, Ord a, Read a, Show a)
  => Aliases -> Maybe Java.Expression -> Type a -> Type a -> Elimination a -> GraphFlow a Java.Expression
encodeElimination aliases marg dom cod elm = case elm of
  EliminationElement -> case marg of
    Nothing -> encodeFunction aliases dom cod $ FunctionLambda $ Lambda var $ TermVariable var
      where
        var = Name "e"
    Just jarg -> pure jarg
  EliminationOptional (OptionalCases nothing just) -> do
    jnothing <- encodeTerm aliases nothing
    jjust <- encodeTerm aliases just
    let var = Name "m"

    let jobj = case marg of
                  Nothing -> Left $ javaIdentifierToJavaExpressionName $ variableToJavaIdentifier var
                  Just jarg -> Right $ javaExpressionToJavaPrimary jarg
    let jhead = javaMethodInvocationToJavaExpression $ methodInvocation
          (Just jobj)
          (Java.Identifier "map") [jjust]
    let jbody = javaMethodInvocationToJavaExpression $ methodInvocation
          (Just $ Right $ javaExpressionToJavaPrimary jhead)
          (Java.Identifier "orElse") [jnothing]
    castType <- encodeType aliases (TypeFunction $ FunctionType dom cod) >>= javaTypeToJavaReferenceType
    return $ case marg of
      Nothing -> javaCastExpressionToJavaExpression $ javaCastExpression aliases castType $
                       javaExpressionToJavaUnaryExpression $ javaLambda var jbody
      Just _ -> jbody
  EliminationRecord (Projection _ fname) -> do
    jdomr <- encodeType aliases dom >>= javaTypeToJavaReferenceType
    jexp <- case marg of
      Nothing -> pure $ javaLambda var jbody
        where
          var = Name "r"
          jbody = javaExpressionNameToJavaExpression $
            fieldExpression (variableToJavaIdentifier var) (javaIdentifier $ unFieldName fname)
      Just jarg -> pure $ javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier $ unFieldName fname)
        where
          qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
    return jexp
  EliminationUnion (CaseStatement tname def fields) -> case marg of
      Nothing -> do
        g <- getState
        let anns = graphAnnotations g
        let lhs = annotationClassSetTermType anns g (Just $ Types.function (Types.wrap tname) cod) $ Terms.elimination elm
        let var = "u"
        encodeTerm aliases $ Terms.lambda var $ Terms.apply lhs (Terms.var var)
        -- TODO: default value
      Just jarg -> applyElimination jarg
    where
      applyElimination jarg = do
          let prim = javaExpressionToJavaPrimary jarg
          let consId = innerClassRef aliases tname $ case def of
                Nothing -> visitorName
                Just _ -> partialVisitorName
          jcod <- encodeType aliases cod
          let targs = Java.TypeArgumentsOrDiamondDiamond
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
            let param = javaTypeToJavaFormalParameter jdom $ FieldName instanceName
            let result = Java.ResultType $ Java.UnannType jcod
            jret <- encodeTerm aliases d
            let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret
            return $ noComment $ methodDeclaration mods [] anns otherwiseMethodName [param] result (Just [returnStmt])

          visitBranch jcod field = do
            targs <- javaTypeArgumentsForNamedType tname
            let jdom = Java.TypeReference $ nameToJavaReferenceType aliases True targs tname (Just $ capitalize $ unFieldName $ fieldName field)
            let mods = [Java.MethodModifierPublic]
            let anns = [overrideAnnotation]
            let param = javaTypeToJavaFormalParameter jdom $ FieldName instanceName
            let result = Java.ResultType $ Java.UnannType jcod
            -- Note: the escaping is necessary because the instance.value field reference does not correspond to an actual Hydra projection term
            let value = Terms.var ("$" ++ instanceName ++ "." ++ valueFieldName)
            jret <- encodeTerm aliases $ contractTerm $ Terms.apply (fieldTerm field) value
            let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret
            return $ noComment $ methodDeclaration mods [] anns visitMethodName [param] result (Just [returnStmt])
  EliminationWrap name -> case marg of
    Nothing -> pure $ javaLambda var jbody
      where
        var = Name "w"
        arg = javaIdentifierToJavaExpression $ variableToJavaIdentifier var
        jbody = javaConstructorCall (javaConstructorName (nameToJavaName aliases name) Nothing) [arg] Nothing
    Just jarg -> pure $ javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier valueFieldName)
      where
        qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
  _ -> pure $ encodeLiteral $ LiteralString $
    "Unimplemented elimination variant: " ++ show (eliminationVariant elm) -- TODO: temporary

encodeFunction :: (Eq a, Ord a, Read a, Show a)
  => Aliases -> Type a -> Type a -> Function a -> GraphFlow a Java.Expression
encodeFunction aliases dom cod fun = case fun of
  FunctionElimination elm -> do
    encodeElimination aliases Nothing dom cod elm
  FunctionLambda (Lambda var body) -> do
    jbody <- encodeTerm aliases body
    return $ javaLambda var jbody
--  FunctionPrimitive name ->
  _ -> pure $ encodeLiteral $ LiteralString $
    "Unimplemented function variant: " ++ show (functionVariant fun) -- TODO: temporary

encodeLiteral :: Literal -> Java.Expression
encodeLiteral lit = javaLiteralToJavaExpression $ case lit of
  LiteralBoolean b -> javaBoolean b
  LiteralFloat f -> Java.LiteralFloatingPoint $ Java.FloatingPointLiteral $ case f of
    FloatValueFloat32 v -> realToFrac v
    FloatValueFloat64 v -> v
  LiteralInteger i -> case i of
      IntegerValueBigint v -> integer v -- BigInteger
      IntegerValueInt16 v -> int v -- short
      IntegerValueInt32 v -> int v -- int
      IntegerValueInt64 v -> integer v -- long
      IntegerValueUint8 v -> int v -- byte
      IntegerValueUint16 v -> Java.LiteralCharacter $ fromIntegral v -- char
    where
      integer = Java.LiteralInteger . Java.IntegerLiteral
      int = integer . fromIntegral
  LiteralString s -> javaString s

-- Note: we use Java object types everywhere, rather than primitive types, as the latter cannot be used
--       to build function types, parameterized types, etc.
encodeLiteralType :: LiteralType -> GraphFlow a Java.Type
encodeLiteralType lt = case lt of
    LiteralTypeBoolean -> simple "Boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeFloat32 -> simple "Float"
      FloatTypeFloat64 -> simple "Double"
      _ -> fail $ "unexpected float type: " ++ show ft
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigInteger"
      IntegerTypeInt16 -> simple "Short"
      IntegerTypeInt32 -> simple "Integer"
      IntegerTypeInt64 -> simple "Long"
      IntegerTypeUint8 -> simple "Byte"
      IntegerTypeUint16 -> simple "Character"
      _ -> fail $ "unexpected integer type: " ++ show it
    LiteralTypeString -> simple "String"
    _ -> fail $ "unexpected literal type: " ++ show lt
  where
    simple n = pure $ javaRefType [] Nothing n

encodeNullaryConstant :: (Eq a, Ord a, Read a, Show a)
  => Aliases -> Type a -> Function a -> GraphFlow a Java.Expression
encodeNullaryConstant aliases typ fun = case fun of
  FunctionPrimitive name -> forNamedFunction aliases True name []
  _ -> unexpected "nullary function" fun

encodeTerm :: (Eq a, Ord a, Read a, Show a)
  => Aliases -> Term a -> GraphFlow a Java.Expression
encodeTerm aliases term0 = encodeInternal [] term0
  where
    encode = encodeTerm aliases
    failAsLiteral msg = pure $ encodeLiteral $ LiteralString msg
    encodeInternal anns term = case term of
        TermAnnotated (Annotated term' ann) -> encodeInternal (ann:anns) term'

        TermApplication app -> encodeApplication aliases app

        TermElement name -> pure $ javaIdentifierToJavaExpression $ elementJavaIdentifier False aliases name

        TermFunction f -> do
          t <- requireTypeAnnotation term0
          case t of
            TypeFunction (FunctionType dom cod) -> do
              encodeFunction aliases dom cod f
            _ -> encodeNullaryConstant aliases t f

        TermList els -> do
          jels <- CM.mapM encode els
          return $ javaMethodInvocationToJavaExpression $
            methodInvocationStatic (Java.Identifier "java.util.Arrays") (Java.Identifier "asList") jels

        TermLiteral l -> pure $ encodeLiteral l

        TermWrap (Nominal name arg) -> do
          jarg <- encode arg
          return $ javaConstructorCall (javaConstructorName (nameToJavaName aliases name) Nothing) [jarg] Nothing

        TermOptional mt -> case mt of
          Nothing -> pure $ javaMethodInvocationToJavaExpression $
            methodInvocationStatic (Java.Identifier "java.util.Optional") (Java.Identifier "empty") []
          Just term1 -> do
            expr <- encode term1
            return $ javaMethodInvocationToJavaExpression $
              methodInvocationStatic (Java.Identifier "java.util.Optional") (Java.Identifier "of") [expr]

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

        TermUnion (Injection name (Field (FieldName fname) v)) -> do
          let (Java.Identifier typeId) = nameToJavaName aliases name
          let consId = Java.Identifier $ typeId ++ "." ++ sanitizeJavaName (capitalize fname)
          args <- if isUnitTerm v
            then return []
            else do
              ex <- encode v
              return [ex]
          return $ javaConstructorCall (javaConstructorName consId Nothing) args Nothing

        TermVariable (Name v) -> pure $ javaIdentifierToJavaExpression $ javaIdentifier v

        _ -> failAsLiteral $ "Unimplemented term variant: " ++ show (termVariant term)

encodeType :: Show a => Aliases -> Type a -> GraphFlow a Java.Type
encodeType aliases t = case stripType t of
  TypeApplication (ApplicationType lhs rhs) -> do
    jlhs <- encode lhs
    jrhs <- encode rhs >>= javaTypeToJavaReferenceType
    addJavaTypeParameter jrhs jlhs
  TypeElement et -> encode et -- Elements are simply unboxed
  TypeFunction (FunctionType dom cod) -> do
    jdom <- encode dom >>= javaTypeToJavaReferenceType
    jcod <- encode cod >>= javaTypeToJavaReferenceType
    return $ javaRefType [jdom, jcod] javaUtilFunctionPackageName "Function"
  TypeLambda (LambdaType (Name v) body) -> do
    jbody <- encode body
    addJavaTypeParameter (javaTypeVariable v) jbody
  TypeList et -> do
    jet <- encode et
    if listsAsArrays
      then toJavaArrayType jet
      else do
        rt <- javaTypeToJavaReferenceType jet
        return $ javaRefType [rt] javaUtilPackageName "List"
  TypeLiteral lt -> encodeLiteralType lt
  TypeMap (MapType kt vt) -> do
    jkt <- encode kt >>= javaTypeToJavaReferenceType
    jvt <- encode vt >>= javaTypeToJavaReferenceType
    return $ javaRefType [jkt, jvt] javaUtilPackageName "Map"
  TypeProduct [] -> unit
  TypeRecord (RowType _UnitType _ []) -> unit
  TypeRecord (RowType name _ _) -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True [] name Nothing
  TypeOptional ot -> do
    jot <- encode ot >>= javaTypeToJavaReferenceType
    return $ javaRefType [jot] javaUtilPackageName "Optional"
  TypeSet st -> do
    jst <- encode st >>= javaTypeToJavaReferenceType
    return $ javaRefType [jst] javaUtilPackageName "Set"
  TypeUnion (RowType name _ _) -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True [] name Nothing
  TypeVariable name -> pure $ if isLambdaBoundVariable name
    then variableReference name
    else nameReference name
  TypeWrap name -> pure $ nameReference name
  _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    nameReference name = Java.TypeReference $ nameToJavaReferenceType aliases True [] name Nothing
    variableReference name = Java.TypeReference $ javaTypeVariable $ unName name
    encode = encodeType aliases
    unit = return $ javaRefType [] javaLangPackageName "Void"

fieldTypeToFormalParam aliases (FieldType fname ft) = do
  jt <- encodeType aliases ft
  return $ javaTypeToJavaFormalParameter jt fname

forNamedFunction :: (Eq a, Ord a, Read a, Show a) => Aliases -> Bool -> Name -> [Term a] -> GraphFlow a Java.Expression
forNamedFunction aliases prim name args = do
    jargs <- CM.mapM (encodeTerm aliases) args
    let header = Java.MethodInvocation_HeaderSimple $ Java.MethodName $ elementJavaIdentifier prim aliases name
    return $ javaMethodInvocationToJavaExpression $ Java.MethodInvocation header jargs

getCodomain :: Show a => a -> GraphFlow a (Type a)
getCodomain ann = functionTypeCodomain <$> getFunctionType ann

getFunctionType :: Show a => a -> GraphFlow a (FunctionType a)
getFunctionType ann = do
  g <- getState
  mt <- annotationClassTypeOf (graphAnnotations g) ann
  case mt of
    Nothing -> fail "type annotation is required for function and elimination terms in Java"
    Just t -> case t of
      TypeFunction ft -> return ft
      _ -> unexpected "function type (3)" t

innerClassRef :: Aliases -> Name -> String -> Java.Identifier
innerClassRef aliases name local = Java.Identifier $ id ++ "." ++ local
  where
    Java.Identifier id = nameToJavaName aliases name

isLambdaBoundVariable :: Name -> Bool
isLambdaBoundVariable (Name v) = L.length v <= 3

javaTypeArgumentsForNamedType :: Show a => Name -> GraphFlow a [Java.TypeArgument]
javaTypeArgumentsForNamedType tname = do
    params <- javaTypeParametersForType <$> requireType tname
    return $ typeParameterToTypeArgument <$> params

-- Note: this is somewhat of a hack; it compensates for the irregular way in which type parameters are currently used.
--       When this irregularity is resolved, a better approach will be to simply pick up type parameters from type applications.
javaTypeParametersForType :: Type a -> [Java.TypeParameter]
javaTypeParametersForType typ = toParam <$> vars
  where
    toParam (Name v) = Java.TypeParameter [] (javaTypeIdentifier $ capitalize v) Nothing
    vars = L.filter isLambdaBoundVariable $ S.toList $ freeVariablesInType typ

maybeLet :: (Ord a, Read a, Show a) => Aliases -> Term a -> (Term a -> [Java.BlockStatement] -> GraphFlow a x) -> GraphFlow a x
maybeLet aliases term cons = helper [] term
  where
    helper anns term = case term of
      TermAnnotated (Annotated term' ann) -> helper (ann:anns) term'
      TermLet (Let bindings env) -> do
          stmts <- L.concat <$> CM.mapM toDeclStatements sorted
          maybeLet aliases env $ \tm stmts' -> cons (reannotate anns tm) (stmts ++ stmts')
        where
          toDeclStatements names = do
            inits <- Y.catMaybes <$> CM.mapM toDeclInit names
            impls <- CM.mapM toDeclStatement names
            return $ inits ++ impls

          toDeclInit name = if S.member name recursiveVars
            then do
              -- TODO: repeated
              let value = Y.fromJust $ M.lookup name bindings
              typ <- requireAnnotatedType value
              jtype <- encodeType aliases typ
              let id = variableToJavaIdentifier name

              let pkg = javaPackageName ["java", "util", "concurrent", "atomic"]
              let arid = Java.Identifier "java.util.concurrent.atomic.AtomicReference" -- TODO
              let targs = Java.TypeArgumentsOrDiamondDiamond
              let aid = Java.AnnotatedIdentifier [] arid
              let ci = Java.ClassOrInterfaceTypeToInstantiate [aid] (Just targs)
              let body = javaConstructorCall ci [] Nothing

              rt <- javaTypeToJavaReferenceType jtype
              let artype = javaRefType [rt] (Just pkg) "AtomicReference"
              return $ Just $ variableDeclarationStatement aliases artype id body
            else pure Nothing

          toDeclStatement name = do
            -- TODO: repeated
            let value = Y.fromJust $ M.lookup name bindings
            typ <- requireAnnotatedType value
            jtype <- encodeType aliases typ
            let id = variableToJavaIdentifier name

            rhs <- encodeTerm aliases value
            return $ if S.member name recursiveVars
              then Java.BlockStatementStatement $ javaMethodInvocationToJavaStatement $
                methodInvocation (Just $ Left $ Java.ExpressionName Nothing id) (Java.Identifier "set") [rhs]
              else variableDeclarationStatement aliases jtype id rhs
          bindingVars = S.fromList $ M.keys bindings
          recursiveVars = S.fromList $ L.concat (ifRec <$> sorted)
            where
              ifRec names = case names of
                [name] -> case M.lookup name allDeps of
                  Nothing -> []
                  Just deps -> if S.member name deps
                    then [name]
                    else []
                _ -> names
          allDeps = M.fromList (toDeps <$> M.toList bindings)
            where
              toDeps (key, value) = (key, S.filter (\n -> S.member n bindingVars) $ freeVariablesInTerm value)
          sorted = topologicalSortComponents (toDeps <$> M.toList allDeps)
            where
              toDeps (key, deps) = (key, S.toList deps)
      TermFunction (FunctionLambda (Lambda v body)) -> maybeLet aliases body $
        \tm stmts' -> cons (reannotate anns (TermFunction (FunctionLambda (Lambda v tm)))) stmts'
      _ -> cons (reannotate anns term) []

reannotate anns term = case anns of
  [] -> term
  (h:r) -> reannotate r $ TermAnnotated (Annotated term h)

requireAnnotatedType :: Show a => Term a -> GraphFlow a (Type a)
requireAnnotatedType term = case term of
  TermAnnotated (Annotated _ ann) -> do
    g <- getState
    mt <- annotationClassTypeOf (graphAnnotations g) ann
    case mt of
      Nothing -> fail $ "expected a type annotation for term: " ++ show term
      Just t -> pure t

toClassDecl :: (Eq a, Ord a, Read a, Show a) => Bool -> Aliases -> [Java.TypeParameter]
  -> Name -> Type a -> GraphFlow a Java.ClassDeclaration
toClassDecl isInner aliases tparams elName t = case stripType t of
    TypeRecord rt -> declarationForRecordType isInner aliases tparams elName $ rowTypeFields rt
    TypeUnion rt -> declarationForUnionType aliases tparams elName $ rowTypeFields rt
    TypeLambda ut -> declarationForLambdaType aliases tparams elName ut
    -- Other types are not supported as class declarations, so we wrap them as record types.
    _ -> wrap t -- TODO: wrap and unwrap the corresponding terms as record terms.
  where
    wrap t' = declarationForRecordType isInner aliases tparams elName [Types.field valueFieldName t']

toDataDeclaration :: Aliases -> (a, TypedTerm a) -> GraphFlow a a
toDataDeclaration aliases (el, TypedTerm typ term) = do
  fail "not implemented" -- TODO

typeNameDecl :: (Ord a, Read a, Show a) => Aliases -> Name -> GraphFlow a Java.ClassBodyDeclarationWithComments
typeNameDecl aliases name = do
  jt <- encodeType aliases $ Types.wrap _Name
  arg <- encodeTerm aliases $ Terms.string $ unName name
  let init = Java.VariableInitializerExpression $ javaConstructorCall (javaConstructorName nameName Nothing) [arg] Nothing
  let var = javaVariableDeclarator (Java.Identifier "NAME") (Just init)
  return $ noComment $ javaMemberField mods jt var
  where
    mods = [Java.FieldModifierPublic, Java.FieldModifierStatic, Java.FieldModifierFinal]
    nameName = nameToJavaName aliases _Name
