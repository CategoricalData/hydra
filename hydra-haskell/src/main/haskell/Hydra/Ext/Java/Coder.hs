module Hydra.Ext.Java.Coder (printGraph) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.CoreDecoding
import Hydra.Ext.Java.Utils
import Hydra.Ext.Java.Language
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import qualified Hydra.Ext.Java.Syntax as Java
import Hydra.Util.Coders
import Hydra.Util.Formatting
import Hydra.Util.Codetree.Script
import Hydra.Ext.Java.Serde
import Hydra.Ext.Java.Settings
import Hydra.Impl.Haskell.Extras
import Hydra.Basics

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


printGraph :: (Ord m, Read m, Show m) => Context m -> Graph m -> Qualified (M.Map FilePath String)
printGraph cx g = do
    units <- moduleToJavaCompilationUnit cx g
    return $ M.fromList $ forPair <$> M.toList units
  where
    forPair (name, unit) = (
      elementNameToFilePath name,
      printExpr $ parenthesize $ writeCompilationUnit unit)

commentsFromElement :: Context m -> Element m -> Result (Maybe String)
commentsFromElement cx el = annotationClassTermDescription (contextAnnotations cx) cx (elementData el)

commentsFromFieldType :: Context m -> FieldType m -> Result (Maybe String)
commentsFromFieldType cx (FieldType _ t) = annotationClassTypeDescription (contextAnnotations cx) cx t

addComment :: Context m -> Java.ClassBodyDeclaration -> FieldType m -> Result Java.ClassBodyDeclarationWithComments
addComment cx decl field = Java.ClassBodyDeclarationWithComments decl <$> commentsFromFieldType cx field

noComment :: Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments
noComment decl = Java.ClassBodyDeclarationWithComments decl Nothing

elementNameToFilePath :: Name -> FilePath
elementNameToFilePath name = nameToFilePath False (FileExtension "java") $ fromQname ns (sanitizeJavaName local)
  where
    (ns, local) = toQname name

moduleToJavaCompilationUnit :: (Ord m, Read m, Show m) => Context m -> Graph m
  -> Qualified (M.Map Name Java.CompilationUnit)
moduleToJavaCompilationUnit cx g = graphToExternalModule language (encodeTerm aliases) constructModule cx g
  where
    aliases = importAliasesForGraph g

classModsPublic :: [Java.ClassModifier]
classModsPublic = [Java.ClassModifierPublic]

constructModule :: (Ord m, Read m, Show m)
  => Context m -> Graph m -> M.Map (Type m) (Coder (Term m) Java.Expression) -> [(Element m, TypedTerm m)]
  -> Result (M.Map Name Java.CompilationUnit)
constructModule cx g coders pairs = do
    typeUnits <- CM.mapM typeToClass typePairs
    dataMembers <- CM.mapM (termToInterfaceMember coders) dataPairs
    return $ M.fromList $ typeUnits ++ ([constructElementsInterface g dataMembers | not (L.null dataMembers)])
  where
    pkg = javaPackageDeclaration $ graphName g
    isTypePair = isType cx . typedTermType . snd
    typePairs = L.filter isTypePair pairs
    dataPairs = L.filter (not . isTypePair) pairs
    aliases = importAliasesForGraph g

    typeToClass pair@(el, _) = do
      let imports = []
      decl <- declarationForType aliases cx pair
      return (elementName el,
        Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) imports [decl])

    termToInterfaceMember coders pair@(el, TypedTerm typ term) = do
        {-
          = InterfaceMemberDeclarationConstant ConstantDeclaration
          | InterfaceMemberDeclarationInterfaceMethod InterfaceMethodDeclaration
        -}

        jtype <- Java.UnannType <$> encodeType cx aliases typ
        jterm <- coderEncode (Y.fromJust $ M.lookup typ coders) term
        let mods = []
        let var = javaVariableDeclarator (javaVariableName $ elementName el) $ Just $ Java.VariableInitializerExpression jterm
        return $ Java.InterfaceMemberDeclarationConstant $ Java.ConstantDeclaration mods jtype [var]
      where
        isFunctionType t = case t of
          TypeFunction _ -> True
          _ -> False

constructElementsInterface :: Graph m -> [Java.InterfaceMemberDeclaration] -> (Name, Java.CompilationUnit)
constructElementsInterface g members = (elName, Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) [] [decl])
  where
    pkg = javaPackageDeclaration $ graphName g
    mods = []
    elName = fromQname (graphName g) "Elements"
    body = Java.InterfaceBody members
    itf = Java.TypeDeclarationInterface $ Java.InterfaceDeclarationNormalInterface $
      Java.NormalInterfaceDeclaration mods (javaTypeIdentifier "Elements") [] [] body
    decl = Java.TypeDeclarationWithComments itf Nothing

declarationForRecordType :: Show m => M.Map GraphName Java.PackageName -> Context m -> [Java.TypeParameter] -> Name
  -> [FieldType m] -> Result Java.ClassDeclaration
declarationForRecordType aliases cx tparams elName fields = do
    memberVars <- CM.mapM toMemberVar fields
    memberVars' <- CM.zipWithM (addComment cx) memberVars fields
    withMethods <- if L.length fields > 1
      then CM.mapM toWithMethod fields
      else pure []
    cons <- constructor
    let bodyDecls = memberVars' ++ (noComment <$> [cons, equalsMethod, hashCodeMethod] ++ withMethods)
    return $ javaClassDeclaration aliases tparams elName classModsPublic Nothing bodyDecls
  where
    constructor = do
      params <- CM.mapM fieldToFormalParam fields
      let stmts = Java.BlockStatementStatement . toAssignStmt . fieldTypeName <$> fields
      return $ makeConstructor aliases elName False params stmts

    fieldArgs = fieldNameToJavaExpression . fieldTypeName <$> fields

    toMemberVar (FieldType fname ft) = do
      let mods = [Java.FieldModifierPublic, Java.FieldModifierFinal]
      jt <- encodeType cx aliases ft
      let var = fieldNameToJavaVariableDeclarator fname
      return $ javaMemberField mods jt var

    toWithMethod field = do
      let mods = [Java.MethodModifierPublic]
      let methodName = "with" ++ capitalize (unFieldName $ fieldTypeName field)
      param <- fieldToFormalParam field
      let anns = [] -- TODO
      let result = referenceTypeToResult $ nameToJavaReferenceType aliases False elName
      let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $
            javaConstructorCall (javaConstructorName True $ localNameOf elName) fieldArgs
      return $ methodDeclaration mods [] anns methodName [param] result (Just [returnStmt])

    fieldToFormalParam (FieldType fname ft) = do
      jt <- encodeType cx aliases ft
      return $ javaTypeToJavaFormalParameter jt fname

    equalsMethod = methodDeclaration mods [] anns "equals" [param] result $
        Just [instanceOfStmt,
          castStmt,
          returnAllFieldsEqual]
      where
        anns = [overrideAnnotation]
        mods = [Java.MethodModifierPublic]
        param = javaTypeToJavaFormalParameter (javaRefType [] Nothing "Object") (FieldName otherName)
        result = javaTypeToJavaResult javaBooleanType
        otherName = "other"
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
                other = javaIdentifierToJavaRelationalExpression $ javaIdentifier otherName
                parent = nameToJavaReferenceType aliases False elName

            returnFalse = javaReturnStatement $ Just $ javaBooleanExpression False

        castStmt = variableDeclarationStatement aliases elName id rhs
          where
            id = javaIdentifier tmpName
            rhs = javaUnaryExpressionToJavaExpression $ Java.UnaryExpressionOther $
              Java.UnaryExpressionNotPlusMinusCast $ javaCastExpression aliases elName $ Java.Identifier $
                sanitizeJavaName otherName

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
                header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex var [] (Java.Identifier "equals")
                var = Java.MethodInvocation_VariantExpression $ Java.ExpressionName Nothing $ Java.Identifier $
                  sanitizeJavaName fname

    hashCodeMethod = methodDeclaration mods [] anns "hashCode" [] result $ Just [returnSum]
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
                  javaLiteralToPrimary $ javaInt i
                rhs = javaPostfixExpressionToJavaUnaryExpression $
                  javaMethodInvocationToJavaPostfixExpression $
                  methodInvocation (Just $ javaIdentifier fname) (Java.Identifier "hashCode") []

            multipliers = L.cycle first20Primes
              where
                first20Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

declarationForType :: (Ord m, Read m, Show m)
  => M.Map GraphName Java.PackageName -> Context m -> (Element m, TypedTerm m) -> Result Java.TypeDeclarationWithComments
declarationForType aliases cx (el, TypedTerm _ term) = do
    t <- decodeType cx term >>= adaptType cx language
    cd <- toClassDecl aliases cx [] (elementName el) t
    comments <- commentsFromElement cx el
    return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass cd) comments

declarationForUnionType :: (Show m, Eq m) => M.Map GraphName Java.PackageName -> Context m
  -> [Java.TypeParameter] -> Name -> [FieldType m] -> Result Java.ClassDeclaration
declarationForUnionType aliases cx tparams elName fields = do
    variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
    let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
    variantDecls' <- CM.zipWithM (addComment cx) variantDecls fields
    let otherDecls = noComment <$> [privateConstructor, toAcceptMethod True, visitor, partialVisitor]
    let bodyDecls = otherDecls ++ variantDecls'
    let mods = classModsPublic ++ [Java.ClassModifierAbstract]
    return $ javaClassDeclaration aliases tparams elName mods Nothing bodyDecls
  where
    privateConstructor = makeConstructor aliases elName True [] []
    unionFieldClass (FieldType fname ftype) = do
      let rtype = Types.record $ if Types.isUnit ftype then [] else [FieldType (FieldName "value") ftype]
      toClassDecl aliases cx [] (variantClassName elName fname) rtype
    augmentVariantClass (Java.ClassDeclarationNormal cd) = Java.ClassDeclarationNormal $ cd {
        Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic, Java.ClassModifierStatic, Java.ClassModifierFinal],
        Java.normalClassDeclarationExtends = Just $ nameToJavaClassType aliases True args elName,
        Java.normalClassDeclarationParameters = tparams,
        Java.normalClassDeclarationBody = newBody (Java.normalClassDeclarationBody cd)}
      where
        newBody (Java.ClassBody decls) = Java.ClassBody $ decls ++ [noComment $ toAcceptMethod False]
        args = typeParameterToTypeArgument <$> tparams

    visitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration mods ti tparams extends body
      where
        mods = [Java.InterfaceModifierPublic]
        ti = Java.TypeIdentifier $ Java.Identifier "Visitor"
        tparams = [javaTypeParameter "R"]
        extends = []
        body = Java.InterfaceBody (toVisitMethod . fieldTypeName <$> fields)
          where
            toVisitMethod fname = interfaceMethodDeclaration [] [] "visit" [variantInstanceParam fname] resultR Nothing

    partialVisitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration {
            Java.normalInterfaceDeclarationModifiers = [Java.InterfaceModifierPublic],
            Java.normalInterfaceDeclarationIdentifier = Java.TypeIdentifier $ Java.Identifier "PartialVisitor",
            Java.normalInterfaceDeclarationParameters = [javaTypeParameter "R"],
            Java.normalInterfaceDeclarationExtends =
              [Java.InterfaceType $ javaClassType [visitorTypeVariable] Nothing "Visitor"],
            Java.normalInterfaceDeclarationBody = Java.InterfaceBody $ otherwise:(toVisitMethod . fieldTypeName <$> fields)}
      where
        otherwise = interfaceMethodDeclaration defaultMod [] "otherwise" [mainInstanceParam] resultR $ Just [throw]
          where
            throw = Java.BlockStatementStatement $ Java.StatementWithoutTrailing $
                Java.StatementWithoutTrailingSubstatementThrow $ Java.ThrowStatement $
                javaConstructorCall (javaConstructorName False "IllegalStateException") args
              where
                args = [javaAdditiveExpressionToJavaExpression $ addExpressions [
                  javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                  Java.MultiplicativeExpressionUnary $ javaIdentifierToJavaUnaryExpression $ Java.Identifier "instance"]]

        toVisitMethod fname = interfaceMethodDeclaration defaultMod [] "visit" [variantInstanceParam fname] resultR $
            Just [returnOtherwise]
          where
            returnOtherwise = Java.BlockStatementStatement $ javaReturnStatement $ Just $
              javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray $ Java.PrimaryNoNewArrayMethodInvocation $
              methodInvocation Nothing (Java.Identifier "otherwise") [javaIdentifierToJavaExpression $ Java.Identifier "instance"]

    defaultMod = [Java.InterfaceMethodModifierDefault]

    resultR = javaTypeToJavaResult $ Java.TypeReference visitorTypeVariable

    mainInstanceParam = javaTypeToJavaFormalParameter classRef (FieldName "instance")
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False [] elName

    variantInstanceParam fname = javaTypeToJavaFormalParameter classRef (FieldName "instance")
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False [] $ variantClassName elName fname

declarationForLambdaType :: (Show m, Eq m) => M.Map GraphName Java.PackageName -> Context m
  -> [Java.TypeParameter] -> Name -> LambdaType m -> Result Java.ClassDeclaration
declarationForLambdaType aliases cx tparams elName (LambdaType (VariableType v) body) =
    toClassDecl aliases cx (tparams ++ [param]) elName body
  where
    param = javaTypeParameter $ capitalize v

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
encodeLiteralType :: LiteralType -> Result Java.Type
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

encodeTerm :: (Eq m, Ord m, Read m, Show m)
  => M.Map GraphName Java.PackageName -> Context m -> Term m -> Result Java.Expression
encodeTerm aliases cx term = case term of
    TermAnnotated (Annotated term' _) -> encode term' -- TODO: annotations to comments where possible
  --  TermApplication (Application m)
  --  TermElement Name
  --  TermFunction (Function m)
    TermList els -> do
      jels <- CM.mapM encode els
      return $ javaMethodInvocationToJavaExpression $
        methodInvocation (Just $ Java.Identifier "java.util.Arrays") (Java.Identifier "asList") jels
    TermLiteral l -> pure $ encodeLiteral l
  --  TermMap (Map (Term m) (Term m))
  --  TermNominal (Named m)
  --  TermOptional (Maybe (Term m))
  --  TermRecord [Field m]
  --  TermSet (Set (Term m))
--    TermUnion (Field (FieldName fname) v) -> do
--      let foo = javaConstructorCall (javaConstructorName True $ localNameOf elName) fieldArgs
    TermVariable (Variable v) -> pure $ javaIdentifierToJavaExpression $ javaIdentifier v
    _ -> pure $ encodeLiteral $ LiteralString $ "TODO: " ++ show (termVariant term)
  --  _ -> unexpected "term" $ show term
  where
    encode = encodeTerm aliases cx
    

encodeType :: Show m => Context m -> M.Map GraphName Java.PackageName -> Type m -> Result Java.Type
encodeType cx aliases t = case typeExpr cx t of
  TypeApplication (ApplicationType lhs rhs) -> do
    jlhs <- encode lhs
    jrhs <- encode rhs >>= javaTypeToJavaReferenceType
    addJavaTypeParameter jrhs jlhs
  TypeElement et -> encode et -- Elements are simply unboxed
  TypeFunction (FunctionType dom cod) -> do
    jdom <- encode dom >>= javaTypeToJavaReferenceType
    jcod <- encode cod >>= javaTypeToJavaReferenceType
    return $ javaRefType [jdom, jcod] javaUtilFunctionPackageName "Function"
  TypeLambda (LambdaType (VariableType v) body) -> do
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
  TypeNominal name -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True name
  TypeOptional ot -> do
    jot <- encode ot >>= javaTypeToJavaReferenceType
    return $ javaRefType [jot] javaUtilPackageName "Optional"
  TypeSet st -> do
    jst <- encode st >>= javaTypeToJavaReferenceType
    return $ javaRefType [jst] javaUtilPackageName "Set"
  TypeVariable (VariableType v) -> pure $ Java.TypeReference $ javaTypeVariable v
  TypeRecord (RowType _ []) -> return $ javaRefType [] javaLangPackageName "Void"
  -- Note: record (other than unit) and union types should not appear at this level
  _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    encode = encodeType cx aliases

toClassDecl :: (Show m, Eq m) => M.Map GraphName Java.PackageName -> Context m -> [Java.TypeParameter]
  -> Name -> Type m -> Result Java.ClassDeclaration
toClassDecl aliases cx tparams elName t = case typeExpr cx t of
    TypeRecord rt -> declarationForRecordType aliases cx tparams elName $ rowTypeFields rt
    TypeUnion rt -> declarationForUnionType aliases cx tparams elName $ rowTypeFields rt
    TypeLambda ut -> declarationForLambdaType aliases cx tparams elName ut
    -- Other types are not supported as class declarations, so we wrap them as record types.
    _ -> wrap t -- TODO: wrap and unwrap the corresponding terms as record terms.
  where
    wrap t' = declarationForRecordType aliases cx tparams elName [Types.field "value" t']

toDataDeclaration :: M.Map GraphName Java.PackageName -> Context m -> (a, TypedTerm m) -> Result a
toDataDeclaration aliases cx (el, TypedTerm typ term) = do
  fail "not implemented" -- TODO
