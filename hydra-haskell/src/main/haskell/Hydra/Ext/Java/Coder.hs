module Hydra.Ext.Java.Coder (
  moduleToJavaCompilationUnit,
  javaLanguage,
) where

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

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


listsAsArrays :: Bool
listsAsArrays = False

moduleToJavaCompilationUnit :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified Java.CompilationUnit
moduleToJavaCompilationUnit cx g = dataGraphToExternalModule javaLanguage (encodeTerm aliases) constructModule cx g
  where
    aliases = importAliasesForGraph g

classModsPublic :: [Java.ClassModifier]
classModsPublic = [Java.ClassModifierPublic]

constructModule :: (Default m, Ord m, Read m, Show m)
  => Context m -> Graph m -> M.Map (Type m) (Step (Term m) Java.Block) -> [(Element m, TypedTerm m)]
  -> Result Java.CompilationUnit
constructModule cx g coders pairs = do
    let pkg = javaPackageDeclaration $ graphName g
    let imports = []
    typeDecls <- CM.mapM (toTypeDeclaration aliases cx) typePairs
    dataDecls <- CM.mapM (toDataDeclaration aliases cx) dataPairs
    let types = typeDecls -- TODO
    return $ Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) imports types
  where
    isTypePair = isType . typedTermType . snd
    typePairs = L.filter isTypePair pairs
    dataPairs = [] -- TODO   L.filter (not . isTypePair) pairs
    aliases = importAliasesForGraph g

toTypeDeclaration :: (Default m, Ord m, Read m, Show m)
  => M.Map GraphName Java.PackageName -> Context m -> (Element m, TypedTerm m) -> Result Java.TypeDeclaration
toTypeDeclaration aliases cx (el, TypedTerm _ term) = do
    t <- decodeType cx term >>= adaptType cx javaLanguage
    cd <- toClassDecl aliases (elementName el) t
    return $ Java.TypeDeclarationClass cd

toClassDecl :: (Show m, Default m, Eq m) => M.Map GraphName Java.PackageName -> Name -> Type m
  -> Result Java.ClassDeclaration
toClassDecl aliases elName t = case typeExpr t of
      TypeExprNominal name -> return $ javaClassDeclaration aliases elName classModsPublic (Just name) []
      TypeExprRecord fields -> declarationForRecordType aliases elName fields
      TypeExprUnion fields -> declarationForUnionType aliases elName fields
      TypeExprUniversal ut -> declarationForUniversalType aliases elName ut
      -- Other types are not supported as class declarations, so we wrap them as record types.
      -- TODO: wrap and unwrap the corresponding terms as record terms.
      _ -> declarationForRecordType aliases elName [Types.field "value" t]

declarationForRecordType :: Show m => M.Map GraphName Java.PackageName -> Name -> [FieldType m]
  -> Result Java.ClassDeclaration
declarationForRecordType aliases elName fields = do
    memberVars <- CM.mapM toMemberVar fields
    withMethods <- if L.length fields > 1
      then CM.mapM toWithMethod fields
      else pure []
    cons <- constructor
    let bodyDecls = memberVars ++ [cons, equalsMethod, hashCodeMethod] ++ withMethods
    return $ javaClassDeclaration aliases elName classModsPublic Nothing bodyDecls
  where
    constructor = do
      params <- CM.mapM fieldToFormalParam fields
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
      param <- fieldToFormalParam field
      let anns = [] -- TODO
      let result = referenceTypeToResult $ nameToJavaReferenceType aliases False elName
      let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $
            javaConstructorCall (javaConstructorName $ localNameOf elName) fieldArgs
      return $ methodDeclaration mods [] anns methodName [param] result (Just [returnStmt])

    fieldToFormalParam (FieldType fname ft) = do
      jt <- encodeType aliases ft
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
                other = javaIdentifierToJavaRelationalExpression $ Java.Identifier $ sanitizeJavaName otherName
                parent = nameToJavaReferenceType aliases False elName

            returnFalse = javaReturnStatement $ Just $ javaBooleanExpression False

        castStmt = variableDeclarationStatement aliases elName id rhs
          where
            id = Java.Identifier $ sanitizeJavaName tmpName
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
                  fieldExpression (Java.Identifier $ sanitizeJavaName tmpName) (Java.Identifier $ sanitizeJavaName fname)
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
                  methodInvocation (Just $ Java.Identifier $ sanitizeJavaName fname) (Java.Identifier "hashCode") []

            multipliers = L.cycle first20Primes
              where
                first20Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

declarationForUnionType :: (Show m, Default m, Eq m) => M.Map GraphName Java.PackageName -> Name -> [FieldType m]
  -> Result Java.ClassDeclaration
declarationForUnionType aliases elName fields = do
    variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
    let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
    let bodyDecls = [privateConstructor, toAcceptMethod True, visitor, partialVisitor] ++ variantDecls
    let mods = classModsPublic ++ [Java.ClassModifierAbstract]
    return $ javaClassDeclaration aliases elName mods Nothing bodyDecls
  where
    privateConstructor = makeConstructor aliases elName True [] []
    unionFieldClass (FieldType fname ftype) = do
      let rtype = Types.record $ if isUnit ftype then [] else [FieldType (FieldName "value") ftype]
      toClassDecl aliases (variantClassName elName fname) rtype
    augmentVariantClass (Java.ClassDeclarationNormal cd) = Java.ClassDeclarationNormal $ cd {
        Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic, Java.ClassModifierStatic, Java.ClassModifierFinal],
        Java.normalClassDeclarationExtends = Just $ nameToJavaClassType aliases True elName,
        Java.normalClassDeclarationBody = newBody (Java.normalClassDeclarationBody cd)}
      where
        newBody (Java.ClassBody decls) = Java.ClassBody $ decls ++ [toAcceptMethod False]

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
              [Java.InterfaceType $ javaClassType [javaTypeVariable "R"] Nothing "Visitor"],
            Java.normalInterfaceDeclarationBody = Java.InterfaceBody $ otherwise:(toVisitMethod . fieldTypeName <$> fields)}
      where
        otherwise = interfaceMethodDeclaration defaultMod [] "otherwise" [mainInstanceParam] resultR $ Just [throw]
          where
            throw = Java.BlockStatementStatement $ Java.StatementWithoutTrailing $
                Java.StatementWithoutTrailingSubstatementThrow $ Java.ThrowStatement $
                javaConstructorCall (javaConstructorName "IllegalStateException") args
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

    resultR = javaTypeToJavaResult $ Java.TypeReference $ javaTypeVariable "R"

    mainInstanceParam = javaTypeToJavaFormalParameter classRef (FieldName "instance")
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False elName

    variantInstanceParam fname = javaTypeToJavaFormalParameter classRef (FieldName "instance")
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False $ variantClassName elName fname

declarationForUniversalType :: (Show m, Default m, Eq m) => M.Map GraphName Java.PackageName -> Name -> UniversalType m
  -> Result Java.ClassDeclaration
declarationForUniversalType aliases elName (UniversalType (TypeVariable v) body) = do
    (Java.ClassDeclarationNormal cd) <- toClassDecl aliases elName body
    return $ Java.ClassDeclarationNormal $ cd {
      Java.normalClassDeclarationParameters = addParameter v (Java.normalClassDeclarationParameters cd)}
  where
    addParameter v params = params ++ [javaTypeParameter v]

isUnit :: Eq m => Type m -> Bool
isUnit t = typeExpr t  == TypeExprRecord []

variantClassName :: Name -> FieldName -> Name
variantClassName elName (FieldName fname) = fromQname (graphNameOf elName) $ capitalize fname

-- | Transform a given type into a type which can be used as the basis for a Java class
toDeclarationType :: Default m => Type m -> Result (Type m)
toDeclarationType t = case typeExpr t of
  TypeExprNominal _ -> pure t
  TypeExprRecord _ -> pure t
  TypeExprUnion _ -> pure t
  TypeExprUniversal (UniversalType v body) -> do
    b <- toDeclarationType body
    return t {typeExpr = TypeExprUniversal $ UniversalType v b}
  TypeExprVariable _ -> fail "unexpected type variable"
  _ -> do
    let rt = Types.record [Types.field "value" t]
    return t {typeExpr = typeExpr rt}

toDataDeclaration :: M.Map GraphName Java.PackageName -> Context m -> (a, TypedTerm m) -> Result a
toDataDeclaration aliases cx (el, TypedTerm typ term) = do
  fail "not implemented" -- TODO

encodeTerm :: (Default m, Eq m, Ord m, Read m, Show m)
  => M.Map GraphName Java.PackageName -> Context m -> Term m -> Result Java.Block
encodeTerm aliases cx term@(Term expr meta) = do
  return $ javaStatementsToBlock [javaEmptyStatement] -- TODO

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
      IntegerTypeUint16 -> simple "Char"
      _ -> fail $ "unexpected integer type: " ++ show it
    LiteralTypeString -> simple "String"
    _ -> fail $ "unexpected literal type: " ++ show lt
  where
    simple n = pure $ javaRefType [] Nothing n

encodeType :: Show m => M.Map GraphName Java.PackageName -> Type m -> Result Java.Type
encodeType aliases t = case typeExpr t of
  TypeExprElement et -> encode et -- Elements are simply unboxed
  TypeExprFunction (FunctionType dom cod) -> do
    jdom <- encode dom >>= asJavaReferenceType
    jcod <- encode cod >>= asJavaReferenceType
    return $ javaRefType [jdom, jcod] javaUtilPackageName "Function"
  TypeExprList et -> do
    jet <- encode et
    if listsAsArrays
      then toJavaArrayType jet
      else do
        rt <- asJavaReferenceType jet
        return $ javaRefType [rt] javaUtilPackageName "List"
  TypeExprLiteral lt -> encodeLiteralType lt
  TypeExprMap (MapType kt vt) -> do
    jkt <- encode kt >>= asJavaReferenceType
    jvt <- encode vt >>= asJavaReferenceType
    return $ javaRefType [jkt, jvt] javaUtilPackageName "Map"
  TypeExprNominal name -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True name
  TypeExprOptional ot -> do
    jot <- encode ot >>= asJavaReferenceType
    return $ javaRefType [jot] javaUtilPackageName "Optional"
  TypeExprSet st -> do
    jst <- encode st >>= asJavaReferenceType
    return $ javaRefType [jst] javaUtilPackageName "Set"
  TypeExprUniversal (UniversalType (TypeVariable v) body) -> do
    jbody <- encode body
    addJavaTypeParameter (javaTypeVariable v) jbody
  TypeExprVariable (TypeVariable v) -> pure $ Java.TypeReference $ javaTypeVariable v
  TypeExprRecord [] -> return $ javaRefType [] javaLangPackageName "Void"
  -- Note: record (other than unit) and union types should not appear at this level
  _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    encode = encodeType aliases
