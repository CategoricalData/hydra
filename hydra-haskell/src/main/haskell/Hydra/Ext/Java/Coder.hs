module Hydra.Ext.Java.Coder (printModule) where

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Monads
import Hydra.CoreDecoding
import Hydra.Ext.Java.Utils
import Hydra.Ext.Java.Language
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import qualified Hydra.Ext.Java.Syntax as Java
import Hydra.Adapters.Coders
import Hydra.Util.Formatting
import Hydra.Util.Codetree.Script
import Hydra.Ext.Java.Serde
import Hydra.Ext.Java.Settings
import Hydra.Monads
import Hydra.Basics
import Hydra.Adapters.UtilsEtc
import Hydra.Rewriting
import Hydra.Reduction

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Aliases = M.Map Namespace Java.PackageName

printModule :: (Ord m, Read m, Show m) => Module m -> GraphFlow m (M.Map FilePath String)
printModule mod = do
    withTrace "encode in Java" $ do
      units <- moduleToJavaCompilationUnit mod
      return $ M.fromList $ forPair <$> M.toList units
  where
    forPair (name, unit) = (
      elementNameToFilePath name,
      printExpr $ parenthesize $ writeCompilationUnit unit)

boundTypeVariables :: Type m -> [VariableType]
boundTypeVariables typ = case typ of
  TypeAnnotated (Annotated typ1 _) -> boundTypeVariables typ1
  TypeLambda (LambdaType v body) -> v:(boundTypeVariables body)
  _ -> []

commentsFromElement :: Element m -> GraphFlow m (Maybe String)
commentsFromElement el = do
  cx <- getState
  annotationClassTermDescription (contextAnnotations cx) (elementData el)

commentsFromFieldType :: FieldType m -> GraphFlow m (Maybe String)
commentsFromFieldType (FieldType _ t) = do
  cx <- getState
  annotationClassTypeDescription (contextAnnotations cx) t

addComment :: Java.ClassBodyDeclaration -> FieldType m -> GraphFlow m Java.ClassBodyDeclarationWithComments
addComment decl field = Java.ClassBodyDeclarationWithComments decl <$> commentsFromFieldType field

noComment :: Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments
noComment decl = Java.ClassBodyDeclarationWithComments decl Nothing

elementNameToFilePath :: Name -> FilePath
elementNameToFilePath name = nameToFilePath False (FileExtension "java") $ fromQname ns (sanitizeJavaName local)
  where
    (ns, local) = toQname name

moduleToJavaCompilationUnit :: (Ord m, Read m, Show m) => Module m -> GraphFlow m (M.Map Name Java.CompilationUnit)
moduleToJavaCompilationUnit mod = transformModule language (encodeTerm aliases) constructModule mod
  where
    aliases = importAliasesForModule mod

classModsPublic :: [Java.ClassModifier]
classModsPublic = [Java.ClassModifierPublic]

constructModule :: (Ord m, Read m, Show m)
  => Module m -> M.Map (Type m) (Coder (Context m) (Term m) Java.Expression) -> [(Element m, TypedTerm m)]
  -> GraphFlow m (M.Map Name Java.CompilationUnit)
constructModule mod coders pairs = do
    cx <- getState
    let isTypePair = isType cx . typedTermType . snd
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

    termToInterfaceMember coders pair =
        if isLambda (typedTermTerm $ snd pair)
          then termToMethod coders pair
          else termToConstant coders pair
      where
        isLambda t = case stripTerm t of
          TermFunction (FunctionLambda _) -> True
          _ -> False

    termToConstant coders pair@(el, TypedTerm typ term) = do
      jtype <- Java.UnannType <$> encodeType aliases typ
      jterm <- coderEncode (Y.fromJust $ M.lookup typ coders) term
      let mods = []
      let var = javaVariableDeclarator (javaVariableName $ elementName el) $ Just $ Java.VariableInitializerExpression jterm
      return $ Java.InterfaceMemberDeclarationConstant $ Java.ConstantDeclaration mods jtype [var]

    -- Lambdas cannot (in general) be turned into top-level constants, as there is no way of declaring type parameters for constants
    termToMethod coders pair@(el, TypedTerm typ term) = case stripType typ of
      TypeFunction (FunctionType dom cod) -> case stripTerm term of
        TermFunction (FunctionLambda (Lambda v body)) -> do
          jdom <- encodeType aliases dom
          jcod <- encodeType aliases cod
          let mods = [Java.InterfaceMethodModifierStatic]
          let anns = []
          let mname = sanitizeJavaName $ decapitalize $ localNameOf $ elementName el
          let param = javaTypeToJavaFormalParameter jdom (FieldName $ unVariable v)
          let result = javaTypeToJavaResult jcod
          jbody <- encodeTerm aliases body
          let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
          let tparams = javaTypeParametersForType typ
          return $ interfaceMethodDeclaration mods tparams mname [param] result (Just [returnSt])
        _ -> unexpected "function term" term
      _ -> unexpected "function type" typ

constructElementsInterface :: Module m -> [Java.InterfaceMemberDeclaration] -> (Name, Java.CompilationUnit)
constructElementsInterface mod members = (elName, cu)
  where
    cu = Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) [] [decl]
    pkg = javaPackageDeclaration $ moduleNamespace mod
    mods = []
    elName = fromQname (moduleNamespace mod) elementsClassName
    body = Java.InterfaceBody members
    itf = Java.TypeDeclarationInterface $ Java.InterfaceDeclarationNormalInterface $
      Java.NormalInterfaceDeclaration mods (javaTypeIdentifier elementsClassName) [] [] body
    decl = Java.TypeDeclarationWithComments itf $ moduleDescription mod

declarationForLambdaType :: (Eq m, Ord m, Read m, Show m) => Aliases
  -> [Java.TypeParameter] -> Name -> LambdaType m -> GraphFlow m Java.ClassDeclaration
declarationForLambdaType aliases tparams elName (LambdaType (VariableType v) body) =
    toClassDecl False aliases (tparams ++ [param]) elName body
  where
    param = javaTypeParameter $ capitalize v

declarationForRecordType :: (Ord m, Read m, Show m) => Bool -> Aliases -> [Java.TypeParameter] -> Name
  -> [FieldType m] -> GraphFlow m Java.ClassDeclaration
declarationForRecordType isInner aliases tparams elName fields = do
    memberVars <- CM.mapM toMemberVar fields
    cx <- getState
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
      let result = referenceTypeToResult $ nameToJavaReferenceType aliases False elName Nothing
      let consId = Java.Identifier $ sanitizeJavaName $ localNameOf elName
      let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $
            javaConstructorCall (javaConstructorName consId Nothing) fieldArgs Nothing
      return $ methodDeclaration mods [] anns methodName [param] result (Just [returnStmt])

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
                parent = nameToJavaReferenceType aliases False elName Nothing

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
                  methodInvocationStatic (javaIdentifier fname) (Java.Identifier "hashCode") []

            multipliers = L.cycle first20Primes
              where
                first20Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

declarationForType :: (Ord m, Read m, Show m)
  => Aliases -> (Element m, TypedTerm m) -> GraphFlow m Java.TypeDeclarationWithComments
declarationForType aliases (el, TypedTerm _ term) = do
    t <- decodeType term >>= adaptType language
    cd <- toClassDecl False aliases [] (elementName el) t
    cx <- getState
    comments <- commentsFromElement el
    return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass cd) comments

declarationForUnionType :: (Eq m, Ord m, Read m, Show m)
  => Aliases
  -> [Java.TypeParameter] -> Name -> [FieldType m] -> GraphFlow m Java.ClassDeclaration
declarationForUnionType aliases tparams elName fields = do
    variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
    let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
    cx <- getState
    variantDecls' <- CM.zipWithM addComment variantDecls fields
    let otherDecls = noComment <$> [privateConstructor, toAcceptMethod True, visitor, partialVisitor]
    tn <- typeNameDecl aliases elName
    let bodyDecls = [tn] ++ otherDecls ++ variantDecls'
    let mods = classModsPublic ++ [Java.ClassModifierAbstract]
    return $ javaClassDeclaration aliases tparams elName mods Nothing bodyDecls
  where
    privateConstructor = makeConstructor aliases elName True [] []
    unionFieldClass (FieldType fname ftype) = do
      let rtype = Types.record $ if Types.isUnit ftype then [] else [FieldType (FieldName valueFieldName) ftype]
      toClassDecl True aliases [] (variantClassName False elName fname) rtype
    augmentVariantClass (Java.ClassDeclarationNormal cd) = Java.ClassDeclarationNormal $ cd {
        Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic, Java.ClassModifierStatic, Java.ClassModifierFinal],
        Java.normalClassDeclarationExtends = Just $ nameToJavaClassType aliases True args elName Nothing,
        Java.normalClassDeclarationParameters = tparams,
        Java.normalClassDeclarationBody = newBody (Java.normalClassDeclarationBody cd)}
      where
        newBody (Java.ClassBody decls) = Java.ClassBody $ decls ++ [noComment $ toAcceptMethod False]
        args = typeParameterToTypeArgument <$> tparams

    visitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration mods ti tparams extends body
      where
        mods = [Java.InterfaceModifierPublic]
        ti = Java.TypeIdentifier $ Java.Identifier visitorName
        tparams = [javaTypeParameter "R"]
        extends = []
        body = Java.InterfaceBody (toVisitMethod . fieldTypeName <$> fields)
          where
            toVisitMethod fname = interfaceMethodDeclaration [] [] visitMethodName [variantInstanceParam fname] resultR Nothing

    partialVisitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration {
            Java.normalInterfaceDeclarationModifiers = [Java.InterfaceModifierPublic],
            Java.normalInterfaceDeclarationIdentifier = Java.TypeIdentifier $ Java.Identifier partialVisitorName,
            Java.normalInterfaceDeclarationParameters = [javaTypeParameter "R"],
            Java.normalInterfaceDeclarationExtends =
              [Java.InterfaceType $ javaClassType [visitorTypeVariable] Nothing visitorName],
            Java.normalInterfaceDeclarationBody = Java.InterfaceBody $ otherwise:(toVisitMethod . fieldTypeName <$> fields)}
      where
        otherwise = interfaceMethodDeclaration defaultMod [] "otherwise" [mainInstanceParam] resultR $ Just [throw]
          where
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
              methodInvocation Nothing (Java.Identifier "otherwise") [javaIdentifierToJavaExpression $ Java.Identifier "instance"]

    defaultMod = [Java.InterfaceMethodModifierDefault]

    resultR = javaTypeToJavaResult $ Java.TypeReference visitorTypeVariable

    mainInstanceParam = javaTypeToJavaFormalParameter classRef instanceFieldName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False [] elName Nothing

    variantInstanceParam fname = javaTypeToJavaFormalParameter classRef instanceFieldName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False [] (variantClassName False elName fname) Nothing

elementsClassName = "Elements_"

elementJavaIdentifier :: Aliases -> Name -> Java.Identifier
elementJavaIdentifier aliases name = Java.Identifier $ jname ++ "." ++ local
  where
    (gname, local) = toQname name
    elementsName = fromQname gname elementsClassName
    Java.Identifier jname = nameToJavaName aliases elementsName

encodeElimination :: (Eq m, Ord m, Read m, Show m)
  => Aliases -> Maybe Java.Expression -> Type m -> Elimination m -> GraphFlow m Java.Expression
encodeElimination aliases marg cod elm = case elm of
  EliminationElement -> case marg of
    Nothing -> encodeFunction aliases cod $ FunctionLambda $ Lambda var $ TermVariable var
      where
        var = Variable "v"
    Just jarg -> pure jarg
  EliminationNominal name -> case marg of
    Nothing -> pure $ javaLambda var jbody
      where
        var = Variable "v"
        arg = javaIdentifierToJavaExpression $ variableToJavaIdentifier var
        jbody = javaConstructorCall (javaConstructorName (nameToJavaName aliases name) Nothing) [arg] Nothing
    Just jarg -> pure $ javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier valueFieldName)
      where
        qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
--  EliminationOptional (OptionalCases nothing just) ->
  EliminationRecord (Projection _ fname) -> case marg of
    Nothing -> pure $ javaLambda var jbody
      where
        var = Variable "v"
        jbody = javaExpressionNameToJavaExpression $
          fieldExpression (variableToJavaIdentifier var) (javaIdentifier $ unFieldName fname)
    Just jarg -> pure $ javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier $ unFieldName fname)
      where
        qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
  EliminationUnion (CaseStatement tname fields) -> case marg of
      Nothing -> encodeTerm aliases $ Terms.lambda "v" $ Terms.apply (Terms.elimination elm) (Terms.variable "v")
      Just jarg -> applyElimination jarg
    where
      applyElimination jarg = do
          let prim = javaExpressionToJavaPrimary jarg
          let consId = innerClassRef aliases tname visitorName
          jcod <- encodeType aliases cod
          let targs = Java.TypeArgumentsOrDiamondArguments [javaTypeToJavaTypeArgument jcod]
          body <- Java.ClassBody <$> CM.mapM (bodyDecl jcod) fields
          let visitor = javaConstructorCall (javaConstructorName consId $ Just targs) [] (Just body)
          return $ javaMethodInvocationToJavaExpression $
            methodInvocation (Just $ Right prim) (Java.Identifier "accept") [visitor]
        where
          bodyDecl jcod field = do
            let jdom = Java.TypeReference $ nameToJavaReferenceType aliases True tname (Just $ capitalize $ unFieldName $ fieldName field)
            let term = stripTerm $ fieldTerm field
            let mods = [Java.MethodModifierPublic]
            let anns = [overrideAnnotation]
            let param = javaTypeToJavaFormalParameter jdom instanceFieldName
            let result = Java.ResultType $ Java.UnannType jcod

            jret <- encodeTerm aliases $ contractTerm $ Terms.apply (fieldTerm field) (Terms.variable "instance")
            let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret

            return $ noComment $ methodDeclaration mods [] anns visitMethodName [param] result (Just [returnStmt])
  _ -> pure $ encodeLiteral $ LiteralString $
    "Unimplemented elimination variant: " ++ show (eliminationVariant elm) -- TODO: temporary

encodeFunction :: (Eq m, Ord m, Read m, Show m)
  => Aliases -> Type m -> Function m -> GraphFlow m Java.Expression
encodeFunction aliases cod fun = case fun of
--  FunctionCompareTo other ->
  FunctionElimination elm -> encodeElimination aliases Nothing cod elm
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
encodeLiteralType :: LiteralType -> GraphFlow m Java.Type
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
  => Aliases -> Term m -> GraphFlow m Java.Expression
encodeTerm aliases term = case term of
    TermAnnotated (Annotated term' ann) -> case term' of
      TermFunction fun -> do
        cod <- getCodomain ann
        encodeFunction aliases cod fun
      _ -> encode term' -- TODO: annotations to comments where possible
    TermApplication (Application fun arg) -> case fun of
        TermAnnotated (Annotated (TermFunction f) ann) -> case f of
--          FunctionCompareTo (Term m)
          FunctionElimination elm -> do
              jarg <- encode arg
              cod <- getCodomain ann
              encodeElimination aliases (Just jarg) cod elm
--          FunctionPrimitive Name
          _ -> defaultExpression
        _ -> defaultExpression
      where
        defaultExpression = do
          jfun <- encode fun
          jarg <- encode arg
          let prim = javaExpressionToJavaPrimary jfun
          return $ javaMethodInvocationToJavaExpression $ methodInvocation (Just $ Right prim) (Java.Identifier "apply") [jarg]
    TermElement name -> pure $ javaIdentifierToJavaExpression $ elementJavaIdentifier aliases name
    TermList els -> do
      jels <- CM.mapM encode els
      return $ javaMethodInvocationToJavaExpression $
        methodInvocationStatic (Java.Identifier "java.util.Arrays") (Java.Identifier "asList") jels
    TermLiteral l -> pure $ encodeLiteral l
  --  TermMap (Map (Term m) (Term m))
    TermNominal (Named name arg) -> do
      jarg <- encode arg
      return $ javaConstructorCall (javaConstructorName (nameToJavaName aliases name) Nothing) [jarg] Nothing
    TermOptional mt -> case mt of
      Nothing -> pure $ javaMethodInvocationToJavaExpression $
        methodInvocationStatic (javaIdentifier "java.util.Optional") (Java.Identifier "empty") []
      Just term1 -> do
        expr <- encode term1
        return $ javaMethodInvocationToJavaExpression $
          methodInvocationStatic (javaIdentifier "java.util.Optional") (Java.Identifier "of") [expr]
    TermRecord (Record name fields) -> do
      fieldExprs <- CM.mapM encode (fieldTerm <$> fields)
      let consId = nameToJavaName aliases name
      return $ javaConstructorCall (javaConstructorName consId Nothing) fieldExprs Nothing
    TermSet s -> do
      jels <- CM.mapM encode $ S.toList s
      let prim = javaMethodInvocationToJavaPrimary $
                 methodInvocationStatic (Java.Identifier "java.util.Stream") (Java.Identifier "of") jels
      let coll = javaMethodInvocationToJavaExpression $
                 methodInvocationStatic (javaIdentifier "java.util.stream.Collectors") (Java.Identifier "toSet") []
      return $ javaMethodInvocationToJavaExpression $
        methodInvocation (Just $ Right prim) (Java.Identifier "collect") [coll]
    TermUnion (Union name (Field (FieldName fname) v)) -> do
      args <- if Terms.isUnit v
        then return []
        else do
          ex <- encode v
          return [ex]
      let (Java.Identifier typeId) = nameToJavaName aliases name
      let consId = Java.Identifier $ typeId ++ "." ++ sanitizeJavaName (capitalize fname)
      return $ javaConstructorCall (javaConstructorName consId Nothing) args Nothing
    TermVariable (Variable v) -> pure $ javaIdentifierToJavaExpression $ javaIdentifier v
    TermFunction f -> failAsLiteral $ "unannotated function: " ++ show f
    _ -> failAsLiteral $
      "Unimplemented term variant: " ++ show (termVariant term) -- TODO: temporary
  --  _ -> unexpected "term" $ show term
  where
    encode = encodeTerm aliases

    failAsLiteral msg = pure $ encodeLiteral $ LiteralString msg

encodeType :: Show m => Aliases -> Type m -> GraphFlow m Java.Type
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
  TypeNominal name -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True name Nothing
  TypeRecord (RowType _UnitType []) -> return $ javaRefType [] javaLangPackageName "Void"
  TypeRecord (RowType name _) -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True name Nothing
  TypeOptional ot -> do
    jot <- encode ot >>= javaTypeToJavaReferenceType
    return $ javaRefType [jot] javaUtilPackageName "Optional"
  TypeSet st -> do
    jst <- encode st >>= javaTypeToJavaReferenceType
    return $ javaRefType [jst] javaUtilPackageName "Set"
  TypeUnion (RowType name _) -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True name Nothing
  TypeVariable (VariableType v) -> pure $ Java.TypeReference $ javaTypeVariable v
  _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    encode = encodeType aliases

fieldTypeToFormalParam aliases (FieldType fname ft) = do
  jt <- encodeType aliases ft
  return $ javaTypeToJavaFormalParameter jt fname

getDomain ann = functionTypeDomain <$> getFunctionType ann
getCodomain ann = functionTypeCodomain <$> getFunctionType ann
getFunctionType ann = do
  cx <- getState
  mt <- annotationClassTypeOf (contextAnnotations cx) ann
  case mt of
    Nothing -> fail "type annotation is required for function and elimination terms in Java"
    Just t -> case t of
      TypeFunction ft -> return ft
      _ -> unexpected "function type" t

innerClassRef :: Aliases -> Name -> String -> Java.Identifier
innerClassRef aliases name local = Java.Identifier $ id ++ "." ++ local
  where
    Java.Identifier id = nameToJavaName aliases name

instanceFieldName = FieldName "instance"

javaTypeParametersForType :: Type m -> [Java.TypeParameter]
javaTypeParametersForType typ = toParam <$> vars
  where
    toParam (VariableType v) = Java.TypeParameter [] (javaTypeIdentifier $ capitalize v) Nothing
--    vars = boundTypeVariables typ
    vars = S.toList $ freeVariablesInType typ -- TODO: the fact that the variables are free is a bug, not a feature

partialVisitorName :: String
partialVisitorName = "PartialVisitor"

toClassDecl :: (Eq m, Ord m, Read m, Show m) => Bool -> Aliases -> [Java.TypeParameter]
  -> Name -> Type m -> GraphFlow m Java.ClassDeclaration
toClassDecl isInner aliases tparams elName t = case stripType t of
    TypeRecord rt -> declarationForRecordType isInner aliases tparams elName $ rowTypeFields rt
    TypeUnion rt -> declarationForUnionType aliases tparams elName $ rowTypeFields rt
    TypeLambda ut -> declarationForLambdaType aliases tparams elName ut
    -- Other types are not supported as class declarations, so we wrap them as record types.
    _ -> wrap t -- TODO: wrap and unwrap the corresponding terms as record terms.
  where
    wrap t' = declarationForRecordType isInner aliases tparams elName [Types.field valueFieldName t']

toDataDeclaration :: Aliases -> (a, TypedTerm m) -> GraphFlow m a
toDataDeclaration aliases (el, TypedTerm typ term) = do
  fail "not implemented" -- TODO

typeNameDecl :: (Ord m, Read m, Show m) => Aliases -> Name -> GraphFlow m Java.ClassBodyDeclarationWithComments
typeNameDecl aliases name = do
  jt <- encodeType aliases $ Types.nominal _Name
  arg <- encodeTerm aliases $ Terms.string $ unName name
  let init = Java.VariableInitializerExpression $ javaConstructorCall (javaConstructorName nameName Nothing) [arg] Nothing
  let var = javaVariableDeclarator (Java.Identifier "NAME") (Just init)
  return $ noComment $ javaMemberField mods jt var
  where
    mods = [Java.FieldModifierPublic, Java.FieldModifierStatic, Java.FieldModifierFinal]
    nameName = nameToJavaName aliases _Name

valueFieldName :: String
valueFieldName = "value"

visitMethodName :: String
visitMethodName = "visit"

visitorName :: String
visitorName = "Visitor"
