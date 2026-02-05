module Hydra.Ext.Staging.Java.Utils where

import Hydra.Kernel
import Hydra.Ext.Java.Language
import Hydra.Ext.Staging.Java.Names
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type PackageMap = M.Map Namespace Java.PackageName

data Aliases = Aliases {
  aliasesCurrentNamespace :: Namespace,
  aliasesPackages :: PackageMap,
  aliasesBranchVars :: S.Set Name,
  aliasesRecursiveVars :: S.Set Name,
  -- | Type parameters that are in scope (from method-level type parameters)
  aliasesInScopeTypeParams :: S.Set Name,
  -- | Local variables that have polymorphic types (declared with raw types)
  aliasesPolymorphicLocals :: S.Set Name,
  -- | All in-scope Java variable names (for avoiding lambda parameter shadowing)
  aliasesInScopeJavaVars :: S.Set Name,
  -- | Variable renames for avoiding shadowing (maps Hydra name to Java name)
  aliasesVarRenames :: M.Map Name Name,
  -- | Lambda-bound variables (including hoisted captures with qualified names)
  aliasesLambdaVars :: S.Set Name,
  -- | Type variable substitution: maps fresh inference variable names to canonical scheme variable names
  aliasesTypeVarSubst :: M.Map Name Name,
  -- | Type variables that actually appear in the method's formal parameter types.
  -- Used to validate lambda cast type variables — only these are trusted.
  aliasesTrustedTypeVars :: S.Set Name,
  -- | The enclosing method's codomain (return type), used for casting pair expressions
  -- that have over-generalized type variables from inference.
  aliasesMethodCodomain :: Maybe Type,
  -- | Variables that have been thunked (wrapped in Supplier) for lazy evaluation.
  -- References to these variables must use .get() to obtain the actual value.
  aliasesThunkedVars :: S.Set Name } deriving Show

addExpressions :: [Java.MultiplicativeExpression] -> Java.AdditiveExpression
addExpressions exprs = L.foldl add (Java.AdditiveExpressionUnary $ L.head exprs) $ L.tail exprs
  where
    add ae me = Java.AdditiveExpressionPlus $ Java.AdditiveExpression_Binary ae me

addJavaTypeParameter :: Java.ReferenceType -> Java.Type -> Flow (Graph) Java.Type
addJavaTypeParameter rt t = case t of
  Java.TypeReference rt1 -> case rt1 of
    Java.ReferenceTypeClassOrInterface cit -> case cit of
      Java.ClassOrInterfaceTypeClass (Java.ClassType anns qual id args) -> pure $
        Java.TypeReference $ Java.ReferenceTypeClassOrInterface $
          Java.ClassOrInterfaceTypeClass $ Java.ClassType anns qual id (args ++ [Java.TypeArgumentReference rt])
      _ -> fail $ "expected a Java class type. Found: " ++ show cit
    Java.ReferenceTypeVariable tv -> pure $ javaTypeVariableToType tv
    _ -> fail $ "expected a Java class or interface type, or a variable. Found: " ++ show rt
  _ -> fail $ "expected a reference type. Found: " ++ show t

fieldExpression :: Java.Identifier -> Java.Identifier -> Java.ExpressionName
fieldExpression varId fieldId = Java.ExpressionName (Just $ Java.AmbiguousName [varId]) fieldId

fieldNameToJavaExpression :: Name -> Java.Expression
fieldNameToJavaExpression fname = javaPostfixExpressionToJavaExpression $
  Java.PostfixExpressionName $ Java.ExpressionName Nothing (fieldNameToJavaIdentifier fname)

fieldNameToJavaIdentifier :: Name -> Java.Identifier
fieldNameToJavaIdentifier (Name name) = javaIdentifier name

fieldNameToJavaVariableDeclarator :: Name -> Java.VariableDeclarator
fieldNameToJavaVariableDeclarator (Name n) = javaVariableDeclarator (javaIdentifier n) Nothing

fieldNameToJavaVariableDeclaratorId :: Name -> Java.VariableDeclaratorId
fieldNameToJavaVariableDeclaratorId (Name n) = javaVariableDeclaratorId $ javaIdentifier n

importAliasesForModule :: Module -> Aliases
importAliasesForModule mod = Aliases (moduleNamespace mod) M.empty S.empty S.empty S.empty S.empty S.empty M.empty S.empty M.empty S.empty Nothing S.empty

interfaceMethodDeclaration :: [Java.InterfaceMethodModifier] -> [Java.TypeParameter] -> String -> [Java.FormalParameter]
   -> Java.Result -> Maybe [Java.BlockStatement] -> Java.InterfaceMemberDeclaration
interfaceMethodDeclaration mods tparams methodName params result stmts = Java.InterfaceMemberDeclarationInterfaceMethod $
    Java.InterfaceMethodDeclaration mods header body
  where
    header = javaMethodHeader tparams methodName params result
    body = javaMethodBody stmts

isEscaped :: String -> Bool
isEscaped s = L.head s == '$'

javaAdditiveExpressionToJavaExpression :: Java.AdditiveExpression -> Java.Expression
javaAdditiveExpressionToJavaExpression = javaRelationalExpressionToJavaExpression .
  Java.RelationalExpressionSimple . Java.ShiftExpressionUnary

javaAssignmentStatement :: Java.LeftHandSide -> Java.Expression -> Java.Statement
javaAssignmentStatement lhs rhs = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementExpression $
    Java.ExpressionStatement $ Java.StatementExpressionAssignment ass
  where
    ass = Java.Assignment lhs Java.AssignmentOperatorSimple rhs

javaBoolean :: Bool -> Java.Literal
javaBoolean = Java.LiteralBoolean

javaBooleanExpression :: Bool -> Java.Expression
javaBooleanExpression = javaPrimaryToJavaExpression . javaLiteralToJavaPrimary . javaBoolean

javaBooleanType :: Java.Type
javaBooleanType = javaPrimitiveTypeToJavaType Java.PrimitiveTypeBoolean

javaCastExpression :: Java.ReferenceType -> Java.UnaryExpression -> Java.CastExpression
javaCastExpression rt expr = Java.CastExpressionNotPlusMinus $ Java.CastExpression_NotPlusMinus rb expr
  where
    rb = Java.CastExpression_RefAndBounds rt []

javaCastPrimitive :: Java.PrimitiveType -> Java.UnaryExpression -> Java.CastExpression
javaCastPrimitive pt expr = Java.CastExpressionPrimitive $
  Java.CastExpression_Primitive (Java.PrimitiveTypeWithAnnotations pt []) expr

javaCastExpressionToJavaExpression :: Java.CastExpression -> Java.Expression
javaCastExpressionToJavaExpression = javaUnaryExpressionToJavaExpression . Java.UnaryExpressionOther .
  Java.UnaryExpressionNotPlusMinusCast

-- | Double-cast expression for bypassing Java's generic type variance checking.
-- When casting from a raw type (or type with Object) to a parameterized type,
-- we need to first cast to the raw type, then to the target parameterized type.
-- Example: (Function<Type,Type>) (Function) rawVar
javaDoubleCastExpression :: Java.ReferenceType -> Java.ReferenceType -> Java.UnaryExpression -> Java.CastExpression
javaDoubleCastExpression rawRt targetRt expr =
  let firstCast = javaCastExpressionToJavaExpression $ javaCastExpression rawRt expr
  in javaCastExpression targetRt (javaExpressionToJavaUnaryExpression firstCast)

javaDoubleCastExpressionToJavaExpression :: Java.ReferenceType -> Java.ReferenceType -> Java.UnaryExpression -> Java.Expression
javaDoubleCastExpressionToJavaExpression rawRt targetRt expr =
  javaCastExpressionToJavaExpression $ javaDoubleCastExpression rawRt targetRt expr

-- | Extract the raw type (without type arguments) from a reference type.
-- For java.util.function.Function<A,B>, returns java.util.function.Function
javaReferenceTypeToRawType :: Java.ReferenceType -> Java.ReferenceType
javaReferenceTypeToRawType rt = case rt of
  Java.ReferenceTypeClassOrInterface (Java.ClassOrInterfaceTypeClass (Java.ClassType anns qual id _)) ->
    Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $ Java.ClassType anns qual id []
  Java.ReferenceTypeClassOrInterface (Java.ClassOrInterfaceTypeInterface (Java.InterfaceType (Java.ClassType anns qual id _))) ->
    Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeInterface $ Java.InterfaceType $ Java.ClassType anns qual id []
  _ -> rt  -- Keep as is for type variables and arrays

javaClassDeclaration :: Aliases -> [Java.TypeParameter] -> Name -> [Java.ClassModifier]
   -> Maybe Name -> [Java.InterfaceType] -> [Java.ClassBodyDeclarationWithComments] -> Java.ClassDeclaration
javaClassDeclaration aliases tparams elName mods supname impls bodyDecls = Java.ClassDeclarationNormal $ Java.NormalClassDeclaration {
  Java.normalClassDeclarationModifiers = mods,
  Java.normalClassDeclarationIdentifier = javaDeclName elName,
  Java.normalClassDeclarationParameters = tparams,
  Java.normalClassDeclarationExtends = fmap (\n -> nameToJavaClassType aliases True [] n Nothing) supname,
  Java.normalClassDeclarationImplements = impls,
  Java.normalClassDeclarationBody = Java.ClassBody bodyDecls}

javaClassType :: [Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.ClassType
javaClassType args pkg id = Java.ClassType [] qual (javaTypeIdentifier id) targs
  where
    qual = maybe Java.ClassTypeQualifierNone Java.ClassTypeQualifierPackage pkg
    targs = Java.TypeArgumentReference <$> args

javaClassTypeToJavaType :: Java.ClassType -> Java.Type
javaClassTypeToJavaType = Java.TypeReference . Java.ReferenceTypeClassOrInterface . Java.ClassOrInterfaceTypeClass

javaConditionalAndExpressionToJavaExpression :: Java.ConditionalAndExpression -> Java.Expression
javaConditionalAndExpressionToJavaExpression condAndEx = Java.ExpressionAssignment $
  Java.AssignmentExpressionConditional $ Java.ConditionalExpressionSimple $ Java.ConditionalOrExpression [condAndEx]

javaConstructorCall :: Java.ClassOrInterfaceTypeToInstantiate -> [Java.Expression] -> Maybe Java.ClassBody -> Java.Expression
javaConstructorCall ci args mbody = javaPrimaryToJavaExpression $
  Java.PrimaryNoNewArray_ $
  Java.PrimaryNoNewArrayClassInstance $
  Java.ClassInstanceCreationExpression Nothing $
  Java.UnqualifiedClassInstanceCreationExpression [] ci args mbody

javaConstructorName :: Java.Identifier -> Maybe Java.TypeArgumentsOrDiamond -> Java.ClassOrInterfaceTypeToInstantiate
javaConstructorName id targs = Java.ClassOrInterfaceTypeToInstantiate [Java.AnnotatedIdentifier [] id] targs

javaDeclName :: Name -> Java.TypeIdentifier
javaDeclName = Java.TypeIdentifier . javaVariableName

javaEmptyStatement :: Java.Statement
javaEmptyStatement = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementEmpty

javaEqualityExpressionToJavaExpression :: Java.EqualityExpression -> Java.Expression
javaEqualityExpressionToJavaExpression eqEx = javaConditionalAndExpressionToJavaExpression $
    Java.ConditionalAndExpression [javaEqualityExpressionToJavaInclusiveOrExpression eqEx]

javaEqualityExpressionToJavaInclusiveOrExpression :: Java.EqualityExpression -> Java.InclusiveOrExpression
javaEqualityExpressionToJavaInclusiveOrExpression eq = Java.InclusiveOrExpression [
  Java.ExclusiveOrExpression [Java.AndExpression [eq]]]

javaEquals :: Java.EqualityExpression -> Java.RelationalExpression -> Java.EqualityExpression
javaEquals lhs rhs = Java.EqualityExpressionEqual $ Java.EqualityExpression_Binary lhs rhs

javaEqualsNull :: Java.EqualityExpression -> Java.EqualityExpression
javaEqualsNull lhs = javaEquals lhs $ javaLiteralToJavaRelationalExpression Java.LiteralNull

javaExpressionNameToJavaExpression :: Java.ExpressionName -> Java.Expression
javaExpressionNameToJavaExpression = javaPostfixExpressionToJavaExpression . Java.PostfixExpressionName

javaExpressionToJavaPrimary :: Java.Expression -> Java.Primary
javaExpressionToJavaPrimary = Java.PrimaryNoNewArray_ . Java.PrimaryNoNewArrayParens

javaExpressionToJavaUnaryExpression :: Java.Expression -> Java.UnaryExpression
javaExpressionToJavaUnaryExpression = javaPrimaryToJavaUnaryExpression . javaExpressionToJavaPrimary

javaFieldAccessToJavaExpression :: Java.FieldAccess -> Java.Expression
javaFieldAccessToJavaExpression = javaPrimaryToJavaExpression . Java.PrimaryNoNewArray_ . Java.PrimaryNoNewArrayFieldAccess

javaIdentifier :: String -> Java.Identifier
javaIdentifier = Java.Identifier . sanitizeJavaName

javaIdentifierToJavaExpressionName :: Java.Identifier -> Java.ExpressionName
javaIdentifierToJavaExpressionName id = Java.ExpressionName Nothing id

javaIdentifierToJavaExpression :: Java.Identifier -> Java.Expression
javaIdentifierToJavaExpression = javaUnaryExpressionToJavaExpression . javaIdentifierToJavaUnaryExpression

javaIdentifierToJavaRelationalExpression :: Java.Identifier -> Java.RelationalExpression
javaIdentifierToJavaRelationalExpression id = javaPostfixExpressionToJavaRelationalExpression $
  Java.PostfixExpressionName $ Java.ExpressionName Nothing id

javaIdentifierToJavaUnaryExpression :: Java.Identifier -> Java.UnaryExpression
javaIdentifierToJavaUnaryExpression = javaRelationalExpressionToJavaUnaryExpression . javaIdentifierToJavaRelationalExpression

javaInstanceOf :: Java.RelationalExpression -> Java.ReferenceType -> Java.RelationalExpression
javaInstanceOf lhs rhs = Java.RelationalExpressionInstanceof $ Java.RelationalExpression_InstanceOf lhs rhs

javaInt :: Integral a => a -> Java.Literal
javaInt i = Java.LiteralInteger $ Java.IntegerLiteral $ fromIntegral i

javaIntExpression :: Integer -> Java.Expression
javaIntExpression = javaPrimaryToJavaExpression . javaLiteralToJavaPrimary . javaInt

javaIntType :: Java.Type
javaIntType = javaPrimitiveTypeToJavaType $ Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeInt

javaInterfaceDeclarationToJavaClassBodyDeclaration :: Java.NormalInterfaceDeclaration -> Java.ClassBodyDeclaration
javaInterfaceDeclarationToJavaClassBodyDeclaration = Java.ClassBodyDeclarationClassMember .
  Java.ClassMemberDeclarationInterface . Java.InterfaceDeclarationNormalInterface

javaLambda :: Name -> Java.Expression -> Java.Expression
javaLambda var jbody = Java.ExpressionLambda $ Java.LambdaExpression params (Java.LambdaBodyExpression jbody)
  where
    params = Java.LambdaParametersSingle $ variableToJavaIdentifier var

javaLambdaFromBlock :: Name -> Java.Block -> Java.Expression
javaLambdaFromBlock var block = Java.ExpressionLambda $ Java.LambdaExpression params (Java.LambdaBodyBlock block)
  where
    params = Java.LambdaParametersSingle $ variableToJavaIdentifier var

javaLiteralToJavaRelationalExpression :: Java.Literal -> Java.RelationalExpression
javaLiteralToJavaRelationalExpression = javaMultiplicativeExpressionToJavaRelationalExpression .
  javaLiteralToJavaMultiplicativeExpression

javaLiteralToJavaExpression = javaRelationalExpressionToJavaExpression .
  javaMultiplicativeExpressionToJavaRelationalExpression .
  javaLiteralToJavaMultiplicativeExpression

javaLiteralToJavaMultiplicativeExpression = Java.MultiplicativeExpressionUnary . javaPrimaryToJavaUnaryExpression .
  javaLiteralToJavaPrimary

javaLiteralToJavaPrimary :: Java.Literal -> Java.Primary
javaLiteralToJavaPrimary = Java.PrimaryNoNewArray_ . Java.PrimaryNoNewArrayLiteral

javaMemberField :: [Java.FieldModifier] -> Java.Type -> Java.VariableDeclarator -> Java.ClassBodyDeclaration
javaMemberField mods jt var = Java.ClassBodyDeclarationClassMember $ Java.ClassMemberDeclarationField $
  Java.FieldDeclaration mods (Java.UnannType jt) [var]

javaMethodBody :: Maybe [Java.BlockStatement] -> Java.MethodBody
javaMethodBody stmts = Y.maybe Java.MethodBodyNone (Java.MethodBodyBlock . Java.Block) stmts

javaMethodDeclarationToJavaClassBodyDeclaration :: Java.MethodDeclaration -> Java.ClassBodyDeclaration
javaMethodDeclarationToJavaClassBodyDeclaration = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationMethod

javaMethodHeader :: [Java.TypeParameter] -> String -> [Java.FormalParameter] -> Java.Result -> Java.MethodHeader
javaMethodHeader tparams methodName params result = Java.MethodHeader tparams result decl mthrows
  where
    decl = Java.MethodDeclarator (Java.Identifier methodName) Nothing params
    mthrows = Nothing

javaMethodInvocationToJavaExpression :: Java.MethodInvocation -> Java.Expression
javaMethodInvocationToJavaExpression = javaPrimaryToJavaExpression . javaMethodInvocationToJavaPrimary

javaMethodInvocationToJavaStatement :: Java.MethodInvocation -> Java.Statement
javaMethodInvocationToJavaStatement = Java.StatementWithoutTrailing . Java.StatementWithoutTrailingSubstatementExpression .
  Java.ExpressionStatement . Java.StatementExpressionMethodInvocation

javaMethodInvocationToJavaPostfixExpression :: Java.MethodInvocation -> Java.PostfixExpression
javaMethodInvocationToJavaPostfixExpression = Java.PostfixExpressionPrimary . Java.PrimaryNoNewArray_ .
  Java.PrimaryNoNewArrayMethodInvocation

javaMethodInvocationToJavaPrimary :: Java.MethodInvocation -> Java.Primary
javaMethodInvocationToJavaPrimary = Java.PrimaryNoNewArray_ .
  Java.PrimaryNoNewArrayMethodInvocation

javaMultiplicativeExpressionToJavaRelationalExpression :: Java.MultiplicativeExpression -> Java.RelationalExpression
javaMultiplicativeExpressionToJavaRelationalExpression = Java.RelationalExpressionSimple .
  Java.ShiftExpressionUnary . Java.AdditiveExpressionUnary

javaPackageDeclaration :: Namespace -> Java.PackageDeclaration
javaPackageDeclaration (Namespace name) = Java.PackageDeclaration [] (Java.Identifier <$> Strings.splitOn "." name)

javaPostfixExpressionToJavaEqualityExpression :: Java.PostfixExpression -> Java.EqualityExpression
javaPostfixExpressionToJavaEqualityExpression = Java.EqualityExpressionUnary .
  javaUnaryExpressionToJavaRelationalExpression . Java.UnaryExpressionOther . Java.UnaryExpressionNotPlusMinusPostfix

javaPostfixExpressionToJavaExpression :: Java.PostfixExpression -> Java.Expression
javaPostfixExpressionToJavaExpression = javaRelationalExpressionToJavaExpression .
  javaPostfixExpressionToJavaRelationalExpression

javaPostfixExpressionToJavaInclusiveOrExpression :: Java.PostfixExpression -> Java.InclusiveOrExpression
javaPostfixExpressionToJavaInclusiveOrExpression = javaEqualityExpressionToJavaInclusiveOrExpression .
  javaPostfixExpressionToJavaEqualityExpression

javaPostfixExpressionToJavaRelationalExpression :: Java.PostfixExpression -> Java.RelationalExpression
javaPostfixExpressionToJavaRelationalExpression =
  javaUnaryExpressionToJavaRelationalExpression . javaPostfixExpressionToJavaUnaryExpression

javaPostfixExpressionToJavaUnaryExpression :: Java.PostfixExpression -> Java.UnaryExpression
javaPostfixExpressionToJavaUnaryExpression = Java.UnaryExpressionOther . Java.UnaryExpressionNotPlusMinusPostfix

javaPrimaryToJavaExpression :: Java.Primary -> Java.Expression
javaPrimaryToJavaExpression = javaPostfixExpressionToJavaExpression . Java.PostfixExpressionPrimary

javaPrimaryToJavaUnaryExpression :: Java.Primary -> Java.UnaryExpression
javaPrimaryToJavaUnaryExpression = Java.UnaryExpressionOther .
  Java.UnaryExpressionNotPlusMinusPostfix .
  Java.PostfixExpressionPrimary

javaPrimitiveTypeToJavaType :: Java.PrimitiveType -> Java.Type
javaPrimitiveTypeToJavaType pt = Java.TypePrimitive $ Java.PrimitiveTypeWithAnnotations pt []

javaRefType :: [Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.Type
javaRefType args pkg id = Java.TypeReference $ Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
  javaClassType args pkg id

javaRelationalExpressionToJavaEqualityExpression :: Java.RelationalExpression -> Java.EqualityExpression
javaRelationalExpressionToJavaEqualityExpression = Java.EqualityExpressionUnary

javaRelationalExpressionToJavaExpression :: Java.RelationalExpression -> Java.Expression
javaRelationalExpressionToJavaExpression = javaEqualityExpressionToJavaExpression . javaRelationalExpressionToJavaEqualityExpression

javaRelationalExpressionToJavaUnaryExpression :: Java.RelationalExpression -> Java.UnaryExpression
javaRelationalExpressionToJavaUnaryExpression = javaPrimaryToJavaUnaryExpression .
  Java.PrimaryNoNewArray_ .
  Java.PrimaryNoNewArrayParens .
  javaRelationalExpressionToJavaExpression

javaReturnStatement :: Y.Maybe Java.Expression -> Java.Statement
javaReturnStatement = Java.StatementWithoutTrailing . Java.StatementWithoutTrailingSubstatementReturn .
  Java.ReturnStatement

javaStatementsToBlock :: [Java.Statement] -> Java.Block
javaStatementsToBlock stmts = Java.Block (Java.BlockStatementStatement <$> stmts)

javaString :: String -> Java.Literal
javaString = Java.LiteralString . Java.StringLiteral

javaStringMultiplicativeExpression :: String -> Java.MultiplicativeExpression
javaStringMultiplicativeExpression = javaLiteralToJavaMultiplicativeExpression . javaString

javaThrowIllegalArgumentException :: [Java.Expression] -> Java.Statement
javaThrowIllegalArgumentException args = javaThrowStatement $
  javaConstructorCall (javaConstructorName (Java.Identifier "IllegalArgumentException") Nothing) args Nothing

javaThrowIllegalStateException :: [Java.Expression] -> Java.Statement
javaThrowIllegalStateException args = javaThrowStatement $
  javaConstructorCall (javaConstructorName (Java.Identifier "IllegalStateException") Nothing) args Nothing

javaThrowStatement :: Java.Expression -> Java.Statement
javaThrowStatement = Java.StatementWithoutTrailing .
  Java.StatementWithoutTrailingSubstatementThrow . Java.ThrowStatement

javaThis :: Java.Expression
javaThis = javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray_ Java.PrimaryNoNewArrayThis

javaTypeFromTypeName :: Aliases -> Name -> Java.Type
javaTypeFromTypeName aliases elName = javaTypeVariableToType $ Java.TypeVariable [] $
  nameToJavaTypeIdentifier aliases False elName

javaTypeIdentifier :: String -> Java.TypeIdentifier
javaTypeIdentifier = Java.TypeIdentifier . Java.Identifier

javaTypeIdentifierToJavaTypeArgument :: Java.TypeIdentifier -> Java.TypeArgument
javaTypeIdentifierToJavaTypeArgument id = Java.TypeArgumentReference $ Java.ReferenceTypeVariable $ Java.TypeVariable [] id

javaTypeName :: Java.Identifier -> Java.TypeName
javaTypeName id = Java.TypeName (Java.TypeIdentifier id) Nothing

javaTypeParameter :: String -> Java.TypeParameter
javaTypeParameter v = Java.TypeParameter [] (javaTypeIdentifier v) Nothing

javaTypeToJavaFormalParameter :: Java.Type -> Name -> Java.FormalParameter
javaTypeToJavaFormalParameter jt fname = Java.FormalParameterSimple $ Java.FormalParameter_Simple [] argType argId
  where
    argType = Java.UnannType jt
    argId = fieldNameToJavaVariableDeclaratorId fname

javaTypeToJavaReferenceType :: Java.Type -> Flow (Graph) Java.ReferenceType
javaTypeToJavaReferenceType t = case t of
  Java.TypeReference rt -> pure rt
  _ -> fail $ "expected a Java reference type. Found: " ++ show t

javaTypeToJavaResult :: Java.Type -> Java.Result
javaTypeToJavaResult = Java.ResultType . Java.UnannType

javaTypeToJavaTypeArgument :: Java.Type -> Java.TypeArgument
javaTypeToJavaTypeArgument t = case t of
  Java.TypeReference rt -> Java.TypeArgumentReference rt
  _ -> Java.TypeArgumentWildcard $ Java.Wildcard [] Nothing -- TODO

javaTypeVariable :: String -> Java.ReferenceType
javaTypeVariable v = Java.ReferenceTypeVariable $ Java.TypeVariable [] $ javaTypeIdentifier $ capitalize v

javaTypeVariableToType :: Java.TypeVariable -> Java.Type
javaTypeVariableToType = Java.TypeReference . Java.ReferenceTypeVariable

javaUnaryExpressionToJavaExpression :: Java.UnaryExpression -> Java.Expression
javaUnaryExpressionToJavaExpression = javaRelationalExpressionToJavaExpression .
  javaUnaryExpressionToJavaRelationalExpression

javaUnaryExpressionToJavaRelationalExpression :: Java.UnaryExpression -> Java.RelationalExpression
javaUnaryExpressionToJavaRelationalExpression = javaMultiplicativeExpressionToJavaRelationalExpression .
  Java.MultiplicativeExpressionUnary

javaVariableDeclarator :: Java.Identifier -> Y.Maybe Java.VariableInitializer -> Java.VariableDeclarator
javaVariableDeclarator id = Java.VariableDeclarator (javaVariableDeclaratorId id)

javaVariableDeclaratorId :: Java.Identifier -> Java.VariableDeclaratorId
javaVariableDeclaratorId id = Java.VariableDeclaratorId id Nothing

javaVariableName :: Name -> Java.Identifier
javaVariableName = javaIdentifier . localNameOf

makeConstructor :: Aliases -> Name -> Bool -> [Java.FormalParameter]
  -> [Java.BlockStatement] -> Java.ClassBodyDeclaration
makeConstructor aliases elName private params stmts = Java.ClassBodyDeclarationConstructorDeclaration $
    Java.ConstructorDeclaration mods cons Nothing body
  where
    nm = Java.SimpleTypeName $ nameToJavaTypeIdentifier aliases False elName
    cons = Java.ConstructorDeclarator [] nm Nothing params
    mods = [if private then Java.ConstructorModifierPrivate else Java.ConstructorModifierPublic]
    body = Java.ConstructorBody Nothing stmts

methodDeclaration :: [Java.MethodModifier] -> [Java.TypeParameter] -> [Java.Annotation] -> String
  -> [Java.FormalParameter] -> Java.Result -> Maybe [Java.BlockStatement] -> Java.ClassBodyDeclaration
methodDeclaration mods tparams anns methodName params result stmts =
    javaMethodDeclarationToJavaClassBodyDeclaration $
    Java.MethodDeclaration anns mods header body
  where
    header = javaMethodHeader tparams methodName params result
    body = javaMethodBody stmts

methodInvocation :: Y.Maybe (Either Java.ExpressionName Java.Primary) -> Java.Identifier -> [Java.Expression] -> Java.MethodInvocation
methodInvocation lhs methodName = Java.MethodInvocation header
  where
    header = case lhs of
      Nothing -> Java.MethodInvocation_HeaderSimple $ Java.MethodName methodName
      Just either -> Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex inject targs methodName
        where
          targs = []
          inject = case either of
            Left name -> Java.MethodInvocation_VariantExpression name
            Right prim -> Java.MethodInvocation_VariantPrimary prim

methodInvocationStatic :: Java.Identifier -> Java.Identifier -> [Java.Expression] -> Java.MethodInvocation
methodInvocationStatic self methodName = methodInvocation (Just $ Left name) methodName
  where
    name = Java.ExpressionName Nothing self

methodInvocationStaticWithTypeArgs :: Java.Identifier -> Java.Identifier -> [Java.TypeArgument] -> [Java.Expression] -> Java.MethodInvocation
methodInvocationStaticWithTypeArgs self methodName targs = Java.MethodInvocation header
  where
    header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex inject targs methodName
    inject = Java.MethodInvocation_VariantExpression name
    name = Java.ExpressionName Nothing self

nameToJavaClassType :: Aliases -> Bool -> [Java.TypeArgument] -> Name -> Maybe String -> Java.ClassType
nameToJavaClassType aliases qualify args name mlocal = Java.ClassType [] pkg id args
  where
    (id, pkg) = nameToQualifiedJavaName aliases qualify name mlocal

nameToQualifiedJavaName :: Aliases -> Bool -> Name -> Maybe String -> (Java.TypeIdentifier, Java.ClassTypeQualifier)
nameToQualifiedJavaName aliases qualify name mlocal = (jid, pkg)
  where
    QualifiedName ns local = qualifyName name
    alias = case ns of
      Nothing -> Nothing
      Just n -> case M.lookup n (aliasesPackages aliases) of
          Nothing -> Just $ javaPackageName $ Strings.splitOn "." $ unNamespace n
          Just id -> Just id
    pkg = if qualify
      then Y.maybe none Java.ClassTypeQualifierPackage alias
      else none
    none = Java.ClassTypeQualifierNone
    jid = javaTypeIdentifier $ case mlocal of
      Nothing -> sanitizeJavaName local
      Just l -> sanitizeJavaName local ++ "." ++ sanitizeJavaName l

nameToJavaName :: Aliases -> Name -> Java.Identifier
nameToJavaName aliases name = Java.Identifier $ if isEscaped (unName name)
    then sanitizeJavaName local
    else case ns of
      Nothing -> local
      Just gname -> case M.lookup gname (aliasesPackages aliases) of
        Nothing -> fromParts $ Strings.splitOn "." $ unNamespace gname
        Just (Java.PackageName parts) -> fromParts (Java.unIdentifier <$> parts)
  where
    QualifiedName ns local = qualifyName name
    fromParts parts = L.intercalate "." $ parts ++ [sanitizeJavaName local]

nameToJavaReferenceType :: Aliases -> Bool -> [Java.TypeArgument] -> Name -> Maybe String -> Java.ReferenceType
nameToJavaReferenceType aliases qualify args name mlocal = Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
  nameToJavaClassType aliases qualify args name mlocal

nameToJavaTypeIdentifier :: Aliases -> Bool -> Name -> Java.TypeIdentifier
nameToJavaTypeIdentifier aliases qualify name = fst $ nameToQualifiedJavaName aliases qualify name Nothing

overrideAnnotation :: Java.Annotation
overrideAnnotation = Java.AnnotationMarker $ Java.MarkerAnnotation $ javaTypeName $ Java.Identifier "Override"

suppressWarningsUncheckedAnnotation :: Java.Annotation
suppressWarningsUncheckedAnnotation = Java.AnnotationSingleElement $ Java.SingleElementAnnotation
    (javaTypeName $ Java.Identifier "SuppressWarnings")
    (Just $ Java.ElementValueConditionalExpression $
      Java.ConditionalExpressionSimple $ Java.ConditionalOrExpression
        [Java.ConditionalAndExpression
          [javaPostfixExpressionToJavaInclusiveOrExpression $
            Java.PostfixExpressionPrimary $ javaLiteralToJavaPrimary $
              Java.LiteralString $ Java.StringLiteral "unchecked"]])

referenceTypeToResult :: Java.ReferenceType -> Java.Result
referenceTypeToResult = javaTypeToJavaResult . Java.TypeReference

sanitizeJavaName :: String -> String
sanitizeJavaName name = if isEscaped name
  -- The '$' prefix allows names to be excluded from sanitization
  then unescape name
  else if name == "_"
  then "ignored"
  else sanitizeWithUnderscores reservedWords name

toAcceptMethod :: Bool -> [Java.TypeParameter] -> Java.ClassBodyDeclaration
toAcceptMethod abstract vtparams = methodDeclaration mods tparams anns acceptMethodName [param] result body
  where
    mods = [Java.MethodModifierPublic] ++ if abstract then [Java.MethodModifierAbstract] else []
    tparams = [javaTypeParameter visitorReturnParameter]
    anns = if abstract
      then []
      else [overrideAnnotation]
    param = javaTypeToJavaFormalParameter ref (Name varName)
      where
        ref = javaClassTypeToJavaType $
          Java.ClassType
            []
            Java.ClassTypeQualifierNone
            (javaTypeIdentifier visitorName)
            (typeArgs ++ [Java.TypeArgumentReference visitorTypeVariable])
    typeArgs = (Java.TypeArgumentReference . typeParameterToReferenceType) <$> vtparams
    result = javaTypeToJavaResult $ Java.TypeReference visitorTypeVariable
    varName = "visitor"
    body = if abstract
      then Nothing
      else Just [Java.BlockStatementStatement $ javaReturnStatement $ Just returnExpr]
    returnExpr = javaMethodInvocationToJavaExpression $
        methodInvocationStatic (Java.Identifier varName) (Java.Identifier visitMethodName) [javaThis]

toAssignStmt :: Name -> Java.Statement
toAssignStmt fname = javaAssignmentStatement lhs rhs
  where
    lhs = Java.LeftHandSideFieldAccess $ thisField id
      where
        id = fieldNameToJavaIdentifier fname
    rhs = fieldNameToJavaExpression fname
    thisField = Java.FieldAccess $ Java.FieldAccess_QualifierPrimary $ Java.PrimaryNoNewArray_ Java.PrimaryNoNewArrayThis

toJavaArrayType :: Java.Type -> Flow (Graph) Java.Type
toJavaArrayType t = Java.TypeReference . Java.ReferenceTypeArray <$> case t of
  Java.TypeReference rt -> case rt of
    Java.ReferenceTypeClassOrInterface cit -> pure $
      Java.ArrayType (Java.Dims [[]]) $ Java.ArrayType_VariantClassOrInterface cit
    Java.ReferenceTypeArray (Java.ArrayType (Java.Dims d) v) -> pure $
      Java.ArrayType (Java.Dims $ d ++ [[]]) v
    _ -> fail $ "don't know how to make Java reference type into array type: " ++ show rt
  _ -> fail $ "don't know how to make Java type into array type: " ++ show t

typeParameterToTypeArgument :: Java.TypeParameter -> Java.TypeArgument
typeParameterToTypeArgument (Java.TypeParameter _ id _) = javaTypeIdentifierToJavaTypeArgument id

typeParameterToReferenceType :: Java.TypeParameter -> Java.ReferenceType
typeParameterToReferenceType = javaTypeVariable . unTypeParameter

unTypeParameter :: Java.TypeParameter -> String
unTypeParameter (Java.TypeParameter [] (Java.TypeIdentifier (Java.Identifier v)) Nothing) = v

unescape :: String -> String
unescape = L.tail

varDeclarationStatement :: Java.Identifier -> Java.Expression -> Java.BlockStatement
varDeclarationStatement id rhs = Java.BlockStatementLocalVariableDeclaration $
    Java.LocalVariableDeclarationStatement $ Java.LocalVariableDeclaration [] Java.LocalVariableTypeVar [vdec]
  where
    vdec = javaVariableDeclarator id (Just init)
      where
        init = Java.VariableInitializerExpression rhs

variableDeclarationStatement :: Aliases -> Java.Type -> Java.Identifier -> Java.Expression -> Java.BlockStatement
variableDeclarationStatement aliases jtype id rhs = Java.BlockStatementLocalVariableDeclaration $
    Java.LocalVariableDeclarationStatement $ Java.LocalVariableDeclaration [] (Java.LocalVariableTypeType $ Java.UnannType jtype) [vdec]
  where
    vdec = javaVariableDeclarator id (Just init)
      where
        init = Java.VariableInitializerExpression rhs

variableToJavaIdentifier :: Name -> Java.Identifier
variableToJavaIdentifier (Name var) = Java.Identifier $ if var == ignoredVariable
  then "ignored"
  else sanitizeJavaName var

variantClassName :: Bool -> Name -> Name -> Name
variantClassName qualify elName (Name fname) = unqualifyName (QualifiedName ns local1)
  where
    QualifiedName ns local = qualifyName elName
    flocal = capitalize fname
    local1 = if qualify
      then local ++ "." ++ flocal
      else if flocal == local then flocal ++ "_" else flocal

visitorTypeVariable :: Java.ReferenceType
visitorTypeVariable = javaTypeVariable "r"

-- | Look up the Java variable name for a Hydra variable, applying any renames
lookupJavaVarName :: Aliases -> Name -> Name
lookupJavaVarName aliases name = case M.lookup name (aliasesVarRenames aliases) of
  Just renamed -> renamed
  Nothing -> name

-- | Generate a unique variable name that doesn't conflict with in-scope names
uniqueVarName :: Aliases -> Name -> Name
uniqueVarName aliases name@(Name base) =
  if S.member name (aliasesInScopeJavaVars aliases)
    then findUnique 2
    else name
  where
    findUnique n =
      let candidate = Name (base ++ show n)
      in if S.member candidate (aliasesInScopeJavaVars aliases)
         then findUnique (n + 1)
         else candidate

-- | Add a variable to the in-scope set and return updated aliases
addInScopeVar :: Name -> Aliases -> Aliases
addInScopeVar name aliases = aliases {
  aliasesInScopeJavaVars = S.insert name (aliasesInScopeJavaVars aliases)
}

-- | Add multiple variables to the in-scope set
addInScopeVars :: [Name] -> Aliases -> Aliases
addInScopeVars names aliases = L.foldl (flip addInScopeVar) aliases names

-- | Register a variable rename
addVarRename :: Name -> Name -> Aliases -> Aliases
addVarRename original renamed aliases = aliases {
  aliasesVarRenames = M.insert original renamed (aliasesVarRenames aliases)
}

-- | Create a Java primitive type for byte
javaBytePrimitiveType :: Java.PrimitiveTypeWithAnnotations
javaBytePrimitiveType = Java.PrimitiveTypeWithAnnotations
  (Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeByte) []

-- | Create an array creation expression for primitive byte arrays with an initializer
--   e.g. new byte[] {1, 2, 3}
javaArrayCreation :: Java.PrimitiveTypeWithAnnotations -> Maybe Java.ArrayInitializer -> Java.Expression
javaArrayCreation primType minit = javaPrimaryToJavaExpression $
    Java.PrimaryArrayCreation $
    Java.ArrayCreationExpressionPrimitiveArray $
    Java.ArrayCreationExpression_PrimitiveArray primType [] init
  where
    init = Y.fromMaybe (Java.ArrayInitializer []) minit

-- | Create an array initializer from a list of expressions
--   e.g. {1, 2, 3}
javaArrayInitializer :: [Java.Expression] -> Java.ArrayInitializer
javaArrayInitializer exprs = Java.ArrayInitializer [fmap toVarInit exprs]
  where
    toVarInit = Java.VariableInitializerExpression
