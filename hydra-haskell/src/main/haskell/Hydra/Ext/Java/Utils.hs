module Hydra.Ext.Java.Utils where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Coders
import Hydra.Ext.Java.Language

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


addJavaTypeParameter :: Java.ReferenceType -> Java.Type -> Result Java.Type
addJavaTypeParameter rt t = case t of
  Java.TypeReference (Java.ReferenceTypeClassOrInterface cit) -> case cit of
    Java.ClassOrInterfaceTypeClass (Java.ClassType anns qual id args) -> pure $
      Java.TypeReference $ Java.ReferenceTypeClassOrInterface $
        Java.ClassOrInterfaceTypeClass $ Java.ClassType anns qual id (args ++ [Java.TypeArgumentReference rt])
    _ -> fail $ "expected a Java class type. Found: " ++ show cit
  _ -> fail $ "expected a Java class or interface type. Found: " ++ show t

asJavaReferenceType :: Java.Type -> Result Java.ReferenceType
asJavaReferenceType t = case t of
  Java.TypeReference rt -> pure rt
  _ -> fail $ "expected a Java reference type. Found: " ++ show t

fieldExpression :: Java.Identifier -> Java.Identifier -> Java.ExpressionName
fieldExpression varId fieldId = Java.ExpressionName fieldId (Just $ Java.AmbiguousName [varId])

fieldNameToJavaExpression :: FieldName -> Java.Expression
fieldNameToJavaExpression fname = javaPostfixExpressionToJavaExpression $
  Java.PostfixExpressionName $ Java.ExpressionName (fieldNameToJavaIdentifier fname) Nothing

fieldNameToJavaIdentifier :: FieldName -> Java.Identifier
fieldNameToJavaIdentifier (FieldName name) = Java.Identifier $ javaEscape name

fieldNameToJavaVariableDeclarator :: FieldName -> Java.VariableDeclarator
fieldNameToJavaVariableDeclarator (FieldName n) = javaVariableDeclarator $ Java.Identifier $ javaEscape n

fieldNameToJavaVariableDeclaratorId :: FieldName -> Java.VariableDeclaratorId
fieldNameToJavaVariableDeclaratorId (FieldName n) = javaVariableDeclaratorId $ Java.Identifier $ javaEscape n

importAliasesForGraph :: Graph m -> M.Map GraphName Java.PackageName
importAliasesForGraph g = L.foldl addName M.empty $ S.toList deps
  where
    deps = dataGraphDependencies True True True g
    addName m name = M.insert name (graphNameToPackageName name) m
    graphNameToPackageName (GraphName n) = javaPackageName $ Strings.splitOn "/" n

javaAssignmentStatement :: Java.LeftHandSide -> Java.Expression -> Java.Statement
javaAssignmentStatement lhs rhs = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementExpression $
    Java.ExpressionStatement $ Java.StatementExpressionAssignment ass
  where
    ass = Java.Assignment lhs Java.AssignmentOperatorSimple rhs

javaClassDeclaration :: M.Map GraphName Java.PackageName -> Name -> [Java.ClassModifier] -> Maybe Name -> [Java.ClassBodyDeclaration] -> Java.ClassDeclaration
javaClassDeclaration aliases elName mods supname bodyDecls = Java.ClassDeclarationNormal $ Java.NormalClassDeclaration {
  Java.normalClassDeclarationModifiers = mods,
  Java.normalClassDeclarationIdentifier = javaDeclName elName,
  Java.normalClassDeclarationParameters = [],
  Java.normalClassDeclarationExtends = fmap (nameToJavaClassType aliases True) supname,
  Java.normalClassDeclarationImplements = [],
  Java.normalClassDeclarationBody = Java.ClassBody bodyDecls}

javaClassType :: [Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.ClassType
javaClassType args pkg id = Java.ClassType [] qual (javaTypeIdentifier id) targs
  where
    qual = maybe Java.ClassTypeQualifierNone Java.ClassTypeQualifierPackage pkg
    targs = Java.TypeArgumentReference <$> args

javaConstructorCall :: Name -> [Java.Expression] -> Java.Expression
javaConstructorCall elName args = javaPrimaryToJavaExpression $
  Java.PrimaryNoNewArray $
  Java.PrimaryNoNewArrayClassInstance $
  Java.ClassInstanceCreationExpression Nothing $
  Java.UnqualifiedClassInstanceCreationExpression [] (javaConstructorName elName) args Nothing

javaConstructorName :: Name -> Java.ClassOrInterfaceTypeToInstantiate
javaConstructorName name = Java.ClassOrInterfaceTypeToInstantiate [id] Nothing
  where
    local = localNameOf name
    id = Java.AnnotatedIdentifier [] $ Java.Identifier $ if S.member local javaReservedWords
      then local ++ "_"
      else local

javaDeclName :: Name -> Java.TypeIdentifier
javaDeclName elName = javaTypeIdentifier (localNameOf elName)

javaEscape :: String -> String
javaEscape s = if S.member s javaReservedWords then s ++ "_" else s

javaEmptyStatement :: Java.Statement
javaEmptyStatement = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementEmpty Java.EmptyStatement

javaMemberField :: [Java.FieldModifier] -> Java.Type -> Java.VariableDeclarator -> Java.ClassBodyDeclaration
javaMemberField mods jt var = Java.ClassBodyDeclarationClassMember $ Java.ClassMemberDeclarationField $
  Java.FieldDeclaration mods (Java.UnannType jt) [var]

javaPackageDeclaration :: GraphName -> Java.PackageDeclaration
javaPackageDeclaration (GraphName name) = Java.PackageDeclaration [] (Java.Identifier <$> Strings.splitOn "/" name)

javaPackageName :: [String] -> Java.PackageName
javaPackageName parts = Java.PackageName (Java.Identifier <$> parts)

javaPostfixExpressionToJavaExpression :: Java.PostfixExpression -> Java.Expression
javaPostfixExpressionToJavaExpression pe = Java.ExpressionAssignment $ Java.AssignmentExpressionConditional $
    Java.ConditionalExpressionSimple $ Java.ConditionalOrExpression [condAndEx]
  where
    relEx = Java.RelationalExpressionSimple $
      Java.ShiftExpressionUnary $
      Java.AdditiveExpressionUnary $
      Java.MultiplicativeExpressionUnary $
      Java.UnaryExpressionOther $
      Java.UnaryExpressionNotPlusMinusPostfix pe
    andEx = Java.AndExpression [Java.EqualityExpressionUnary relEx]
    exOrEx = Java.ExclusiveOrExpression [andEx]
    incOrEx = Java.InclusiveOrExpression [exOrEx]
    condAndEx = Java.ConditionalAndExpression [incOrEx]

javaPrimaryToJavaExpression :: Java.Primary -> Java.Expression
javaPrimaryToJavaExpression = javaPostfixExpressionToJavaExpression . Java.PostfixExpressionPrimary

javaRefType :: [Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.Type
javaRefType args pkg id = Java.TypeReference $ Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
  javaClassType args pkg id

javaReturnStatement :: Y.Maybe Java.Expression -> Java.Statement
javaReturnStatement mex = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementReturn $
  Java.ReturnStatement mex

javaStatementsToBlock :: [Java.Statement] -> Java.Block
javaStatementsToBlock stmts = Java.Block (Java.BlockStatementStatement <$> stmts)

javaThis :: Java.Expression
javaThis = javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray Java.PrimaryNoNewArrayThis

javaTypeIdentifier :: String -> Java.TypeIdentifier
javaTypeIdentifier = Java.TypeIdentifier . Java.Identifier

javaTypeParameter :: String -> Java.TypeParameter
javaTypeParameter v = Java.TypeParameter [] (javaTypeIdentifier v) Nothing

javaTypeVariable :: String -> Java.ReferenceType
javaTypeVariable v = Java.ReferenceTypeVariable $ Java.TypeVariable [] $ javaTypeIdentifier v

javaUtilPackageName :: Maybe Java.PackageName
javaUtilPackageName = Just $ javaPackageName ["java", "util"]

javaTypeToJavaFormalParameter :: Java.Type -> FieldName -> Java.FormalParameter
javaTypeToJavaFormalParameter jt fname = Java.FormalParameterSimple $ Java.FormalParameter_Simple [] argType argId
  where
    argType = Java.UnannType jt
    argId = fieldNameToJavaVariableDeclaratorId fname

javaTypeToResult :: Java.Type -> Java.Result
javaTypeToResult = Java.ResultType . Java.UnannType

javaVariableDeclarator :: Java.Identifier -> Java.VariableDeclarator
javaVariableDeclarator id = Java.VariableDeclarator (javaVariableDeclaratorId id) Nothing

javaVariableDeclaratorId :: Java.Identifier -> Java.VariableDeclaratorId
javaVariableDeclaratorId id = Java.VariableDeclaratorId id Nothing

makeConstructor :: M.Map GraphName Java.PackageName -> Name -> Bool -> [Java.FormalParameter]
  -> [Java.BlockStatement] -> Java.ClassBodyDeclaration
makeConstructor aliases elName private params stmts = Java.ClassBodyDeclarationConstructorDeclaration $
    Java.ConstructorDeclaration mods cons Nothing body
  where
    nm = Java.SimpleTypeName $ nameToJavaTypeIdentifier aliases False elName
    cons = Java.ConstructorDeclarator [] nm Nothing params
    mods = [if private then Java.ConstructorModifierPrivate else Java.ConstructorModifierPublic]
    body = Java.ConstructorBody Nothing stmts

methodDeclaration :: [Java.MethodModifier] -> [Java.TypeParameter] -> [Java.Annotation] -> String
  -> [Java.FormalParameter] -> Java.Result -> Y.Maybe [Java.Statement] -> Java.ClassBodyDeclaration
methodDeclaration mods tparams anns methodName params result stmts = Java.ClassBodyDeclarationClassMember $
    Java.ClassMemberDeclarationMethod $
    Java.MethodDeclaration mods header $
    (Y.maybe Java.MethodBodyNone (\s -> Java.MethodBodyBlock $ javaStatementsToBlock s) stmts)
  where
    header = Java.MethodHeader tparams anns result decl mthrows
    decl = Java.MethodDeclarator (Java.Identifier methodName) Nothing params
    mthrows = Nothing

nameToJavaClassType :: M.Map GraphName Java.PackageName -> Bool -> Name -> Java.ClassType
nameToJavaClassType aliases qualify name = Java.ClassType [] pkg id []
  where
    (id, pkg) = nameToQualifiedJavaName aliases qualify name

nameToQualifiedJavaName :: M.Map GraphName Java.PackageName -> Bool -> Name
  -> (Java.TypeIdentifier, Java.ClassTypeQualifier)
nameToQualifiedJavaName aliases qualify name = (javaTypeIdentifier local, pkg)
  where
    (gname, local) = toQname name
    pkg = if qualify || S.member local javaReservedWords
      then Y.maybe none Java.ClassTypeQualifierPackage $ M.lookup gname aliases
      else none
    none = Java.ClassTypeQualifierNone

nameToJavaReferenceType :: M.Map GraphName Java.PackageName -> Bool -> Name -> Java.ReferenceType
nameToJavaReferenceType aliases qualify name = Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
  nameToJavaClassType aliases qualify name

nameToJavaTypeIdentifier :: M.Map GraphName Java.PackageName -> Bool -> Name -> Java.TypeIdentifier
nameToJavaTypeIdentifier aliases qualify name = fst $ nameToQualifiedJavaName aliases qualify name

referenceTypeToResult :: Java.ReferenceType -> Java.Result
referenceTypeToResult = javaTypeToResult . Java.TypeReference

toAcceptMethod :: Bool -> Java.ClassBodyDeclaration
toAcceptMethod abstract = methodDeclaration mods tparams anns "accept" [param] result body
  where
    mods = [Java.MethodModifierPublic] ++ if abstract then [Java.MethodModifierAbstract] else []
    tparams = [javaTypeParameter "R"]
    anns = []
    param = javaTypeToJavaFormalParameter ref (FieldName varName)
      where
        ref = Java.TypeReference $ Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
          Java.ClassType
            []
            Java.ClassTypeQualifierNone
            (javaTypeIdentifier "Visitor")
            [Java.TypeArgumentReference $ javaTypeVariable "R"]
    result = javaTypeToResult $ Java.TypeReference $ javaTypeVariable "R"
    varName = "visitor"
    visitMethodName = Java.Identifier "visit"
    body = if abstract
      then Nothing
      else Just [javaReturnStatement $ Just returnExpr]
    returnExpr = javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray $ Java.PrimaryNoNewArrayMethodInvocation $
        Java.MethodInvocation header args
      where
        header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex variant targs visitMethodName
        args = [javaThis]
        targs = []
        variant = Java.MethodInvocation_VariantExpression $ Java.ExpressionName (Java.Identifier varName) Nothing

toAssignStmt :: FieldName -> Java.Statement
toAssignStmt fname = javaAssignmentStatement lhs rhs
  where
    lhs = Java.LeftHandSideFieldAccess $ thisField id
      where
        id = fieldNameToJavaIdentifier fname
    rhs = fieldNameToJavaExpression fname
    thisField = Java.FieldAccess $ Java.FieldAccess_QualifierPrimary $ Java.PrimaryNoNewArray Java.PrimaryNoNewArrayThis

toJavaArrayType :: Java.Type -> Result Java.Type
toJavaArrayType t = Java.TypeReference . Java.ReferenceTypeArray <$> case t of
  Java.TypeReference rt -> case rt of
    Java.ReferenceTypeClassOrInterface cit -> pure $
      Java.ArrayType (Java.Dims [[]]) $ Java.ArrayType_VariantClassOrInterface cit
    Java.ReferenceTypeArray (Java.ArrayType (Java.Dims d) v) -> pure $
      Java.ArrayType (Java.Dims $ d ++ [[]]) v
    _ -> fail $ "don't know how to make Java reference type into array type: " ++ show rt
  _ -> fail $ "don't know how to make Java type into array type: " ++ show t
