-- Note: this is an automatically generated file. Do not edit.

-- | Java utilities for constructing Java syntax trees

module Hydra.Java.Utils where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Java.Environment as Environment
import qualified Hydra.Java.Language as Language
import qualified Hydra.Java.Names as JavaNames
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

addExpressions :: [Syntax.MultiplicativeExpression] -> Syntax.AdditiveExpression
addExpressions exprs =

      let first = Syntax.AdditiveExpressionUnary (Lists.head exprs)
          rest = Lists.tail exprs
      in (Lists.foldl (\ae -> \me -> Syntax.AdditiveExpressionPlus (Syntax.AdditiveExpression_Binary {
        Syntax.additiveExpression_BinaryLhs = ae,
        Syntax.additiveExpression_BinaryRhs = me})) first rest)

addInScopeVar :: Core.Name -> Environment.Aliases -> Environment.Aliases
addInScopeVar name aliases =
    Environment.Aliases {
      Environment.aliasesCurrentNamespace = (Environment.aliasesCurrentNamespace aliases),
      Environment.aliasesPackages = (Environment.aliasesPackages aliases),
      Environment.aliasesBranchVars = (Environment.aliasesBranchVars aliases),
      Environment.aliasesRecursiveVars = (Environment.aliasesRecursiveVars aliases),
      Environment.aliasesInScopeTypeParams = (Environment.aliasesInScopeTypeParams aliases),
      Environment.aliasesPolymorphicLocals = (Environment.aliasesPolymorphicLocals aliases),
      Environment.aliasesInScopeJavaVars = (Sets.insert name (Environment.aliasesInScopeJavaVars aliases)),
      Environment.aliasesVarRenames = (Environment.aliasesVarRenames aliases),
      Environment.aliasesLambdaVars = (Environment.aliasesLambdaVars aliases),
      Environment.aliasesTypeVarSubst = (Environment.aliasesTypeVarSubst aliases),
      Environment.aliasesTrustedTypeVars = (Environment.aliasesTrustedTypeVars aliases),
      Environment.aliasesMethodCodomain = (Environment.aliasesMethodCodomain aliases),
      Environment.aliasesThunkedVars = (Environment.aliasesThunkedVars aliases)}

addInScopeVars :: [Core.Name] -> Environment.Aliases -> Environment.Aliases
addInScopeVars names aliases = Lists.foldl (\a -> \n -> addInScopeVar n a) aliases names

addJavaTypeParameter :: Syntax.ReferenceType -> Syntax.Type -> t0 -> Either Errors.Error Syntax.Type
addJavaTypeParameter rt t cx =
    case t of
      Syntax.TypeReference v0 -> case v0 of
        Syntax.ReferenceTypeClassOrInterface v1 -> case v1 of
          Syntax.ClassOrInterfaceTypeClass v2 ->
            let anns = Syntax.classTypeAnnotations v2
                qual = Syntax.classTypeQualifier v2
                id = Syntax.classTypeIdentifier v2
                args = Syntax.classTypeArguments v2
            in (Right (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
              Syntax.classTypeAnnotations = anns,
              Syntax.classTypeQualifier = qual,
              Syntax.classTypeIdentifier = id,
              Syntax.classTypeArguments = (Lists.concat2 args [
                Syntax.TypeArgumentReference rt])})))))
          Syntax.ClassOrInterfaceTypeInterface _ -> Left (Errors.ErrorOther (Errors.OtherError "expected a Java class type"))
        Syntax.ReferenceTypeVariable v1 -> Right (javaTypeVariableToType v1)
        Syntax.ReferenceTypeArray _ -> Left (Errors.ErrorOther (Errors.OtherError "expected a Java class or interface type, or a variable"))
      Syntax.TypePrimitive _ -> Left (Errors.ErrorOther (Errors.OtherError "expected a reference type"))

addVarRename :: Core.Name -> Core.Name -> Environment.Aliases -> Environment.Aliases
addVarRename original renamed aliases =
    Environment.Aliases {
      Environment.aliasesCurrentNamespace = (Environment.aliasesCurrentNamespace aliases),
      Environment.aliasesPackages = (Environment.aliasesPackages aliases),
      Environment.aliasesBranchVars = (Environment.aliasesBranchVars aliases),
      Environment.aliasesRecursiveVars = (Environment.aliasesRecursiveVars aliases),
      Environment.aliasesInScopeTypeParams = (Environment.aliasesInScopeTypeParams aliases),
      Environment.aliasesPolymorphicLocals = (Environment.aliasesPolymorphicLocals aliases),
      Environment.aliasesInScopeJavaVars = (Environment.aliasesInScopeJavaVars aliases),
      Environment.aliasesVarRenames = (Maps.insert original renamed (Environment.aliasesVarRenames aliases)),
      Environment.aliasesLambdaVars = (Environment.aliasesLambdaVars aliases),
      Environment.aliasesTypeVarSubst = (Environment.aliasesTypeVarSubst aliases),
      Environment.aliasesTrustedTypeVars = (Environment.aliasesTrustedTypeVars aliases),
      Environment.aliasesMethodCodomain = (Environment.aliasesMethodCodomain aliases),
      Environment.aliasesThunkedVars = (Environment.aliasesThunkedVars aliases)}

fieldExpression :: Syntax.Identifier -> Syntax.Identifier -> Syntax.ExpressionName
fieldExpression varId fieldId =
    Syntax.ExpressionName {
      Syntax.expressionNameQualifier = (Just (Syntax.AmbiguousName [
        varId])),
      Syntax.expressionNameIdentifier = fieldId}

fieldNameToJavaExpression :: Core.Name -> Syntax.Expression
fieldNameToJavaExpression fname =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (javaIdentifierToJavaExpressionName (fieldNameToJavaIdentifier fname)))))))))]]]]])))

fieldNameToJavaIdentifier :: Core.Name -> Syntax.Identifier
fieldNameToJavaIdentifier fname = javaIdentifier (Core.unName fname)

fieldNameToJavaVariableDeclarator :: Core.Name -> Syntax.VariableDeclarator
fieldNameToJavaVariableDeclarator fname = javaVariableDeclarator (javaIdentifier (Core.unName fname)) Nothing

fieldNameToJavaVariableDeclaratorId :: Core.Name -> Syntax.VariableDeclaratorId
fieldNameToJavaVariableDeclaratorId fname = javaVariableDeclaratorId (javaIdentifier (Core.unName fname))

finalVarDeclarationStatement :: Syntax.Identifier -> Syntax.Expression -> Syntax.BlockStatement
finalVarDeclarationStatement id rhs =
    Syntax.BlockStatementLocalVariableDeclaration (Syntax.LocalVariableDeclarationStatement (Syntax.LocalVariableDeclaration {
      Syntax.localVariableDeclarationModifiers = [
        Syntax.VariableModifierFinal],
      Syntax.localVariableDeclarationType = Syntax.LocalVariableTypeVar,
      Syntax.localVariableDeclarationDeclarators = [
        javaVariableDeclarator id (Just (Syntax.VariableInitializerExpression rhs))]}))

importAliasesForModule :: Packaging.Module -> Environment.Aliases
importAliasesForModule mod =
    Environment.Aliases {
      Environment.aliasesCurrentNamespace = (Packaging.moduleNamespace mod),
      Environment.aliasesPackages = Maps.empty,
      Environment.aliasesBranchVars = Sets.empty,
      Environment.aliasesRecursiveVars = Sets.empty,
      Environment.aliasesInScopeTypeParams = Sets.empty,
      Environment.aliasesPolymorphicLocals = Sets.empty,
      Environment.aliasesInScopeJavaVars = Sets.empty,
      Environment.aliasesVarRenames = Maps.empty,
      Environment.aliasesLambdaVars = Sets.empty,
      Environment.aliasesTypeVarSubst = Maps.empty,
      Environment.aliasesTrustedTypeVars = Sets.empty,
      Environment.aliasesMethodCodomain = Nothing,
      Environment.aliasesThunkedVars = Sets.empty}

interfaceMethodDeclaration :: [Syntax.InterfaceMethodModifier] -> [Syntax.TypeParameter] -> String -> [Syntax.FormalParameter] -> Syntax.Result -> Maybe [Syntax.BlockStatement] -> Syntax.InterfaceMemberDeclaration
interfaceMethodDeclaration mods tparams methodName params result stmts =
    Syntax.InterfaceMemberDeclarationInterfaceMethod (Syntax.InterfaceMethodDeclaration {
      Syntax.interfaceMethodDeclarationModifiers = mods,
      Syntax.interfaceMethodDeclarationHeader = (javaMethodHeader tparams methodName params result),
      Syntax.interfaceMethodDeclarationBody = (javaMethodBody stmts)})

isEscaped :: String -> Bool
isEscaped s = Equality.equal (Strings.charAt 0 s) 36

javaAdditiveExpressionToJavaExpression :: Syntax.AdditiveExpression -> Syntax.Expression
javaAdditiveExpressionToJavaExpression ae =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary ae))]]]]])))

javaArrayCreation :: Syntax.PrimitiveTypeWithAnnotations -> Maybe Syntax.ArrayInitializer -> Syntax.Expression
javaArrayCreation primType minit =

      let init_ = Maybes.cases minit (Syntax.ArrayInitializer []) (\i -> i)
      in (javaPrimaryToJavaExpression (Syntax.PrimaryArrayCreation (Syntax.ArrayCreationExpressionPrimitiveArray (Syntax.ArrayCreationExpression_PrimitiveArray {
        Syntax.arrayCreationExpression_PrimitiveArrayType = primType,
        Syntax.arrayCreationExpression_PrimitiveArrayDims = [],
        Syntax.arrayCreationExpression_PrimitiveArrayArray = init_}))))

javaArrayInitializer :: [Syntax.Expression] -> Syntax.ArrayInitializer
javaArrayInitializer exprs = Syntax.ArrayInitializer [
  Lists.map (\e -> Syntax.VariableInitializerExpression e) exprs]

javaAssignmentStatement :: Syntax.LeftHandSide -> Syntax.Expression -> Syntax.Statement
javaAssignmentStatement lhs rhs =
    Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementExpression (Syntax.ExpressionStatement (Syntax.StatementExpressionAssignment (Syntax.Assignment {
      Syntax.assignmentLhs = lhs,
      Syntax.assignmentOp = Syntax.AssignmentOperatorSimple,
      Syntax.assignmentExpression = rhs}))))

javaBoolean :: Bool -> Syntax.Literal
javaBoolean b = Syntax.LiteralBoolean b

javaBooleanExpression :: Bool -> Syntax.Expression
javaBooleanExpression b = javaPrimaryToJavaExpression (javaLiteralToJavaPrimary (javaBoolean b))

javaBooleanType :: Syntax.Type
javaBooleanType = javaPrimitiveTypeToJavaType Syntax.PrimitiveTypeBoolean

javaBytePrimitiveType :: Syntax.PrimitiveTypeWithAnnotations
javaBytePrimitiveType =
    Syntax.PrimitiveTypeWithAnnotations {
      Syntax.primitiveTypeWithAnnotationsType = (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)),
      Syntax.primitiveTypeWithAnnotationsAnnotations = []}

javaCastExpression :: Syntax.ReferenceType -> Syntax.UnaryExpression -> Syntax.CastExpression
javaCastExpression rt expr =
    Syntax.CastExpressionNotPlusMinus (Syntax.CastExpression_NotPlusMinus {
      Syntax.castExpression_NotPlusMinusRefAndBounds = Syntax.CastExpression_RefAndBounds {
        Syntax.castExpression_RefAndBoundsType = rt,
        Syntax.castExpression_RefAndBoundsBounds = []},
      Syntax.castExpression_NotPlusMinusExpression = expr})

javaCastExpressionToJavaExpression :: Syntax.CastExpression -> Syntax.Expression
javaCastExpressionToJavaExpression ce =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusCast ce))))))]]]]])))

javaCastPrimitive :: Syntax.PrimitiveType -> Syntax.UnaryExpression -> Syntax.CastExpression
javaCastPrimitive pt expr =
    Syntax.CastExpressionPrimitive (Syntax.CastExpression_Primitive {
      Syntax.castExpression_PrimitiveType = Syntax.PrimitiveTypeWithAnnotations {
        Syntax.primitiveTypeWithAnnotationsType = pt,
        Syntax.primitiveTypeWithAnnotationsAnnotations = []},
      Syntax.castExpression_PrimitiveExpression = expr})

javaClassDeclaration :: Environment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Syntax.ClassModifier] -> Maybe Core.Name -> [Syntax.InterfaceType] -> [Syntax.ClassBodyDeclarationWithComments] -> Syntax.ClassDeclaration
javaClassDeclaration aliases tparams elName mods supname impls bodyDecls =

      let extends_ = Maybes.map (\n -> nameToJavaClassType aliases True [] n Nothing) supname
      in (Syntax.ClassDeclarationNormal (Syntax.NormalClassDeclaration {
        Syntax.normalClassDeclarationModifiers = mods,
        Syntax.normalClassDeclarationIdentifier = (javaDeclName elName),
        Syntax.normalClassDeclarationParameters = tparams,
        Syntax.normalClassDeclarationExtends = extends_,
        Syntax.normalClassDeclarationImplements = impls,
        Syntax.normalClassDeclarationBody = (Syntax.ClassBody bodyDecls)}))

javaClassType :: [Syntax.ReferenceType] -> Maybe Syntax.PackageName -> String -> Syntax.ClassType
javaClassType args pkg id =

      let qual = Maybes.cases pkg Syntax.ClassTypeQualifierNone (\p -> Syntax.ClassTypeQualifierPackage p)
          targs = Lists.map (\rt -> Syntax.TypeArgumentReference rt) args
      in Syntax.ClassType {
        Syntax.classTypeAnnotations = [],
        Syntax.classTypeQualifier = qual,
        Syntax.classTypeIdentifier = (javaTypeIdentifier id),
        Syntax.classTypeArguments = targs}

javaClassTypeToJavaType :: Syntax.ClassType -> Syntax.Type
javaClassTypeToJavaType ct =
    Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass ct))

javaConditionalAndExpressionToJavaExpression :: Syntax.ConditionalAndExpression -> Syntax.Expression
javaConditionalAndExpressionToJavaExpression cae =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      cae])))

javaConstructorCall :: Syntax.ClassOrInterfaceTypeToInstantiate -> [Syntax.Expression] -> Maybe Syntax.ClassBody -> Syntax.Expression
javaConstructorCall ci args mbody =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionClassInstance (Syntax.ClassInstanceCreationExpression {
                Syntax.classInstanceCreationExpressionQualifier = Nothing,
                Syntax.classInstanceCreationExpressionExpression = Syntax.UnqualifiedClassInstanceCreationExpression {
                  Syntax.unqualifiedClassInstanceCreationExpressionTypeArguments = [],
                  Syntax.unqualifiedClassInstanceCreationExpressionClassOrInterface = ci,
                  Syntax.unqualifiedClassInstanceCreationExpressionArguments = args,
                  Syntax.unqualifiedClassInstanceCreationExpressionBody = mbody}}))))))))))]]]]])))

javaConstructorName :: Syntax.Identifier -> Maybe Syntax.TypeArgumentsOrDiamond -> Syntax.ClassOrInterfaceTypeToInstantiate
javaConstructorName id targs =
    Syntax.ClassOrInterfaceTypeToInstantiate {
      Syntax.classOrInterfaceTypeToInstantiateIdentifiers = [
        Syntax.AnnotatedIdentifier {
          Syntax.annotatedIdentifierAnnotations = [],
          Syntax.annotatedIdentifierIdentifier = id}],
      Syntax.classOrInterfaceTypeToInstantiateTypeArguments = targs}

javaDeclName :: Core.Name -> Syntax.TypeIdentifier
javaDeclName name = Syntax.TypeIdentifier (javaVariableName name)

javaDoubleCastExpression :: Syntax.ReferenceType -> Syntax.ReferenceType -> Syntax.UnaryExpression -> Syntax.CastExpression
javaDoubleCastExpression rawRt targetRt expr =

      let firstCast = javaCastExpressionToJavaExpression (javaCastExpression rawRt expr)
      in (javaCastExpression targetRt (javaExpressionToJavaUnaryExpression firstCast))

javaDoubleCastExpressionToJavaExpression :: Syntax.ReferenceType -> Syntax.ReferenceType -> Syntax.UnaryExpression -> Syntax.Expression
javaDoubleCastExpressionToJavaExpression rawRt targetRt expr =
    javaCastExpressionToJavaExpression (javaDoubleCastExpression rawRt targetRt expr)

javaEmptyStatement :: Syntax.Statement
javaEmptyStatement = Syntax.StatementWithoutTrailing Syntax.StatementWithoutTrailingSubstatementEmpty

javaEqualityExpressionToJavaExpression :: Syntax.EqualityExpression -> Syntax.Expression
javaEqualityExpressionToJavaExpression ee =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              ee]]]]])))

javaEqualityExpressionToJavaInclusiveOrExpression :: Syntax.EqualityExpression -> Syntax.InclusiveOrExpression
javaEqualityExpressionToJavaInclusiveOrExpression ee =
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          ee]]]

javaEquals :: Syntax.EqualityExpression -> Syntax.RelationalExpression -> Syntax.EqualityExpression
javaEquals lhs rhs =
    Syntax.EqualityExpressionEqual (Syntax.EqualityExpression_Binary {
      Syntax.equalityExpression_BinaryLhs = lhs,
      Syntax.equalityExpression_BinaryRhs = rhs})

javaEqualsNull :: Syntax.EqualityExpression -> Syntax.EqualityExpression
javaEqualsNull lhs = javaEquals lhs (javaLiteralToJavaRelationalExpression Syntax.LiteralNull)

javaExpressionNameToJavaExpression :: Syntax.ExpressionName -> Syntax.Expression
javaExpressionNameToJavaExpression en =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName en)))))))]]]]])))

-- | Convert an Expression to a Primary, avoiding unnecessary parentheses when the expression is already a simple primary chain
javaExpressionToJavaPrimary :: Syntax.Expression -> Syntax.Primary
javaExpressionToJavaPrimary e =

      let fallback = Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionParens e)
      in case e of
        Syntax.ExpressionAssignment v0 -> case v0 of
          Syntax.AssignmentExpressionConditional v1 -> case v1 of
            Syntax.ConditionalExpressionSimple v2 ->
              let cands = Syntax.unConditionalOrExpression v2
              in (Logic.ifElse (Equality.equal (Lists.length cands) 1) (
                let iors = Syntax.unConditionalAndExpression (Lists.head cands)
                in (Logic.ifElse (Equality.equal (Lists.length iors) 1) (
                  let xors = Syntax.unInclusiveOrExpression (Lists.head iors)
                  in (Logic.ifElse (Equality.equal (Lists.length xors) 1) (
                    let ands = Syntax.unExclusiveOrExpression (Lists.head xors)
                    in (Logic.ifElse (Equality.equal (Lists.length ands) 1) (
                      let eqs = Syntax.unAndExpression (Lists.head ands)
                      in (Logic.ifElse (Equality.equal (Lists.length eqs) 1) (case (Lists.head eqs) of
                        Syntax.EqualityExpressionUnary v3 -> case v3 of
                          Syntax.RelationalExpressionSimple v4 -> case v4 of
                            Syntax.ShiftExpressionUnary v5 -> case v5 of
                              Syntax.AdditiveExpressionUnary v6 -> case v6 of
                                Syntax.MultiplicativeExpressionUnary v7 -> case v7 of
                                  Syntax.UnaryExpressionOther v8 -> case v8 of
                                    Syntax.UnaryExpressionNotPlusMinusPostfix v9 -> case v9 of
                                      Syntax.PostfixExpressionPrimary v10 -> v10
                                      _ -> fallback
                                    _ -> fallback
                                  _ -> fallback
                                _ -> fallback
                              _ -> fallback
                            _ -> fallback
                          _ -> fallback
                        _ -> fallback) fallback)) fallback)) fallback)) fallback)) fallback)
            _ -> fallback
          _ -> fallback
        _ -> fallback

javaExpressionToJavaUnaryExpression :: Syntax.Expression -> Syntax.UnaryExpression
javaExpressionToJavaUnaryExpression e =
    Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionParens e))))

javaFieldAccessToJavaExpression :: Syntax.FieldAccess -> Syntax.Expression
javaFieldAccessToJavaExpression fa =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionFieldAccess fa)))))))))]]]]])))

javaIdentifier :: String -> Syntax.Identifier
javaIdentifier s = Syntax.Identifier (sanitizeJavaName s)

javaIdentifierToJavaExpression :: Syntax.Identifier -> Syntax.Expression
javaIdentifierToJavaExpression id =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (Syntax.ExpressionName {
                Syntax.expressionNameQualifier = Nothing,
                Syntax.expressionNameIdentifier = id}))))))))]]]]])))

javaIdentifierToJavaExpressionName :: Syntax.Identifier -> Syntax.ExpressionName
javaIdentifierToJavaExpressionName id =
    Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = id}

javaIdentifierToJavaRelationalExpression :: Syntax.Identifier -> Syntax.RelationalExpression
javaIdentifierToJavaRelationalExpression id =
    Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = id})))))))

javaIdentifierToJavaUnaryExpression :: Syntax.Identifier -> Syntax.UnaryExpression
javaIdentifierToJavaUnaryExpression id =
    Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionName (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = id})))

javaInstanceOf :: Syntax.RelationalExpression -> Syntax.ReferenceType -> Syntax.RelationalExpression
javaInstanceOf lhs rhs =
    Syntax.RelationalExpressionInstanceof (Syntax.RelationalExpression_InstanceOf {
      Syntax.relationalExpression_InstanceOfLhs = lhs,
      Syntax.relationalExpression_InstanceOfRhs = rhs})

javaInt :: Integer -> Syntax.Literal
javaInt i = Syntax.LiteralInteger (Syntax.IntegerLiteral i)

javaIntExpression :: Integer -> Syntax.Expression
javaIntExpression i = javaPrimaryToJavaExpression (javaLiteralToJavaPrimary (javaInt i))

javaIntType :: Syntax.Type
javaIntType = javaPrimitiveTypeToJavaType (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeInt))

javaInterfaceDeclarationToJavaClassBodyDeclaration :: Syntax.NormalInterfaceDeclaration -> Syntax.ClassBodyDeclaration
javaInterfaceDeclarationToJavaClassBodyDeclaration nid =
    Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationInterface (Syntax.InterfaceDeclarationNormalInterface nid))

javaLambda :: Core.Name -> Syntax.Expression -> Syntax.Expression
javaLambda v body =
    Syntax.ExpressionLambda (Syntax.LambdaExpression {
      Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersSingle (variableToJavaIdentifier v)),
      Syntax.lambdaExpressionBody = (Syntax.LambdaBodyExpression body)})

javaLambdaFromBlock :: Core.Name -> Syntax.Block -> Syntax.Expression
javaLambdaFromBlock v block =
    Syntax.ExpressionLambda (Syntax.LambdaExpression {
      Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersSingle (variableToJavaIdentifier v)),
      Syntax.lambdaExpressionBody = (Syntax.LambdaBodyBlock block)})

javaLiteralToJavaExpression :: Syntax.Literal -> Syntax.Expression
javaLiteralToJavaExpression lit =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionLiteral lit)))))))))]]]]])))

javaLiteralToJavaMultiplicativeExpression :: Syntax.Literal -> Syntax.MultiplicativeExpression
javaLiteralToJavaMultiplicativeExpression lit =
    Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionLiteral lit)))))

javaLiteralToJavaPrimary :: Syntax.Literal -> Syntax.Primary
javaLiteralToJavaPrimary lit = Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionLiteral lit)

javaLiteralToJavaRelationalExpression :: Syntax.Literal -> Syntax.RelationalExpression
javaLiteralToJavaRelationalExpression lit =
    Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionLiteral lit))))))))

javaMemberField :: [Syntax.FieldModifier] -> Syntax.Type -> Syntax.VariableDeclarator -> Syntax.ClassBodyDeclaration
javaMemberField mods jt v =
    Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationField (Syntax.FieldDeclaration {
      Syntax.fieldDeclarationModifiers = mods,
      Syntax.fieldDeclarationUnannType = (Syntax.UnannType jt),
      Syntax.fieldDeclarationVariableDeclarators = [
        v]}))

javaMethodBody :: Maybe [Syntax.BlockStatement] -> Syntax.MethodBody
javaMethodBody mstmts = Maybes.cases mstmts Syntax.MethodBodyNone (\stmts -> Syntax.MethodBodyBlock (Syntax.Block stmts))

javaMethodDeclarationToJavaClassBodyDeclaration :: Syntax.MethodDeclaration -> Syntax.ClassBodyDeclaration
javaMethodDeclarationToJavaClassBodyDeclaration md =
    Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationMethod md)

javaMethodHeader :: [Syntax.TypeParameter] -> String -> [Syntax.FormalParameter] -> Syntax.Result -> Syntax.MethodHeader
javaMethodHeader tparams methodName params result =
    Syntax.MethodHeader {
      Syntax.methodHeaderParameters = tparams,
      Syntax.methodHeaderResult = result,
      Syntax.methodHeaderDeclarator = Syntax.MethodDeclarator {
        Syntax.methodDeclaratorIdentifier = (Syntax.Identifier methodName),
        Syntax.methodDeclaratorReceiverParameter = Nothing,
        Syntax.methodDeclaratorFormalParameters = params},
      Syntax.methodHeaderThrows = Nothing}

javaMethodInvocationToJavaExpression :: Syntax.MethodInvocation -> Syntax.Expression
javaMethodInvocationToJavaExpression mi =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionMethodInvocation mi)))))))))]]]]])))

javaMethodInvocationToJavaPostfixExpression :: Syntax.MethodInvocation -> Syntax.PostfixExpression
javaMethodInvocationToJavaPostfixExpression mi =
    Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionMethodInvocation mi))

javaMethodInvocationToJavaPrimary :: Syntax.MethodInvocation -> Syntax.Primary
javaMethodInvocationToJavaPrimary mi = Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionMethodInvocation mi)

javaMethodInvocationToJavaStatement :: Syntax.MethodInvocation -> Syntax.Statement
javaMethodInvocationToJavaStatement mi =
    Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementExpression (Syntax.ExpressionStatement (Syntax.StatementExpressionMethodInvocation mi)))

javaMultiplicativeExpressionToJavaRelationalExpression :: Syntax.MultiplicativeExpression -> Syntax.RelationalExpression
javaMultiplicativeExpressionToJavaRelationalExpression me =
    Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary me))

javaPackageDeclaration :: Packaging.Namespace -> Syntax.PackageDeclaration
javaPackageDeclaration ns =
    Syntax.PackageDeclaration {
      Syntax.packageDeclarationModifiers = [],
      Syntax.packageDeclarationIdentifiers = (Lists.map (\s -> Syntax.Identifier s) (Strings.splitOn "." (Packaging.unNamespace ns)))}

javaPostfixExpressionToJavaEqualityExpression :: Syntax.PostfixExpression -> Syntax.EqualityExpression
javaPostfixExpressionToJavaEqualityExpression pe =
    Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))))))

javaPostfixExpressionToJavaExpression :: Syntax.PostfixExpression -> Syntax.Expression
javaPostfixExpressionToJavaExpression pe =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))))))]]]]])))

javaPostfixExpressionToJavaInclusiveOrExpression :: Syntax.PostfixExpression -> Syntax.InclusiveOrExpression
javaPostfixExpressionToJavaInclusiveOrExpression pe =
    Syntax.InclusiveOrExpression [
      Syntax.ExclusiveOrExpression [
        Syntax.AndExpression [
          Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe))))))]]]

javaPostfixExpressionToJavaRelationalExpression :: Syntax.PostfixExpression -> Syntax.RelationalExpression
javaPostfixExpressionToJavaRelationalExpression pe =
    Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe)))))

javaPostfixExpressionToJavaUnaryExpression :: Syntax.PostfixExpression -> Syntax.UnaryExpression
javaPostfixExpressionToJavaUnaryExpression pe = Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix pe)

javaPrimaryToJavaExpression :: Syntax.Primary -> Syntax.Expression
javaPrimaryToJavaExpression p =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary p)))))))]]]]])))

javaPrimaryToJavaUnaryExpression :: Syntax.Primary -> Syntax.UnaryExpression
javaPrimaryToJavaUnaryExpression p =
    Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary p))

javaPrimitiveTypeToJavaType :: Syntax.PrimitiveType -> Syntax.Type
javaPrimitiveTypeToJavaType pt =
    Syntax.TypePrimitive (Syntax.PrimitiveTypeWithAnnotations {
      Syntax.primitiveTypeWithAnnotationsType = pt,
      Syntax.primitiveTypeWithAnnotationsAnnotations = []})

javaRefType :: [Syntax.ReferenceType] -> Maybe Syntax.PackageName -> String -> Syntax.Type
javaRefType args pkg id =
    Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (javaClassType args pkg id)))

javaReferenceTypeToRawType :: Syntax.ReferenceType -> Syntax.ReferenceType
javaReferenceTypeToRawType rt =
    case rt of
      Syntax.ReferenceTypeClassOrInterface v0 -> case v0 of
        Syntax.ClassOrInterfaceTypeClass v1 ->
          let anns = Syntax.classTypeAnnotations v1
              qual = Syntax.classTypeQualifier v1
              id = Syntax.classTypeIdentifier v1
          in (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
            Syntax.classTypeAnnotations = anns,
            Syntax.classTypeQualifier = qual,
            Syntax.classTypeIdentifier = id,
            Syntax.classTypeArguments = []})))
        Syntax.ClassOrInterfaceTypeInterface v1 ->
          let ct = Syntax.unInterfaceType v1
              anns = Syntax.classTypeAnnotations ct
              qual = Syntax.classTypeQualifier ct
              id = Syntax.classTypeIdentifier ct
          in (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeInterface (Syntax.InterfaceType (Syntax.ClassType {
            Syntax.classTypeAnnotations = anns,
            Syntax.classTypeQualifier = qual,
            Syntax.classTypeIdentifier = id,
            Syntax.classTypeArguments = []}))))
      _ -> rt

javaRelationalExpressionToJavaEqualityExpression :: Syntax.RelationalExpression -> Syntax.EqualityExpression
javaRelationalExpressionToJavaEqualityExpression re = Syntax.EqualityExpressionUnary re

javaRelationalExpressionToJavaExpression :: Syntax.RelationalExpression -> Syntax.Expression
javaRelationalExpressionToJavaExpression re = javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionUnary re)

javaRelationalExpressionToJavaUnaryExpression :: Syntax.RelationalExpression -> Syntax.UnaryExpression
javaRelationalExpressionToJavaUnaryExpression re =
    Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray (Syntax.PrimaryNoNewArrayExpressionParens (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary re]]]]]))))))))

javaReturnStatement :: Maybe Syntax.Expression -> Syntax.Statement
javaReturnStatement mex =
    Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementReturn (Syntax.ReturnStatement mex))

javaStatementsToBlock :: [Syntax.Statement] -> Syntax.Block
javaStatementsToBlock stmts = Syntax.Block (Lists.map (\s -> Syntax.BlockStatementStatement s) stmts)

javaString :: String -> Syntax.Literal
javaString s = Syntax.LiteralString (Syntax.StringLiteral s)

javaStringMultiplicativeExpression :: String -> Syntax.MultiplicativeExpression
javaStringMultiplicativeExpression s = javaLiteralToJavaMultiplicativeExpression (javaString s)

javaThis :: Syntax.Expression
javaThis =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryNoNewArray Syntax.PrimaryNoNewArrayExpressionThis))))))))]]]]])))

javaThrowIllegalArgumentException :: [Syntax.Expression] -> Syntax.Statement
javaThrowIllegalArgumentException args =
    javaThrowStatement (javaConstructorCall (javaConstructorName (Syntax.Identifier "IllegalArgumentException") Nothing) args Nothing)

javaThrowIllegalStateException :: [Syntax.Expression] -> Syntax.Statement
javaThrowIllegalStateException args =
    javaThrowStatement (javaConstructorCall (javaConstructorName (Syntax.Identifier "IllegalStateException") Nothing) args Nothing)

javaThrowStatement :: Syntax.Expression -> Syntax.Statement
javaThrowStatement e =
    Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementThrow (Syntax.ThrowStatement e))

javaTypeFromTypeName :: Environment.Aliases -> Core.Name -> Syntax.Type
javaTypeFromTypeName aliases elName =
    javaTypeVariableToType (Syntax.TypeVariable {
      Syntax.typeVariableAnnotations = [],
      Syntax.typeVariableIdentifier = (nameToJavaTypeIdentifier aliases False elName)})

javaTypeIdentifier :: String -> Syntax.TypeIdentifier
javaTypeIdentifier s = Syntax.TypeIdentifier (Syntax.Identifier s)

javaTypeIdentifierToJavaTypeArgument :: Syntax.TypeIdentifier -> Syntax.TypeArgument
javaTypeIdentifierToJavaTypeArgument id =
    Syntax.TypeArgumentReference (Syntax.ReferenceTypeVariable (Syntax.TypeVariable {
      Syntax.typeVariableAnnotations = [],
      Syntax.typeVariableIdentifier = id}))

javaTypeName :: Syntax.Identifier -> Syntax.TypeName
javaTypeName id =
    Syntax.TypeName {
      Syntax.typeNameIdentifier = (Syntax.TypeIdentifier id),
      Syntax.typeNameQualifier = Nothing}

javaTypeParameter :: String -> Syntax.TypeParameter
javaTypeParameter v =
    Syntax.TypeParameter {
      Syntax.typeParameterModifiers = [],
      Syntax.typeParameterIdentifier = (javaTypeIdentifier v),
      Syntax.typeParameterBound = Nothing}

javaTypeToJavaFormalParameter :: Syntax.Type -> Core.Name -> Syntax.FormalParameter
javaTypeToJavaFormalParameter jt fname =
    Syntax.FormalParameterSimple (Syntax.FormalParameter_Simple {
      Syntax.formalParameter_SimpleModifiers = [],
      Syntax.formalParameter_SimpleType = (Syntax.UnannType jt),
      Syntax.formalParameter_SimpleId = (fieldNameToJavaVariableDeclaratorId fname)})

javaTypeToJavaReferenceType :: Syntax.Type -> t0 -> Either Errors.Error Syntax.ReferenceType
javaTypeToJavaReferenceType t cx =
    case t of
      Syntax.TypeReference v0 -> Right v0
      Syntax.TypePrimitive _ -> Left (Errors.ErrorOther (Errors.OtherError "expected a Java reference type"))

javaTypeToJavaResult :: Syntax.Type -> Syntax.Result
javaTypeToJavaResult jt = Syntax.ResultType (Syntax.UnannType jt)

javaTypeToJavaTypeArgument :: Syntax.Type -> Syntax.TypeArgument
javaTypeToJavaTypeArgument t =
    case t of
      Syntax.TypeReference v0 -> Syntax.TypeArgumentReference v0
      Syntax.TypePrimitive _ -> Syntax.TypeArgumentWildcard (Syntax.Wildcard {
        Syntax.wildcardAnnotations = [],
        Syntax.wildcardWildcard = Nothing})

javaTypeVariable :: String -> Syntax.ReferenceType
javaTypeVariable v =
    Syntax.ReferenceTypeVariable (Syntax.TypeVariable {
      Syntax.typeVariableAnnotations = [],
      Syntax.typeVariableIdentifier = (javaTypeIdentifier (Formatting.capitalize v))})

javaTypeVariableToType :: Syntax.TypeVariable -> Syntax.Type
javaTypeVariableToType tv = Syntax.TypeReference (Syntax.ReferenceTypeVariable tv)

javaUnaryExpressionToJavaExpression :: Syntax.UnaryExpression -> Syntax.Expression
javaUnaryExpressionToJavaExpression ue =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
      Syntax.ConditionalAndExpression [
        Syntax.InclusiveOrExpression [
          Syntax.ExclusiveOrExpression [
            Syntax.AndExpression [
              Syntax.EqualityExpressionUnary (Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary ue))))]]]]])))

javaUnaryExpressionToJavaRelationalExpression :: Syntax.UnaryExpression -> Syntax.RelationalExpression
javaUnaryExpressionToJavaRelationalExpression ue =
    Syntax.RelationalExpressionSimple (Syntax.ShiftExpressionUnary (Syntax.AdditiveExpressionUnary (Syntax.MultiplicativeExpressionUnary ue)))

javaVariableDeclarator :: Syntax.Identifier -> Maybe Syntax.VariableInitializer -> Syntax.VariableDeclarator
javaVariableDeclarator id minit =
    Syntax.VariableDeclarator {
      Syntax.variableDeclaratorId = (javaVariableDeclaratorId id),
      Syntax.variableDeclaratorInitializer = minit}

javaVariableDeclaratorId :: Syntax.Identifier -> Syntax.VariableDeclaratorId
javaVariableDeclaratorId id =
    Syntax.VariableDeclaratorId {
      Syntax.variableDeclaratorIdIdentifier = id,
      Syntax.variableDeclaratorIdDims = Nothing}

javaVariableName :: Core.Name -> Syntax.Identifier
javaVariableName name = javaIdentifier (Names.localNameOf name)

lookupJavaVarName :: Environment.Aliases -> Core.Name -> Core.Name
lookupJavaVarName aliases name =
    Maybes.cases (Maps.lookup name (Environment.aliasesVarRenames aliases)) name (\renamed -> renamed)

makeConstructor :: Environment.Aliases -> Core.Name -> Bool -> [Syntax.FormalParameter] -> [Syntax.BlockStatement] -> Syntax.ClassBodyDeclaration
makeConstructor aliases elName private params stmts =

      let nm = Syntax.SimpleTypeName (nameToJavaTypeIdentifier aliases False elName)
          cons =
                  Syntax.ConstructorDeclarator {
                    Syntax.constructorDeclaratorParameters = [],
                    Syntax.constructorDeclaratorName = nm,
                    Syntax.constructorDeclaratorReceiverParameter = Nothing,
                    Syntax.constructorDeclaratorFormalParameters = params}
          mods = [
                Logic.ifElse private Syntax.ConstructorModifierPrivate Syntax.ConstructorModifierPublic]
          body =
                  Syntax.ConstructorBody {
                    Syntax.constructorBodyInvocation = Nothing,
                    Syntax.constructorBodyStatements = stmts}
      in (Syntax.ClassBodyDeclarationConstructorDeclaration (Syntax.ConstructorDeclaration {
        Syntax.constructorDeclarationModifiers = mods,
        Syntax.constructorDeclarationConstructor = cons,
        Syntax.constructorDeclarationThrows = Nothing,
        Syntax.constructorDeclarationBody = body}))

methodDeclaration :: [Syntax.MethodModifier] -> [Syntax.TypeParameter] -> [Syntax.Annotation] -> String -> [Syntax.FormalParameter] -> Syntax.Result -> Maybe [Syntax.BlockStatement] -> Syntax.ClassBodyDeclaration
methodDeclaration mods tparams anns methodName params result stmts =
    javaMethodDeclarationToJavaClassBodyDeclaration (Syntax.MethodDeclaration {
      Syntax.methodDeclarationAnnotations = anns,
      Syntax.methodDeclarationModifiers = mods,
      Syntax.methodDeclarationHeader = (javaMethodHeader tparams methodName params result),
      Syntax.methodDeclarationBody = (javaMethodBody stmts)})

methodInvocation :: Maybe (Either Syntax.ExpressionName Syntax.Primary) -> Syntax.Identifier -> [Syntax.Expression] -> Syntax.MethodInvocation
methodInvocation lhs methodName args =

      let header =
              Maybes.cases lhs (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName methodName)) (\either -> Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Eithers.either (\en -> Syntax.MethodInvocation_VariantExpression en) (\p -> Syntax.MethodInvocation_VariantPrimary p) either),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = methodName}))
      in Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = args}

methodInvocationStatic :: Syntax.Identifier -> Syntax.Identifier -> [Syntax.Expression] -> Syntax.MethodInvocation
methodInvocationStatic self methodName args =
    methodInvocation (Just (Left (javaIdentifierToJavaExpressionName self))) methodName args

methodInvocationStaticWithTypeArgs :: Syntax.Identifier -> Syntax.Identifier -> [Syntax.TypeArgument] -> [Syntax.Expression] -> Syntax.MethodInvocation
methodInvocationStaticWithTypeArgs self methodName targs args =

      let header =
              Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (javaIdentifierToJavaExpressionName self)),
                Syntax.methodInvocation_ComplexTypeArguments = targs,
                Syntax.methodInvocation_ComplexIdentifier = methodName})
      in Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = args}

nameToJavaClassType :: Environment.Aliases -> Bool -> [Syntax.TypeArgument] -> Core.Name -> Maybe String -> Syntax.ClassType
nameToJavaClassType aliases qualify args name mlocal =

      let result = nameToQualifiedJavaName aliases qualify name mlocal
          id = Pairs.first result
          pkg = Pairs.second result
      in Syntax.ClassType {
        Syntax.classTypeAnnotations = [],
        Syntax.classTypeQualifier = pkg,
        Syntax.classTypeIdentifier = id,
        Syntax.classTypeArguments = args}

nameToJavaName :: Environment.Aliases -> Core.Name -> Syntax.Identifier
nameToJavaName aliases name =

      let qn = Names.qualifyName name
          ns_ = Packaging.qualifiedNameNamespace qn
          local = Packaging.qualifiedNameLocal qn
      in (Logic.ifElse (isEscaped (Core.unName name)) (Syntax.Identifier (sanitizeJavaName local)) (Maybes.cases ns_ (Syntax.Identifier local) (\gname ->
        let parts =
                Maybes.cases (Maps.lookup gname (Environment.aliasesPackages aliases)) (Strings.splitOn "." (Packaging.unNamespace gname)) (\pkgName -> Lists.map (\i -> Syntax.unIdentifier i) (Syntax.unPackageName pkgName))
            allParts = Lists.concat2 parts [
                  sanitizeJavaName local]
        in (Syntax.Identifier (Strings.intercalate "." allParts)))))

nameToJavaReferenceType :: Environment.Aliases -> Bool -> [Syntax.TypeArgument] -> Core.Name -> Maybe String -> Syntax.ReferenceType
nameToJavaReferenceType aliases qualify args name mlocal =
    Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (nameToJavaClassType aliases qualify args name mlocal))

nameToJavaTypeIdentifier :: Environment.Aliases -> Bool -> Core.Name -> Syntax.TypeIdentifier
nameToJavaTypeIdentifier aliases qualify name = Pairs.first (nameToQualifiedJavaName aliases qualify name Nothing)

nameToQualifiedJavaName :: Environment.Aliases -> Bool -> Core.Name -> Maybe String -> (Syntax.TypeIdentifier, Syntax.ClassTypeQualifier)
nameToQualifiedJavaName aliases qualify name mlocal =

      let qn = Names.qualifyName name
          ns_ = Packaging.qualifiedNameNamespace qn
          local = Packaging.qualifiedNameLocal qn
          alias =
                  Maybes.cases ns_ Nothing (\n -> Just (Maybes.cases (Maps.lookup n (Environment.aliasesPackages aliases)) (JavaNames.javaPackageName (Strings.splitOn "." (Packaging.unNamespace n))) (\id -> id)))
          pkg =
                  Logic.ifElse qualify (Maybes.cases alias Syntax.ClassTypeQualifierNone (\p -> Syntax.ClassTypeQualifierPackage p)) Syntax.ClassTypeQualifierNone
          jid =
                  javaTypeIdentifier (Maybes.cases mlocal (sanitizeJavaName local) (\l -> Strings.cat2 (Strings.cat2 (sanitizeJavaName local) ".") (sanitizeJavaName l)))
      in (jid, pkg)

overrideAnnotation :: Syntax.Annotation
overrideAnnotation = Syntax.AnnotationMarker (Syntax.MarkerAnnotation (javaTypeName (Syntax.Identifier "Override")))

referenceTypeToResult :: Syntax.ReferenceType -> Syntax.Result
referenceTypeToResult rt = javaTypeToJavaResult (Syntax.TypeReference rt)

sanitizeJavaName :: String -> String
sanitizeJavaName name =
    Logic.ifElse (isEscaped name) (unescape name) (Logic.ifElse (Equality.equal name "_") "ignored" (Formatting.sanitizeWithUnderscores Language.reservedWords name))

suppressWarningsUncheckedAnnotation :: Syntax.Annotation
suppressWarningsUncheckedAnnotation =
    Syntax.AnnotationSingleElement (Syntax.SingleElementAnnotation {
      Syntax.singleElementAnnotationName = (javaTypeName (Syntax.Identifier "SuppressWarnings")),
      Syntax.singleElementAnnotationValue = (Just (Syntax.ElementValueConditionalExpression (Syntax.ConditionalExpressionSimple (Syntax.ConditionalOrExpression [
        Syntax.ConditionalAndExpression [
          javaPostfixExpressionToJavaInclusiveOrExpression (Syntax.PostfixExpressionPrimary (javaLiteralToJavaPrimary (javaString "unchecked")))]]))))})

toAcceptMethod :: Bool -> [Syntax.TypeParameter] -> Syntax.ClassBodyDeclaration
toAcceptMethod abstract vtparams =

      let mods =
              Logic.ifElse abstract [
                Syntax.MethodModifierPublic,
                Syntax.MethodModifierAbstract] [
                Syntax.MethodModifierPublic]
          tparams = [
                javaTypeParameter JavaNames.visitorReturnParameter]
          anns = Logic.ifElse abstract [] [
                overrideAnnotation]
          typeArgs = Lists.map (\tp -> Syntax.TypeArgumentReference (typeParameterToReferenceType tp)) vtparams
          ref =
                  javaClassTypeToJavaType (Syntax.ClassType {
                    Syntax.classTypeAnnotations = [],
                    Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
                    Syntax.classTypeIdentifier = (javaTypeIdentifier JavaNames.visitorName),
                    Syntax.classTypeArguments = (Lists.concat2 typeArgs [
                      Syntax.TypeArgumentReference visitorTypeVariable])})
          param = javaTypeToJavaFormalParameter ref (Core.Name "visitor")
          result = javaTypeToJavaResult (Syntax.TypeReference visitorTypeVariable)
          returnExpr =
                  javaMethodInvocationToJavaExpression (methodInvocationStatic (Syntax.Identifier "visitor") (Syntax.Identifier JavaNames.visitMethodName) [
                    javaThis])
          body = Logic.ifElse abstract Nothing (Just [
                Syntax.BlockStatementStatement (javaReturnStatement (Just returnExpr))])
      in (methodDeclaration mods tparams anns JavaNames.acceptMethodName [
        param] result body)

toAssignStmt :: Core.Name -> Syntax.Statement
toAssignStmt fname =

      let id = fieldNameToJavaIdentifier fname
          lhs =
                  Syntax.LeftHandSideFieldAccess (Syntax.FieldAccess {
                    Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Syntax.PrimaryNoNewArray Syntax.PrimaryNoNewArrayExpressionThis)),
                    Syntax.fieldAccessIdentifier = id})
          rhs = fieldNameToJavaExpression fname
      in (javaAssignmentStatement lhs rhs)

toJavaArrayType :: Syntax.Type -> t0 -> Either Errors.Error Syntax.Type
toJavaArrayType t cx =
    case t of
      Syntax.TypeReference v0 -> case v0 of
        Syntax.ReferenceTypeClassOrInterface v1 -> Right (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
          Syntax.arrayTypeDims = (Syntax.Dims [
            []]),
          Syntax.arrayTypeVariant = (Syntax.ArrayType_VariantClassOrInterface v1)})))
        Syntax.ReferenceTypeArray v1 ->
          let oldDims = Syntax.unDims (Syntax.arrayTypeDims v1)
              newDims = Syntax.Dims (Lists.concat2 oldDims [
                    []])
              variant = Syntax.arrayTypeVariant v1
          in (Right (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
            Syntax.arrayTypeDims = newDims,
            Syntax.arrayTypeVariant = variant}))))
        Syntax.ReferenceTypeVariable _ -> Left (Errors.ErrorOther (Errors.OtherError "don't know how to make Java reference type into array type"))
      Syntax.TypePrimitive _ -> Left (Errors.ErrorOther (Errors.OtherError "don't know how to make Java type into array type"))

typeParameterToReferenceType :: Syntax.TypeParameter -> Syntax.ReferenceType
typeParameterToReferenceType tp =
    javaTypeVariable (Syntax.unIdentifier (Syntax.unTypeIdentifier (Syntax.typeParameterIdentifier tp)))

typeParameterToTypeArgument :: Syntax.TypeParameter -> Syntax.TypeArgument
typeParameterToTypeArgument tp = javaTypeIdentifierToJavaTypeArgument (Syntax.typeParameterIdentifier tp)

unTypeParameter :: Syntax.TypeParameter -> String
unTypeParameter tp = Syntax.unIdentifier (Syntax.unTypeIdentifier (Syntax.typeParameterIdentifier tp))

unescape :: String -> String
unescape s = Strings.fromList (Lists.tail (Strings.toList s))

uniqueVarName :: Environment.Aliases -> Core.Name -> Core.Name
uniqueVarName aliases name =
    Logic.ifElse (Sets.member name (Environment.aliasesInScopeJavaVars aliases)) (uniqueVarName_go aliases (Core.unName name) 2) name

uniqueVarName_go :: Environment.Aliases -> String -> Int -> Core.Name
uniqueVarName_go aliases base n =

      let candidate = Core.Name (Strings.cat2 base (Literals.showInt32 n))
      in (Logic.ifElse (Sets.member candidate (Environment.aliasesInScopeJavaVars aliases)) (uniqueVarName_go aliases base (Math.add n 1)) candidate)

varDeclarationStatement :: Syntax.Identifier -> Syntax.Expression -> Syntax.BlockStatement
varDeclarationStatement id rhs =
    Syntax.BlockStatementLocalVariableDeclaration (Syntax.LocalVariableDeclarationStatement (Syntax.LocalVariableDeclaration {
      Syntax.localVariableDeclarationModifiers = [],
      Syntax.localVariableDeclarationType = Syntax.LocalVariableTypeVar,
      Syntax.localVariableDeclarationDeclarators = [
        javaVariableDeclarator id (Just (Syntax.VariableInitializerExpression rhs))]}))

variableDeclarationStatement :: t0 -> Syntax.Type -> Syntax.Identifier -> Syntax.Expression -> Syntax.BlockStatement
variableDeclarationStatement aliases jtype id rhs =

      let init_ = Syntax.VariableInitializerExpression rhs
          vdec = javaVariableDeclarator id (Just init_)
      in (Syntax.BlockStatementLocalVariableDeclaration (Syntax.LocalVariableDeclarationStatement (Syntax.LocalVariableDeclaration {
        Syntax.localVariableDeclarationModifiers = [],
        Syntax.localVariableDeclarationType = (Syntax.LocalVariableTypeType (Syntax.UnannType jtype)),
        Syntax.localVariableDeclarationDeclarators = [
          vdec]})))

variableToJavaIdentifier :: Core.Name -> Syntax.Identifier
variableToJavaIdentifier name =

      let v = Core.unName name
      in (Logic.ifElse (Equality.equal v "_") (Syntax.Identifier "ignored") (Syntax.Identifier (sanitizeJavaName v)))

variantClassName :: Bool -> Core.Name -> Core.Name -> Core.Name
variantClassName qualify elName fname =

      let qn = Names.qualifyName elName
          ns_ = Packaging.qualifiedNameNamespace qn
          local = Packaging.qualifiedNameLocal qn
          flocal = Formatting.capitalize (Core.unName fname)
          local1 =
                  Logic.ifElse qualify (Strings.cat2 (Strings.cat2 local ".") flocal) (Logic.ifElse (Equality.equal flocal local) (Strings.cat2 flocal "_") flocal)
      in (Names.unqualifyName (Packaging.QualifiedName {
        Packaging.qualifiedNameNamespace = ns_,
        Packaging.qualifiedNameLocal = local1}))

visitorTypeVariable :: Syntax.ReferenceType
visitorTypeVariable = javaTypeVariable "r"
