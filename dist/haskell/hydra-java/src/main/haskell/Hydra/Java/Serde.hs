-- Note: this is an automatically generated file. Do not edit.
-- | Java serializer: converts Java AST to concrete syntax

module Hydra.Java.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
additionalBoundToExpr :: Syntax.AdditionalBound -> Ast.Expr
additionalBoundToExpr ab =
    Serialization.spaceSep [
      Serialization.cst "&",
      (interfaceTypeToExpr (Syntax.unAdditionalBound ab))]
additiveExpressionToExpr :: Syntax.AdditiveExpression -> Ast.Expr
additiveExpressionToExpr e =
    case e of
      Syntax.AdditiveExpressionUnary v0 -> multiplicativeExpressionToExpr v0
      Syntax.AdditiveExpressionPlus v0 -> Serialization.infixWs "+" (additiveExpressionToExpr (Syntax.additiveExpression_BinaryLhs v0)) (multiplicativeExpressionToExpr (Syntax.additiveExpression_BinaryRhs v0))
      Syntax.AdditiveExpressionMinus v0 -> Serialization.infixWs "-" (additiveExpressionToExpr (Syntax.additiveExpression_BinaryLhs v0)) (multiplicativeExpressionToExpr (Syntax.additiveExpression_BinaryRhs v0))
ambiguousNameToExpr :: Syntax.AmbiguousName -> Ast.Expr
ambiguousNameToExpr an = Serialization.dotSep (Lists.map identifierToExpr (Syntax.unAmbiguousName an))
andExpressionToExpr :: Syntax.AndExpression -> Ast.Expr
andExpressionToExpr ae = Serialization.infixWsList "&" (Lists.map equalityExpressionToExpr (Syntax.unAndExpression ae))
annotatedIdentifierToExpr :: Syntax.AnnotatedIdentifier -> Ast.Expr
annotatedIdentifierToExpr ai = identifierToExpr (Syntax.annotatedIdentifierIdentifier ai)
annotationToExpr :: Syntax.Annotation -> Ast.Expr
annotationToExpr ann =
    case ann of
      Syntax.AnnotationNormal v0 -> normalAnnotationToExpr v0
      Syntax.AnnotationMarker v0 -> markerAnnotationToExpr v0
      Syntax.AnnotationSingleElement v0 -> singleElementAnnotationToExpr v0
annotationTypeDeclarationToExpr :: t0 -> Ast.Expr
annotationTypeDeclarationToExpr _ = Serialization.cst "STUB:AnnotationTypeDeclaration"
arrayAccessToExpr :: t0 -> Ast.Expr
arrayAccessToExpr _ = Serialization.cst "STUB:ArrayAccess"
arrayCreationExpressionToExpr :: Syntax.ArrayCreationExpression -> Ast.Expr
arrayCreationExpressionToExpr ace =
    case ace of
      Syntax.ArrayCreationExpressionPrimitiveArray v0 ->
        let pt = Syntax.arrayCreationExpression_PrimitiveArrayType v0
            ai = Syntax.arrayCreationExpression_PrimitiveArrayArray v0
        in (Serialization.spaceSep [
          Serialization.cst "new",
          (Serialization.noSep [
            primitiveTypeWithAnnotationsToExpr pt,
            (Serialization.cst "[]")]),
          (arrayInitializerToExpr ai)])
      Syntax.ArrayCreationExpressionClassOrInterfaceArray _ -> Serialization.cst "STUB:ArrayCreationExpression"
      Syntax.ArrayCreationExpressionPrimitive _ -> Serialization.cst "STUB:ArrayCreationExpression"
      Syntax.ArrayCreationExpressionClassOrInterface _ -> Serialization.cst "STUB:ArrayCreationExpression"
arrayInitializerToExpr :: Syntax.ArrayInitializer -> Ast.Expr
arrayInitializerToExpr ai =

      let groups = Syntax.unArrayInitializer ai
      in (Maybes.fromMaybe (Serialization.cst "{}") (Maybes.map (\firstGroup -> Logic.ifElse (Equality.equal (Lists.length groups) 1) (Serialization.noSep [
        Serialization.cst "{",
        (Serialization.commaSep Serialization.inlineStyle (Lists.map variableInitializerToExpr firstGroup)),
        (Serialization.cst "}")]) (Serialization.cst "{}")) (Lists.maybeHead groups)))
arrayTypeToExpr :: Syntax.ArrayType -> Ast.Expr
arrayTypeToExpr at =

      let dims = Syntax.arrayTypeDims at
          variant = Syntax.arrayTypeVariant at
          varExpr =
                  case variant of
                    Syntax.ArrayType_VariantPrimitive v0 -> primitiveTypeWithAnnotationsToExpr v0
                    Syntax.ArrayType_VariantClassOrInterface v0 -> classOrInterfaceTypeToExpr v0
                    Syntax.ArrayType_VariantVariable v0 -> typeVariableToExpr v0
      in (Serialization.noSep [
        varExpr,
        (dimsToExpr dims)])
assertStatementToExpr :: t0 -> Ast.Expr
assertStatementToExpr _ = Serialization.cst "STUB:AssertStatement"
assignmentExpressionToExpr :: Syntax.AssignmentExpression -> Ast.Expr
assignmentExpressionToExpr e =
    case e of
      Syntax.AssignmentExpressionConditional v0 -> conditionalExpressionToExpr v0
      Syntax.AssignmentExpressionAssignment v0 -> assignmentToExpr v0
assignmentToExpr :: Syntax.Assignment -> Ast.Expr
assignmentToExpr a =

      let lhs = Syntax.assignmentLhs a
          op = Syntax.assignmentOp a
          rhs = Syntax.assignmentExpression a
          ctop =
                  case op of
                    Syntax.AssignmentOperatorSimple -> "="
                    Syntax.AssignmentOperatorTimes -> "*="
                    Syntax.AssignmentOperatorDiv -> "/="
                    Syntax.AssignmentOperatorMod -> "%="
                    Syntax.AssignmentOperatorPlus -> "+="
                    Syntax.AssignmentOperatorMinus -> "-="
                    Syntax.AssignmentOperatorShiftLeft -> "<<="
                    Syntax.AssignmentOperatorShiftRight -> ">>="
                    Syntax.AssignmentOperatorShiftRightZeroFill -> ">>>="
                    Syntax.AssignmentOperatorAnd -> "&="
                    Syntax.AssignmentOperatorXor -> "^="
                    Syntax.AssignmentOperatorOr -> "|="
      in (Serialization.infixWs ctop (leftHandSideToExpr lhs) (expressionToExpr rhs))
blockStatementToExpr :: Syntax.BlockStatement -> Ast.Expr
blockStatementToExpr s =
    case s of
      Syntax.BlockStatementLocalVariableDeclaration v0 -> localVariableDeclarationStatementToExpr v0
      Syntax.BlockStatementClass v0 -> classDeclarationToExpr v0
      Syntax.BlockStatementStatement v0 -> statementToExpr v0
blockToExpr :: Syntax.Block -> Ast.Expr
blockToExpr b =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map blockStatementToExpr (Syntax.unBlock b)))
breakStatementToExpr :: Syntax.BreakStatement -> Ast.Expr
breakStatementToExpr bs =

      let mlabel = Syntax.unBreakStatement bs
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "break"),
        (Maybes.map identifierToExpr mlabel)])))
castExpressionLambdaToExpr :: t0 -> Ast.Expr
castExpressionLambdaToExpr _ = Serialization.cst "STUB:CastExpression_Lambda"
castExpressionNotPlusMinusToExpr :: Syntax.CastExpression_NotPlusMinus -> Ast.Expr
castExpressionNotPlusMinusToExpr npm =

      let rb = Syntax.castExpression_NotPlusMinusRefAndBounds npm
          ex = Syntax.castExpression_NotPlusMinusExpression npm
      in (Serialization.spaceSep [
        castExpressionRefAndBoundsToExpr rb,
        (unaryExpressionToExpr ex)])
castExpressionPrimitiveToExpr :: Syntax.CastExpression_Primitive -> Ast.Expr
castExpressionPrimitiveToExpr cp =

      let pt = Syntax.castExpression_PrimitiveType cp
          ex = Syntax.castExpression_PrimitiveExpression cp
      in (Serialization.spaceSep [
        Serialization.parenList False [
          primitiveTypeWithAnnotationsToExpr pt],
        (unaryExpressionToExpr ex)])
castExpressionRefAndBoundsToExpr :: Syntax.CastExpression_RefAndBounds -> Ast.Expr
castExpressionRefAndBoundsToExpr rab =

      let rt = Syntax.castExpression_RefAndBoundsType rab
          adds = Syntax.castExpression_RefAndBoundsBounds rab
      in (Serialization.parenList False [
        Serialization.spaceSep (Maybes.cat [
          Just (referenceTypeToExpr rt),
          (Logic.ifElse (Lists.null adds) Nothing (Just (Serialization.spaceSep (Lists.map additionalBoundToExpr adds))))])])
castExpressionToExpr :: Syntax.CastExpression -> Ast.Expr
castExpressionToExpr e =
    case e of
      Syntax.CastExpressionPrimitive v0 -> castExpressionPrimitiveToExpr v0
      Syntax.CastExpressionNotPlusMinus v0 -> castExpressionNotPlusMinusToExpr v0
      Syntax.CastExpressionLambda v0 -> castExpressionLambdaToExpr v0
classBodyDeclarationToExpr :: Syntax.ClassBodyDeclaration -> Ast.Expr
classBodyDeclarationToExpr d =
    case d of
      Syntax.ClassBodyDeclarationClassMember v0 -> classMemberDeclarationToExpr v0
      Syntax.ClassBodyDeclarationInstanceInitializer v0 -> instanceInitializerToExpr v0
      Syntax.ClassBodyDeclarationStaticInitializer v0 -> staticInitializerToExpr v0
      Syntax.ClassBodyDeclarationConstructorDeclaration v0 -> constructorDeclarationToExpr v0
classBodyDeclarationWithCommentsToExpr :: Syntax.ClassBodyDeclarationWithComments -> Ast.Expr
classBodyDeclarationWithCommentsToExpr cbdwc =

      let d = Syntax.classBodyDeclarationWithCommentsValue cbdwc
          mc = Syntax.classBodyDeclarationWithCommentsComments cbdwc
      in (withComments mc (classBodyDeclarationToExpr d))
classBodyToExpr :: Syntax.ClassBody -> Ast.Expr
classBodyToExpr cb =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map classBodyDeclarationWithCommentsToExpr (Syntax.unClassBody cb)))
classDeclarationToExpr :: Syntax.ClassDeclaration -> Ast.Expr
classDeclarationToExpr d =
    case d of
      Syntax.ClassDeclarationNormal v0 -> normalClassDeclarationToExpr v0
      Syntax.ClassDeclarationEnum v0 -> enumDeclarationToExpr v0
classInstanceCreationExpressionQualifierToExpr :: Syntax.ClassInstanceCreationExpression_Qualifier -> Ast.Expr
classInstanceCreationExpressionQualifierToExpr q =
    case q of
      Syntax.ClassInstanceCreationExpression_QualifierExpression v0 -> expressionNameToExpr v0
      Syntax.ClassInstanceCreationExpression_QualifierPrimary v0 -> primaryToExpr v0
classInstanceCreationExpressionToExpr :: Syntax.ClassInstanceCreationExpression -> Ast.Expr
classInstanceCreationExpressionToExpr cice =

      let mqual = Syntax.classInstanceCreationExpressionQualifier cice
          e = Syntax.classInstanceCreationExpressionExpression cice
      in (Maybes.maybe (unqualifiedClassInstanceCreationExpressionToExpr e) (\q -> Serialization.dotSep [
        classInstanceCreationExpressionQualifierToExpr q,
        (unqualifiedClassInstanceCreationExpressionToExpr e)]) mqual)
classLiteralToExpr :: t0 -> Ast.Expr
classLiteralToExpr _ = Serialization.cst "STUB:ClassLiteral"
classMemberDeclarationToExpr :: Syntax.ClassMemberDeclaration -> Ast.Expr
classMemberDeclarationToExpr d =
    case d of
      Syntax.ClassMemberDeclarationField v0 -> fieldDeclarationToExpr v0
      Syntax.ClassMemberDeclarationMethod v0 -> methodDeclarationToExpr v0
      Syntax.ClassMemberDeclarationClass v0 -> classDeclarationToExpr v0
      Syntax.ClassMemberDeclarationInterface v0 -> interfaceDeclarationToExpr v0
      Syntax.ClassMemberDeclarationNone -> Serialization.cst ";"
classModifierToExpr :: Syntax.ClassModifier -> Ast.Expr
classModifierToExpr m =
    case m of
      Syntax.ClassModifierAnnotation v0 -> annotationToExpr v0
      Syntax.ClassModifierPublic -> Serialization.cst "public"
      Syntax.ClassModifierProtected -> Serialization.cst "protected"
      Syntax.ClassModifierPrivate -> Serialization.cst "private"
      Syntax.ClassModifierAbstract -> Serialization.cst "abstract"
      Syntax.ClassModifierStatic -> Serialization.cst "static"
      Syntax.ClassModifierFinal -> Serialization.cst "final"
      Syntax.ClassModifierStrictfp -> Serialization.cst "strictfp"
classOrInterfaceTypeToExpr :: Syntax.ClassOrInterfaceType -> Ast.Expr
classOrInterfaceTypeToExpr cit =
    case cit of
      Syntax.ClassOrInterfaceTypeClass v0 -> classTypeToExpr v0
      Syntax.ClassOrInterfaceTypeInterface v0 -> interfaceTypeToExpr v0
classOrInterfaceTypeToInstantiateToExpr :: Syntax.ClassOrInterfaceTypeToInstantiate -> Ast.Expr
classOrInterfaceTypeToInstantiateToExpr coitti =

      let ids = Syntax.classOrInterfaceTypeToInstantiateIdentifiers coitti
          margs = Syntax.classOrInterfaceTypeToInstantiateTypeArguments coitti
      in (Serialization.noSep (Maybes.cat [
        Just (Serialization.dotSep (Lists.map annotatedIdentifierToExpr ids)),
        (Maybes.map typeArgumentsOrDiamondToExpr margs)]))
classTypeToExpr :: Syntax.ClassType -> Ast.Expr
classTypeToExpr ct =

      let anns = Syntax.classTypeAnnotations ct
          qual = Syntax.classTypeQualifier ct
          id = Syntax.classTypeIdentifier ct
          args = Syntax.classTypeArguments ct
          qualifiedId =
                  case qual of
                    Syntax.ClassTypeQualifierNone -> typeIdentifierToExpr id
                    Syntax.ClassTypeQualifierPackage v0 -> Serialization.dotSep [
                      packageNameToExpr v0,
                      (typeIdentifierToExpr id)]
                    Syntax.ClassTypeQualifierParent v0 -> Serialization.dotSep [
                      classOrInterfaceTypeToExpr v0,
                      (typeIdentifierToExpr id)]
      in (Serialization.noSep (Maybes.cat [
        Just (Serialization.spaceSep (Maybes.cat [
          Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map annotationToExpr anns))),
          (Just qualifiedId)])),
        (Logic.ifElse (Lists.null args) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeArgumentToExpr args))))]))
compilationUnitToExpr :: Syntax.CompilationUnit -> Ast.Expr
compilationUnitToExpr u =
    case u of
      Syntax.CompilationUnitOrdinary v0 ->
        let mpkg = Syntax.ordinaryCompilationUnitPackage v0
            imports = Syntax.ordinaryCompilationUnitImports v0
            types = Syntax.ordinaryCompilationUnitTypes v0
            warning = Just (singleLineComment Constants.warningAutoGeneratedFile)
            pkgSec = Maybes.map packageDeclarationToExpr mpkg
            importsSec =
                    Logic.ifElse (Lists.null imports) Nothing (Just (Serialization.newlineSep (Lists.map importDeclarationToExpr imports)))
            typesSec =
                    Logic.ifElse (Lists.null types) Nothing (Just (Serialization.doubleNewlineSep (Lists.map typeDeclarationWithCommentsToExpr types)))
        in (Serialization.doubleNewlineSep (Maybes.cat [
          warning,
          pkgSec,
          importsSec,
          typesSec]))
conditionalAndExpressionToExpr :: Syntax.ConditionalAndExpression -> Ast.Expr
conditionalAndExpressionToExpr cae =
    Serialization.infixWsList "&&" (Lists.map inclusiveOrExpressionToExpr (Syntax.unConditionalAndExpression cae))
conditionalExpressionTernaryCondToExpr :: t0 -> Ast.Expr
conditionalExpressionTernaryCondToExpr _ = Serialization.cst "STUB:ConditionalExpression_TernaryCond"
conditionalExpressionTernaryLambdaToExpr :: t0 -> Ast.Expr
conditionalExpressionTernaryLambdaToExpr _ = Serialization.cst "STUB:ConditionalExpression_TernaryLambda"
conditionalExpressionToExpr :: Syntax.ConditionalExpression -> Ast.Expr
conditionalExpressionToExpr c =
    case c of
      Syntax.ConditionalExpressionSimple v0 -> conditionalOrExpressionToExpr v0
      Syntax.ConditionalExpressionTernaryCond v0 -> conditionalExpressionTernaryCondToExpr v0
      Syntax.ConditionalExpressionTernaryLambda v0 -> conditionalExpressionTernaryLambdaToExpr v0
conditionalOrExpressionToExpr :: Syntax.ConditionalOrExpression -> Ast.Expr
conditionalOrExpressionToExpr coe =
    Serialization.infixWsList "||" (Lists.map conditionalAndExpressionToExpr (Syntax.unConditionalOrExpression coe))
constantDeclarationToExpr :: Syntax.ConstantDeclaration -> Ast.Expr
constantDeclarationToExpr cd =

      let mods = Syntax.constantDeclarationModifiers cd
          typ = Syntax.constantDeclarationType cd
          vars = Syntax.constantDeclarationVariables cd
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map constantModifierToExpr mods))),
        (Just (unannTypeToExpr typ)),
        (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map variableDeclaratorToExpr vars)))])))
constantModifierToExpr :: t0 -> Ast.Expr
constantModifierToExpr _ = Serialization.cst "STUB:ConstantModifier"
constructorBodyToExpr :: Syntax.ConstructorBody -> Ast.Expr
constructorBodyToExpr cb =

      let minvoc = Syntax.constructorBodyInvocation cb
          stmts = Syntax.constructorBodyStatements cb
      in (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Maybes.cat [
        Maybes.map explicitConstructorInvocationToExpr minvoc,
        (Just (Serialization.newlineSep (Lists.map blockStatementToExpr stmts)))])))
constructorDeclarationToExpr :: Syntax.ConstructorDeclaration -> Ast.Expr
constructorDeclarationToExpr cd =

      let mods = Syntax.constructorDeclarationModifiers cd
          cons = Syntax.constructorDeclarationConstructor cd
          mthrows = Syntax.constructorDeclarationThrows cd
          body = Syntax.constructorDeclarationBody cd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map constructorModifierToExpr mods))),
        (Just (constructorDeclaratorToExpr cons)),
        (Maybes.map throwsToExpr mthrows),
        (Just (constructorBodyToExpr body))]))
constructorDeclaratorToExpr :: Syntax.ConstructorDeclarator -> Ast.Expr
constructorDeclaratorToExpr cd =

      let tparams = Syntax.constructorDeclaratorParameters cd
          name = Syntax.constructorDeclaratorName cd
          fparams = Syntax.constructorDeclaratorFormalParameters cd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeParameterToExpr tparams))),
        (Just (simpleTypeNameToExpr name)),
        (Just (Serialization.parenListAdaptive (Lists.map formalParameterToExpr fparams)))]))
constructorModifierToExpr :: Syntax.ConstructorModifier -> Ast.Expr
constructorModifierToExpr m =
    case m of
      Syntax.ConstructorModifierAnnotation v0 -> annotationToExpr v0
      Syntax.ConstructorModifierPublic -> Serialization.cst "public"
      Syntax.ConstructorModifierProtected -> Serialization.cst "protected"
      Syntax.ConstructorModifierPrivate -> Serialization.cst "private"
continueStatementToExpr :: Syntax.ContinueStatement -> Ast.Expr
continueStatementToExpr cs =

      let mlabel = Syntax.unContinueStatement cs
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "continue"),
        (Maybes.map identifierToExpr mlabel)])))
dimsToExpr :: Syntax.Dims -> Ast.Expr
dimsToExpr d = Serialization.noSep (Lists.map (\_ -> Serialization.cst "[]") (Syntax.unDims d))
doStatementToExpr :: t0 -> Ast.Expr
doStatementToExpr _ = Serialization.cst "STUB:DoStatement"
elementValuePairToExpr :: Syntax.ElementValuePair -> Ast.Expr
elementValuePairToExpr evp =

      let k = Syntax.elementValuePairKey evp
          v = Syntax.elementValuePairValue evp
      in (Serialization.infixWs "=" (identifierToExpr k) (elementValueToExpr v))
elementValueToExpr :: Syntax.ElementValue -> Ast.Expr
elementValueToExpr ev =
    case ev of
      Syntax.ElementValueConditionalExpression v0 -> conditionalExpressionToExpr v0
      Syntax.ElementValueElementValueArrayInitializer v0 -> Serialization.commaSep Serialization.inlineStyle (Lists.map elementValueToExpr (Syntax.unElementValueArrayInitializer v0))
      Syntax.ElementValueAnnotation v0 -> annotationToExpr v0
enumDeclarationToExpr :: t0 -> Ast.Expr
enumDeclarationToExpr _ = Serialization.cst "STUB:EnumDeclaration"
equalityExpressionToExpr :: Syntax.EqualityExpression -> Ast.Expr
equalityExpressionToExpr e =
    case e of
      Syntax.EqualityExpressionUnary v0 -> relationalExpressionToExpr v0
      Syntax.EqualityExpressionEqual v0 -> Serialization.infixWs "==" (equalityExpressionToExpr (Syntax.equalityExpression_BinaryLhs v0)) (relationalExpressionToExpr (Syntax.equalityExpression_BinaryRhs v0))
      Syntax.EqualityExpressionNotEqual v0 -> Serialization.infixWs "!=" (equalityExpressionToExpr (Syntax.equalityExpression_BinaryLhs v0)) (relationalExpressionToExpr (Syntax.equalityExpression_BinaryRhs v0))
escapeJavaChar :: Int -> String
escapeJavaChar c =
    Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Logic.ifElse (Equality.equal c 9) "\\t" (Logic.ifElse (Equality.equal c 8) "\\b" (Logic.ifElse (Equality.equal c 12) "\\f" (Logic.ifElse (Logic.and (Equality.gte c 32) (Equality.lt c 127)) (Strings.fromList [
      c]) (javaUnicodeEscape c))))))))
escapeJavaString :: String -> String
escapeJavaString s = Strings.cat (Lists.map (\c -> escapeJavaChar c) (Strings.toList s))
exclusiveOrExpressionToExpr :: Syntax.ExclusiveOrExpression -> Ast.Expr
exclusiveOrExpressionToExpr eoe =
    Serialization.infixWsList "^" (Lists.map andExpressionToExpr (Syntax.unExclusiveOrExpression eoe))
explicitConstructorInvocationToExpr :: t0 -> Ast.Expr
explicitConstructorInvocationToExpr _ = Serialization.cst "STUB:ExplicitConstructorInvocation"
expressionNameToExpr :: Syntax.ExpressionName -> Ast.Expr
expressionNameToExpr en =

      let mqual = Syntax.expressionNameQualifier en
          id = Syntax.expressionNameIdentifier en
      in (Serialization.dotSep (Maybes.cat [
        Maybes.map ambiguousNameToExpr mqual,
        (Just (identifierToExpr id))]))
expressionStatementToExpr :: Syntax.ExpressionStatement -> Ast.Expr
expressionStatementToExpr es = Serialization.withSemi (statementExpressionToExpr (Syntax.unExpressionStatement es))
expressionToExpr :: Syntax.Expression -> Ast.Expr
expressionToExpr e =
    case e of
      Syntax.ExpressionLambda v0 -> lambdaExpressionToExpr v0
      Syntax.ExpressionAssignment v0 -> assignmentExpressionToExpr v0
fieldAccessToExpr :: Syntax.FieldAccess -> Ast.Expr
fieldAccessToExpr fa =

      let qual = Syntax.fieldAccessQualifier fa
          id = Syntax.fieldAccessIdentifier fa
      in case qual of
        Syntax.FieldAccess_QualifierPrimary v0 -> Serialization.dotSep [
          primaryToExpr v0,
          (identifierToExpr id)]
        Syntax.FieldAccess_QualifierSuper -> Serialization.dotSep [
          Serialization.cst "super",
          (identifierToExpr id)]
        Syntax.FieldAccess_QualifierTyped v0 -> Serialization.dotSep [
          typeNameToExpr v0,
          (Serialization.cst "super"),
          (identifierToExpr id)]
fieldDeclarationToExpr :: Syntax.FieldDeclaration -> Ast.Expr
fieldDeclarationToExpr fd =

      let mods = Syntax.fieldDeclarationModifiers fd
          typ = Syntax.fieldDeclarationUnannType fd
          vars = Syntax.fieldDeclarationVariableDeclarators fd
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map fieldModifierToExpr mods))),
        (Just (unannTypeToExpr typ)),
        (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map variableDeclaratorToExpr vars)))])))
fieldModifierToExpr :: Syntax.FieldModifier -> Ast.Expr
fieldModifierToExpr m =
    case m of
      Syntax.FieldModifierAnnotation v0 -> annotationToExpr v0
      Syntax.FieldModifierPublic -> Serialization.cst "public"
      Syntax.FieldModifierProtected -> Serialization.cst "protected"
      Syntax.FieldModifierPrivate -> Serialization.cst "private"
      Syntax.FieldModifierStatic -> Serialization.cst "static"
      Syntax.FieldModifierFinal -> Serialization.cst "final"
      Syntax.FieldModifierTransient -> Serialization.cst "transient"
      Syntax.FieldModifierVolatile -> Serialization.cst "volatile"
floatingPointLiteralToExpr :: Syntax.FloatingPointLiteral -> Ast.Expr
floatingPointLiteralToExpr fl =
    Serialization.cst (javaFloatLiteralText (Literals.showBigfloat (Syntax.unFloatingPointLiteral fl)))
floatingPointTypeToExpr :: Syntax.FloatingPointType -> Ast.Expr
floatingPointTypeToExpr ft =
    case ft of
      Syntax.FloatingPointTypeFloat -> Serialization.cst "float"
      Syntax.FloatingPointTypeDouble -> Serialization.cst "double"
forStatementToExpr :: t0 -> Ast.Expr
forStatementToExpr _ = Serialization.cst "STUB:ForStatement"
formalParameterSimpleToExpr :: Syntax.FormalParameter_Simple -> Ast.Expr
formalParameterSimpleToExpr fps =

      let mods = Syntax.formalParameter_SimpleModifiers fps
          typ = Syntax.formalParameter_SimpleType fps
          id = Syntax.formalParameter_SimpleId fps
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map variableModifierToExpr mods))),
        (Just (unannTypeToExpr typ)),
        (Just (variableDeclaratorIdToExpr id))]))
formalParameterToExpr :: Syntax.FormalParameter -> Ast.Expr
formalParameterToExpr p =
    case p of
      Syntax.FormalParameterSimple v0 -> formalParameterSimpleToExpr v0
      Syntax.FormalParameterVariableArity v0 -> variableArityParameterToExpr v0
hexDigit :: Int -> Int
hexDigit n = Logic.ifElse (Equality.lt n 10) (Math.add n 48) (Math.add (Math.sub n 10) 65)
identifierToExpr :: Syntax.Identifier -> Ast.Expr
identifierToExpr id = Serialization.cst (Syntax.unIdentifier id)
ifThenElseStatementToExpr :: t0 -> Ast.Expr
ifThenElseStatementToExpr _ = Serialization.cst "STUB:IfThenElseStatement"
ifThenStatementToExpr :: Syntax.IfThenStatement -> Ast.Expr
ifThenStatementToExpr its =

      let cond = Syntax.ifThenStatementExpression its
          thn = Syntax.ifThenStatementStatement its
      in (Serialization.spaceSep [
        Serialization.cst "if",
        (Serialization.parenList False [
          expressionToExpr cond]),
        (Serialization.curlyBlock Serialization.fullBlockStyle (statementToExpr thn))])
importDeclarationToExpr :: Syntax.ImportDeclaration -> Ast.Expr
importDeclarationToExpr imp =
    case imp of
      Syntax.ImportDeclarationSingleType v0 -> Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "import",
        (typeNameToExpr (Syntax.unSingleTypeImportDeclaration v0))])
      Syntax.ImportDeclarationTypeImportOnDemand _ -> Serialization.cst "STUB:ImportDeclarationTypeImportOnDemand"
      Syntax.ImportDeclarationSingleStaticImport _ -> Serialization.cst "STUB:ImportDeclarationSingleStaticImport"
      Syntax.ImportDeclarationStaticImportOnDemand _ -> Serialization.cst "STUB:ImportDeclarationStaticImportOnDemand"
inclusiveOrExpressionToExpr :: Syntax.InclusiveOrExpression -> Ast.Expr
inclusiveOrExpressionToExpr ioe =
    Serialization.infixWsList "|" (Lists.map exclusiveOrExpressionToExpr (Syntax.unInclusiveOrExpression ioe))
instanceInitializerToExpr :: t0 -> Ast.Expr
instanceInitializerToExpr _ = Serialization.cst "STUB:InstanceInitializer"
integerLiteralToExpr :: Syntax.IntegerLiteral -> Ast.Expr
integerLiteralToExpr il =

      let i = Syntax.unIntegerLiteral il
          suffix = Logic.ifElse (Logic.or (Equality.gt i 2147483647) (Equality.lt i (-2147483648))) "L" ""
      in (Serialization.cst (Strings.cat2 (Literals.showBigint i) suffix))
integralTypeToExpr :: Syntax.IntegralType -> Ast.Expr
integralTypeToExpr t =
    case t of
      Syntax.IntegralTypeByte -> Serialization.cst "byte"
      Syntax.IntegralTypeShort -> Serialization.cst "short"
      Syntax.IntegralTypeInt -> Serialization.cst "int"
      Syntax.IntegralTypeLong -> Serialization.cst "long"
      Syntax.IntegralTypeChar -> Serialization.cst "char"
interfaceBodyToExpr :: Syntax.InterfaceBody -> Ast.Expr
interfaceBodyToExpr ib =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map interfaceMemberDeclarationToExpr (Syntax.unInterfaceBody ib)))
interfaceDeclarationToExpr :: Syntax.InterfaceDeclaration -> Ast.Expr
interfaceDeclarationToExpr d =
    case d of
      Syntax.InterfaceDeclarationNormalInterface v0 -> normalInterfaceDeclarationToExpr v0
      Syntax.InterfaceDeclarationAnnotationType v0 -> annotationTypeDeclarationToExpr v0
interfaceMemberDeclarationToExpr :: Syntax.InterfaceMemberDeclaration -> Ast.Expr
interfaceMemberDeclarationToExpr d =
    case d of
      Syntax.InterfaceMemberDeclarationConstant v0 -> constantDeclarationToExpr v0
      Syntax.InterfaceMemberDeclarationInterfaceMethod v0 -> interfaceMethodDeclarationToExpr v0
      Syntax.InterfaceMemberDeclarationClass v0 -> classDeclarationToExpr v0
      Syntax.InterfaceMemberDeclarationInterface v0 -> interfaceDeclarationToExpr v0
interfaceMethodDeclarationToExpr :: Syntax.InterfaceMethodDeclaration -> Ast.Expr
interfaceMethodDeclarationToExpr imd =

      let mods = Syntax.interfaceMethodDeclarationModifiers imd
          header = Syntax.interfaceMethodDeclarationHeader imd
          body = Syntax.interfaceMethodDeclarationBody imd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map interfaceMethodModifierToExpr mods))),
        (Just (methodHeaderToExpr header)),
        (Just (methodBodyToExpr body))]))
interfaceMethodModifierToExpr :: Syntax.InterfaceMethodModifier -> Ast.Expr
interfaceMethodModifierToExpr m =
    case m of
      Syntax.InterfaceMethodModifierAnnotation v0 -> annotationToExpr v0
      Syntax.InterfaceMethodModifierPublic -> Serialization.cst "public"
      Syntax.InterfaceMethodModifierPrivate -> Serialization.cst "private"
      Syntax.InterfaceMethodModifierAbstract -> Serialization.cst "abstract"
      Syntax.InterfaceMethodModifierDefault -> Serialization.cst "default"
      Syntax.InterfaceMethodModifierStatic -> Serialization.cst "static"
      Syntax.InterfaceMethodModifierStrictfp -> Serialization.cst "strictfp"
interfaceModifierToExpr :: Syntax.InterfaceModifier -> Ast.Expr
interfaceModifierToExpr m =
    case m of
      Syntax.InterfaceModifierAnnotation v0 -> annotationToExpr v0
      Syntax.InterfaceModifierPublic -> Serialization.cst "public"
      Syntax.InterfaceModifierProtected -> Serialization.cst "protected"
      Syntax.InterfaceModifierPrivate -> Serialization.cst "private"
      Syntax.InterfaceModifierAbstract -> Serialization.cst "abstract"
      Syntax.InterfaceModifierStatic -> Serialization.cst "static"
      Syntax.InterfaceModifierStrictfb -> Serialization.cst "strictfb"
interfaceTypeToExpr :: Syntax.InterfaceType -> Ast.Expr
interfaceTypeToExpr it = classTypeToExpr (Syntax.unInterfaceType it)
javaFloatLiteralText :: String -> String
javaFloatLiteralText s =
    Logic.ifElse (Equality.equal s "NaN") "Double.NaN" (Logic.ifElse (Equality.equal s "Infinity") "Double.POSITIVE_INFINITY" (Logic.ifElse (Equality.equal s "-Infinity") "Double.NEGATIVE_INFINITY" s))
javaUnicodeEscape :: Int -> String
javaUnicodeEscape n =
    Logic.ifElse (Equality.gt n 65535) (
      let n_ = Math.sub n 65536
          hi = Math.add 55296 (Maybes.fromMaybe 0 (Math.maybeDiv n_ 1024))
          lo = Math.add 56320 (Maybes.fromMaybe 0 (Math.maybeMod n_ 1024))
      in (Strings.cat2 (Strings.cat2 "\\u" (padHex4 hi)) (Strings.cat2 "\\u" (padHex4 lo)))) (Strings.cat2 "\\u" (padHex4 n))
labeledStatementToExpr :: t0 -> Ast.Expr
labeledStatementToExpr _ = Serialization.cst "STUB:LabeledStatement"
lambdaBodyToExpr :: Syntax.LambdaBody -> Ast.Expr
lambdaBodyToExpr b =
    case b of
      Syntax.LambdaBodyExpression v0 -> expressionToExpr v0
      Syntax.LambdaBodyBlock v0 -> blockToExpr v0
lambdaExpressionToExpr :: Syntax.LambdaExpression -> Ast.Expr
lambdaExpressionToExpr le =

      let params = Syntax.lambdaExpressionParameters le
          body = Syntax.lambdaExpressionBody le
      in (Serialization.infixWs "->" (lambdaParametersToExpr params) (lambdaBodyToExpr body))
lambdaParametersToExpr :: Syntax.LambdaParameters -> Ast.Expr
lambdaParametersToExpr p =
    case p of
      Syntax.LambdaParametersTuple v0 -> Serialization.parenList False (Lists.map lambdaParametersToExpr v0)
      Syntax.LambdaParametersSingle v0 -> identifierToExpr v0
leftHandSideToExpr :: Syntax.LeftHandSide -> Ast.Expr
leftHandSideToExpr lhs =
    case lhs of
      Syntax.LeftHandSideExpressionName v0 -> expressionNameToExpr v0
      Syntax.LeftHandSideFieldAccess v0 -> fieldAccessToExpr v0
      Syntax.LeftHandSideArrayAccess v0 -> arrayAccessToExpr v0
literalToExpr :: Syntax.Literal -> Ast.Expr
literalToExpr l =
    case l of
      Syntax.LiteralNull -> Serialization.cst "null"
      Syntax.LiteralInteger v0 -> integerLiteralToExpr v0
      Syntax.LiteralFloatingPoint v0 -> floatingPointLiteralToExpr v0
      Syntax.LiteralBoolean v0 -> Serialization.cst (Logic.ifElse v0 "true" "false")
      Syntax.LiteralCharacter v0 ->
        let ci = Literals.bigintToInt32 (Literals.uint16ToBigint v0)
        in (Serialization.cst (Strings.cat2 "'" (Strings.cat2 (Logic.ifElse (Equality.equal ci 39) "\\'" (Logic.ifElse (Equality.equal ci 92) "\\\\" (Logic.ifElse (Equality.equal ci 10) "\\n" (Logic.ifElse (Equality.equal ci 13) "\\r" (Logic.ifElse (Equality.equal ci 9) "\\t" (Logic.ifElse (Logic.and (Equality.gte ci 32) (Equality.lt ci 127)) (Strings.fromList [
          ci]) (javaUnicodeEscape ci))))))) "'")))
      Syntax.LiteralString v0 -> stringLiteralToExpr v0
localNameToExpr :: Syntax.LocalVariableType -> Ast.Expr
localNameToExpr t =
    case t of
      Syntax.LocalVariableTypeType v0 -> unannTypeToExpr v0
      Syntax.LocalVariableTypeVar -> Serialization.cst "var"
localVariableDeclarationStatementToExpr :: Syntax.LocalVariableDeclarationStatement -> Ast.Expr
localVariableDeclarationStatementToExpr lvds =
    Serialization.withSemi (localVariableDeclarationToExpr (Syntax.unLocalVariableDeclarationStatement lvds))
localVariableDeclarationToExpr :: Syntax.LocalVariableDeclaration -> Ast.Expr
localVariableDeclarationToExpr lvd =

      let mods = Syntax.localVariableDeclarationModifiers lvd
          t = Syntax.localVariableDeclarationType lvd
          decls = Syntax.localVariableDeclarationDeclarators lvd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map variableModifierToExpr mods))),
        (Just (localNameToExpr t)),
        (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map variableDeclaratorToExpr decls)))]))
markerAnnotationToExpr :: Syntax.MarkerAnnotation -> Ast.Expr
markerAnnotationToExpr ma = Serialization.prefix "@" (typeNameToExpr (Syntax.unMarkerAnnotation ma))
methodBodyToExpr :: Syntax.MethodBody -> Ast.Expr
methodBodyToExpr b =
    case b of
      Syntax.MethodBodyBlock v0 -> blockToExpr v0
      Syntax.MethodBodyNone -> Serialization.cst ";"
methodDeclarationToExpr :: Syntax.MethodDeclaration -> Ast.Expr
methodDeclarationToExpr md =

      let anns = Syntax.methodDeclarationAnnotations md
          mods = Syntax.methodDeclarationModifiers md
          header = Syntax.methodDeclarationHeader md
          body = Syntax.methodDeclarationBody md
          headerAndBody =
                  Serialization.spaceSep (Maybes.cat [
                    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map methodModifierToExpr mods))),
                    (Just (methodHeaderToExpr header)),
                    (Just (methodBodyToExpr body))])
      in (Serialization.newlineSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.newlineSep (Lists.map annotationToExpr anns))),
        (Just headerAndBody)]))
methodDeclaratorToExpr :: Syntax.MethodDeclarator -> Ast.Expr
methodDeclaratorToExpr md =

      let id = Syntax.methodDeclaratorIdentifier md
          params = Syntax.methodDeclaratorFormalParameters md
      in (Serialization.noSep [
        identifierToExpr id,
        (Serialization.parenListAdaptive (Lists.map formalParameterToExpr params))])
methodHeaderToExpr :: Syntax.MethodHeader -> Ast.Expr
methodHeaderToExpr mh =

      let params = Syntax.methodHeaderParameters mh
          result = Syntax.methodHeaderResult mh
          decl = Syntax.methodHeaderDeclarator mh
          mthrows = Syntax.methodHeaderThrows mh
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null params) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeParameterToExpr params))),
        (Just (resultToExpr result)),
        (Just (methodDeclaratorToExpr decl)),
        (Maybes.map throwsToExpr mthrows)]))
methodInvocationToExpr :: Syntax.MethodInvocation -> Ast.Expr
methodInvocationToExpr mi =

      let header = Syntax.methodInvocationHeader mi
          args = Syntax.methodInvocationArguments mi
          argSec = Serialization.parenListAdaptive (Lists.map expressionToExpr args)
          headerSec =
                  case header of
                    Syntax.MethodInvocation_HeaderSimple v0 -> methodNameToExpr v0
                    Syntax.MethodInvocation_HeaderComplex v0 ->
                      let cvar = Syntax.methodInvocation_ComplexVariant v0
                          targs = Syntax.methodInvocation_ComplexTypeArguments v0
                          cid = Syntax.methodInvocation_ComplexIdentifier v0
                          idSec =
                                  Serialization.noSep (Maybes.cat [
                                    Logic.ifElse (Lists.null targs) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeArgumentToExpr targs))),
                                    (Just (identifierToExpr cid))])
                      in case cvar of
                        Syntax.MethodInvocation_VariantType v1 -> Serialization.dotSep [
                          typeNameToExpr v1,
                          idSec]
                        Syntax.MethodInvocation_VariantExpression v1 -> Serialization.dotSep [
                          expressionNameToExpr v1,
                          idSec]
                        Syntax.MethodInvocation_VariantPrimary v1 -> Serialization.dotSep [
                          primaryToExpr v1,
                          idSec]
                        Syntax.MethodInvocation_VariantSuper -> Serialization.dotSep [
                          Serialization.cst "super",
                          idSec]
                        Syntax.MethodInvocation_VariantTypeSuper v1 -> Serialization.dotSep [
                          typeNameToExpr v1,
                          (Serialization.cst "super"),
                          idSec]
      in (Serialization.noSep [
        headerSec,
        argSec])
methodModifierToExpr :: Syntax.MethodModifier -> Ast.Expr
methodModifierToExpr m =
    case m of
      Syntax.MethodModifierAnnotation v0 -> annotationToExpr v0
      Syntax.MethodModifierPublic -> Serialization.cst "public"
      Syntax.MethodModifierProtected -> Serialization.cst "protected"
      Syntax.MethodModifierPrivate -> Serialization.cst "private"
      Syntax.MethodModifierAbstract -> Serialization.cst "abstract"
      Syntax.MethodModifierFinal -> Serialization.cst "final"
      Syntax.MethodModifierSynchronized -> Serialization.cst "synchronized"
      Syntax.MethodModifierNative -> Serialization.cst "native"
      Syntax.MethodModifierStrictfb -> Serialization.cst "strictfb"
methodNameToExpr :: Syntax.MethodName -> Ast.Expr
methodNameToExpr mn = identifierToExpr (Syntax.unMethodName mn)
methodReferenceToExpr :: t0 -> Ast.Expr
methodReferenceToExpr _ = Serialization.cst "STUB:MethodReference"
multiplicativeExpressionToExpr :: Syntax.MultiplicativeExpression -> Ast.Expr
multiplicativeExpressionToExpr e =
    case e of
      Syntax.MultiplicativeExpressionUnary v0 -> unaryExpressionToExpr v0
      Syntax.MultiplicativeExpressionTimes v0 -> Serialization.infixWs "*" (multiplicativeExpressionToExpr (Syntax.multiplicativeExpression_BinaryLhs v0)) (unaryExpressionToExpr (Syntax.multiplicativeExpression_BinaryRhs v0))
      Syntax.MultiplicativeExpressionDivide v0 -> Serialization.infixWs "/" (multiplicativeExpressionToExpr (Syntax.multiplicativeExpression_BinaryLhs v0)) (unaryExpressionToExpr (Syntax.multiplicativeExpression_BinaryRhs v0))
      Syntax.MultiplicativeExpressionMod v0 -> Serialization.infixWs "%" (multiplicativeExpressionToExpr (Syntax.multiplicativeExpression_BinaryLhs v0)) (unaryExpressionToExpr (Syntax.multiplicativeExpression_BinaryRhs v0))
normalAnnotationToExpr :: Syntax.NormalAnnotation -> Ast.Expr
normalAnnotationToExpr na =

      let tname = Syntax.normalAnnotationTypeName na
          pairs = Syntax.normalAnnotationPairs na
      in (Serialization.prefix "@" (Serialization.noSep [
        typeNameToExpr tname,
        (Serialization.commaSep Serialization.inlineStyle (Lists.map elementValuePairToExpr pairs))]))
normalClassDeclarationToExpr :: Syntax.NormalClassDeclaration -> Ast.Expr
normalClassDeclarationToExpr ncd =

      let mods = Syntax.normalClassDeclarationModifiers ncd
          id = Syntax.normalClassDeclarationIdentifier ncd
          tparams = Syntax.normalClassDeclarationParameters ncd
          msuperc = Syntax.normalClassDeclarationExtends ncd
          superi = Syntax.normalClassDeclarationImplements ncd
          body = Syntax.normalClassDeclarationBody ncd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map classModifierToExpr mods))),
        (Just (Serialization.cst "class")),
        (Just (Serialization.noSep (Maybes.cat [
          Just (typeIdentifierToExpr id),
          (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeParameterToExpr tparams))))]))),
        (Maybes.map (\c -> Serialization.spaceSep [
          Serialization.cst "extends",
          (classTypeToExpr c)]) msuperc),
        (Logic.ifElse (Lists.null superi) Nothing (Just (Serialization.spaceSep [
          Serialization.cst "implements",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map interfaceTypeToExpr superi))]))),
        (Just (classBodyToExpr body))]))
normalInterfaceDeclarationToExpr :: Syntax.NormalInterfaceDeclaration -> Ast.Expr
normalInterfaceDeclarationToExpr nid =

      let mods = Syntax.normalInterfaceDeclarationModifiers nid
          id = Syntax.normalInterfaceDeclarationIdentifier nid
          tparams = Syntax.normalInterfaceDeclarationParameters nid
          extends = Syntax.normalInterfaceDeclarationExtends nid
          body = Syntax.normalInterfaceDeclarationBody nid
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map interfaceModifierToExpr mods))),
        (Just (Serialization.cst "interface")),
        (Just (Serialization.noSep (Maybes.cat [
          Just (typeIdentifierToExpr id),
          (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeParameterToExpr tparams))))]))),
        (Logic.ifElse (Lists.null extends) Nothing (Just (Serialization.spaceSep [
          Serialization.cst "extends",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map interfaceTypeToExpr extends))]))),
        (Just (interfaceBodyToExpr body))]))
numericTypeToExpr :: Syntax.NumericType -> Ast.Expr
numericTypeToExpr nt =
    case nt of
      Syntax.NumericTypeIntegral v0 -> integralTypeToExpr v0
      Syntax.NumericTypeFloatingPoint v0 -> floatingPointTypeToExpr v0
packageDeclarationToExpr :: Syntax.PackageDeclaration -> Ast.Expr
packageDeclarationToExpr pd =

      let mods = Syntax.packageDeclarationModifiers pd
          ids = Syntax.packageDeclarationIdentifiers pd
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map packageModifierToExpr mods))),
        (Just (Serialization.spaceSep [
          Serialization.cst "package",
          (Serialization.cst (Strings.intercalate "." (Lists.map (\id -> Syntax.unIdentifier id) ids)))]))])))
packageModifierToExpr :: Syntax.PackageModifier -> Ast.Expr
packageModifierToExpr pm = annotationToExpr (Syntax.unPackageModifier pm)
packageNameToExpr :: Syntax.PackageName -> Ast.Expr
packageNameToExpr pn = Serialization.dotSep (Lists.map identifierToExpr (Syntax.unPackageName pn))
packageOrTypeNameToExpr :: Syntax.PackageOrTypeName -> Ast.Expr
packageOrTypeNameToExpr potn = Serialization.dotSep (Lists.map identifierToExpr (Syntax.unPackageOrTypeName potn))
padHex4 :: Int -> String
padHex4 n =

      let d3 = Maybes.fromMaybe 0 (Math.maybeDiv n 4096)
          r3 = Maybes.fromMaybe 0 (Math.maybeMod n 4096)
          d2 = Maybes.fromMaybe 0 (Math.maybeDiv r3 256)
          r2 = Maybes.fromMaybe 0 (Math.maybeMod r3 256)
          d1 = Maybes.fromMaybe 0 (Math.maybeDiv r2 16)
          d0 = Maybes.fromMaybe 0 (Math.maybeMod r2 16)
      in (Strings.fromList [
        hexDigit d3,
        (hexDigit d2),
        (hexDigit d1),
        (hexDigit d0)])
postDecrementExpressionToExpr :: t0 -> Ast.Expr
postDecrementExpressionToExpr _ = Serialization.cst "STUB:PostDecrementExpression"
postIncrementExpressionToExpr :: t0 -> Ast.Expr
postIncrementExpressionToExpr _ = Serialization.cst "STUB:PostIncrementExpression"
postfixExpressionToExpr :: Syntax.PostfixExpression -> Ast.Expr
postfixExpressionToExpr e =
    case e of
      Syntax.PostfixExpressionPrimary v0 -> primaryToExpr v0
      Syntax.PostfixExpressionName v0 -> expressionNameToExpr v0
      Syntax.PostfixExpressionPostIncrement v0 -> postIncrementExpressionToExpr v0
      Syntax.PostfixExpressionPostDecrement v0 -> postDecrementExpressionToExpr v0
preDecrementExpressionToExpr :: t0 -> Ast.Expr
preDecrementExpressionToExpr _ = Serialization.cst "STUB:PreDecrementExpression"
preIncrementExpressionToExpr :: t0 -> Ast.Expr
preIncrementExpressionToExpr _ = Serialization.cst "STUB:PreIncrementExpression"
primaryNoNewArrayExpressionExpressionToExpr :: Syntax.PrimaryNoNewArrayExpression -> Ast.Expr
primaryNoNewArrayExpressionExpressionToExpr p =
    case p of
      Syntax.PrimaryNoNewArrayExpressionLiteral v0 -> literalToExpr v0
      Syntax.PrimaryNoNewArrayExpressionClassLiteral v0 -> classLiteralToExpr v0
      Syntax.PrimaryNoNewArrayExpressionThis -> Serialization.cst "this"
      Syntax.PrimaryNoNewArrayExpressionDotThis v0 -> Serialization.dotSep [
        typeNameToExpr v0,
        (Serialization.cst "this")]
      Syntax.PrimaryNoNewArrayExpressionParens v0 -> Serialization.parenList False [
        expressionToExpr v0]
      Syntax.PrimaryNoNewArrayExpressionClassInstance v0 -> classInstanceCreationExpressionToExpr v0
      Syntax.PrimaryNoNewArrayExpressionFieldAccess v0 -> fieldAccessToExpr v0
      Syntax.PrimaryNoNewArrayExpressionArrayAccess v0 -> arrayAccessToExpr v0
      Syntax.PrimaryNoNewArrayExpressionMethodInvocation v0 -> methodInvocationToExpr v0
      Syntax.PrimaryNoNewArrayExpressionMethodReference v0 -> methodReferenceToExpr v0
primaryToExpr :: Syntax.Primary -> Ast.Expr
primaryToExpr p =
    case p of
      Syntax.PrimaryNoNewArray v0 -> primaryNoNewArrayExpressionExpressionToExpr v0
      Syntax.PrimaryArrayCreation v0 -> arrayCreationExpressionToExpr v0
primitiveTypeToExpr :: Syntax.PrimitiveType -> Ast.Expr
primitiveTypeToExpr pt =
    case pt of
      Syntax.PrimitiveTypeNumeric v0 -> numericTypeToExpr v0
      Syntax.PrimitiveTypeBoolean -> Serialization.cst "boolean"
primitiveTypeWithAnnotationsToExpr :: Syntax.PrimitiveTypeWithAnnotations -> Ast.Expr
primitiveTypeWithAnnotationsToExpr ptwa =

      let pt = Syntax.primitiveTypeWithAnnotationsType ptwa
          anns = Syntax.primitiveTypeWithAnnotationsAnnotations ptwa
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.spaceSep (Lists.map annotationToExpr anns))),
        (Just (primitiveTypeToExpr pt))]))
receiverParameterToExpr :: t0 -> Ast.Expr
receiverParameterToExpr _ = Serialization.cst "STUB:ReceiverParameter"
referenceTypeToExpr :: Syntax.ReferenceType -> Ast.Expr
referenceTypeToExpr rt =
    case rt of
      Syntax.ReferenceTypeClassOrInterface v0 -> classOrInterfaceTypeToExpr v0
      Syntax.ReferenceTypeVariable v0 -> typeVariableToExpr v0
      Syntax.ReferenceTypeArray v0 -> arrayTypeToExpr v0
relationalExpressionGreaterThanEqualToExpr :: Syntax.RelationalExpression_GreaterThanEqual -> Ast.Expr
relationalExpressionGreaterThanEqualToExpr gte =
    Serialization.infixWs ">=" (relationalExpressionToExpr (Syntax.relationalExpression_GreaterThanEqualLhs gte)) (shiftExpressionToExpr (Syntax.relationalExpression_GreaterThanEqualRhs gte))
relationalExpressionGreaterThanToExpr :: Syntax.RelationalExpression_GreaterThan -> Ast.Expr
relationalExpressionGreaterThanToExpr gt =
    Serialization.infixWs ">" (relationalExpressionToExpr (Syntax.relationalExpression_GreaterThanLhs gt)) (shiftExpressionToExpr (Syntax.relationalExpression_GreaterThanRhs gt))
relationalExpressionInstanceOfToExpr :: Syntax.RelationalExpression_InstanceOf -> Ast.Expr
relationalExpressionInstanceOfToExpr io =
    Serialization.infixWs "instanceof" (relationalExpressionToExpr (Syntax.relationalExpression_InstanceOfLhs io)) (referenceTypeToExpr (Syntax.relationalExpression_InstanceOfRhs io))
relationalExpressionLessThanEqualToExpr :: Syntax.RelationalExpression_LessThanEqual -> Ast.Expr
relationalExpressionLessThanEqualToExpr lte =
    Serialization.infixWs "<=" (relationalExpressionToExpr (Syntax.relationalExpression_LessThanEqualLhs lte)) (shiftExpressionToExpr (Syntax.relationalExpression_LessThanEqualRhs lte))
relationalExpressionLessThanToExpr :: Syntax.RelationalExpression_LessThan -> Ast.Expr
relationalExpressionLessThanToExpr lt =
    Serialization.infixWs "<" (relationalExpressionToExpr (Syntax.relationalExpression_LessThanLhs lt)) (shiftExpressionToExpr (Syntax.relationalExpression_LessThanRhs lt))
relationalExpressionToExpr :: Syntax.RelationalExpression -> Ast.Expr
relationalExpressionToExpr e =
    case e of
      Syntax.RelationalExpressionSimple v0 -> shiftExpressionToExpr v0
      Syntax.RelationalExpressionLessThan v0 -> relationalExpressionLessThanToExpr v0
      Syntax.RelationalExpressionGreaterThan v0 -> relationalExpressionGreaterThanToExpr v0
      Syntax.RelationalExpressionLessThanEqual v0 -> relationalExpressionLessThanEqualToExpr v0
      Syntax.RelationalExpressionGreaterThanEqual v0 -> relationalExpressionGreaterThanEqualToExpr v0
      Syntax.RelationalExpressionInstanceof v0 -> relationalExpressionInstanceOfToExpr v0
resultToExpr :: Syntax.Result -> Ast.Expr
resultToExpr r =
    case r of
      Syntax.ResultType v0 -> unannTypeToExpr v0
      Syntax.ResultVoid -> Serialization.cst "void"
returnStatementToExpr :: Syntax.ReturnStatement -> Ast.Expr
returnStatementToExpr rs =

      let mex = Syntax.unReturnStatement rs
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "return"),
        (Maybes.map expressionToExpr mex)])))
-- | Sanitize a string for use in a Java comment
sanitizeJavaComment :: String -> String
sanitizeJavaComment s = Strings.intercalate "&gt;" (Strings.splitOn ">" (Strings.intercalate "&lt;" (Strings.splitOn "<" s)))
shiftExpressionToExpr :: Syntax.ShiftExpression -> Ast.Expr
shiftExpressionToExpr e =
    case e of
      Syntax.ShiftExpressionUnary v0 -> additiveExpressionToExpr v0
      Syntax.ShiftExpressionShiftLeft v0 -> Serialization.infixWs "<<" (shiftExpressionToExpr (Syntax.shiftExpression_BinaryLhs v0)) (additiveExpressionToExpr (Syntax.shiftExpression_BinaryRhs v0))
      Syntax.ShiftExpressionShiftRight v0 -> Serialization.infixWs ">>" (shiftExpressionToExpr (Syntax.shiftExpression_BinaryLhs v0)) (additiveExpressionToExpr (Syntax.shiftExpression_BinaryRhs v0))
      Syntax.ShiftExpressionShiftRightZeroFill v0 -> Serialization.infixWs ">>>" (shiftExpressionToExpr (Syntax.shiftExpression_BinaryLhs v0)) (additiveExpressionToExpr (Syntax.shiftExpression_BinaryRhs v0))
simpleTypeNameToExpr :: Syntax.SimpleTypeName -> Ast.Expr
simpleTypeNameToExpr stn = typeIdentifierToExpr (Syntax.unSimpleTypeName stn)
singleElementAnnotationToExpr :: Syntax.SingleElementAnnotation -> Ast.Expr
singleElementAnnotationToExpr sea =

      let tname = Syntax.singleElementAnnotationName sea
          mv = Syntax.singleElementAnnotationValue sea
      in (Maybes.maybe (markerAnnotationToExpr (Syntax.MarkerAnnotation tname)) (\v -> Serialization.prefix "@" (Serialization.noSep [
        typeNameToExpr tname,
        (Serialization.parenList False [
          elementValueToExpr v])])) mv)
-- | Create a single-line Java comment. Empty text emits `//` (no trailing space) so blank line comments don't carry trailing whitespace.
singleLineComment :: String -> Ast.Expr
singleLineComment c =

      let sanitized = sanitizeJavaComment c
      in (Serialization.cst (Logic.ifElse (Equality.equal sanitized "") "//" (Strings.cat2 "// " sanitized)))
statementExpressionToExpr :: Syntax.StatementExpression -> Ast.Expr
statementExpressionToExpr e =
    case e of
      Syntax.StatementExpressionAssignment v0 -> assignmentToExpr v0
      Syntax.StatementExpressionPreIncrement v0 -> preIncrementExpressionToExpr v0
      Syntax.StatementExpressionPreDecrement v0 -> preDecrementExpressionToExpr v0
      Syntax.StatementExpressionPostIncrement v0 -> postIncrementExpressionToExpr v0
      Syntax.StatementExpressionPostDecrement v0 -> postDecrementExpressionToExpr v0
      Syntax.StatementExpressionMethodInvocation v0 -> methodInvocationToExpr v0
      Syntax.StatementExpressionClassInstanceCreation v0 -> classInstanceCreationExpressionToExpr v0
statementToExpr :: Syntax.Statement -> Ast.Expr
statementToExpr s =
    case s of
      Syntax.StatementWithoutTrailing v0 -> statementWithoutTrailingSubstatementToExpr v0
      Syntax.StatementLabeled v0 -> labeledStatementToExpr v0
      Syntax.StatementIfThen v0 -> ifThenStatementToExpr v0
      Syntax.StatementIfThenElse v0 -> ifThenElseStatementToExpr v0
      Syntax.StatementWhile v0 -> whileStatementToExpr v0
      Syntax.StatementFor v0 -> forStatementToExpr v0
statementWithoutTrailingSubstatementToExpr :: Syntax.StatementWithoutTrailingSubstatement -> Ast.Expr
statementWithoutTrailingSubstatementToExpr s =
    case s of
      Syntax.StatementWithoutTrailingSubstatementBlock v0 -> blockToExpr v0
      Syntax.StatementWithoutTrailingSubstatementEmpty -> Serialization.cst ";"
      Syntax.StatementWithoutTrailingSubstatementExpression v0 -> expressionStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementAssert v0 -> assertStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementSwitch v0 -> switchStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementDo v0 -> doStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementBreak v0 -> breakStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementContinue v0 -> continueStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementReturn v0 -> returnStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementSynchronized v0 -> synchronizedStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementThrow v0 -> throwStatementToExpr v0
      Syntax.StatementWithoutTrailingSubstatementTry v0 -> tryStatementToExpr v0
staticInitializerToExpr :: t0 -> Ast.Expr
staticInitializerToExpr _ = Serialization.cst "STUB:StaticInitializer"
-- | Serialize a Java string literal with proper Unicode escaping.
stringLiteralToExpr :: Syntax.StringLiteral -> Ast.Expr
stringLiteralToExpr sl =

      let s = Syntax.unStringLiteral sl
      in (Serialization.cst (Strings.cat2 "\"" (Strings.cat2 (escapeJavaString s) "\"")))
switchStatementToExpr :: t0 -> Ast.Expr
switchStatementToExpr _ = Serialization.cst "STUB:SwitchStatement"
synchronizedStatementToExpr :: t0 -> Ast.Expr
synchronizedStatementToExpr _ = Serialization.cst "STUB:SynchronizedStatement"
throwStatementToExpr :: Syntax.ThrowStatement -> Ast.Expr
throwStatementToExpr ts =
    Serialization.withSemi (Serialization.spaceSep [
      Serialization.cst "throw",
      (expressionToExpr (Syntax.unThrowStatement ts))])
throwsToExpr :: t0 -> Ast.Expr
throwsToExpr _ = Serialization.cst "STUB:Throws"
tryStatementToExpr :: t0 -> Ast.Expr
tryStatementToExpr _ = Serialization.cst "STUB:TryStatement"
typeArgumentToExpr :: Syntax.TypeArgument -> Ast.Expr
typeArgumentToExpr a =
    case a of
      Syntax.TypeArgumentReference v0 -> referenceTypeToExpr v0
      Syntax.TypeArgumentWildcard v0 -> wildcardToExpr v0
typeArgumentsOrDiamondToExpr :: Syntax.TypeArgumentsOrDiamond -> Ast.Expr
typeArgumentsOrDiamondToExpr targs =
    case targs of
      Syntax.TypeArgumentsOrDiamondArguments v0 -> Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeArgumentToExpr v0)
      Syntax.TypeArgumentsOrDiamondDiamond -> Serialization.cst "<>"
typeBoundToExpr :: Syntax.TypeBound -> Ast.Expr
typeBoundToExpr b =
    case b of
      Syntax.TypeBoundVariable v0 -> typeVariableToExpr v0
      Syntax.TypeBoundClassOrInterface v0 ->
        let cit = Syntax.typeBound_ClassOrInterfaceType v0
            additional = Syntax.typeBound_ClassOrInterfaceAdditional v0
        in (Logic.ifElse (Lists.null additional) (classOrInterfaceTypeToExpr cit) (Serialization.spaceSep (Lists.cons (classOrInterfaceTypeToExpr cit) (Lists.map additionalBoundToExpr additional))))
typeDeclarationToExpr :: Syntax.TypeDeclaration -> Ast.Expr
typeDeclarationToExpr d =
    case d of
      Syntax.TypeDeclarationClass v0 -> classDeclarationToExpr v0
      Syntax.TypeDeclarationInterface v0 -> interfaceDeclarationToExpr v0
      Syntax.TypeDeclarationNone -> Serialization.cst ";"
typeDeclarationWithCommentsToExpr :: Syntax.TypeDeclarationWithComments -> Ast.Expr
typeDeclarationWithCommentsToExpr tdwc =

      let d = Syntax.typeDeclarationWithCommentsValue tdwc
          mc = Syntax.typeDeclarationWithCommentsComments tdwc
      in (withComments mc (typeDeclarationToExpr d))
typeIdentifierToExpr :: Syntax.TypeIdentifier -> Ast.Expr
typeIdentifierToExpr tid = identifierToExpr (Syntax.unTypeIdentifier tid)
typeNameToExpr :: Syntax.TypeName -> Ast.Expr
typeNameToExpr tn =

      let id = Syntax.typeNameIdentifier tn
          mqual = Syntax.typeNameQualifier tn
      in (Serialization.dotSep (Maybes.cat [
        Maybes.map packageOrTypeNameToExpr mqual,
        (Just (typeIdentifierToExpr id))]))
typeParameterModifierToExpr :: Syntax.TypeParameterModifier -> Ast.Expr
typeParameterModifierToExpr tpm = annotationToExpr (Syntax.unTypeParameterModifier tpm)
typeParameterToExpr :: Syntax.TypeParameter -> Ast.Expr
typeParameterToExpr tp =

      let mods = Syntax.typeParameterModifiers tp
          id = Syntax.typeParameterIdentifier tp
          bound = Syntax.typeParameterBound tp
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map typeParameterModifierToExpr mods))),
        (Just (typeIdentifierToExpr id)),
        (Maybes.map (\b -> Serialization.spaceSep [
          Serialization.cst "extends",
          (typeBoundToExpr b)]) bound)]))
typeToExpr :: Syntax.Type -> Ast.Expr
typeToExpr t =
    case t of
      Syntax.TypePrimitive v0 -> primitiveTypeWithAnnotationsToExpr v0
      Syntax.TypeReference v0 -> referenceTypeToExpr v0
typeVariableToExpr :: Syntax.TypeVariable -> Ast.Expr
typeVariableToExpr tv =

      let anns = Syntax.typeVariableAnnotations tv
          id = Syntax.typeVariableIdentifier tv
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.spaceSep (Lists.map annotationToExpr anns))),
        (Just (typeIdentifierToExpr id))]))
unannTypeToExpr :: Syntax.UnannType -> Ast.Expr
unannTypeToExpr ut = typeToExpr (Syntax.unUnannType ut)
unaryExpressionNotPlusMinusToExpr :: Syntax.UnaryExpressionNotPlusMinus -> Ast.Expr
unaryExpressionNotPlusMinusToExpr e =
    case e of
      Syntax.UnaryExpressionNotPlusMinusPostfix v0 -> postfixExpressionToExpr v0
      Syntax.UnaryExpressionNotPlusMinusTilde v0 -> Serialization.spaceSep [
        Serialization.cst "~",
        (unaryExpressionToExpr v0)]
      Syntax.UnaryExpressionNotPlusMinusNot v0 -> Serialization.noSep [
        Serialization.cst "!",
        (unaryExpressionToExpr v0)]
      Syntax.UnaryExpressionNotPlusMinusCast v0 -> castExpressionToExpr v0
unaryExpressionToExpr :: Syntax.UnaryExpression -> Ast.Expr
unaryExpressionToExpr e =
    case e of
      Syntax.UnaryExpressionPreIncrement v0 -> preIncrementExpressionToExpr v0
      Syntax.UnaryExpressionPreDecrement v0 -> preDecrementExpressionToExpr v0
      Syntax.UnaryExpressionPlus v0 -> Serialization.spaceSep [
        Serialization.cst "+",
        (unaryExpressionToExpr v0)]
      Syntax.UnaryExpressionMinus v0 -> Serialization.spaceSep [
        Serialization.cst "-",
        (unaryExpressionToExpr v0)]
      Syntax.UnaryExpressionOther v0 -> unaryExpressionNotPlusMinusToExpr v0
unqualifiedClassInstanceCreationExpressionToExpr :: Syntax.UnqualifiedClassInstanceCreationExpression -> Ast.Expr
unqualifiedClassInstanceCreationExpressionToExpr ucice =

      let targs = Syntax.unqualifiedClassInstanceCreationExpressionTypeArguments ucice
          cit = Syntax.unqualifiedClassInstanceCreationExpressionClassOrInterface ucice
          args = Syntax.unqualifiedClassInstanceCreationExpressionArguments ucice
          mbody = Syntax.unqualifiedClassInstanceCreationExpressionBody ucice
      in (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "new"),
        (Logic.ifElse (Lists.null targs) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map typeArgumentToExpr targs)))),
        (Just (Serialization.noSep [
          classOrInterfaceTypeToInstantiateToExpr cit,
          (Serialization.parenList False (Lists.map expressionToExpr args))])),
        (Maybes.map classBodyToExpr mbody)]))
variableArityParameterToExpr :: t0 -> Ast.Expr
variableArityParameterToExpr _ = Serialization.cst "STUB:VariableArityParameter"
variableDeclaratorIdToExpr :: Syntax.VariableDeclaratorId -> Ast.Expr
variableDeclaratorIdToExpr vdi =

      let id = Syntax.variableDeclaratorIdIdentifier vdi
          mdims = Syntax.variableDeclaratorIdDims vdi
      in (Serialization.noSep (Maybes.cat [
        Just (identifierToExpr id),
        (Maybes.map dimsToExpr mdims)]))
variableDeclaratorToExpr :: Syntax.VariableDeclarator -> Ast.Expr
variableDeclaratorToExpr vd =

      let id = Syntax.variableDeclaratorId vd
          minit = Syntax.variableDeclaratorInitializer vd
          idSec = variableDeclaratorIdToExpr id
      in (Maybes.maybe idSec (\init -> Serialization.infixWs "=" idSec (variableInitializerToExpr init)) minit)
variableInitializerToExpr :: Syntax.VariableInitializer -> Ast.Expr
variableInitializerToExpr i =
    case i of
      Syntax.VariableInitializerExpression v0 -> expressionToExpr v0
      Syntax.VariableInitializerArrayInitializer v0 -> arrayInitializerToExpr v0
variableModifierToExpr :: Syntax.VariableModifier -> Ast.Expr
variableModifierToExpr m =
    case m of
      Syntax.VariableModifierAnnotation v0 -> annotationToExpr v0
      Syntax.VariableModifierFinal -> Serialization.cst "final"
whileStatementToExpr :: Syntax.WhileStatement -> Ast.Expr
whileStatementToExpr ws =

      let mcond = Syntax.whileStatementCond ws
          body = Syntax.whileStatementBody ws
          condSer = Maybes.maybe (Serialization.cst "true") (\c -> expressionToExpr c) mcond
      in (Serialization.spaceSep [
        Serialization.cst "while",
        (Serialization.parenList False [
          condSer]),
        (Serialization.curlyBlock Serialization.fullBlockStyle (statementToExpr body))])
wildcardBoundsToExpr :: Syntax.WildcardBounds -> Ast.Expr
wildcardBoundsToExpr b =
    case b of
      Syntax.WildcardBoundsExtends v0 -> Serialization.spaceSep [
        Serialization.cst "extends",
        (referenceTypeToExpr v0)]
      Syntax.WildcardBoundsSuper v0 -> Serialization.spaceSep [
        Serialization.cst "super",
        (referenceTypeToExpr v0)]
wildcardToExpr :: Syntax.Wildcard -> Ast.Expr
wildcardToExpr w =

      let anns = Syntax.wildcardAnnotations w
          mbounds = Syntax.wildcardWildcard w
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map annotationToExpr anns))),
        (Just (Serialization.cst "*")),
        (Maybes.map wildcardBoundsToExpr mbounds)]))
-- | Wrap an expression with optional Javadoc comments. Blank lines inside the doc body emit ` *` (no trailing space) instead of ` * `.
withComments :: Maybe String -> Ast.Expr -> Ast.Expr
withComments mc expr =
    Maybes.maybe expr (\c -> Serialization.newlineSep [
      Serialization.cst (Strings.cat2 "/**\n" (Strings.cat2 (Strings.intercalate "\n" (Lists.map (\l -> Logic.ifElse (Equality.equal l "") " *" (Strings.cat2 " * " l)) (Strings.lines (sanitizeJavaComment c)))) "\n */")),
      expr]) mc
