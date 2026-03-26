-- Note: this is an automatically generated file. Do not edit.

-- | Java serializer: converts Java AST to concrete syntax

module Hydra.Ext.Java.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

escapeJavaChar :: Int -> String
escapeJavaChar c =
    Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Logic.ifElse (Equality.equal c 9) "\\t" (Logic.ifElse (Equality.equal c 8) "\\b" (Logic.ifElse (Equality.equal c 12) "\\f" (Logic.ifElse (Logic.and (Equality.gte c 32) (Equality.lt c 127)) (Strings.fromList [
      c]) (javaUnicodeEscape c))))))))

escapeJavaString :: String -> String
escapeJavaString s = Strings.cat (Lists.map (\c -> escapeJavaChar c) (Strings.toList s))

hexDigit :: Int -> Int
hexDigit n = Logic.ifElse (Equality.lt n 10) (Math.add n 48) (Math.add (Math.sub n 10) 65)

javaUnicodeEscape :: Int -> String
javaUnicodeEscape n =
    Logic.ifElse (Equality.gt n 65535) (
      let n_ = Math.sub n 65536
          hi = Math.add 55296 (Math.div n_ 1024)
          lo = Math.add 56320 (Math.mod n_ 1024)
      in (Strings.cat2 (Strings.cat2 "\\u" (padHex4 hi)) (Strings.cat2 "\\u" (padHex4 lo)))) (Strings.cat2 "\\u" (padHex4 n))

padHex4 :: Int -> String
padHex4 n =

      let d3 = Math.div n 4096
          r3 = Math.mod n 4096
          d2 = Math.div r3 256
          r2 = Math.mod r3 256
          d1 = Math.div r2 16
          d0 = Math.mod r2 16
      in (Strings.fromList [
        hexDigit d3,
        (hexDigit d2),
        (hexDigit d1),
        (hexDigit d0)])

-- | Sanitize a string for use in a Java comment
sanitizeJavaComment :: String -> String
sanitizeJavaComment s = Strings.intercalate "&gt;" (Strings.splitOn ">" (Strings.intercalate "&lt;" (Strings.splitOn "<" s)))

-- | Create a single-line Java comment
singleLineComment :: String -> Ast.Expr
singleLineComment c = Serialization.cst (Strings.cat2 "// " (sanitizeJavaComment c))

-- | Wrap an expression with optional Javadoc comments
withComments :: Maybe String -> Ast.Expr -> Ast.Expr
withComments mc expr =
    Maybes.maybe expr (\c -> Serialization.newlineSep [
      Serialization.cst (Strings.cat2 "/**\n" (Strings.cat2 (Strings.intercalate "\n" (Lists.map (\l -> Strings.cat2 " * " l) (Strings.lines (sanitizeJavaComment c)))) "\n */")),
      expr]) mc

writeAdditionalBound :: Syntax.AdditionalBound -> Ast.Expr
writeAdditionalBound ab =
    Serialization.spaceSep [
      Serialization.cst "&",
      (writeInterfaceType (Syntax.unAdditionalBound ab))]

writeAdditiveExpression :: Syntax.AdditiveExpression -> Ast.Expr
writeAdditiveExpression e =
    case e of
      Syntax.AdditiveExpressionUnary v0 -> writeMultiplicativeExpression v0
      Syntax.AdditiveExpressionPlus v0 -> Serialization.infixWs "+" (writeAdditiveExpression (Syntax.additiveExpression_BinaryLhs v0)) (writeMultiplicativeExpression (Syntax.additiveExpression_BinaryRhs v0))
      Syntax.AdditiveExpressionMinus v0 -> Serialization.infixWs "-" (writeAdditiveExpression (Syntax.additiveExpression_BinaryLhs v0)) (writeMultiplicativeExpression (Syntax.additiveExpression_BinaryRhs v0))

writeAmbiguousName :: Syntax.AmbiguousName -> Ast.Expr
writeAmbiguousName an = Serialization.dotSep (Lists.map writeIdentifier (Syntax.unAmbiguousName an))

writeAndExpression :: Syntax.AndExpression -> Ast.Expr
writeAndExpression ae = Serialization.infixWsList "&" (Lists.map writeEqualityExpression (Syntax.unAndExpression ae))

writeAnnotatedIdentifier :: Syntax.AnnotatedIdentifier -> Ast.Expr
writeAnnotatedIdentifier ai = writeIdentifier (Syntax.annotatedIdentifierIdentifier ai)

writeAnnotation :: Syntax.Annotation -> Ast.Expr
writeAnnotation ann =
    case ann of
      Syntax.AnnotationNormal v0 -> writeNormalAnnotation v0
      Syntax.AnnotationMarker v0 -> writeMarkerAnnotation v0
      Syntax.AnnotationSingleElement v0 -> writeSingleElementAnnotation v0

writeAnnotationTypeDeclaration :: t0 -> Ast.Expr
writeAnnotationTypeDeclaration _ = Serialization.cst "STUB:AnnotationTypeDeclaration"

writeArrayAccess :: t0 -> Ast.Expr
writeArrayAccess _ = Serialization.cst "STUB:ArrayAccess"

writeArrayCreationExpression :: Syntax.ArrayCreationExpression -> Ast.Expr
writeArrayCreationExpression ace =
    case ace of
      Syntax.ArrayCreationExpressionPrimitiveArray v0 ->
        let pt = Syntax.arrayCreationExpression_PrimitiveArrayType v0
            ai = Syntax.arrayCreationExpression_PrimitiveArrayArray v0
        in (Serialization.spaceSep [
          Serialization.cst "new",
          (Serialization.noSep [
            writePrimitiveTypeWithAnnotations pt,
            (Serialization.cst "[]")]),
          (writeArrayInitializer ai)])
      Syntax.ArrayCreationExpressionClassOrInterfaceArray _ -> Serialization.cst "STUB:ArrayCreationExpression"
      Syntax.ArrayCreationExpressionPrimitive _ -> Serialization.cst "STUB:ArrayCreationExpression"
      Syntax.ArrayCreationExpressionClassOrInterface _ -> Serialization.cst "STUB:ArrayCreationExpression"

writeArrayInitializer :: Syntax.ArrayInitializer -> Ast.Expr
writeArrayInitializer ai =

      let groups = Syntax.unArrayInitializer ai
      in (Logic.ifElse (Equality.equal (Lists.length groups) 1) (Serialization.noSep [
        Serialization.cst "{",
        (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableInitializer (Lists.head groups))),
        (Serialization.cst "}")]) (Serialization.cst "{}"))

writeArrayType :: Syntax.ArrayType -> Ast.Expr
writeArrayType at =

      let dims = Syntax.arrayTypeDims at
          variant = Syntax.arrayTypeVariant at
          varExpr =
                  case variant of
                    Syntax.ArrayType_VariantPrimitive v0 -> writePrimitiveTypeWithAnnotations v0
                    Syntax.ArrayType_VariantClassOrInterface v0 -> writeClassOrInterfaceType v0
                    Syntax.ArrayType_VariantVariable v0 -> writeTypeVariable v0
      in (Serialization.noSep [
        varExpr,
        (writeDims dims)])

writeAssertStatement :: t0 -> Ast.Expr
writeAssertStatement _ = Serialization.cst "STUB:AssertStatement"

writeAssignment :: Syntax.Assignment -> Ast.Expr
writeAssignment a =

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
      in (Serialization.infixWs ctop (writeLeftHandSide lhs) (writeExpression rhs))

writeAssignmentExpression :: Syntax.AssignmentExpression -> Ast.Expr
writeAssignmentExpression e =
    case e of
      Syntax.AssignmentExpressionConditional v0 -> writeConditionalExpression v0
      Syntax.AssignmentExpressionAssignment v0 -> writeAssignment v0

writeBlock :: Syntax.Block -> Ast.Expr
writeBlock b =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map writeBlockStatement (Syntax.unBlock b)))

writeBlockStatement :: Syntax.BlockStatement -> Ast.Expr
writeBlockStatement s =
    case s of
      Syntax.BlockStatementLocalVariableDeclaration v0 -> writeLocalVariableDeclarationStatement v0
      Syntax.BlockStatementClass v0 -> writeClassDeclaration v0
      Syntax.BlockStatementStatement v0 -> writeStatement v0

writeBreakStatement :: Syntax.BreakStatement -> Ast.Expr
writeBreakStatement bs =

      let mlabel = Syntax.unBreakStatement bs
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "break"),
        (Maybes.map writeIdentifier mlabel)])))

writeCastExpression :: Syntax.CastExpression -> Ast.Expr
writeCastExpression e =
    case e of
      Syntax.CastExpressionPrimitive v0 -> writeCastExpression_Primitive v0
      Syntax.CastExpressionNotPlusMinus v0 -> writeCastExpression_NotPlusMinus v0
      Syntax.CastExpressionLambda v0 -> writeCastExpression_Lambda v0

writeCastExpression_Lambda :: t0 -> Ast.Expr
writeCastExpression_Lambda _ = Serialization.cst "STUB:CastExpression_Lambda"

writeCastExpression_NotPlusMinus :: Syntax.CastExpression_NotPlusMinus -> Ast.Expr
writeCastExpression_NotPlusMinus npm =

      let rb = Syntax.castExpression_NotPlusMinusRefAndBounds npm
          ex = Syntax.castExpression_NotPlusMinusExpression npm
      in (Serialization.spaceSep [
        writeCastExpression_RefAndBounds rb,
        (writeUnaryExpression ex)])

writeCastExpression_Primitive :: Syntax.CastExpression_Primitive -> Ast.Expr
writeCastExpression_Primitive cp =

      let pt = Syntax.castExpression_PrimitiveType cp
          ex = Syntax.castExpression_PrimitiveExpression cp
      in (Serialization.spaceSep [
        Serialization.parenList False [
          writePrimitiveTypeWithAnnotations pt],
        (writeUnaryExpression ex)])

writeCastExpression_RefAndBounds :: Syntax.CastExpression_RefAndBounds -> Ast.Expr
writeCastExpression_RefAndBounds rab =

      let rt = Syntax.castExpression_RefAndBoundsType rab
          adds = Syntax.castExpression_RefAndBoundsBounds rab
      in (Serialization.parenList False [
        Serialization.spaceSep (Maybes.cat [
          Just (writeReferenceType rt),
          (Logic.ifElse (Lists.null adds) Nothing (Just (Serialization.spaceSep (Lists.map writeAdditionalBound adds))))])])

writeClassBody :: Syntax.ClassBody -> Ast.Expr
writeClassBody cb =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map writeClassBodyDeclarationWithComments (Syntax.unClassBody cb)))

writeClassBodyDeclaration :: Syntax.ClassBodyDeclaration -> Ast.Expr
writeClassBodyDeclaration d =
    case d of
      Syntax.ClassBodyDeclarationClassMember v0 -> writeClassMemberDeclaration v0
      Syntax.ClassBodyDeclarationInstanceInitializer v0 -> writeInstanceInitializer v0
      Syntax.ClassBodyDeclarationStaticInitializer v0 -> writeStaticInitializer v0
      Syntax.ClassBodyDeclarationConstructorDeclaration v0 -> writeConstructorDeclaration v0

writeClassBodyDeclarationWithComments :: Syntax.ClassBodyDeclarationWithComments -> Ast.Expr
writeClassBodyDeclarationWithComments cbdwc =

      let d = Syntax.classBodyDeclarationWithCommentsValue cbdwc
          mc = Syntax.classBodyDeclarationWithCommentsComments cbdwc
      in (withComments mc (writeClassBodyDeclaration d))

writeClassDeclaration :: Syntax.ClassDeclaration -> Ast.Expr
writeClassDeclaration d =
    case d of
      Syntax.ClassDeclarationNormal v0 -> writeNormalClassDeclaration v0
      Syntax.ClassDeclarationEnum v0 -> writeEnumDeclaration v0

writeClassInstanceCreationExpression :: Syntax.ClassInstanceCreationExpression -> Ast.Expr
writeClassInstanceCreationExpression cice =

      let mqual = Syntax.classInstanceCreationExpressionQualifier cice
          e = Syntax.classInstanceCreationExpressionExpression cice
      in (Maybes.maybe (writeUnqualifiedClassInstanceCreationExpression e) (\q -> Serialization.dotSep [
        writeClassInstanceCreationExpression_Qualifier q,
        (writeUnqualifiedClassInstanceCreationExpression e)]) mqual)

writeClassInstanceCreationExpression_Qualifier :: Syntax.ClassInstanceCreationExpression_Qualifier -> Ast.Expr
writeClassInstanceCreationExpression_Qualifier q =
    case q of
      Syntax.ClassInstanceCreationExpression_QualifierExpression v0 -> writeExpressionName v0
      Syntax.ClassInstanceCreationExpression_QualifierPrimary v0 -> writePrimary v0

writeClassLiteral :: t0 -> Ast.Expr
writeClassLiteral _ = Serialization.cst "STUB:ClassLiteral"

writeClassMemberDeclaration :: Syntax.ClassMemberDeclaration -> Ast.Expr
writeClassMemberDeclaration d =
    case d of
      Syntax.ClassMemberDeclarationField v0 -> writeFieldDeclaration v0
      Syntax.ClassMemberDeclarationMethod v0 -> writeMethodDeclaration v0
      Syntax.ClassMemberDeclarationClass v0 -> writeClassDeclaration v0
      Syntax.ClassMemberDeclarationInterface v0 -> writeInterfaceDeclaration v0
      Syntax.ClassMemberDeclarationNone -> Serialization.cst ";"

writeClassModifier :: Syntax.ClassModifier -> Ast.Expr
writeClassModifier m =
    case m of
      Syntax.ClassModifierAnnotation v0 -> writeAnnotation v0
      Syntax.ClassModifierPublic -> Serialization.cst "public"
      Syntax.ClassModifierProtected -> Serialization.cst "protected"
      Syntax.ClassModifierPrivate -> Serialization.cst "private"
      Syntax.ClassModifierAbstract -> Serialization.cst "abstract"
      Syntax.ClassModifierStatic -> Serialization.cst "static"
      Syntax.ClassModifierFinal -> Serialization.cst "final"
      Syntax.ClassModifierStrictfp -> Serialization.cst "strictfp"

writeClassOrInterfaceType :: Syntax.ClassOrInterfaceType -> Ast.Expr
writeClassOrInterfaceType cit =
    case cit of
      Syntax.ClassOrInterfaceTypeClass v0 -> writeClassType v0
      Syntax.ClassOrInterfaceTypeInterface v0 -> writeInterfaceType v0

writeClassOrInterfaceTypeToInstantiate :: Syntax.ClassOrInterfaceTypeToInstantiate -> Ast.Expr
writeClassOrInterfaceTypeToInstantiate coitti =

      let ids = Syntax.classOrInterfaceTypeToInstantiateIdentifiers coitti
          margs = Syntax.classOrInterfaceTypeToInstantiateTypeArguments coitti
      in (Serialization.noSep (Maybes.cat [
        Just (Serialization.dotSep (Lists.map writeAnnotatedIdentifier ids)),
        (Maybes.map writeTypeArgumentsOrDiamond margs)]))

writeClassType :: Syntax.ClassType -> Ast.Expr
writeClassType ct =

      let anns = Syntax.classTypeAnnotations ct
          qual = Syntax.classTypeQualifier ct
          id = Syntax.classTypeIdentifier ct
          args = Syntax.classTypeArguments ct
          qualifiedId =
                  case qual of
                    Syntax.ClassTypeQualifierNone -> writeTypeIdentifier id
                    Syntax.ClassTypeQualifierPackage v0 -> Serialization.dotSep [
                      writePackageName v0,
                      (writeTypeIdentifier id)]
                    Syntax.ClassTypeQualifierParent v0 -> Serialization.dotSep [
                      writeClassOrInterfaceType v0,
                      (writeTypeIdentifier id)]
      in (Serialization.noSep (Maybes.cat [
        Just (Serialization.spaceSep (Maybes.cat [
          Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeAnnotation anns))),
          (Just qualifiedId)])),
        (Logic.ifElse (Lists.null args) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument args))))]))

writeCompilationUnit :: Syntax.CompilationUnit -> Ast.Expr
writeCompilationUnit u =
    case u of
      Syntax.CompilationUnitOrdinary v0 ->
        let mpkg = Syntax.ordinaryCompilationUnitPackage v0
            imports = Syntax.ordinaryCompilationUnitImports v0
            types = Syntax.ordinaryCompilationUnitTypes v0
            warning = Just (singleLineComment Constants.warningAutoGeneratedFile)
            pkgSec = Maybes.map writePackageDeclaration mpkg
            importsSec =
                    Logic.ifElse (Lists.null imports) Nothing (Just (Serialization.newlineSep (Lists.map writeImportDeclaration imports)))
            typesSec =
                    Logic.ifElse (Lists.null types) Nothing (Just (Serialization.doubleNewlineSep (Lists.map writeTypeDeclarationWithComments types)))
        in (Serialization.doubleNewlineSep (Maybes.cat [
          warning,
          pkgSec,
          importsSec,
          typesSec]))

writeConditionalAndExpression :: Syntax.ConditionalAndExpression -> Ast.Expr
writeConditionalAndExpression cae =
    Serialization.infixWsList "&&" (Lists.map writeInclusiveOrExpression (Syntax.unConditionalAndExpression cae))

writeConditionalExpression :: Syntax.ConditionalExpression -> Ast.Expr
writeConditionalExpression c =
    case c of
      Syntax.ConditionalExpressionSimple v0 -> writeConditionalOrExpression v0
      Syntax.ConditionalExpressionTernaryCond v0 -> writeConditionalExpression_TernaryCond v0
      Syntax.ConditionalExpressionTernaryLambda v0 -> writeConditionalExpression_TernaryLambda v0

writeConditionalExpression_TernaryCond :: t0 -> Ast.Expr
writeConditionalExpression_TernaryCond _ = Serialization.cst "STUB:ConditionalExpression_TernaryCond"

writeConditionalExpression_TernaryLambda :: t0 -> Ast.Expr
writeConditionalExpression_TernaryLambda _ = Serialization.cst "STUB:ConditionalExpression_TernaryLambda"

writeConditionalOrExpression :: Syntax.ConditionalOrExpression -> Ast.Expr
writeConditionalOrExpression coe =
    Serialization.infixWsList "||" (Lists.map writeConditionalAndExpression (Syntax.unConditionalOrExpression coe))

writeConstantDeclaration :: Syntax.ConstantDeclaration -> Ast.Expr
writeConstantDeclaration cd =

      let mods = Syntax.constantDeclarationModifiers cd
          typ = Syntax.constantDeclarationType cd
          vars = Syntax.constantDeclarationVariables cd
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeConstantModifier mods))),
        (Just (writeUnannType typ)),
        (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableDeclarator vars)))])))

writeConstantModifier :: t0 -> Ast.Expr
writeConstantModifier _ = Serialization.cst "STUB:ConstantModifier"

writeConstructorBody :: Syntax.ConstructorBody -> Ast.Expr
writeConstructorBody cb =

      let minvoc = Syntax.constructorBodyInvocation cb
          stmts = Syntax.constructorBodyStatements cb
      in (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Maybes.cat [
        Maybes.map writeExplicitConstructorInvocation minvoc,
        (Just (Serialization.newlineSep (Lists.map writeBlockStatement stmts)))])))

writeConstructorDeclaration :: Syntax.ConstructorDeclaration -> Ast.Expr
writeConstructorDeclaration cd =

      let mods = Syntax.constructorDeclarationModifiers cd
          cons = Syntax.constructorDeclarationConstructor cd
          mthrows = Syntax.constructorDeclarationThrows cd
          body = Syntax.constructorDeclarationBody cd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeConstructorModifier mods))),
        (Just (writeConstructorDeclarator cons)),
        (Maybes.map writeThrows mthrows),
        (Just (writeConstructorBody body))]))

writeConstructorDeclarator :: Syntax.ConstructorDeclarator -> Ast.Expr
writeConstructorDeclarator cd =

      let tparams = Syntax.constructorDeclaratorParameters cd
          name = Syntax.constructorDeclaratorName cd
          fparams = Syntax.constructorDeclaratorFormalParameters cd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeParameter tparams))),
        (Just (writeSimpleTypeName name)),
        (Just (Serialization.parenList False (Lists.map writeFormalParameter fparams)))]))

writeConstructorModifier :: Syntax.ConstructorModifier -> Ast.Expr
writeConstructorModifier m =
    case m of
      Syntax.ConstructorModifierAnnotation v0 -> writeAnnotation v0
      Syntax.ConstructorModifierPublic -> Serialization.cst "public"
      Syntax.ConstructorModifierProtected -> Serialization.cst "protected"
      Syntax.ConstructorModifierPrivate -> Serialization.cst "private"

writeContinueStatement :: Syntax.ContinueStatement -> Ast.Expr
writeContinueStatement cs =

      let mlabel = Syntax.unContinueStatement cs
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "continue"),
        (Maybes.map writeIdentifier mlabel)])))

writeDims :: Syntax.Dims -> Ast.Expr
writeDims d = Serialization.noSep (Lists.map (\_ -> Serialization.cst "[]") (Syntax.unDims d))

writeDoStatement :: t0 -> Ast.Expr
writeDoStatement _ = Serialization.cst "STUB:DoStatement"

writeElementValue :: Syntax.ElementValue -> Ast.Expr
writeElementValue ev =
    case ev of
      Syntax.ElementValueConditionalExpression v0 -> writeConditionalExpression v0
      Syntax.ElementValueElementValueArrayInitializer v0 -> Serialization.commaSep Serialization.inlineStyle (Lists.map writeElementValue (Syntax.unElementValueArrayInitializer v0))
      Syntax.ElementValueAnnotation v0 -> writeAnnotation v0

writeElementValuePair :: Syntax.ElementValuePair -> Ast.Expr
writeElementValuePair evp =

      let k = Syntax.elementValuePairKey evp
          v = Syntax.elementValuePairValue evp
      in (Serialization.infixWs "=" (writeIdentifier k) (writeElementValue v))

writeEnumDeclaration :: t0 -> Ast.Expr
writeEnumDeclaration _ = Serialization.cst "STUB:EnumDeclaration"

writeEqualityExpression :: Syntax.EqualityExpression -> Ast.Expr
writeEqualityExpression e =
    case e of
      Syntax.EqualityExpressionUnary v0 -> writeRelationalExpression v0
      Syntax.EqualityExpressionEqual v0 -> Serialization.infixWs "==" (writeEqualityExpression (Syntax.equalityExpression_BinaryLhs v0)) (writeRelationalExpression (Syntax.equalityExpression_BinaryRhs v0))
      Syntax.EqualityExpressionNotEqual v0 -> Serialization.infixWs "!=" (writeEqualityExpression (Syntax.equalityExpression_BinaryLhs v0)) (writeRelationalExpression (Syntax.equalityExpression_BinaryRhs v0))

writeExclusiveOrExpression :: Syntax.ExclusiveOrExpression -> Ast.Expr
writeExclusiveOrExpression eoe =
    Serialization.infixWsList "^" (Lists.map writeAndExpression (Syntax.unExclusiveOrExpression eoe))

writeExplicitConstructorInvocation :: t0 -> Ast.Expr
writeExplicitConstructorInvocation _ = Serialization.cst "STUB:ExplicitConstructorInvocation"

writeExpression :: Syntax.Expression -> Ast.Expr
writeExpression e =
    case e of
      Syntax.ExpressionLambda v0 -> writeLambdaExpression v0
      Syntax.ExpressionAssignment v0 -> writeAssignmentExpression v0

writeExpressionName :: Syntax.ExpressionName -> Ast.Expr
writeExpressionName en =

      let mqual = Syntax.expressionNameQualifier en
          id = Syntax.expressionNameIdentifier en
      in (Serialization.dotSep (Maybes.cat [
        Maybes.map writeAmbiguousName mqual,
        (Just (writeIdentifier id))]))

writeExpressionStatement :: Syntax.ExpressionStatement -> Ast.Expr
writeExpressionStatement es = Serialization.withSemi (writeStatementExpression (Syntax.unExpressionStatement es))

writeFieldAccess :: Syntax.FieldAccess -> Ast.Expr
writeFieldAccess fa =

      let qual = Syntax.fieldAccessQualifier fa
          id = Syntax.fieldAccessIdentifier fa
      in case qual of
        Syntax.FieldAccess_QualifierPrimary v0 -> Serialization.dotSep [
          writePrimary v0,
          (writeIdentifier id)]
        Syntax.FieldAccess_QualifierSuper -> Serialization.dotSep [
          Serialization.cst "super",
          (writeIdentifier id)]
        Syntax.FieldAccess_QualifierTyped v0 -> Serialization.dotSep [
          writeTypeName v0,
          (Serialization.cst "super"),
          (writeIdentifier id)]

writeFieldDeclaration :: Syntax.FieldDeclaration -> Ast.Expr
writeFieldDeclaration fd =

      let mods = Syntax.fieldDeclarationModifiers fd
          typ = Syntax.fieldDeclarationUnannType fd
          vars = Syntax.fieldDeclarationVariableDeclarators fd
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeFieldModifier mods))),
        (Just (writeUnannType typ)),
        (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableDeclarator vars)))])))

writeFieldModifier :: Syntax.FieldModifier -> Ast.Expr
writeFieldModifier m =
    case m of
      Syntax.FieldModifierAnnotation v0 -> writeAnnotation v0
      Syntax.FieldModifierPublic -> Serialization.cst "public"
      Syntax.FieldModifierProtected -> Serialization.cst "protected"
      Syntax.FieldModifierPrivate -> Serialization.cst "private"
      Syntax.FieldModifierStatic -> Serialization.cst "static"
      Syntax.FieldModifierFinal -> Serialization.cst "final"
      Syntax.FieldModifierTransient -> Serialization.cst "transient"
      Syntax.FieldModifierVolatile -> Serialization.cst "volatile"

writeFloatingPointLiteral :: Syntax.FloatingPointLiteral -> Ast.Expr
writeFloatingPointLiteral fl = Serialization.cst (Literals.showBigfloat (Syntax.unFloatingPointLiteral fl))

writeFloatingPointType :: Syntax.FloatingPointType -> Ast.Expr
writeFloatingPointType ft =
    case ft of
      Syntax.FloatingPointTypeFloat -> Serialization.cst "float"
      Syntax.FloatingPointTypeDouble -> Serialization.cst "double"

writeForStatement :: t0 -> Ast.Expr
writeForStatement _ = Serialization.cst "STUB:ForStatement"

writeFormalParameter :: Syntax.FormalParameter -> Ast.Expr
writeFormalParameter p =
    case p of
      Syntax.FormalParameterSimple v0 -> writeFormalParameter_Simple v0
      Syntax.FormalParameterVariableArity v0 -> writeVariableArityParameter v0

writeFormalParameter_Simple :: Syntax.FormalParameter_Simple -> Ast.Expr
writeFormalParameter_Simple fps =

      let mods = Syntax.formalParameter_SimpleModifiers fps
          typ = Syntax.formalParameter_SimpleType fps
          id = Syntax.formalParameter_SimpleId fps
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeVariableModifier mods))),
        (Just (writeUnannType typ)),
        (Just (writeVariableDeclaratorId id))]))

writeIdentifier :: Syntax.Identifier -> Ast.Expr
writeIdentifier id = Serialization.cst (Syntax.unIdentifier id)

writeIfThenElseStatement :: t0 -> Ast.Expr
writeIfThenElseStatement _ = Serialization.cst "STUB:IfThenElseStatement"

writeIfThenStatement :: Syntax.IfThenStatement -> Ast.Expr
writeIfThenStatement its =

      let cond = Syntax.ifThenStatementExpression its
          thn = Syntax.ifThenStatementStatement its
      in (Serialization.spaceSep [
        Serialization.cst "if",
        (Serialization.parenList False [
          writeExpression cond]),
        (Serialization.curlyBlock Serialization.fullBlockStyle (writeStatement thn))])

writeImportDeclaration :: Syntax.ImportDeclaration -> Ast.Expr
writeImportDeclaration imp =
    case imp of
      Syntax.ImportDeclarationSingleType v0 -> Serialization.withSemi (Serialization.spaceSep [
        Serialization.cst "import",
        (writeTypeName (Syntax.unSingleTypeImportDeclaration v0))])
      Syntax.ImportDeclarationTypeImportOnDemand _ -> Serialization.cst "STUB:ImportDeclarationTypeImportOnDemand"
      Syntax.ImportDeclarationSingleStaticImport _ -> Serialization.cst "STUB:ImportDeclarationSingleStaticImport"
      Syntax.ImportDeclarationStaticImportOnDemand _ -> Serialization.cst "STUB:ImportDeclarationStaticImportOnDemand"

writeInclusiveOrExpression :: Syntax.InclusiveOrExpression -> Ast.Expr
writeInclusiveOrExpression ioe =
    Serialization.infixWsList "|" (Lists.map writeExclusiveOrExpression (Syntax.unInclusiveOrExpression ioe))

writeInstanceInitializer :: t0 -> Ast.Expr
writeInstanceInitializer _ = Serialization.cst "STUB:InstanceInitializer"

writeIntegerLiteral :: Syntax.IntegerLiteral -> Ast.Expr
writeIntegerLiteral il =

      let i = Syntax.unIntegerLiteral il
          suffix = Logic.ifElse (Logic.or (Equality.gt i 2147483647) (Equality.lt i (-2147483648))) "L" ""
      in (Serialization.cst (Strings.cat2 (Literals.showBigint i) suffix))

writeIntegralType :: Syntax.IntegralType -> Ast.Expr
writeIntegralType t =
    case t of
      Syntax.IntegralTypeByte -> Serialization.cst "byte"
      Syntax.IntegralTypeShort -> Serialization.cst "short"
      Syntax.IntegralTypeInt -> Serialization.cst "int"
      Syntax.IntegralTypeLong -> Serialization.cst "long"
      Syntax.IntegralTypeChar -> Serialization.cst "char"

writeInterfaceBody :: Syntax.InterfaceBody -> Ast.Expr
writeInterfaceBody ib =
    Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map writeInterfaceMemberDeclaration (Syntax.unInterfaceBody ib)))

writeInterfaceDeclaration :: Syntax.InterfaceDeclaration -> Ast.Expr
writeInterfaceDeclaration d =
    case d of
      Syntax.InterfaceDeclarationNormalInterface v0 -> writeNormalInterfaceDeclaration v0
      Syntax.InterfaceDeclarationAnnotationType v0 -> writeAnnotationTypeDeclaration v0

writeInterfaceMemberDeclaration :: Syntax.InterfaceMemberDeclaration -> Ast.Expr
writeInterfaceMemberDeclaration d =
    case d of
      Syntax.InterfaceMemberDeclarationConstant v0 -> writeConstantDeclaration v0
      Syntax.InterfaceMemberDeclarationInterfaceMethod v0 -> writeInterfaceMethodDeclaration v0
      Syntax.InterfaceMemberDeclarationClass v0 -> writeClassDeclaration v0
      Syntax.InterfaceMemberDeclarationInterface v0 -> writeInterfaceDeclaration v0

writeInterfaceMethodDeclaration :: Syntax.InterfaceMethodDeclaration -> Ast.Expr
writeInterfaceMethodDeclaration imd =

      let mods = Syntax.interfaceMethodDeclarationModifiers imd
          header = Syntax.interfaceMethodDeclarationHeader imd
          body = Syntax.interfaceMethodDeclarationBody imd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeInterfaceMethodModifier mods))),
        (Just (writeMethodHeader header)),
        (Just (writeMethodBody body))]))

writeInterfaceMethodModifier :: Syntax.InterfaceMethodModifier -> Ast.Expr
writeInterfaceMethodModifier m =
    case m of
      Syntax.InterfaceMethodModifierAnnotation v0 -> writeAnnotation v0
      Syntax.InterfaceMethodModifierPublic -> Serialization.cst "public"
      Syntax.InterfaceMethodModifierPrivate -> Serialization.cst "private"
      Syntax.InterfaceMethodModifierAbstract -> Serialization.cst "abstract"
      Syntax.InterfaceMethodModifierDefault -> Serialization.cst "default"
      Syntax.InterfaceMethodModifierStatic -> Serialization.cst "static"
      Syntax.InterfaceMethodModifierStrictfp -> Serialization.cst "strictfp"

writeInterfaceModifier :: Syntax.InterfaceModifier -> Ast.Expr
writeInterfaceModifier m =
    case m of
      Syntax.InterfaceModifierAnnotation v0 -> writeAnnotation v0
      Syntax.InterfaceModifierPublic -> Serialization.cst "public"
      Syntax.InterfaceModifierProtected -> Serialization.cst "protected"
      Syntax.InterfaceModifierPrivate -> Serialization.cst "private"
      Syntax.InterfaceModifierAbstract -> Serialization.cst "abstract"
      Syntax.InterfaceModifierStatic -> Serialization.cst "static"
      Syntax.InterfaceModifierStrictfb -> Serialization.cst "strictfb"

writeInterfaceType :: Syntax.InterfaceType -> Ast.Expr
writeInterfaceType it = writeClassType (Syntax.unInterfaceType it)

writeLabeledStatement :: t0 -> Ast.Expr
writeLabeledStatement _ = Serialization.cst "STUB:LabeledStatement"

writeLambdaBody :: Syntax.LambdaBody -> Ast.Expr
writeLambdaBody b =
    case b of
      Syntax.LambdaBodyExpression v0 -> writeExpression v0
      Syntax.LambdaBodyBlock v0 -> writeBlock v0

writeLambdaExpression :: Syntax.LambdaExpression -> Ast.Expr
writeLambdaExpression le =

      let params = Syntax.lambdaExpressionParameters le
          body = Syntax.lambdaExpressionBody le
      in (Serialization.infixWs "->" (writeLambdaParameters params) (writeLambdaBody body))

writeLambdaParameters :: Syntax.LambdaParameters -> Ast.Expr
writeLambdaParameters p =
    case p of
      Syntax.LambdaParametersTuple v0 -> Serialization.parenList False (Lists.map writeLambdaParameters v0)
      Syntax.LambdaParametersSingle v0 -> writeIdentifier v0

writeLeftHandSide :: Syntax.LeftHandSide -> Ast.Expr
writeLeftHandSide lhs =
    case lhs of
      Syntax.LeftHandSideExpressionName v0 -> writeExpressionName v0
      Syntax.LeftHandSideFieldAccess v0 -> writeFieldAccess v0
      Syntax.LeftHandSideArrayAccess v0 -> writeArrayAccess v0

writeLiteral :: Syntax.Literal -> Ast.Expr
writeLiteral l =
    case l of
      Syntax.LiteralNull -> Serialization.cst "null"
      Syntax.LiteralInteger v0 -> writeIntegerLiteral v0
      Syntax.LiteralFloatingPoint v0 -> writeFloatingPointLiteral v0
      Syntax.LiteralBoolean v0 -> Serialization.cst (Logic.ifElse v0 "true" "false")
      Syntax.LiteralCharacter v0 ->
        let ci = Literals.bigintToInt32 (Literals.uint16ToBigint v0)
        in (Serialization.cst (Strings.cat2 "'" (Strings.cat2 (Logic.ifElse (Equality.equal ci 39) "\\'" (Logic.ifElse (Equality.equal ci 92) "\\\\" (Logic.ifElse (Equality.equal ci 10) "\\n" (Logic.ifElse (Equality.equal ci 13) "\\r" (Logic.ifElse (Equality.equal ci 9) "\\t" (Logic.ifElse (Logic.and (Equality.gte ci 32) (Equality.lt ci 127)) (Strings.fromList [
          ci]) (javaUnicodeEscape ci))))))) "'")))
      Syntax.LiteralString v0 -> writeStringLiteral v0

writeLocalName :: Syntax.LocalVariableType -> Ast.Expr
writeLocalName t =
    case t of
      Syntax.LocalVariableTypeType v0 -> writeUnannType v0
      Syntax.LocalVariableTypeVar -> Serialization.cst "var"

writeLocalVariableDeclaration :: Syntax.LocalVariableDeclaration -> Ast.Expr
writeLocalVariableDeclaration lvd =

      let mods = Syntax.localVariableDeclarationModifiers lvd
          t = Syntax.localVariableDeclarationType lvd
          decls = Syntax.localVariableDeclarationDeclarators lvd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeVariableModifier mods))),
        (Just (writeLocalName t)),
        (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableDeclarator decls)))]))

writeLocalVariableDeclarationStatement :: Syntax.LocalVariableDeclarationStatement -> Ast.Expr
writeLocalVariableDeclarationStatement lvds =
    Serialization.withSemi (writeLocalVariableDeclaration (Syntax.unLocalVariableDeclarationStatement lvds))

writeMarkerAnnotation :: Syntax.MarkerAnnotation -> Ast.Expr
writeMarkerAnnotation ma = Serialization.prefix "@" (writeTypeName (Syntax.unMarkerAnnotation ma))

writeMethodBody :: Syntax.MethodBody -> Ast.Expr
writeMethodBody b =
    case b of
      Syntax.MethodBodyBlock v0 -> writeBlock v0
      Syntax.MethodBodyNone -> Serialization.cst ";"

writeMethodDeclaration :: Syntax.MethodDeclaration -> Ast.Expr
writeMethodDeclaration md =

      let anns = Syntax.methodDeclarationAnnotations md
          mods = Syntax.methodDeclarationModifiers md
          header = Syntax.methodDeclarationHeader md
          body = Syntax.methodDeclarationBody md
          headerAndBody =
                  Serialization.spaceSep (Maybes.cat [
                    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeMethodModifier mods))),
                    (Just (writeMethodHeader header)),
                    (Just (writeMethodBody body))])
      in (Serialization.newlineSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.newlineSep (Lists.map writeAnnotation anns))),
        (Just headerAndBody)]))

writeMethodDeclarator :: Syntax.MethodDeclarator -> Ast.Expr
writeMethodDeclarator md =

      let id = Syntax.methodDeclaratorIdentifier md
          params = Syntax.methodDeclaratorFormalParameters md
      in (Serialization.noSep [
        writeIdentifier id,
        (Serialization.parenList False (Lists.map writeFormalParameter params))])

writeMethodHeader :: Syntax.MethodHeader -> Ast.Expr
writeMethodHeader mh =

      let params = Syntax.methodHeaderParameters mh
          result = Syntax.methodHeaderResult mh
          decl = Syntax.methodHeaderDeclarator mh
          mthrows = Syntax.methodHeaderThrows mh
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null params) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeParameter params))),
        (Just (writeResult result)),
        (Just (writeMethodDeclarator decl)),
        (Maybes.map writeThrows mthrows)]))

writeMethodInvocation :: Syntax.MethodInvocation -> Ast.Expr
writeMethodInvocation mi =

      let header = Syntax.methodInvocationHeader mi
          args = Syntax.methodInvocationArguments mi
          argSec = Serialization.parenList True (Lists.map writeExpression args)
          headerSec =
                  case header of
                    Syntax.MethodInvocation_HeaderSimple v0 -> writeMethodName v0
                    Syntax.MethodInvocation_HeaderComplex v0 ->
                      let cvar = Syntax.methodInvocation_ComplexVariant v0
                          targs = Syntax.methodInvocation_ComplexTypeArguments v0
                          cid = Syntax.methodInvocation_ComplexIdentifier v0
                          idSec =
                                  Serialization.noSep (Maybes.cat [
                                    Logic.ifElse (Lists.null targs) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument targs))),
                                    (Just (writeIdentifier cid))])
                      in case cvar of
                        Syntax.MethodInvocation_VariantType v1 -> Serialization.dotSep [
                          writeTypeName v1,
                          idSec]
                        Syntax.MethodInvocation_VariantExpression v1 -> Serialization.dotSep [
                          writeExpressionName v1,
                          idSec]
                        Syntax.MethodInvocation_VariantPrimary v1 -> Serialization.dotSep [
                          writePrimary v1,
                          idSec]
                        Syntax.MethodInvocation_VariantSuper -> Serialization.dotSep [
                          Serialization.cst "super",
                          idSec]
                        Syntax.MethodInvocation_VariantTypeSuper v1 -> Serialization.dotSep [
                          writeTypeName v1,
                          (Serialization.cst "super"),
                          idSec]
      in (Serialization.noSep [
        headerSec,
        argSec])

writeMethodModifier :: Syntax.MethodModifier -> Ast.Expr
writeMethodModifier m =
    case m of
      Syntax.MethodModifierAnnotation v0 -> writeAnnotation v0
      Syntax.MethodModifierPublic -> Serialization.cst "public"
      Syntax.MethodModifierProtected -> Serialization.cst "protected"
      Syntax.MethodModifierPrivate -> Serialization.cst "private"
      Syntax.MethodModifierAbstract -> Serialization.cst "abstract"
      Syntax.MethodModifierFinal -> Serialization.cst "final"
      Syntax.MethodModifierSynchronized -> Serialization.cst "synchronized"
      Syntax.MethodModifierNative -> Serialization.cst "native"
      Syntax.MethodModifierStrictfb -> Serialization.cst "strictfb"

writeMethodName :: Syntax.MethodName -> Ast.Expr
writeMethodName mn = writeIdentifier (Syntax.unMethodName mn)

writeMethodReference :: t0 -> Ast.Expr
writeMethodReference _ = Serialization.cst "STUB:MethodReference"

writeMultiplicativeExpression :: Syntax.MultiplicativeExpression -> Ast.Expr
writeMultiplicativeExpression e =
    case e of
      Syntax.MultiplicativeExpressionUnary v0 -> writeUnaryExpression v0
      Syntax.MultiplicativeExpressionTimes v0 -> Serialization.infixWs "*" (writeMultiplicativeExpression (Syntax.multiplicativeExpression_BinaryLhs v0)) (writeUnaryExpression (Syntax.multiplicativeExpression_BinaryRhs v0))
      Syntax.MultiplicativeExpressionDivide v0 -> Serialization.infixWs "/" (writeMultiplicativeExpression (Syntax.multiplicativeExpression_BinaryLhs v0)) (writeUnaryExpression (Syntax.multiplicativeExpression_BinaryRhs v0))
      Syntax.MultiplicativeExpressionMod v0 -> Serialization.infixWs "%" (writeMultiplicativeExpression (Syntax.multiplicativeExpression_BinaryLhs v0)) (writeUnaryExpression (Syntax.multiplicativeExpression_BinaryRhs v0))

writeNormalAnnotation :: Syntax.NormalAnnotation -> Ast.Expr
writeNormalAnnotation na =

      let tname = Syntax.normalAnnotationTypeName na
          pairs = Syntax.normalAnnotationPairs na
      in (Serialization.prefix "@" (Serialization.noSep [
        writeTypeName tname,
        (Serialization.commaSep Serialization.inlineStyle (Lists.map writeElementValuePair pairs))]))

writeNormalClassDeclaration :: Syntax.NormalClassDeclaration -> Ast.Expr
writeNormalClassDeclaration ncd =

      let mods = Syntax.normalClassDeclarationModifiers ncd
          id = Syntax.normalClassDeclarationIdentifier ncd
          tparams = Syntax.normalClassDeclarationParameters ncd
          msuperc = Syntax.normalClassDeclarationExtends ncd
          superi = Syntax.normalClassDeclarationImplements ncd
          body = Syntax.normalClassDeclarationBody ncd
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeClassModifier mods))),
        (Just (Serialization.cst "class")),
        (Just (Serialization.noSep (Maybes.cat [
          Just (writeTypeIdentifier id),
          (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeParameter tparams))))]))),
        (Maybes.map (\c -> Serialization.spaceSep [
          Serialization.cst "extends",
          (writeClassType c)]) msuperc),
        (Logic.ifElse (Lists.null superi) Nothing (Just (Serialization.spaceSep [
          Serialization.cst "implements",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map writeInterfaceType superi))]))),
        (Just (writeClassBody body))]))

writeNormalInterfaceDeclaration :: Syntax.NormalInterfaceDeclaration -> Ast.Expr
writeNormalInterfaceDeclaration nid =

      let mods = Syntax.normalInterfaceDeclarationModifiers nid
          id = Syntax.normalInterfaceDeclarationIdentifier nid
          tparams = Syntax.normalInterfaceDeclarationParameters nid
          extends = Syntax.normalInterfaceDeclarationExtends nid
          body = Syntax.normalInterfaceDeclarationBody nid
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeInterfaceModifier mods))),
        (Just (Serialization.cst "interface")),
        (Just (Serialization.noSep (Maybes.cat [
          Just (writeTypeIdentifier id),
          (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeParameter tparams))))]))),
        (Logic.ifElse (Lists.null extends) Nothing (Just (Serialization.spaceSep [
          Serialization.cst "extends",
          (Serialization.commaSep Serialization.inlineStyle (Lists.map writeInterfaceType extends))]))),
        (Just (writeInterfaceBody body))]))

writeNumericType :: Syntax.NumericType -> Ast.Expr
writeNumericType nt =
    case nt of
      Syntax.NumericTypeIntegral v0 -> writeIntegralType v0
      Syntax.NumericTypeFloatingPoint v0 -> writeFloatingPointType v0

writePackageDeclaration :: Syntax.PackageDeclaration -> Ast.Expr
writePackageDeclaration pd =

      let mods = Syntax.packageDeclarationModifiers pd
          ids = Syntax.packageDeclarationIdentifiers pd
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writePackageModifier mods))),
        (Just (Serialization.spaceSep [
          Serialization.cst "package",
          (Serialization.cst (Strings.intercalate "." (Lists.map (\id -> Syntax.unIdentifier id) ids)))]))])))

writePackageModifier :: Syntax.PackageModifier -> Ast.Expr
writePackageModifier pm = writeAnnotation (Syntax.unPackageModifier pm)

writePackageName :: Syntax.PackageName -> Ast.Expr
writePackageName pn = Serialization.dotSep (Lists.map writeIdentifier (Syntax.unPackageName pn))

writePackageOrTypeName :: Syntax.PackageOrTypeName -> Ast.Expr
writePackageOrTypeName potn = Serialization.dotSep (Lists.map writeIdentifier (Syntax.unPackageOrTypeName potn))

writePostDecrementExpression :: t0 -> Ast.Expr
writePostDecrementExpression _ = Serialization.cst "STUB:PostDecrementExpression"

writePostIncrementExpression :: t0 -> Ast.Expr
writePostIncrementExpression _ = Serialization.cst "STUB:PostIncrementExpression"

writePostfixExpression :: Syntax.PostfixExpression -> Ast.Expr
writePostfixExpression e =
    case e of
      Syntax.PostfixExpressionPrimary v0 -> writePrimary v0
      Syntax.PostfixExpressionName v0 -> writeExpressionName v0
      Syntax.PostfixExpressionPostIncrement v0 -> writePostIncrementExpression v0
      Syntax.PostfixExpressionPostDecrement v0 -> writePostDecrementExpression v0

writePreDecrementExpression :: t0 -> Ast.Expr
writePreDecrementExpression _ = Serialization.cst "STUB:PreDecrementExpression"

writePreIncrementExpression :: t0 -> Ast.Expr
writePreIncrementExpression _ = Serialization.cst "STUB:PreIncrementExpression"

writePrimary :: Syntax.Primary -> Ast.Expr
writePrimary p =
    case p of
      Syntax.PrimaryNoNewArray v0 -> writePrimaryNoNewArrayExpressionExpression v0
      Syntax.PrimaryArrayCreation v0 -> writeArrayCreationExpression v0

writePrimaryNoNewArrayExpressionExpression :: Syntax.PrimaryNoNewArrayExpression -> Ast.Expr
writePrimaryNoNewArrayExpressionExpression p =
    case p of
      Syntax.PrimaryNoNewArrayExpressionLiteral v0 -> writeLiteral v0
      Syntax.PrimaryNoNewArrayExpressionClassLiteral v0 -> writeClassLiteral v0
      Syntax.PrimaryNoNewArrayExpressionThis -> Serialization.cst "this"
      Syntax.PrimaryNoNewArrayExpressionDotThis v0 -> Serialization.dotSep [
        writeTypeName v0,
        (Serialization.cst "this")]
      Syntax.PrimaryNoNewArrayExpressionParens v0 -> Serialization.parenList False [
        writeExpression v0]
      Syntax.PrimaryNoNewArrayExpressionClassInstance v0 -> writeClassInstanceCreationExpression v0
      Syntax.PrimaryNoNewArrayExpressionFieldAccess v0 -> writeFieldAccess v0
      Syntax.PrimaryNoNewArrayExpressionArrayAccess v0 -> writeArrayAccess v0
      Syntax.PrimaryNoNewArrayExpressionMethodInvocation v0 -> writeMethodInvocation v0
      Syntax.PrimaryNoNewArrayExpressionMethodReference v0 -> writeMethodReference v0

writePrimitiveType :: Syntax.PrimitiveType -> Ast.Expr
writePrimitiveType pt =
    case pt of
      Syntax.PrimitiveTypeNumeric v0 -> writeNumericType v0
      Syntax.PrimitiveTypeBoolean -> Serialization.cst "boolean"

writePrimitiveTypeWithAnnotations :: Syntax.PrimitiveTypeWithAnnotations -> Ast.Expr
writePrimitiveTypeWithAnnotations ptwa =

      let pt = Syntax.primitiveTypeWithAnnotationsType ptwa
          anns = Syntax.primitiveTypeWithAnnotationsAnnotations ptwa
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.spaceSep (Lists.map writeAnnotation anns))),
        (Just (writePrimitiveType pt))]))

writeReceiverParameter :: t0 -> Ast.Expr
writeReceiverParameter _ = Serialization.cst "STUB:ReceiverParameter"

writeReferenceType :: Syntax.ReferenceType -> Ast.Expr
writeReferenceType rt =
    case rt of
      Syntax.ReferenceTypeClassOrInterface v0 -> writeClassOrInterfaceType v0
      Syntax.ReferenceTypeVariable v0 -> writeTypeVariable v0
      Syntax.ReferenceTypeArray v0 -> writeArrayType v0

writeRelationalExpression :: Syntax.RelationalExpression -> Ast.Expr
writeRelationalExpression e =
    case e of
      Syntax.RelationalExpressionSimple v0 -> writeShiftExpression v0
      Syntax.RelationalExpressionLessThan v0 -> writeRelationalExpression_LessThan v0
      Syntax.RelationalExpressionGreaterThan v0 -> writeRelationalExpression_GreaterThan v0
      Syntax.RelationalExpressionLessThanEqual v0 -> writeRelationalExpression_LessThanEqual v0
      Syntax.RelationalExpressionGreaterThanEqual v0 -> writeRelationalExpression_GreaterThanEqual v0
      Syntax.RelationalExpressionInstanceof v0 -> writeRelationalExpression_InstanceOf v0

writeRelationalExpression_GreaterThan :: Syntax.RelationalExpression_GreaterThan -> Ast.Expr
writeRelationalExpression_GreaterThan gt =
    Serialization.infixWs ">" (writeRelationalExpression (Syntax.relationalExpression_GreaterThanLhs gt)) (writeShiftExpression (Syntax.relationalExpression_GreaterThanRhs gt))

writeRelationalExpression_GreaterThanEqual :: Syntax.RelationalExpression_GreaterThanEqual -> Ast.Expr
writeRelationalExpression_GreaterThanEqual gte =
    Serialization.infixWs ">=" (writeRelationalExpression (Syntax.relationalExpression_GreaterThanEqualLhs gte)) (writeShiftExpression (Syntax.relationalExpression_GreaterThanEqualRhs gte))

writeRelationalExpression_InstanceOf :: Syntax.RelationalExpression_InstanceOf -> Ast.Expr
writeRelationalExpression_InstanceOf io =
    Serialization.infixWs "instanceof" (writeRelationalExpression (Syntax.relationalExpression_InstanceOfLhs io)) (writeReferenceType (Syntax.relationalExpression_InstanceOfRhs io))

writeRelationalExpression_LessThan :: Syntax.RelationalExpression_LessThan -> Ast.Expr
writeRelationalExpression_LessThan lt =
    Serialization.infixWs "<" (writeRelationalExpression (Syntax.relationalExpression_LessThanLhs lt)) (writeShiftExpression (Syntax.relationalExpression_LessThanRhs lt))

writeRelationalExpression_LessThanEqual :: Syntax.RelationalExpression_LessThanEqual -> Ast.Expr
writeRelationalExpression_LessThanEqual lte =
    Serialization.infixWs "<=" (writeRelationalExpression (Syntax.relationalExpression_LessThanEqualLhs lte)) (writeShiftExpression (Syntax.relationalExpression_LessThanEqualRhs lte))

writeResult :: Syntax.Result -> Ast.Expr
writeResult r =
    case r of
      Syntax.ResultType v0 -> writeUnannType v0
      Syntax.ResultVoid -> Serialization.cst "void"

writeReturnStatement :: Syntax.ReturnStatement -> Ast.Expr
writeReturnStatement rs =

      let mex = Syntax.unReturnStatement rs
      in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "return"),
        (Maybes.map writeExpression mex)])))

writeShiftExpression :: Syntax.ShiftExpression -> Ast.Expr
writeShiftExpression e =
    case e of
      Syntax.ShiftExpressionUnary v0 -> writeAdditiveExpression v0
      Syntax.ShiftExpressionShiftLeft v0 -> Serialization.infixWs "<<" (writeShiftExpression (Syntax.shiftExpression_BinaryLhs v0)) (writeAdditiveExpression (Syntax.shiftExpression_BinaryRhs v0))
      Syntax.ShiftExpressionShiftRight v0 -> Serialization.infixWs ">>" (writeShiftExpression (Syntax.shiftExpression_BinaryLhs v0)) (writeAdditiveExpression (Syntax.shiftExpression_BinaryRhs v0))
      Syntax.ShiftExpressionShiftRightZeroFill v0 -> Serialization.infixWs ">>>" (writeShiftExpression (Syntax.shiftExpression_BinaryLhs v0)) (writeAdditiveExpression (Syntax.shiftExpression_BinaryRhs v0))

writeSimpleTypeName :: Syntax.SimpleTypeName -> Ast.Expr
writeSimpleTypeName stn = writeTypeIdentifier (Syntax.unSimpleTypeName stn)

writeSingleElementAnnotation :: Syntax.SingleElementAnnotation -> Ast.Expr
writeSingleElementAnnotation sea =

      let tname = Syntax.singleElementAnnotationName sea
          mv = Syntax.singleElementAnnotationValue sea
      in (Maybes.maybe (writeMarkerAnnotation (Syntax.MarkerAnnotation tname)) (\v -> Serialization.prefix "@" (Serialization.noSep [
        writeTypeName tname,
        (Serialization.parenList False [
          writeElementValue v])])) mv)

writeStatement :: Syntax.Statement -> Ast.Expr
writeStatement s =
    case s of
      Syntax.StatementWithoutTrailing v0 -> writeStatementWithoutTrailingSubstatement v0
      Syntax.StatementLabeled v0 -> writeLabeledStatement v0
      Syntax.StatementIfThen v0 -> writeIfThenStatement v0
      Syntax.StatementIfThenElse v0 -> writeIfThenElseStatement v0
      Syntax.StatementWhile v0 -> writeWhileStatement v0
      Syntax.StatementFor v0 -> writeForStatement v0

writeStatementExpression :: Syntax.StatementExpression -> Ast.Expr
writeStatementExpression e =
    case e of
      Syntax.StatementExpressionAssignment v0 -> writeAssignment v0
      Syntax.StatementExpressionPreIncrement v0 -> writePreIncrementExpression v0
      Syntax.StatementExpressionPreDecrement v0 -> writePreDecrementExpression v0
      Syntax.StatementExpressionPostIncrement v0 -> writePostIncrementExpression v0
      Syntax.StatementExpressionPostDecrement v0 -> writePostDecrementExpression v0
      Syntax.StatementExpressionMethodInvocation v0 -> writeMethodInvocation v0
      Syntax.StatementExpressionClassInstanceCreation v0 -> writeClassInstanceCreationExpression v0

writeStatementWithoutTrailingSubstatement :: Syntax.StatementWithoutTrailingSubstatement -> Ast.Expr
writeStatementWithoutTrailingSubstatement s =
    case s of
      Syntax.StatementWithoutTrailingSubstatementBlock v0 -> writeBlock v0
      Syntax.StatementWithoutTrailingSubstatementEmpty -> Serialization.cst ";"
      Syntax.StatementWithoutTrailingSubstatementExpression v0 -> writeExpressionStatement v0
      Syntax.StatementWithoutTrailingSubstatementAssert v0 -> writeAssertStatement v0
      Syntax.StatementWithoutTrailingSubstatementSwitch v0 -> writeSwitchStatement v0
      Syntax.StatementWithoutTrailingSubstatementDo v0 -> writeDoStatement v0
      Syntax.StatementWithoutTrailingSubstatementBreak v0 -> writeBreakStatement v0
      Syntax.StatementWithoutTrailingSubstatementContinue v0 -> writeContinueStatement v0
      Syntax.StatementWithoutTrailingSubstatementReturn v0 -> writeReturnStatement v0
      Syntax.StatementWithoutTrailingSubstatementSynchronized v0 -> writeSynchronizedStatement v0
      Syntax.StatementWithoutTrailingSubstatementThrow v0 -> writeThrowStatement v0
      Syntax.StatementWithoutTrailingSubstatementTry v0 -> writeTryStatement v0

writeStaticInitializer :: t0 -> Ast.Expr
writeStaticInitializer _ = Serialization.cst "STUB:StaticInitializer"

-- | Serialize a Java string literal with proper Unicode escaping.
writeStringLiteral :: Syntax.StringLiteral -> Ast.Expr
writeStringLiteral sl =

      let s = Syntax.unStringLiteral sl
      in (Serialization.cst (Strings.cat2 "\"" (Strings.cat2 (escapeJavaString s) "\"")))

writeSwitchStatement :: t0 -> Ast.Expr
writeSwitchStatement _ = Serialization.cst "STUB:SwitchStatement"

writeSynchronizedStatement :: t0 -> Ast.Expr
writeSynchronizedStatement _ = Serialization.cst "STUB:SynchronizedStatement"

writeThrowStatement :: Syntax.ThrowStatement -> Ast.Expr
writeThrowStatement ts =
    Serialization.withSemi (Serialization.spaceSep [
      Serialization.cst "throw",
      (writeExpression (Syntax.unThrowStatement ts))])

writeThrows :: t0 -> Ast.Expr
writeThrows _ = Serialization.cst "STUB:Throws"

writeTryStatement :: t0 -> Ast.Expr
writeTryStatement _ = Serialization.cst "STUB:TryStatement"

writeType :: Syntax.Type -> Ast.Expr
writeType t =
    case t of
      Syntax.TypePrimitive v0 -> writePrimitiveTypeWithAnnotations v0
      Syntax.TypeReference v0 -> writeReferenceType v0

writeTypeArgument :: Syntax.TypeArgument -> Ast.Expr
writeTypeArgument a =
    case a of
      Syntax.TypeArgumentReference v0 -> writeReferenceType v0
      Syntax.TypeArgumentWildcard v0 -> writeWildcard v0

writeTypeArgumentsOrDiamond :: Syntax.TypeArgumentsOrDiamond -> Ast.Expr
writeTypeArgumentsOrDiamond targs =
    case targs of
      Syntax.TypeArgumentsOrDiamondArguments v0 -> Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument v0)
      Syntax.TypeArgumentsOrDiamondDiamond -> Serialization.cst "<>"

writeTypeBound :: Syntax.TypeBound -> Ast.Expr
writeTypeBound b =
    case b of
      Syntax.TypeBoundVariable v0 -> writeTypeVariable v0
      Syntax.TypeBoundClassOrInterface v0 ->
        let cit = Syntax.typeBound_ClassOrInterfaceType v0
            additional = Syntax.typeBound_ClassOrInterfaceAdditional v0
        in (Logic.ifElse (Lists.null additional) (writeClassOrInterfaceType cit) (Serialization.spaceSep (Lists.cons (writeClassOrInterfaceType cit) (Lists.map writeAdditionalBound additional))))

writeTypeDeclaration :: Syntax.TypeDeclaration -> Ast.Expr
writeTypeDeclaration d =
    case d of
      Syntax.TypeDeclarationClass v0 -> writeClassDeclaration v0
      Syntax.TypeDeclarationInterface v0 -> writeInterfaceDeclaration v0
      Syntax.TypeDeclarationNone -> Serialization.cst ";"

writeTypeDeclarationWithComments :: Syntax.TypeDeclarationWithComments -> Ast.Expr
writeTypeDeclarationWithComments tdwc =

      let d = Syntax.typeDeclarationWithCommentsValue tdwc
          mc = Syntax.typeDeclarationWithCommentsComments tdwc
      in (withComments mc (writeTypeDeclaration d))

writeTypeIdentifier :: Syntax.TypeIdentifier -> Ast.Expr
writeTypeIdentifier tid = writeIdentifier (Syntax.unTypeIdentifier tid)

writeTypeName :: Syntax.TypeName -> Ast.Expr
writeTypeName tn =

      let id = Syntax.typeNameIdentifier tn
          mqual = Syntax.typeNameQualifier tn
      in (Serialization.dotSep (Maybes.cat [
        Maybes.map writePackageOrTypeName mqual,
        (Just (writeTypeIdentifier id))]))

writeTypeParameter :: Syntax.TypeParameter -> Ast.Expr
writeTypeParameter tp =

      let mods = Syntax.typeParameterModifiers tp
          id = Syntax.typeParameterIdentifier tp
          bound = Syntax.typeParameterBound tp
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeTypeParameterModifier mods))),
        (Just (writeTypeIdentifier id)),
        (Maybes.map (\b -> Serialization.spaceSep [
          Serialization.cst "extends",
          (writeTypeBound b)]) bound)]))

writeTypeParameterModifier :: Syntax.TypeParameterModifier -> Ast.Expr
writeTypeParameterModifier tpm = writeAnnotation (Syntax.unTypeParameterModifier tpm)

writeTypeVariable :: Syntax.TypeVariable -> Ast.Expr
writeTypeVariable tv =

      let anns = Syntax.typeVariableAnnotations tv
          id = Syntax.typeVariableIdentifier tv
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.spaceSep (Lists.map writeAnnotation anns))),
        (Just (writeTypeIdentifier id))]))

writeUnannType :: Syntax.UnannType -> Ast.Expr
writeUnannType ut = writeType (Syntax.unUnannType ut)

writeUnaryExpression :: Syntax.UnaryExpression -> Ast.Expr
writeUnaryExpression e =
    case e of
      Syntax.UnaryExpressionPreIncrement v0 -> writePreIncrementExpression v0
      Syntax.UnaryExpressionPreDecrement v0 -> writePreDecrementExpression v0
      Syntax.UnaryExpressionPlus v0 -> Serialization.spaceSep [
        Serialization.cst "+",
        (writeUnaryExpression v0)]
      Syntax.UnaryExpressionMinus v0 -> Serialization.spaceSep [
        Serialization.cst "-",
        (writeUnaryExpression v0)]
      Syntax.UnaryExpressionOther v0 -> writeUnaryExpressionNotPlusMinus v0

writeUnaryExpressionNotPlusMinus :: Syntax.UnaryExpressionNotPlusMinus -> Ast.Expr
writeUnaryExpressionNotPlusMinus e =
    case e of
      Syntax.UnaryExpressionNotPlusMinusPostfix v0 -> writePostfixExpression v0
      Syntax.UnaryExpressionNotPlusMinusTilde v0 -> Serialization.spaceSep [
        Serialization.cst "~",
        (writeUnaryExpression v0)]
      Syntax.UnaryExpressionNotPlusMinusNot v0 -> Serialization.noSep [
        Serialization.cst "!",
        (writeUnaryExpression v0)]
      Syntax.UnaryExpressionNotPlusMinusCast v0 -> writeCastExpression v0

writeUnqualifiedClassInstanceCreationExpression :: Syntax.UnqualifiedClassInstanceCreationExpression -> Ast.Expr
writeUnqualifiedClassInstanceCreationExpression ucice =

      let targs = Syntax.unqualifiedClassInstanceCreationExpressionTypeArguments ucice
          cit = Syntax.unqualifiedClassInstanceCreationExpressionClassOrInterface ucice
          args = Syntax.unqualifiedClassInstanceCreationExpressionArguments ucice
          mbody = Syntax.unqualifiedClassInstanceCreationExpressionBody ucice
      in (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "new"),
        (Logic.ifElse (Lists.null targs) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument targs)))),
        (Just (Serialization.noSep [
          writeClassOrInterfaceTypeToInstantiate cit,
          (Serialization.parenList False (Lists.map writeExpression args))])),
        (Maybes.map writeClassBody mbody)]))

writeVariableArityParameter :: t0 -> Ast.Expr
writeVariableArityParameter _ = Serialization.cst "STUB:VariableArityParameter"

writeVariableDeclarator :: Syntax.VariableDeclarator -> Ast.Expr
writeVariableDeclarator vd =

      let id = Syntax.variableDeclaratorId vd
          minit = Syntax.variableDeclaratorInitializer vd
          idSec = writeVariableDeclaratorId id
      in (Maybes.maybe idSec (\init -> Serialization.infixWs "=" idSec (writeVariableInitializer init)) minit)

writeVariableDeclaratorId :: Syntax.VariableDeclaratorId -> Ast.Expr
writeVariableDeclaratorId vdi =

      let id = Syntax.variableDeclaratorIdIdentifier vdi
          mdims = Syntax.variableDeclaratorIdDims vdi
      in (Serialization.noSep (Maybes.cat [
        Just (writeIdentifier id),
        (Maybes.map writeDims mdims)]))

writeVariableInitializer :: Syntax.VariableInitializer -> Ast.Expr
writeVariableInitializer i =
    case i of
      Syntax.VariableInitializerExpression v0 -> writeExpression v0
      Syntax.VariableInitializerArrayInitializer v0 -> writeArrayInitializer v0

writeVariableModifier :: Syntax.VariableModifier -> Ast.Expr
writeVariableModifier m =
    case m of
      Syntax.VariableModifierAnnotation v0 -> writeAnnotation v0
      Syntax.VariableModifierFinal -> Serialization.cst "final"

writeWhileStatement :: Syntax.WhileStatement -> Ast.Expr
writeWhileStatement ws =

      let mcond = Syntax.whileStatementCond ws
          body = Syntax.whileStatementBody ws
          condSer = Maybes.maybe (Serialization.cst "true") (\c -> writeExpression c) mcond
      in (Serialization.spaceSep [
        Serialization.cst "while",
        (Serialization.parenList False [
          condSer]),
        (Serialization.curlyBlock Serialization.fullBlockStyle (writeStatement body))])

writeWildcard :: Syntax.Wildcard -> Ast.Expr
writeWildcard w =

      let anns = Syntax.wildcardAnnotations w
          mbounds = Syntax.wildcardWildcard w
      in (Serialization.spaceSep (Maybes.cat [
        Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeAnnotation anns))),
        (Just (Serialization.cst "*")),
        (Maybes.map writeWildcardBounds mbounds)]))

writeWildcardBounds :: Syntax.WildcardBounds -> Ast.Expr
writeWildcardBounds b =
    case b of
      Syntax.WildcardBoundsExtends v0 -> Serialization.spaceSep [
        Serialization.cst "extends",
        (writeReferenceType v0)]
      Syntax.WildcardBoundsSuper v0 -> Serialization.spaceSep [
        Serialization.cst "super",
        (writeReferenceType v0)]
