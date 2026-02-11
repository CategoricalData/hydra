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

hexDigit :: (Int -> Int)
hexDigit n = (Logic.ifElse (Equality.lt n 10) (Math.add n 48) (Math.add (Math.sub n 10) 65))

padHex4 :: (Int -> String)
padHex4 n =  
  let d3 = (Math.div n 4096)
  in  
    let r3 = (Math.mod n 4096)
    in  
      let d2 = (Math.div r3 256)
      in  
        let r2 = (Math.mod r3 256)
        in  
          let d1 = (Math.div r2 16)
          in  
            let d0 = (Math.mod r2 16)
            in (Strings.fromList [
              hexDigit d3,
              (hexDigit d2),
              (hexDigit d1),
              (hexDigit d0)])

javaUnicodeEscape :: (Int -> String)
javaUnicodeEscape n = (Logic.ifElse (Equality.gt n 65535) ( 
  let n_ = (Math.sub n 65536)
  in  
    let hi = (Math.add 55296 (Math.div n_ 1024))
    in  
      let lo = (Math.add 56320 (Math.mod n_ 1024))
      in (Strings.cat2 (Strings.cat2 "\\u" (padHex4 hi)) (Strings.cat2 "\\u" (padHex4 lo)))) (Strings.cat2 "\\u" (padHex4 n)))

escapeJavaChar :: (Int -> String)
escapeJavaChar c = (Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Logic.ifElse (Equality.equal c 9) "\\t" (Logic.ifElse (Equality.equal c 8) "\\b" (Logic.ifElse (Equality.equal c 12) "\\f" (Logic.ifElse (Logic.and (Equality.gte c 32) (Equality.lt c 127)) (Strings.fromList [
  c]) (javaUnicodeEscape c)))))))))

escapeJavaString :: (String -> String)
escapeJavaString s = (Strings.cat (Lists.map (\c -> escapeJavaChar c) (Strings.toList s)))

writeAdditionalBound :: (Syntax.AdditionalBound -> Ast.Expr)
writeAdditionalBound ab = (Serialization.spaceSep [
  Serialization.cst "&",
  (writeInterfaceType (Syntax.unAdditionalBound ab))])

writeAdditiveExpression :: (Syntax.AdditiveExpression -> Ast.Expr)
writeAdditiveExpression e = ((\x -> case x of
  Syntax.AdditiveExpressionUnary v1 -> (writeMultiplicativeExpression v1)
  Syntax.AdditiveExpressionPlus v1 -> (Serialization.infixWs "+" (writeAdditiveExpression (Syntax.additiveExpression_BinaryLhs v1)) (writeMultiplicativeExpression (Syntax.additiveExpression_BinaryRhs v1)))
  Syntax.AdditiveExpressionMinus v1 -> (Serialization.infixWs "-" (writeAdditiveExpression (Syntax.additiveExpression_BinaryLhs v1)) (writeMultiplicativeExpression (Syntax.additiveExpression_BinaryRhs v1)))) e)

writeAmbiguousName :: (Syntax.AmbiguousName -> Ast.Expr)
writeAmbiguousName an = (Serialization.dotSep (Lists.map writeIdentifier (Syntax.unAmbiguousName an)))

writeAndExpression :: (Syntax.AndExpression -> Ast.Expr)
writeAndExpression ae = (Serialization.infixWsList "&" (Lists.map writeEqualityExpression (Syntax.unAndExpression ae)))

writeAnnotatedIdentifier :: (Syntax.AnnotatedIdentifier -> Ast.Expr)
writeAnnotatedIdentifier ai = (writeIdentifier (Syntax.annotatedIdentifierIdentifier ai))

writeAnnotation :: (Syntax.Annotation -> Ast.Expr)
writeAnnotation ann = ((\x -> case x of
  Syntax.AnnotationNormal v1 -> (writeNormalAnnotation v1)
  Syntax.AnnotationMarker v1 -> (writeMarkerAnnotation v1)
  Syntax.AnnotationSingleElement v1 -> (writeSingleElementAnnotation v1)) ann)

writeAnnotationTypeDeclaration :: (t0 -> Ast.Expr)
writeAnnotationTypeDeclaration _ = (Serialization.cst "STUB:AnnotationTypeDeclaration")

writeArrayAccess :: (t0 -> Ast.Expr)
writeArrayAccess _ = (Serialization.cst "STUB:ArrayAccess")

writeArrayCreationExpression :: (Syntax.ArrayCreationExpression -> Ast.Expr)
writeArrayCreationExpression ace = ((\x -> case x of
  Syntax.ArrayCreationExpressionPrimitiveArray v1 ->  
    let pt = (Syntax.arrayCreationExpression_PrimitiveArrayType v1) 
        ai = (Syntax.arrayCreationExpression_PrimitiveArrayArray v1)
    in (Serialization.spaceSep [
      Serialization.cst "new",
      (Serialization.noSep [
        writePrimitiveTypeWithAnnotations pt,
        (Serialization.cst "[]")]),
      (writeArrayInitializer ai)])
  Syntax.ArrayCreationExpressionClassOrInterfaceArray _ -> (Serialization.cst "STUB:ArrayCreationExpression")
  Syntax.ArrayCreationExpressionPrimitive _ -> (Serialization.cst "STUB:ArrayCreationExpression")
  Syntax.ArrayCreationExpressionClassOrInterface _ -> (Serialization.cst "STUB:ArrayCreationExpression")) ace)

writeArrayInitializer :: (Syntax.ArrayInitializer -> Ast.Expr)
writeArrayInitializer ai =  
  let groups = (Syntax.unArrayInitializer ai)
  in (Logic.ifElse (Equality.equal (Lists.length groups) 1) (Serialization.noSep [
    Serialization.cst "{",
    (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableInitializer (Lists.head groups))),
    (Serialization.cst "}")]) (Serialization.cst "{}"))

writeArrayType :: (Syntax.ArrayType -> Ast.Expr)
writeArrayType at =  
  let dims = (Syntax.arrayTypeDims at) 
      variant = (Syntax.arrayTypeVariant at)
      varExpr = ((\x -> case x of
              Syntax.ArrayType_VariantPrimitive v1 -> (writePrimitiveTypeWithAnnotations v1)
              Syntax.ArrayType_VariantClassOrInterface v1 -> (writeClassOrInterfaceType v1)
              Syntax.ArrayType_VariantVariable v1 -> (writeTypeVariable v1)) variant)
  in (Serialization.noSep [
    varExpr,
    (writeDims dims)])

writeAssertStatement :: (t0 -> Ast.Expr)
writeAssertStatement _ = (Serialization.cst "STUB:AssertStatement")

writeAssignment :: (Syntax.Assignment -> Ast.Expr)
writeAssignment a =  
  let lhs = (Syntax.assignmentLhs a) 
      op = (Syntax.assignmentOp a)
      rhs = (Syntax.assignmentExpression a)
      ctop = ((\x -> case x of
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
              Syntax.AssignmentOperatorOr -> "|=") op)
  in (Serialization.infixWs ctop (writeLeftHandSide lhs) (writeExpression rhs))

writeAssignmentExpression :: (Syntax.AssignmentExpression -> Ast.Expr)
writeAssignmentExpression e = ((\x -> case x of
  Syntax.AssignmentExpressionConditional v1 -> (writeConditionalExpression v1)
  Syntax.AssignmentExpressionAssignment v1 -> (writeAssignment v1)) e)

writeBlock :: (Syntax.Block -> Ast.Expr)
writeBlock b = (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map writeBlockStatement (Syntax.unBlock b))))

writeBlockStatement :: (Syntax.BlockStatement -> Ast.Expr)
writeBlockStatement s = ((\x -> case x of
  Syntax.BlockStatementLocalVariableDeclaration v1 -> (writeLocalVariableDeclarationStatement v1)
  Syntax.BlockStatementClass v1 -> (writeClassDeclaration v1)
  Syntax.BlockStatementStatement v1 -> (writeStatement v1)) s)

writeBreakStatement :: (Syntax.BreakStatement -> Ast.Expr)
writeBreakStatement bs =  
  let mlabel = (Syntax.unBreakStatement bs)
  in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "break"),
    (Maybes.map writeIdentifier mlabel)])))

writeCastExpression :: (Syntax.CastExpression -> Ast.Expr)
writeCastExpression e = ((\x -> case x of
  Syntax.CastExpressionPrimitive v1 -> (writeCastExpression_Primitive v1)
  Syntax.CastExpressionNotPlusMinus v1 -> (writeCastExpression_NotPlusMinus v1)
  Syntax.CastExpressionLambda v1 -> (writeCastExpression_Lambda v1)) e)

writeCastExpression_Lambda :: (t0 -> Ast.Expr)
writeCastExpression_Lambda _ = (Serialization.cst "STUB:CastExpression_Lambda")

writeCastExpression_NotPlusMinus :: (Syntax.CastExpression_NotPlusMinus -> Ast.Expr)
writeCastExpression_NotPlusMinus npm =  
  let rb = (Syntax.castExpression_NotPlusMinusRefAndBounds npm) 
      ex = (Syntax.castExpression_NotPlusMinusExpression npm)
  in (Serialization.spaceSep [
    writeCastExpression_RefAndBounds rb,
    (writeUnaryExpression ex)])

writeCastExpression_RefAndBounds :: (Syntax.CastExpression_RefAndBounds -> Ast.Expr)
writeCastExpression_RefAndBounds rab =  
  let rt = (Syntax.castExpression_RefAndBoundsType rab) 
      adds = (Syntax.castExpression_RefAndBoundsBounds rab)
  in (Serialization.parenList False [
    Serialization.spaceSep (Maybes.cat [
      Just (writeReferenceType rt),
      (Logic.ifElse (Lists.null adds) Nothing (Just (Serialization.spaceSep (Lists.map writeAdditionalBound adds))))])])

writeCastExpression_Primitive :: (Syntax.CastExpression_Primitive -> Ast.Expr)
writeCastExpression_Primitive cp =  
  let pt = (Syntax.castExpression_PrimitiveType cp) 
      ex = (Syntax.castExpression_PrimitiveExpression cp)
  in (Serialization.spaceSep [
    Serialization.parenList False [
      writePrimitiveTypeWithAnnotations pt],
    (writeUnaryExpression ex)])

writeClassBody :: (Syntax.ClassBody -> Ast.Expr)
writeClassBody cb = (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map writeClassBodyDeclarationWithComments (Syntax.unClassBody cb))))

writeClassBodyDeclaration :: (Syntax.ClassBodyDeclaration -> Ast.Expr)
writeClassBodyDeclaration d = ((\x -> case x of
  Syntax.ClassBodyDeclarationClassMember v1 -> (writeClassMemberDeclaration v1)
  Syntax.ClassBodyDeclarationInstanceInitializer v1 -> (writeInstanceInitializer v1)
  Syntax.ClassBodyDeclarationStaticInitializer v1 -> (writeStaticInitializer v1)
  Syntax.ClassBodyDeclarationConstructorDeclaration v1 -> (writeConstructorDeclaration v1)) d)

writeClassBodyDeclarationWithComments :: (Syntax.ClassBodyDeclarationWithComments -> Ast.Expr)
writeClassBodyDeclarationWithComments cbdwc =  
  let d = (Syntax.classBodyDeclarationWithCommentsValue cbdwc) 
      mc = (Syntax.classBodyDeclarationWithCommentsComments cbdwc)
  in (withComments mc (writeClassBodyDeclaration d))

writeClassDeclaration :: (Syntax.ClassDeclaration -> Ast.Expr)
writeClassDeclaration d = ((\x -> case x of
  Syntax.ClassDeclarationNormal v1 -> (writeNormalClassDeclaration v1)
  Syntax.ClassDeclarationEnum v1 -> (writeEnumDeclaration v1)) d)

writeClassInstanceCreationExpression :: (Syntax.ClassInstanceCreationExpression -> Ast.Expr)
writeClassInstanceCreationExpression cice =  
  let mqual = (Syntax.classInstanceCreationExpressionQualifier cice) 
      e = (Syntax.classInstanceCreationExpressionExpression cice)
  in (Maybes.maybe (writeUnqualifiedClassInstanceCreationExpression e) (\q -> Serialization.dotSep [
    writeClassInstanceCreationExpression_Qualifier q,
    (writeUnqualifiedClassInstanceCreationExpression e)]) mqual)

writeClassInstanceCreationExpression_Qualifier :: (Syntax.ClassInstanceCreationExpression_Qualifier -> Ast.Expr)
writeClassInstanceCreationExpression_Qualifier q = ((\x -> case x of
  Syntax.ClassInstanceCreationExpression_QualifierExpression v1 -> (writeExpressionName v1)
  Syntax.ClassInstanceCreationExpression_QualifierPrimary v1 -> (writePrimary v1)) q)

writeClassLiteral :: (t0 -> Ast.Expr)
writeClassLiteral _ = (Serialization.cst "STUB:ClassLiteral")

writeClassMemberDeclaration :: (Syntax.ClassMemberDeclaration -> Ast.Expr)
writeClassMemberDeclaration d = ((\x -> case x of
  Syntax.ClassMemberDeclarationField v1 -> (writeFieldDeclaration v1)
  Syntax.ClassMemberDeclarationMethod v1 -> (writeMethodDeclaration v1)
  Syntax.ClassMemberDeclarationClass v1 -> (writeClassDeclaration v1)
  Syntax.ClassMemberDeclarationInterface v1 -> (writeInterfaceDeclaration v1)
  Syntax.ClassMemberDeclarationNone -> (Serialization.cst ";")) d)

writeClassModifier :: (Syntax.ClassModifier -> Ast.Expr)
writeClassModifier m = ((\x -> case x of
  Syntax.ClassModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.ClassModifierPublic -> (Serialization.cst "public")
  Syntax.ClassModifierProtected -> (Serialization.cst "protected")
  Syntax.ClassModifierPrivate -> (Serialization.cst "private")
  Syntax.ClassModifierAbstract -> (Serialization.cst "abstract")
  Syntax.ClassModifierStatic -> (Serialization.cst "static")
  Syntax.ClassModifierFinal -> (Serialization.cst "final")
  Syntax.ClassModifierStrictfp -> (Serialization.cst "strictfp")) m)

writeClassOrInterfaceType :: (Syntax.ClassOrInterfaceType -> Ast.Expr)
writeClassOrInterfaceType cit = ((\x -> case x of
  Syntax.ClassOrInterfaceTypeClass v1 -> (writeClassType v1)
  Syntax.ClassOrInterfaceTypeInterface v1 -> (writeInterfaceType v1)) cit)

writeClassOrInterfaceTypeToInstantiate :: (Syntax.ClassOrInterfaceTypeToInstantiate -> Ast.Expr)
writeClassOrInterfaceTypeToInstantiate coitti =  
  let ids = (Syntax.classOrInterfaceTypeToInstantiateIdentifiers coitti) 
      margs = (Syntax.classOrInterfaceTypeToInstantiateTypeArguments coitti)
  in (Serialization.noSep (Maybes.cat [
    Just (Serialization.dotSep (Lists.map writeAnnotatedIdentifier ids)),
    (Maybes.map writeTypeArgumentsOrDiamond margs)]))

writeClassType :: (Syntax.ClassType -> Ast.Expr)
writeClassType ct =  
  let anns = (Syntax.classTypeAnnotations ct) 
      qual = (Syntax.classTypeQualifier ct)
      id = (Syntax.classTypeIdentifier ct)
      args = (Syntax.classTypeArguments ct)
      qualifiedId = ((\x -> case x of
              Syntax.ClassTypeQualifierNone -> (writeTypeIdentifier id)
              Syntax.ClassTypeQualifierPackage v1 -> (Serialization.dotSep [
                writePackageName v1,
                (writeTypeIdentifier id)])
              Syntax.ClassTypeQualifierParent v1 -> (Serialization.dotSep [
                writeClassOrInterfaceType v1,
                (writeTypeIdentifier id)])) qual)
  in (Serialization.noSep (Maybes.cat [
    Just (Serialization.spaceSep (Maybes.cat [
      Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeAnnotation anns))),
      (Just qualifiedId)])),
    (Logic.ifElse (Lists.null args) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument args))))]))

writeCompilationUnit :: (Syntax.CompilationUnit -> Ast.Expr)
writeCompilationUnit u = ((\x -> case x of
  Syntax.CompilationUnitOrdinary v1 ->  
    let mpkg = (Syntax.ordinaryCompilationUnitPackage v1) 
        imports = (Syntax.ordinaryCompilationUnitImports v1)
        types = (Syntax.ordinaryCompilationUnitTypes v1)
        warning = (Just (singleLineComment Constants.warningAutoGeneratedFile))
        pkgSec = (Maybes.map writePackageDeclaration mpkg)
        importsSec = (Logic.ifElse (Lists.null imports) Nothing (Just (Serialization.newlineSep (Lists.map writeImportDeclaration imports))))
        typesSec = (Logic.ifElse (Lists.null types) Nothing (Just (Serialization.doubleNewlineSep (Lists.map writeTypeDeclarationWithComments types))))
    in (Serialization.doubleNewlineSep (Maybes.cat [
      warning,
      pkgSec,
      importsSec,
      typesSec]))) u)

writeConditionalAndExpression :: (Syntax.ConditionalAndExpression -> Ast.Expr)
writeConditionalAndExpression cae = (Serialization.infixWsList "&&" (Lists.map writeInclusiveOrExpression (Syntax.unConditionalAndExpression cae)))

writeConditionalExpression :: (Syntax.ConditionalExpression -> Ast.Expr)
writeConditionalExpression c = ((\x -> case x of
  Syntax.ConditionalExpressionSimple v1 -> (writeConditionalOrExpression v1)
  Syntax.ConditionalExpressionTernaryCond v1 -> (writeConditionalExpression_TernaryCond v1)
  Syntax.ConditionalExpressionTernaryLambda v1 -> (writeConditionalExpression_TernaryLambda v1)) c)

writeConditionalExpression_TernaryCond :: (t0 -> Ast.Expr)
writeConditionalExpression_TernaryCond _ = (Serialization.cst "STUB:ConditionalExpression_TernaryCond")

writeConditionalExpression_TernaryLambda :: (t0 -> Ast.Expr)
writeConditionalExpression_TernaryLambda _ = (Serialization.cst "STUB:ConditionalExpression_TernaryLambda")

writeConditionalOrExpression :: (Syntax.ConditionalOrExpression -> Ast.Expr)
writeConditionalOrExpression coe = (Serialization.infixWsList "||" (Lists.map writeConditionalAndExpression (Syntax.unConditionalOrExpression coe)))

writeConstantDeclaration :: (Syntax.ConstantDeclaration -> Ast.Expr)
writeConstantDeclaration cd =  
  let mods = (Syntax.constantDeclarationModifiers cd) 
      typ = (Syntax.constantDeclarationType cd)
      vars = (Syntax.constantDeclarationVariables cd)
  in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeConstantModifier mods))),
    (Just (writeUnannType typ)),
    (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableDeclarator vars)))])))

writeConstantModifier :: (t0 -> Ast.Expr)
writeConstantModifier _ = (Serialization.cst "STUB:ConstantModifier")

writeConstructorBody :: (Syntax.ConstructorBody -> Ast.Expr)
writeConstructorBody cb =  
  let minvoc = (Syntax.constructorBodyInvocation cb) 
      stmts = (Syntax.constructorBodyStatements cb)
  in (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Maybes.cat [
    Maybes.map writeExplicitConstructorInvocation minvoc,
    (Just (Serialization.newlineSep (Lists.map writeBlockStatement stmts)))])))

writeConstructorDeclaration :: (Syntax.ConstructorDeclaration -> Ast.Expr)
writeConstructorDeclaration cd =  
  let mods = (Syntax.constructorDeclarationModifiers cd) 
      cons = (Syntax.constructorDeclarationConstructor cd)
      mthrows = (Syntax.constructorDeclarationThrows cd)
      body = (Syntax.constructorDeclarationBody cd)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeConstructorModifier mods))),
    (Just (writeConstructorDeclarator cons)),
    (Maybes.map writeThrows mthrows),
    (Just (writeConstructorBody body))]))

writeConstructorDeclarator :: (Syntax.ConstructorDeclarator -> Ast.Expr)
writeConstructorDeclarator cd =  
  let tparams = (Syntax.constructorDeclaratorParameters cd) 
      name = (Syntax.constructorDeclaratorName cd)
      fparams = (Syntax.constructorDeclaratorFormalParameters cd)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeParameter tparams))),
    (Just (writeSimpleTypeName name)),
    (Just (Serialization.parenList False (Lists.map writeFormalParameter fparams)))]))

writeConstructorModifier :: (Syntax.ConstructorModifier -> Ast.Expr)
writeConstructorModifier m = ((\x -> case x of
  Syntax.ConstructorModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.ConstructorModifierPublic -> (Serialization.cst "public")
  Syntax.ConstructorModifierProtected -> (Serialization.cst "protected")
  Syntax.ConstructorModifierPrivate -> (Serialization.cst "private")) m)

writeContinueStatement :: (Syntax.ContinueStatement -> Ast.Expr)
writeContinueStatement cs =  
  let mlabel = (Syntax.unContinueStatement cs)
  in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "continue"),
    (Maybes.map writeIdentifier mlabel)])))

writeDims :: (Syntax.Dims -> Ast.Expr)
writeDims d = (Serialization.noSep (Lists.map (\_ -> Serialization.cst "[]") (Syntax.unDims d)))

writeDoStatement :: (t0 -> Ast.Expr)
writeDoStatement _ = (Serialization.cst "STUB:DoStatement")

writeElementValue :: (Syntax.ElementValue -> Ast.Expr)
writeElementValue ev = ((\x -> case x of
  Syntax.ElementValueConditionalExpression v1 -> (writeConditionalExpression v1)
  Syntax.ElementValueElementValueArrayInitializer v1 -> (Serialization.commaSep Serialization.inlineStyle (Lists.map writeElementValue (Syntax.unElementValueArrayInitializer v1)))
  Syntax.ElementValueAnnotation v1 -> (writeAnnotation v1)) ev)

writeElementValuePair :: (Syntax.ElementValuePair -> Ast.Expr)
writeElementValuePair evp =  
  let k = (Syntax.elementValuePairKey evp) 
      v = (Syntax.elementValuePairValue evp)
  in (Serialization.infixWs "=" (writeIdentifier k) (writeElementValue v))

writeEnumDeclaration :: (t0 -> Ast.Expr)
writeEnumDeclaration _ = (Serialization.cst "STUB:EnumDeclaration")

writeEqualityExpression :: (Syntax.EqualityExpression -> Ast.Expr)
writeEqualityExpression e = ((\x -> case x of
  Syntax.EqualityExpressionUnary v1 -> (writeRelationalExpression v1)
  Syntax.EqualityExpressionEqual v1 -> (Serialization.infixWs "==" (writeEqualityExpression (Syntax.equalityExpression_BinaryLhs v1)) (writeRelationalExpression (Syntax.equalityExpression_BinaryRhs v1)))
  Syntax.EqualityExpressionNotEqual v1 -> (Serialization.infixWs "!=" (writeEqualityExpression (Syntax.equalityExpression_BinaryLhs v1)) (writeRelationalExpression (Syntax.equalityExpression_BinaryRhs v1)))) e)

writeExclusiveOrExpression :: (Syntax.ExclusiveOrExpression -> Ast.Expr)
writeExclusiveOrExpression eoe = (Serialization.infixWsList "^" (Lists.map writeAndExpression (Syntax.unExclusiveOrExpression eoe)))

writeExplicitConstructorInvocation :: (t0 -> Ast.Expr)
writeExplicitConstructorInvocation _ = (Serialization.cst "STUB:ExplicitConstructorInvocation")

writeExpression :: (Syntax.Expression -> Ast.Expr)
writeExpression e = ((\x -> case x of
  Syntax.ExpressionLambda v1 -> (writeLambdaExpression v1)
  Syntax.ExpressionAssignment v1 -> (writeAssignmentExpression v1)) e)

writeExpressionName :: (Syntax.ExpressionName -> Ast.Expr)
writeExpressionName en =  
  let mqual = (Syntax.expressionNameQualifier en) 
      id = (Syntax.expressionNameIdentifier en)
  in (Serialization.dotSep (Maybes.cat [
    Maybes.map writeAmbiguousName mqual,
    (Just (writeIdentifier id))]))

writeExpressionStatement :: (Syntax.ExpressionStatement -> Ast.Expr)
writeExpressionStatement es = (Serialization.withSemi (writeStatementExpression (Syntax.unExpressionStatement es)))

writeFieldAccess :: (Syntax.FieldAccess -> Ast.Expr)
writeFieldAccess fa =  
  let qual = (Syntax.fieldAccessQualifier fa) 
      id = (Syntax.fieldAccessIdentifier fa)
  in ((\x -> case x of
    Syntax.FieldAccess_QualifierPrimary v1 -> (Serialization.dotSep [
      writePrimary v1,
      (writeIdentifier id)])
    Syntax.FieldAccess_QualifierSuper -> (Serialization.dotSep [
      Serialization.cst "super",
      (writeIdentifier id)])
    Syntax.FieldAccess_QualifierTyped v1 -> (Serialization.dotSep [
      writeTypeName v1,
      (Serialization.cst "super"),
      (writeIdentifier id)])) qual)

writeFieldDeclaration :: (Syntax.FieldDeclaration -> Ast.Expr)
writeFieldDeclaration fd =  
  let mods = (Syntax.fieldDeclarationModifiers fd) 
      typ = (Syntax.fieldDeclarationUnannType fd)
      vars = (Syntax.fieldDeclarationVariableDeclarators fd)
  in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeFieldModifier mods))),
    (Just (writeUnannType typ)),
    (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableDeclarator vars)))])))

writeFieldModifier :: (Syntax.FieldModifier -> Ast.Expr)
writeFieldModifier m = ((\x -> case x of
  Syntax.FieldModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.FieldModifierPublic -> (Serialization.cst "public")
  Syntax.FieldModifierProtected -> (Serialization.cst "protected")
  Syntax.FieldModifierPrivate -> (Serialization.cst "private")
  Syntax.FieldModifierStatic -> (Serialization.cst "static")
  Syntax.FieldModifierFinal -> (Serialization.cst "final")
  Syntax.FieldModifierTransient -> (Serialization.cst "transient")
  Syntax.FieldModifierVolatile -> (Serialization.cst "volatile")) m)

writeFloatingPointLiteral :: (Syntax.FloatingPointLiteral -> Ast.Expr)
writeFloatingPointLiteral fl = (Serialization.cst (Literals.showFloat64 (Syntax.unFloatingPointLiteral fl)))

writeFloatingPointType :: (Syntax.FloatingPointType -> Ast.Expr)
writeFloatingPointType ft = ((\x -> case x of
  Syntax.FloatingPointTypeFloat -> (Serialization.cst "float")
  Syntax.FloatingPointTypeDouble -> (Serialization.cst "double")) ft)

writeForStatement :: (t0 -> Ast.Expr)
writeForStatement _ = (Serialization.cst "STUB:ForStatement")

writeFormalParameter :: (Syntax.FormalParameter -> Ast.Expr)
writeFormalParameter p = ((\x -> case x of
  Syntax.FormalParameterSimple v1 -> (writeFormalParameter_Simple v1)
  Syntax.FormalParameterVariableArity v1 -> (writeVariableArityParameter v1)) p)

writeFormalParameter_Simple :: (Syntax.FormalParameter_Simple -> Ast.Expr)
writeFormalParameter_Simple fps =  
  let mods = (Syntax.formalParameter_SimpleModifiers fps) 
      typ = (Syntax.formalParameter_SimpleType fps)
      id = (Syntax.formalParameter_SimpleId fps)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeVariableModifier mods))),
    (Just (writeUnannType typ)),
    (Just (writeVariableDeclaratorId id))]))

writeIdentifier :: (Syntax.Identifier -> Ast.Expr)
writeIdentifier id = (Serialization.cst (Syntax.unIdentifier id))

writeIfThenStatement :: (Syntax.IfThenStatement -> Ast.Expr)
writeIfThenStatement its =  
  let cond = (Syntax.ifThenStatementExpression its) 
      thn = (Syntax.ifThenStatementStatement its)
  in (Serialization.spaceSep [
    Serialization.cst "if",
    (Serialization.parenList False [
      writeExpression cond]),
    (Serialization.curlyBlock Serialization.fullBlockStyle (writeStatement thn))])

writeIfThenElseStatement :: (t0 -> Ast.Expr)
writeIfThenElseStatement _ = (Serialization.cst "STUB:IfThenElseStatement")

writeImportDeclaration :: (Syntax.ImportDeclaration -> Ast.Expr)
writeImportDeclaration imp = ((\x -> case x of
  Syntax.ImportDeclarationSingleType v1 -> (Serialization.withSemi (Serialization.spaceSep [
    Serialization.cst "import",
    (writeTypeName (Syntax.unSingleTypeImportDeclaration v1))]))
  Syntax.ImportDeclarationTypeImportOnDemand _ -> (Serialization.cst "STUB:ImportDeclarationTypeImportOnDemand")
  Syntax.ImportDeclarationSingleStaticImport _ -> (Serialization.cst "STUB:ImportDeclarationSingleStaticImport")
  Syntax.ImportDeclarationStaticImportOnDemand _ -> (Serialization.cst "STUB:ImportDeclarationStaticImportOnDemand")) imp)

writeInclusiveOrExpression :: (Syntax.InclusiveOrExpression -> Ast.Expr)
writeInclusiveOrExpression ioe = (Serialization.infixWsList "|" (Lists.map writeExclusiveOrExpression (Syntax.unInclusiveOrExpression ioe)))

writeInstanceInitializer :: (t0 -> Ast.Expr)
writeInstanceInitializer _ = (Serialization.cst "STUB:InstanceInitializer")

writeIntegerLiteral :: (Syntax.IntegerLiteral -> Ast.Expr)
writeIntegerLiteral il =  
  let i = (Syntax.unIntegerLiteral il) 
      suffix = (Logic.ifElse (Logic.or (Equality.gt i 2147483647) (Equality.lt i (-2147483648))) "L" "")
  in (Serialization.cst (Strings.cat2 (Literals.showBigint i) suffix))

writeIntegralType :: (Syntax.IntegralType -> Ast.Expr)
writeIntegralType t = ((\x -> case x of
  Syntax.IntegralTypeByte -> (Serialization.cst "byte")
  Syntax.IntegralTypeShort -> (Serialization.cst "short")
  Syntax.IntegralTypeInt -> (Serialization.cst "int")
  Syntax.IntegralTypeLong -> (Serialization.cst "long")
  Syntax.IntegralTypeChar -> (Serialization.cst "char")) t)

writeInterfaceBody :: (Syntax.InterfaceBody -> Ast.Expr)
writeInterfaceBody ib = (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.doubleNewlineSep (Lists.map writeInterfaceMemberDeclaration (Syntax.unInterfaceBody ib))))

writeInterfaceDeclaration :: (Syntax.InterfaceDeclaration -> Ast.Expr)
writeInterfaceDeclaration d = ((\x -> case x of
  Syntax.InterfaceDeclarationNormalInterface v1 -> (writeNormalInterfaceDeclaration v1)
  Syntax.InterfaceDeclarationAnnotationType v1 -> (writeAnnotationTypeDeclaration v1)) d)

writeInterfaceMemberDeclaration :: (Syntax.InterfaceMemberDeclaration -> Ast.Expr)
writeInterfaceMemberDeclaration d = ((\x -> case x of
  Syntax.InterfaceMemberDeclarationConstant v1 -> (writeConstantDeclaration v1)
  Syntax.InterfaceMemberDeclarationInterfaceMethod v1 -> (writeInterfaceMethodDeclaration v1)
  Syntax.InterfaceMemberDeclarationClass v1 -> (writeClassDeclaration v1)
  Syntax.InterfaceMemberDeclarationInterface v1 -> (writeInterfaceDeclaration v1)) d)

writeInterfaceMethodDeclaration :: (Syntax.InterfaceMethodDeclaration -> Ast.Expr)
writeInterfaceMethodDeclaration imd =  
  let mods = (Syntax.interfaceMethodDeclarationModifiers imd) 
      header = (Syntax.interfaceMethodDeclarationHeader imd)
      body = (Syntax.interfaceMethodDeclarationBody imd)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeInterfaceMethodModifier mods))),
    (Just (writeMethodHeader header)),
    (Just (writeMethodBody body))]))

writeInterfaceMethodModifier :: (Syntax.InterfaceMethodModifier -> Ast.Expr)
writeInterfaceMethodModifier m = ((\x -> case x of
  Syntax.InterfaceMethodModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.InterfaceMethodModifierPublic -> (Serialization.cst "public")
  Syntax.InterfaceMethodModifierPrivate -> (Serialization.cst "private")
  Syntax.InterfaceMethodModifierAbstract -> (Serialization.cst "abstract")
  Syntax.InterfaceMethodModifierDefault -> (Serialization.cst "default")
  Syntax.InterfaceMethodModifierStatic -> (Serialization.cst "static")
  Syntax.InterfaceMethodModifierStrictfp -> (Serialization.cst "strictfp")) m)

writeInterfaceModifier :: (Syntax.InterfaceModifier -> Ast.Expr)
writeInterfaceModifier m = ((\x -> case x of
  Syntax.InterfaceModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.InterfaceModifierPublic -> (Serialization.cst "public")
  Syntax.InterfaceModifierProtected -> (Serialization.cst "protected")
  Syntax.InterfaceModifierPrivate -> (Serialization.cst "private")
  Syntax.InterfaceModifierAbstract -> (Serialization.cst "abstract")
  Syntax.InterfaceModifierStatic -> (Serialization.cst "static")
  Syntax.InterfaceModifierStrictfb -> (Serialization.cst "strictfb")) m)

writeInterfaceType :: (Syntax.InterfaceType -> Ast.Expr)
writeInterfaceType it = (writeClassType (Syntax.unInterfaceType it))

writeLabeledStatement :: (t0 -> Ast.Expr)
writeLabeledStatement _ = (Serialization.cst "STUB:LabeledStatement")

writeLambdaBody :: (Syntax.LambdaBody -> Ast.Expr)
writeLambdaBody b = ((\x -> case x of
  Syntax.LambdaBodyExpression v1 -> (writeExpression v1)
  Syntax.LambdaBodyBlock v1 -> (writeBlock v1)) b)

writeLambdaExpression :: (Syntax.LambdaExpression -> Ast.Expr)
writeLambdaExpression le =  
  let params = (Syntax.lambdaExpressionParameters le) 
      body = (Syntax.lambdaExpressionBody le)
  in (Serialization.infixWs "->" (writeLambdaParameters params) (writeLambdaBody body))

writeLambdaParameters :: (Syntax.LambdaParameters -> Ast.Expr)
writeLambdaParameters p = ((\x -> case x of
  Syntax.LambdaParametersTuple v1 -> (Serialization.parenList False (Lists.map writeLambdaParameters v1))
  Syntax.LambdaParametersSingle v1 -> (writeIdentifier v1)) p)

writeLeftHandSide :: (Syntax.LeftHandSide -> Ast.Expr)
writeLeftHandSide lhs = ((\x -> case x of
  Syntax.LeftHandSideExpressionName v1 -> (writeExpressionName v1)
  Syntax.LeftHandSideFieldAccess v1 -> (writeFieldAccess v1)
  Syntax.LeftHandSideArrayAccess v1 -> (writeArrayAccess v1)) lhs)

writeLiteral :: (Syntax.Literal -> Ast.Expr)
writeLiteral l = ((\x -> case x of
  Syntax.LiteralNull -> (Serialization.cst "null")
  Syntax.LiteralInteger v1 -> (writeIntegerLiteral v1)
  Syntax.LiteralFloatingPoint v1 -> (writeFloatingPointLiteral v1)
  Syntax.LiteralBoolean v1 -> (Serialization.cst (Logic.ifElse v1 "true" "false"))
  Syntax.LiteralCharacter v1 -> (Serialization.cst (Strings.cat2 "'" (Strings.cat2 (Logic.ifElse (Equality.equal v1 39) "\\'" (Logic.ifElse (Equality.equal v1 92) "\\\\" (Logic.ifElse (Equality.equal v1 10) "\\n" (Logic.ifElse (Equality.equal v1 13) "\\r" (Logic.ifElse (Equality.equal v1 9) "\\t" (Logic.ifElse (Logic.and (Equality.gte v1 32) (Equality.lt v1 127)) (Strings.fromList [
    v1]) (javaUnicodeEscape v1))))))) "'")))
  Syntax.LiteralString v1 -> (writeStringLiteral v1)) l)

writeLocalVariableDeclaration :: (Syntax.LocalVariableDeclaration -> Ast.Expr)
writeLocalVariableDeclaration lvd =  
  let mods = (Syntax.localVariableDeclarationModifiers lvd) 
      t = (Syntax.localVariableDeclarationType lvd)
      decls = (Syntax.localVariableDeclarationDeclarators lvd)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeVariableModifier mods))),
    (Just (writeLocalName t)),
    (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeVariableDeclarator decls)))]))

writeLocalVariableDeclarationStatement :: (Syntax.LocalVariableDeclarationStatement -> Ast.Expr)
writeLocalVariableDeclarationStatement lvds = (Serialization.withSemi (writeLocalVariableDeclaration (Syntax.unLocalVariableDeclarationStatement lvds)))

writeLocalName :: (Syntax.LocalVariableType -> Ast.Expr)
writeLocalName t = ((\x -> case x of
  Syntax.LocalVariableTypeType v1 -> (writeUnannType v1)
  Syntax.LocalVariableTypeVar -> (Serialization.cst "var")) t)

writeMarkerAnnotation :: (Syntax.MarkerAnnotation -> Ast.Expr)
writeMarkerAnnotation ma = (Serialization.prefix "@" (writeTypeName (Syntax.unMarkerAnnotation ma)))

writeMethodBody :: (Syntax.MethodBody -> Ast.Expr)
writeMethodBody b = ((\x -> case x of
  Syntax.MethodBodyBlock v1 -> (writeBlock v1)
  Syntax.MethodBodyNone -> (Serialization.cst ";")) b)

writeMethodDeclaration :: (Syntax.MethodDeclaration -> Ast.Expr)
writeMethodDeclaration md =  
  let anns = (Syntax.methodDeclarationAnnotations md) 
      mods = (Syntax.methodDeclarationModifiers md)
      header = (Syntax.methodDeclarationHeader md)
      body = (Syntax.methodDeclarationBody md)
      headerAndBody = (Serialization.spaceSep (Maybes.cat [
              Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeMethodModifier mods))),
              (Just (writeMethodHeader header)),
              (Just (writeMethodBody body))]))
  in (Serialization.newlineSep (Maybes.cat [
    Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.newlineSep (Lists.map writeAnnotation anns))),
    (Just headerAndBody)]))

writeMethodDeclarator :: (Syntax.MethodDeclarator -> Ast.Expr)
writeMethodDeclarator md =  
  let id = (Syntax.methodDeclaratorIdentifier md) 
      params = (Syntax.methodDeclaratorFormalParameters md)
  in (Serialization.noSep [
    writeIdentifier id,
    (Serialization.parenList False (Lists.map writeFormalParameter params))])

writeMethodHeader :: (Syntax.MethodHeader -> Ast.Expr)
writeMethodHeader mh =  
  let params = (Syntax.methodHeaderParameters mh) 
      result = (Syntax.methodHeaderResult mh)
      decl = (Syntax.methodHeaderDeclarator mh)
      mthrows = (Syntax.methodHeaderThrows mh)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null params) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeParameter params))),
    (Just (writeResult result)),
    (Just (writeMethodDeclarator decl)),
    (Maybes.map writeThrows mthrows)]))

writeMethodInvocation :: (Syntax.MethodInvocation -> Ast.Expr)
writeMethodInvocation mi =  
  let header = (Syntax.methodInvocationHeader mi) 
      args = (Syntax.methodInvocationArguments mi)
      argSec = (Serialization.parenList True (Lists.map writeExpression args))
      headerSec = ((\x -> case x of
              Syntax.MethodInvocation_HeaderSimple v1 -> (writeMethodName v1)
              Syntax.MethodInvocation_HeaderComplex v1 ->  
                let cvar = (Syntax.methodInvocation_ComplexVariant v1) 
                    targs = (Syntax.methodInvocation_ComplexTypeArguments v1)
                    cid = (Syntax.methodInvocation_ComplexIdentifier v1)
                    idSec = (Serialization.noSep (Maybes.cat [
                            Logic.ifElse (Lists.null targs) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument targs))),
                            (Just (writeIdentifier cid))]))
                in ((\x -> case x of
                  Syntax.MethodInvocation_VariantType v2 -> (Serialization.dotSep [
                    writeTypeName v2,
                    idSec])
                  Syntax.MethodInvocation_VariantExpression v2 -> (Serialization.dotSep [
                    writeExpressionName v2,
                    idSec])
                  Syntax.MethodInvocation_VariantPrimary v2 -> (Serialization.dotSep [
                    writePrimary v2,
                    idSec])
                  Syntax.MethodInvocation_VariantSuper -> (Serialization.dotSep [
                    Serialization.cst "super",
                    idSec])
                  Syntax.MethodInvocation_VariantTypeSuper v2 -> (Serialization.dotSep [
                    writeTypeName v2,
                    (Serialization.cst "super"),
                    idSec])) cvar)) header)
  in (Serialization.noSep [
    headerSec,
    argSec])

writeMethodModifier :: (Syntax.MethodModifier -> Ast.Expr)
writeMethodModifier m = ((\x -> case x of
  Syntax.MethodModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.MethodModifierPublic -> (Serialization.cst "public")
  Syntax.MethodModifierProtected -> (Serialization.cst "protected")
  Syntax.MethodModifierPrivate -> (Serialization.cst "private")
  Syntax.MethodModifierAbstract -> (Serialization.cst "abstract")
  Syntax.MethodModifierFinal -> (Serialization.cst "final")
  Syntax.MethodModifierSynchronized -> (Serialization.cst "synchronized")
  Syntax.MethodModifierNative -> (Serialization.cst "native")
  Syntax.MethodModifierStrictfb -> (Serialization.cst "strictfb")) m)

writeMethodName :: (Syntax.MethodName -> Ast.Expr)
writeMethodName mn = (writeIdentifier (Syntax.unMethodName mn))

writeMethodReference :: (t0 -> Ast.Expr)
writeMethodReference _ = (Serialization.cst "STUB:MethodReference")

writeMultiplicativeExpression :: (Syntax.MultiplicativeExpression -> Ast.Expr)
writeMultiplicativeExpression e = ((\x -> case x of
  Syntax.MultiplicativeExpressionUnary v1 -> (writeUnaryExpression v1)
  Syntax.MultiplicativeExpressionTimes v1 -> (Serialization.infixWs "*" (writeMultiplicativeExpression (Syntax.multiplicativeExpression_BinaryLhs v1)) (writeUnaryExpression (Syntax.multiplicativeExpression_BinaryRhs v1)))
  Syntax.MultiplicativeExpressionDivide v1 -> (Serialization.infixWs "/" (writeMultiplicativeExpression (Syntax.multiplicativeExpression_BinaryLhs v1)) (writeUnaryExpression (Syntax.multiplicativeExpression_BinaryRhs v1)))
  Syntax.MultiplicativeExpressionMod v1 -> (Serialization.infixWs "%" (writeMultiplicativeExpression (Syntax.multiplicativeExpression_BinaryLhs v1)) (writeUnaryExpression (Syntax.multiplicativeExpression_BinaryRhs v1)))) e)

writeNormalAnnotation :: (Syntax.NormalAnnotation -> Ast.Expr)
writeNormalAnnotation na =  
  let tname = (Syntax.normalAnnotationTypeName na) 
      pairs = (Syntax.normalAnnotationPairs na)
  in (Serialization.prefix "@" (Serialization.noSep [
    writeTypeName tname,
    (Serialization.commaSep Serialization.inlineStyle (Lists.map writeElementValuePair pairs))]))

writeNormalClassDeclaration :: (Syntax.NormalClassDeclaration -> Ast.Expr)
writeNormalClassDeclaration ncd =  
  let mods = (Syntax.normalClassDeclarationModifiers ncd) 
      id = (Syntax.normalClassDeclarationIdentifier ncd)
      tparams = (Syntax.normalClassDeclarationParameters ncd)
      msuperc = (Syntax.normalClassDeclarationExtends ncd)
      superi = (Syntax.normalClassDeclarationImplements ncd)
      body = (Syntax.normalClassDeclarationBody ncd)
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

writeNormalInterfaceDeclaration :: (Syntax.NormalInterfaceDeclaration -> Ast.Expr)
writeNormalInterfaceDeclaration nid =  
  let mods = (Syntax.normalInterfaceDeclarationModifiers nid) 
      id = (Syntax.normalInterfaceDeclarationIdentifier nid)
      tparams = (Syntax.normalInterfaceDeclarationParameters nid)
      extends = (Syntax.normalInterfaceDeclarationExtends nid)
      body = (Syntax.normalInterfaceDeclarationBody nid)
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

writeNumericType :: (Syntax.NumericType -> Ast.Expr)
writeNumericType nt = ((\x -> case x of
  Syntax.NumericTypeIntegral v1 -> (writeIntegralType v1)
  Syntax.NumericTypeFloatingPoint v1 -> (writeFloatingPointType v1)) nt)

writePackageDeclaration :: (Syntax.PackageDeclaration -> Ast.Expr)
writePackageDeclaration pd =  
  let mods = (Syntax.packageDeclarationModifiers pd) 
      ids = (Syntax.packageDeclarationIdentifiers pd)
  in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writePackageModifier mods))),
    (Just (Serialization.spaceSep [
      Serialization.cst "package",
      (Serialization.cst (Strings.intercalate "." (Lists.map (\id -> Syntax.unIdentifier id) ids)))]))])))

writePackageName :: (Syntax.PackageName -> Ast.Expr)
writePackageName pn = (Serialization.dotSep (Lists.map writeIdentifier (Syntax.unPackageName pn)))

writePackageOrTypeName :: (Syntax.PackageOrTypeName -> Ast.Expr)
writePackageOrTypeName potn = (Serialization.dotSep (Lists.map writeIdentifier (Syntax.unPackageOrTypeName potn)))

writePackageModifier :: (Syntax.PackageModifier -> Ast.Expr)
writePackageModifier pm = (writeAnnotation (Syntax.unPackageModifier pm))

writePostDecrementExpression :: (t0 -> Ast.Expr)
writePostDecrementExpression _ = (Serialization.cst "STUB:PostDecrementExpression")

writePostIncrementExpression :: (t0 -> Ast.Expr)
writePostIncrementExpression _ = (Serialization.cst "STUB:PostIncrementExpression")

writePostfixExpression :: (Syntax.PostfixExpression -> Ast.Expr)
writePostfixExpression e = ((\x -> case x of
  Syntax.PostfixExpressionPrimary v1 -> (writePrimary v1)
  Syntax.PostfixExpressionName v1 -> (writeExpressionName v1)
  Syntax.PostfixExpressionPostIncrement v1 -> (writePostIncrementExpression v1)
  Syntax.PostfixExpressionPostDecrement v1 -> (writePostDecrementExpression v1)) e)

writePreDecrementExpression :: (t0 -> Ast.Expr)
writePreDecrementExpression _ = (Serialization.cst "STUB:PreDecrementExpression")

writePreIncrementExpression :: (t0 -> Ast.Expr)
writePreIncrementExpression _ = (Serialization.cst "STUB:PreIncrementExpression")

writePrimary :: (Syntax.Primary -> Ast.Expr)
writePrimary p = ((\x -> case x of
  Syntax.PrimaryNoNewArray_ v1 -> (writePrimaryNoNewArray v1)
  Syntax.PrimaryArrayCreation v1 -> (writeArrayCreationExpression v1)) p)

writePrimaryNoNewArray :: (Syntax.PrimaryNoNewArray -> Ast.Expr)
writePrimaryNoNewArray p = ((\x -> case x of
  Syntax.PrimaryNoNewArrayLiteral v1 -> (writeLiteral v1)
  Syntax.PrimaryNoNewArrayClassLiteral v1 -> (writeClassLiteral v1)
  Syntax.PrimaryNoNewArrayThis -> (Serialization.cst "this")
  Syntax.PrimaryNoNewArrayDotThis v1 -> (Serialization.dotSep [
    writeTypeName v1,
    (Serialization.cst "this")])
  Syntax.PrimaryNoNewArrayParens v1 -> (Serialization.parenList False [
    writeExpression v1])
  Syntax.PrimaryNoNewArrayClassInstance v1 -> (writeClassInstanceCreationExpression v1)
  Syntax.PrimaryNoNewArrayFieldAccess v1 -> (writeFieldAccess v1)
  Syntax.PrimaryNoNewArrayArrayAccess v1 -> (writeArrayAccess v1)
  Syntax.PrimaryNoNewArrayMethodInvocation v1 -> (writeMethodInvocation v1)
  Syntax.PrimaryNoNewArrayMethodReference v1 -> (writeMethodReference v1)) p)

writePrimitiveType :: (Syntax.PrimitiveType -> Ast.Expr)
writePrimitiveType pt = ((\x -> case x of
  Syntax.PrimitiveTypeNumeric v1 -> (writeNumericType v1)
  Syntax.PrimitiveTypeBoolean -> (Serialization.cst "boolean")) pt)

writePrimitiveTypeWithAnnotations :: (Syntax.PrimitiveTypeWithAnnotations -> Ast.Expr)
writePrimitiveTypeWithAnnotations ptwa =  
  let pt = (Syntax.primitiveTypeWithAnnotationsType ptwa) 
      anns = (Syntax.primitiveTypeWithAnnotationsAnnotations ptwa)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.spaceSep (Lists.map writeAnnotation anns))),
    (Just (writePrimitiveType pt))]))

writeReceiverParameter :: (t0 -> Ast.Expr)
writeReceiverParameter _ = (Serialization.cst "STUB:ReceiverParameter")

writeReferenceType :: (Syntax.ReferenceType -> Ast.Expr)
writeReferenceType rt = ((\x -> case x of
  Syntax.ReferenceTypeClassOrInterface v1 -> (writeClassOrInterfaceType v1)
  Syntax.ReferenceTypeVariable v1 -> (writeTypeVariable v1)
  Syntax.ReferenceTypeArray v1 -> (writeArrayType v1)) rt)

writeRelationalExpression :: (Syntax.RelationalExpression -> Ast.Expr)
writeRelationalExpression e = ((\x -> case x of
  Syntax.RelationalExpressionSimple v1 -> (writeShiftExpression v1)
  Syntax.RelationalExpressionLessThan v1 -> (writeRelationalExpression_LessThan v1)
  Syntax.RelationalExpressionGreaterThan v1 -> (writeRelationalExpression_GreaterThan v1)
  Syntax.RelationalExpressionLessThanEqual v1 -> (writeRelationalExpression_LessThanEqual v1)
  Syntax.RelationalExpressionGreaterThanEqual v1 -> (writeRelationalExpression_GreaterThanEqual v1)
  Syntax.RelationalExpressionInstanceof v1 -> (writeRelationalExpression_InstanceOf v1)) e)

writeRelationalExpression_GreaterThan :: (Syntax.RelationalExpression_GreaterThan -> Ast.Expr)
writeRelationalExpression_GreaterThan gt = (Serialization.infixWs ">" (writeRelationalExpression (Syntax.relationalExpression_GreaterThanLhs gt)) (writeShiftExpression (Syntax.relationalExpression_GreaterThanRhs gt)))

writeRelationalExpression_GreaterThanEqual :: (Syntax.RelationalExpression_GreaterThanEqual -> Ast.Expr)
writeRelationalExpression_GreaterThanEqual gte = (Serialization.infixWs ">=" (writeRelationalExpression (Syntax.relationalExpression_GreaterThanEqualLhs gte)) (writeShiftExpression (Syntax.relationalExpression_GreaterThanEqualRhs gte)))

writeRelationalExpression_InstanceOf :: (Syntax.RelationalExpression_InstanceOf -> Ast.Expr)
writeRelationalExpression_InstanceOf io = (Serialization.infixWs "instanceof" (writeRelationalExpression (Syntax.relationalExpression_InstanceOfLhs io)) (writeReferenceType (Syntax.relationalExpression_InstanceOfRhs io)))

writeRelationalExpression_LessThan :: (Syntax.RelationalExpression_LessThan -> Ast.Expr)
writeRelationalExpression_LessThan lt = (Serialization.infixWs "<" (writeRelationalExpression (Syntax.relationalExpression_LessThanLhs lt)) (writeShiftExpression (Syntax.relationalExpression_LessThanRhs lt)))

writeRelationalExpression_LessThanEqual :: (Syntax.RelationalExpression_LessThanEqual -> Ast.Expr)
writeRelationalExpression_LessThanEqual lte = (Serialization.infixWs "<=" (writeRelationalExpression (Syntax.relationalExpression_LessThanEqualLhs lte)) (writeShiftExpression (Syntax.relationalExpression_LessThanEqualRhs lte)))

writeResult :: (Syntax.Result -> Ast.Expr)
writeResult r = ((\x -> case x of
  Syntax.ResultType v1 -> (writeUnannType v1)
  Syntax.ResultVoid -> (Serialization.cst "void")) r)

writeReturnStatement :: (Syntax.ReturnStatement -> Ast.Expr)
writeReturnStatement rs =  
  let mex = (Syntax.unReturnStatement rs)
  in (Serialization.withSemi (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "return"),
    (Maybes.map writeExpression mex)])))

writeShiftExpression :: (Syntax.ShiftExpression -> Ast.Expr)
writeShiftExpression e = ((\x -> case x of
  Syntax.ShiftExpressionUnary v1 -> (writeAdditiveExpression v1)
  Syntax.ShiftExpressionShiftLeft v1 -> (Serialization.infixWs "<<" (writeShiftExpression (Syntax.shiftExpression_BinaryLhs v1)) (writeAdditiveExpression (Syntax.shiftExpression_BinaryRhs v1)))
  Syntax.ShiftExpressionShiftRight v1 -> (Serialization.infixWs ">>" (writeShiftExpression (Syntax.shiftExpression_BinaryLhs v1)) (writeAdditiveExpression (Syntax.shiftExpression_BinaryRhs v1)))
  Syntax.ShiftExpressionShiftRightZeroFill v1 -> (Serialization.infixWs ">>>" (writeShiftExpression (Syntax.shiftExpression_BinaryLhs v1)) (writeAdditiveExpression (Syntax.shiftExpression_BinaryRhs v1)))) e)

writeSimpleTypeName :: (Syntax.SimpleTypeName -> Ast.Expr)
writeSimpleTypeName stn = (writeTypeIdentifier (Syntax.unSimpleTypeName stn))

writeSingleElementAnnotation :: (Syntax.SingleElementAnnotation -> Ast.Expr)
writeSingleElementAnnotation sea =  
  let tname = (Syntax.singleElementAnnotationName sea) 
      mv = (Syntax.singleElementAnnotationValue sea)
  in (Maybes.maybe (writeMarkerAnnotation (Syntax.MarkerAnnotation tname)) (\v -> Serialization.prefix "@" (Serialization.noSep [
    writeTypeName tname,
    (Serialization.parenList False [
      writeElementValue v])])) mv)

writeStatement :: (Syntax.Statement -> Ast.Expr)
writeStatement s = ((\x -> case x of
  Syntax.StatementWithoutTrailing v1 -> (writeStatementWithoutTrailingSubstatement v1)
  Syntax.StatementLabeled v1 -> (writeLabeledStatement v1)
  Syntax.StatementIfThen v1 -> (writeIfThenStatement v1)
  Syntax.StatementIfThenElse v1 -> (writeIfThenElseStatement v1)
  Syntax.StatementWhile v1 -> (writeWhileStatement v1)
  Syntax.StatementFor v1 -> (writeForStatement v1)) s)

writeStatementExpression :: (Syntax.StatementExpression -> Ast.Expr)
writeStatementExpression e = ((\x -> case x of
  Syntax.StatementExpressionAssignment v1 -> (writeAssignment v1)
  Syntax.StatementExpressionPreIncrement v1 -> (writePreIncrementExpression v1)
  Syntax.StatementExpressionPreDecrement v1 -> (writePreDecrementExpression v1)
  Syntax.StatementExpressionPostIncrement v1 -> (writePostIncrementExpression v1)
  Syntax.StatementExpressionPostDecrement v1 -> (writePostDecrementExpression v1)
  Syntax.StatementExpressionMethodInvocation v1 -> (writeMethodInvocation v1)
  Syntax.StatementExpressionClassInstanceCreation v1 -> (writeClassInstanceCreationExpression v1)) e)

writeStatementWithoutTrailingSubstatement :: (Syntax.StatementWithoutTrailingSubstatement -> Ast.Expr)
writeStatementWithoutTrailingSubstatement s = ((\x -> case x of
  Syntax.StatementWithoutTrailingSubstatementBlock v1 -> (writeBlock v1)
  Syntax.StatementWithoutTrailingSubstatementEmpty -> (Serialization.cst ";")
  Syntax.StatementWithoutTrailingSubstatementExpression v1 -> (writeExpressionStatement v1)
  Syntax.StatementWithoutTrailingSubstatementAssert v1 -> (writeAssertStatement v1)
  Syntax.StatementWithoutTrailingSubstatementSwitch v1 -> (writeSwitchStatement v1)
  Syntax.StatementWithoutTrailingSubstatementDo v1 -> (writeDoStatement v1)
  Syntax.StatementWithoutTrailingSubstatementBreak v1 -> (writeBreakStatement v1)
  Syntax.StatementWithoutTrailingSubstatementContinue v1 -> (writeContinueStatement v1)
  Syntax.StatementWithoutTrailingSubstatementReturn v1 -> (writeReturnStatement v1)
  Syntax.StatementWithoutTrailingSubstatementSynchronized v1 -> (writeSynchronizedStatement v1)
  Syntax.StatementWithoutTrailingSubstatementThrow v1 -> (writeThrowStatement v1)
  Syntax.StatementWithoutTrailingSubstatementTry v1 -> (writeTryStatement v1)) s)

writeStaticInitializer :: (t0 -> Ast.Expr)
writeStaticInitializer _ = (Serialization.cst "STUB:StaticInitializer")

-- | Serialize a Java string literal with proper Unicode escaping.
writeStringLiteral :: (Syntax.StringLiteral -> Ast.Expr)
writeStringLiteral sl =  
  let s = (Syntax.unStringLiteral sl)
  in (Serialization.cst (Strings.cat2 "\"" (Strings.cat2 (escapeJavaString s) "\"")))

writeSwitchStatement :: (t0 -> Ast.Expr)
writeSwitchStatement _ = (Serialization.cst "STUB:SwitchStatement")

writeSynchronizedStatement :: (t0 -> Ast.Expr)
writeSynchronizedStatement _ = (Serialization.cst "STUB:SynchronizedStatement")

writeThrowStatement :: (Syntax.ThrowStatement -> Ast.Expr)
writeThrowStatement ts = (Serialization.withSemi (Serialization.spaceSep [
  Serialization.cst "throw",
  (writeExpression (Syntax.unThrowStatement ts))]))

writeThrows :: (t0 -> Ast.Expr)
writeThrows _ = (Serialization.cst "STUB:Throws")

writeTryStatement :: (t0 -> Ast.Expr)
writeTryStatement _ = (Serialization.cst "STUB:TryStatement")

writeType :: (Syntax.Type -> Ast.Expr)
writeType t = ((\x -> case x of
  Syntax.TypePrimitive v1 -> (writePrimitiveTypeWithAnnotations v1)
  Syntax.TypeReference v1 -> (writeReferenceType v1)) t)

writeTypeArgument :: (Syntax.TypeArgument -> Ast.Expr)
writeTypeArgument a = ((\x -> case x of
  Syntax.TypeArgumentReference v1 -> (writeReferenceType v1)
  Syntax.TypeArgumentWildcard v1 -> (writeWildcard v1)) a)

writeTypeArgumentsOrDiamond :: (Syntax.TypeArgumentsOrDiamond -> Ast.Expr)
writeTypeArgumentsOrDiamond targs = ((\x -> case x of
  Syntax.TypeArgumentsOrDiamondArguments v1 -> (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument v1))
  Syntax.TypeArgumentsOrDiamondDiamond -> (Serialization.cst "<>")) targs)

writeTypeBound :: (Syntax.TypeBound -> Ast.Expr)
writeTypeBound b = ((\x -> case x of
  Syntax.TypeBoundVariable v1 -> (writeTypeVariable v1)
  Syntax.TypeBoundClassOrInterface v1 ->  
    let cit = (Syntax.typeBound_ClassOrInterfaceType v1) 
        additional = (Syntax.typeBound_ClassOrInterfaceAdditional v1)
    in (Logic.ifElse (Lists.null additional) (writeClassOrInterfaceType cit) (Serialization.spaceSep (Lists.cons (writeClassOrInterfaceType cit) (Lists.map writeAdditionalBound additional))))) b)

writeTypeDeclaration :: (Syntax.TypeDeclaration -> Ast.Expr)
writeTypeDeclaration d = ((\x -> case x of
  Syntax.TypeDeclarationClass v1 -> (writeClassDeclaration v1)
  Syntax.TypeDeclarationInterface v1 -> (writeInterfaceDeclaration v1)
  Syntax.TypeDeclarationNone -> (Serialization.cst ";")) d)

writeTypeDeclarationWithComments :: (Syntax.TypeDeclarationWithComments -> Ast.Expr)
writeTypeDeclarationWithComments tdwc =  
  let d = (Syntax.typeDeclarationWithCommentsValue tdwc) 
      mc = (Syntax.typeDeclarationWithCommentsComments tdwc)
  in (withComments mc (writeTypeDeclaration d))

writeTypeIdentifier :: (Syntax.TypeIdentifier -> Ast.Expr)
writeTypeIdentifier tid = (writeIdentifier (Syntax.unTypeIdentifier tid))

writeTypeName :: (Syntax.TypeName -> Ast.Expr)
writeTypeName tn =  
  let id = (Syntax.typeNameIdentifier tn) 
      mqual = (Syntax.typeNameQualifier tn)
  in (Serialization.dotSep (Maybes.cat [
    Maybes.map writePackageOrTypeName mqual,
    (Just (writeTypeIdentifier id))]))

writeTypeParameter :: (Syntax.TypeParameter -> Ast.Expr)
writeTypeParameter tp =  
  let mods = (Syntax.typeParameterModifiers tp) 
      id = (Syntax.typeParameterIdentifier tp)
      bound = (Syntax.typeParameterBound tp)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null mods) Nothing (Just (Serialization.spaceSep (Lists.map writeTypeParameterModifier mods))),
    (Just (writeTypeIdentifier id)),
    (Maybes.map (\b -> Serialization.spaceSep [
      Serialization.cst "extends",
      (writeTypeBound b)]) bound)]))

writeTypeParameterModifier :: (Syntax.TypeParameterModifier -> Ast.Expr)
writeTypeParameterModifier tpm = (writeAnnotation (Syntax.unTypeParameterModifier tpm))

writeTypeVariable :: (Syntax.TypeVariable -> Ast.Expr)
writeTypeVariable tv =  
  let anns = (Syntax.typeVariableAnnotations tv) 
      id = (Syntax.typeVariableIdentifier tv)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.spaceSep (Lists.map writeAnnotation anns))),
    (Just (writeTypeIdentifier id))]))

writeUnannType :: (Syntax.UnannType -> Ast.Expr)
writeUnannType ut = (writeType (Syntax.unUnannType ut))

writeUnaryExpression :: (Syntax.UnaryExpression -> Ast.Expr)
writeUnaryExpression e = ((\x -> case x of
  Syntax.UnaryExpressionPreIncrement v1 -> (writePreIncrementExpression v1)
  Syntax.UnaryExpressionPreDecrement v1 -> (writePreDecrementExpression v1)
  Syntax.UnaryExpressionPlus v1 -> (Serialization.spaceSep [
    Serialization.cst "+",
    (writeUnaryExpression v1)])
  Syntax.UnaryExpressionMinus v1 -> (Serialization.spaceSep [
    Serialization.cst "-",
    (writeUnaryExpression v1)])
  Syntax.UnaryExpressionOther v1 -> (writeUnaryExpressionNotPlusMinus v1)) e)

writeUnaryExpressionNotPlusMinus :: (Syntax.UnaryExpressionNotPlusMinus -> Ast.Expr)
writeUnaryExpressionNotPlusMinus e = ((\x -> case x of
  Syntax.UnaryExpressionNotPlusMinusPostfix v1 -> (writePostfixExpression v1)
  Syntax.UnaryExpressionNotPlusMinusTilde v1 -> (Serialization.spaceSep [
    Serialization.cst "~",
    (writeUnaryExpression v1)])
  Syntax.UnaryExpressionNotPlusMinusNot v1 -> (Serialization.noSep [
    Serialization.cst "!",
    (writeUnaryExpression v1)])
  Syntax.UnaryExpressionNotPlusMinusCast v1 -> (writeCastExpression v1)) e)

writeUnqualifiedClassInstanceCreationExpression :: (Syntax.UnqualifiedClassInstanceCreationExpression -> Ast.Expr)
writeUnqualifiedClassInstanceCreationExpression ucice =  
  let targs = (Syntax.unqualifiedClassInstanceCreationExpressionTypeArguments ucice) 
      cit = (Syntax.unqualifiedClassInstanceCreationExpressionClassOrInterface ucice)
      args = (Syntax.unqualifiedClassInstanceCreationExpressionArguments ucice)
      mbody = (Syntax.unqualifiedClassInstanceCreationExpressionBody ucice)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "new"),
    (Logic.ifElse (Lists.null targs) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map writeTypeArgument targs)))),
    (Just (Serialization.noSep [
      writeClassOrInterfaceTypeToInstantiate cit,
      (Serialization.parenList False (Lists.map writeExpression args))])),
    (Maybes.map writeClassBody mbody)]))

writeVariableArityParameter :: (t0 -> Ast.Expr)
writeVariableArityParameter _ = (Serialization.cst "STUB:VariableArityParameter")

writeVariableDeclarator :: (Syntax.VariableDeclarator -> Ast.Expr)
writeVariableDeclarator vd =  
  let id = (Syntax.variableDeclaratorId vd) 
      minit = (Syntax.variableDeclaratorInitializer vd)
      idSec = (writeVariableDeclaratorId id)
  in (Maybes.maybe idSec (\init -> Serialization.infixWs "=" idSec (writeVariableInitializer init)) minit)

writeVariableDeclaratorId :: (Syntax.VariableDeclaratorId -> Ast.Expr)
writeVariableDeclaratorId vdi =  
  let id = (Syntax.variableDeclaratorIdIdentifier vdi) 
      mdims = (Syntax.variableDeclaratorIdDims vdi)
  in (Serialization.noSep (Maybes.cat [
    Just (writeIdentifier id),
    (Maybes.map writeDims mdims)]))

writeVariableInitializer :: (Syntax.VariableInitializer -> Ast.Expr)
writeVariableInitializer i = ((\x -> case x of
  Syntax.VariableInitializerExpression v1 -> (writeExpression v1)
  Syntax.VariableInitializerArrayInitializer v1 -> (writeArrayInitializer v1)) i)

writeVariableModifier :: (Syntax.VariableModifier -> Ast.Expr)
writeVariableModifier m = ((\x -> case x of
  Syntax.VariableModifierAnnotation v1 -> (writeAnnotation v1)
  Syntax.VariableModifierFinal -> (Serialization.cst "final")) m)

writeWhileStatement :: (t0 -> Ast.Expr)
writeWhileStatement _ = (Serialization.cst "STUB:WhileStatement")

writeWildcard :: (Syntax.Wildcard -> Ast.Expr)
writeWildcard w =  
  let anns = (Syntax.wildcardAnnotations w) 
      mbounds = (Syntax.wildcardWildcard w)
  in (Serialization.spaceSep (Maybes.cat [
    Logic.ifElse (Lists.null anns) Nothing (Just (Serialization.commaSep Serialization.inlineStyle (Lists.map writeAnnotation anns))),
    (Just (Serialization.cst "*")),
    (Maybes.map writeWildcardBounds mbounds)]))

writeWildcardBounds :: (Syntax.WildcardBounds -> Ast.Expr)
writeWildcardBounds b = ((\x -> case x of
  Syntax.WildcardBoundsExtends v1 -> (Serialization.spaceSep [
    Serialization.cst "extends",
    (writeReferenceType v1)])
  Syntax.WildcardBoundsSuper v1 -> (Serialization.spaceSep [
    Serialization.cst "super",
    (writeReferenceType v1)])) b)

-- | Sanitize a string for use in a Java comment
sanitizeJavaComment :: (String -> String)
sanitizeJavaComment s = (Strings.intercalate "&gt;" (Strings.splitOn ">" (Strings.intercalate "&lt;" (Strings.splitOn "<" s))))

-- | Create a single-line Java comment
singleLineComment :: (String -> Ast.Expr)
singleLineComment c = (Serialization.cst (Strings.cat2 "// " (sanitizeJavaComment c)))

-- | Wrap an expression with optional Javadoc comments
withComments :: (Maybe String -> Ast.Expr -> Ast.Expr)
withComments mc expr = (Maybes.maybe expr (\c -> Serialization.newlineSep [
  Serialization.cst (Strings.cat2 "/**\n" (Strings.cat2 (Strings.intercalate "\n" (Lists.map (\l -> Strings.cat2 " * " l) (Strings.lines (sanitizeJavaComment c)))) "\n */")),
  expr]) mc)
