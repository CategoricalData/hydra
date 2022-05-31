module Hydra.Ext.Java.Serde (
  moduleToJavaString,
) where

import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Ext.Java.Coder
import Hydra.Ext.Java.Operators
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Codetree.Ast
import Hydra.Util.Codetree.Print
import Hydra.Util.Codetree.Script
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Java.Syntax as Java

import qualified Data.List as L
import qualified Data.Maybe as Y


moduleToJavaString :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified String
moduleToJavaString cx g = do
  unit <- moduleToJavaCompilationUnit cx g
  return $ printExpr $ parenthesize $ writeCompilationUnit unit

----------------------------------------

-- Note: assumes a non-empty list of operands
associateList :: Op -> [CT.Expr] -> CT.Expr
associateList op opers = case opAssociativity op of
    AssociativityRight -> assocRight opers
    _ -> assocLeft $ L.reverse opers
  where
    assocLeft l = case l of
      [x] -> x
      (h:r) -> ifx op (assocLeft r) h
    assocRight l = case l of
      [x] -> x
      (h:r) -> ifx op h $ assocRight r

writeAdditiveExpression :: Java.AdditiveExpression -> CT.Expr
writeAdditiveExpression e = case e of
  Java.AdditiveExpressionUnary m -> writeMultiplicativeExpression m
  Java.AdditiveExpressionPlus (Java.AdditiveExpression_Binary lhs rhs) ->
    ifx plusOp (writeAdditiveExpression lhs) (writeMultiplicativeExpression rhs)
  Java.AdditiveExpressionMinus (Java.AdditiveExpression_Binary lhs rhs) ->
    ifx minusOp (writeAdditiveExpression lhs) (writeMultiplicativeExpression rhs)

writeAndExpression :: Java.AndExpression -> CT.Expr
writeAndExpression (Java.AndExpression eqs) = associateList bitAndOp (writeEqualityExpression <$> eqs)

writeAnnotatedIdentifier :: Java.AnnotatedIdentifier -> CT.Expr
writeAnnotatedIdentifier (Java.AnnotatedIdentifier anns id) = writeIdentifier id -- Note: ignoring annotations for now
      
writeAnnotation :: Java.Annotation -> CT.Expr
writeAnnotation ann = case ann of
  Java.AnnotationNormal n -> writeNormalAnnotation n
  Java.AnnotationMarker m -> writeMarkerAnnotation m
  Java.AnnotationSingleElement s -> writeSingleElementAnnotation s

writeArrayAccess :: Java.ArrayAccess -> CT.Expr
writeArrayAccess _ = cst "TODO:ArrayAccess"

writeArrayCreationExpression :: Java.ArrayCreationExpression -> CT.Expr
writeArrayCreationExpression _ = cst "TODO:ArrayCreationExpression"

writeArrayType :: Java.ArrayType -> CT.Expr
writeArrayType _ = cst "TODO:ArrayType"

writeAssertStatement :: Java.AssertStatement -> CT.Expr
writeAssertStatement _ = cst "TODO:AssertStatement"

writeAssignment :: Java.Assignment -> CT.Expr
writeAssignment _ = cst "TODO:Assignment"

writeAssignmentExpression :: Java.AssignmentExpression -> CT.Expr
writeAssignmentExpression e = case e of
  Java.AssignmentExpressionConditional c -> writeConditionalExpression c
  Java.AssignmentExpressionAssignment a -> writeAssignment a

writeBlock :: Java.Block -> CT.Expr
writeBlock (Java.Block stmts) = curlyBlock $ newlineSep (writeBlockStatement <$> stmts)

writeBlockStatement :: Java.BlockStatement -> CT.Expr
writeBlockStatement s = case s of
  Java.BlockStatementLocalVariableDeclaration d -> writeLocalVariableDeclarationStatement d
  Java.BlockStatementClass cd -> writeClassDeclaration cd
  Java.BlockStatementStatement s -> writeStatement s

writeBreakStatement :: Java.BreakStatement -> CT.Expr
writeBreakStatement _ = cst "TODO:BreakStatement"

writeCastExpression :: Java.CastExpression -> CT.Expr
writeCastExpression _ = cst "TODO:CastExpression"

writeClassBody :: Java.ClassBody -> CT.Expr
writeClassBody (Java.ClassBody decls) = curlyBlock $ doubleNewlineSep (writeClassBodyDeclaration <$> decls)

writeClassBodyDeclaration :: Java.ClassBodyDeclaration -> CT.Expr
writeClassBodyDeclaration d = case d of
  Java.ClassBodyDeclarationClassMember d -> writeClassMemberDeclaration d
  Java.ClassBodyDeclarationInstanceInitializer i -> writeInstanceInitializer i
  Java.ClassBodyDeclarationStaticInitializer i -> writeStaticInitializer i
  Java.ClassBodyDeclarationConstructorDeclaration d -> writeConstructorDeclaration d

writeClassDeclaration :: Java.ClassDeclaration -> CT.Expr
writeClassDeclaration d = case d of
  Java.ClassDeclarationNormal nd -> writeNormalClassDeclaration nd
  Java.ClassDeclarationEnum ed -> writeEnumDeclaration ed

writeClassInstanceCreationExpression :: Java.ClassInstanceCreationExpression -> CT.Expr
writeClassInstanceCreationExpression (Java.ClassInstanceCreationExpression mqual e) = case mqual of
  Nothing -> writeUnqualifiedClassInstanceCreationExpression e
  Just q -> dotSep [writeClassInstanceCreationExpression_Qualifier q, writeUnqualifiedClassInstanceCreationExpression e]

writeClassInstanceCreationExpression_Qualifier :: Java.ClassInstanceCreationExpression_Qualifier -> CT.Expr
writeClassInstanceCreationExpression_Qualifier q = case q of
  Java.ClassInstanceCreationExpression_QualifierExpression en -> writeExpressionName en
  Java.ClassInstanceCreationExpression_QualifierPrimary p -> writePrimary p

writeClassLiteral :: Java.ClassLiteral -> CT.Expr
writeClassLiteral _ = cst "TODO:ClassLiteral"

writeClassMemberDeclaration :: Java.ClassMemberDeclaration -> CT.Expr
writeClassMemberDeclaration d = case d of
  Java.ClassMemberDeclarationField fd -> writeFieldDeclaration fd
  Java.ClassMemberDeclarationMethod md -> writeMethodDeclaration md
  Java.ClassMemberDeclarationClass cd -> writeClassDeclaration cd
  Java.ClassMemberDeclarationInterface id -> writeInterfaceDeclaration id

writeClassModifier :: Java.ClassModifier -> CT.Expr
writeClassModifier m = case m of
  Java.ClassModifierAnnotation ann -> writeAnnotation ann
  Java.ClassModifierPublic -> cst "public"
  Java.ClassModifierProtected -> cst "protected"
  Java.ClassModifierPrivate -> cst "private"
  Java.ClassModifierAbstract -> cst "abstract"
  Java.ClassModifierStatic -> cst "static"
  Java.ClassModifierFinal -> cst "final"
  Java.ClassModifierStrictfp -> cst "strictfp"

writeClassOrInterfaceType :: Java.ClassOrInterfaceType -> CT.Expr
writeClassOrInterfaceType cit = case cit of
  Java.ClassOrInterfaceTypeClass ct -> writeClassType ct
  Java.ClassOrInterfaceTypeInterface it -> writeInterfaceType it

writeClassOrInterfaceTypeToInstantiate :: Java.ClassOrInterfaceTypeToInstantiate -> CT.Expr
writeClassOrInterfaceTypeToInstantiate (Java.ClassOrInterfaceTypeToInstantiate ids margs) =
  noSep $ Y.catMaybes [
    Just $ dotSep (writeAnnotatedIdentifier <$> ids),
    writeTypeArgumentsOrDiamond <$> margs]
     
writeClassType :: Java.ClassType -> CT.Expr
writeClassType (Java.ClassType anns qual id args) = spaceSep $ Y.catMaybes [
  if L.null anns then Nothing else Just $ commaSep False (writeAnnotation <$> anns),
  Just qualifiedId,
  if L.null args then Nothing else Just $ angleBracesList False (writeTypeArgument <$> args)]
  where
    qualifiedId = case qual of
      Java.ClassTypeQualifierNone -> writeTypeIdentifier id
      Java.ClassTypeQualifierPackage pkg -> dotSep [writePackageName pkg, writeTypeIdentifier id]
      Java.ClassTypeQualifierParent cit -> dotSep [writeClassOrInterfaceType cit, writeTypeIdentifier id]

writeCompilationUnit :: Java.CompilationUnit -> CT.Expr
writeCompilationUnit u = case u of
  Java.CompilationUnitOrdinary (Java.OrdinaryCompilationUnit mpkg imports types) -> doubleNewlineSep $ Y.catMaybes
      [pkgSec, importsSec, typesSec]
    where
      pkgSec = fmap writePackageDeclaration mpkg
      importsSec = if L.null imports
        then Nothing
        else Just $ newlineSep (writeImportDeclaration <$> imports)
      typesSec = if L.null types
        then Nothing
        else Just $ doubleNewlineSep (writeTypeDeclaration <$> types)

writeConditionalAndExpression :: Java.ConditionalAndExpression -> CT.Expr
writeConditionalAndExpression (Java.ConditionalAndExpression ors)
  = associateList andOp (writeInclusiveOrExpression <$> ors)

writeConditionalExpression :: Java.ConditionalExpression -> CT.Expr
writeConditionalExpression c = case c of
  Java.ConditionalExpressionSimple co -> writeConditionalOrExpression co
  Java.ConditionalExpressionTernaryCond tc -> writeConditionalExpression_TernaryCond tc
  Java.ConditionalExpressionTernaryLambda tl -> writeConditionalExpression_TernaryLambda tl

writeConditionalExpression_TernaryCond :: Java.ConditionalExpression_TernaryCond -> CT.Expr
writeConditionalExpression_TernaryCond _ = cst "TODO:ConditionalExpression_TernaryCond"

writeConditionalExpression_TernaryLambda :: Java.ConditionalExpression_TernaryLambda -> CT.Expr
writeConditionalExpression_TernaryLambda _ = cst "TODO:ConditionalExpression_TernaryLambda"

writeConditionalOrExpression :: Java.ConditionalOrExpression -> CT.Expr
writeConditionalOrExpression (Java.ConditionalOrExpression ands)
  = associateList orOp (writeConditionalAndExpression <$> ands)

writeConstructorDeclaration :: Java.ConstructorDeclaration -> CT.Expr
writeConstructorDeclaration _ = cst "TODO:ConstructorDeclaration"

writeContinueStatement :: Java.ContinueStatement -> CT.Expr
writeContinueStatement _ = cst "TODO:ContinueStatement"

writeDims :: Java.Dims -> CT.Expr
writeDims (Java.Dims anns) = noSep (write <$> anns)
  where
    write _ = cst "[]" -- Note: ignoring annotations on dimensions for now

writeDoStatement :: Java.DoStatement -> CT.Expr
writeDoStatement _ = cst "TODO:DoStatement"

writeElementValue :: Java.ElementValue -> CT.Expr
writeElementValue ev = case ev of
  Java.ElementValueConditionalExpression c -> writeConditionalExpression c
  Java.ElementValueElementValueArrayInitializer (Java.ElementValueArrayInitializer values) ->
    commaSep False (writeElementValue <$> values)
  Java.ElementValueAnnotation ann -> writeAnnotation ann

writeElementValuePair :: Java.ElementValuePair -> CT.Expr
writeElementValuePair (Java.ElementValuePair k v) = ifx defineOp (writeIdentifier k) (writeElementValue v)

writeEmptyStatement :: Java.EmptyStatement -> CT.Expr
writeEmptyStatement _ = cst ";"

writeEnumDeclaration :: Java.EnumDeclaration -> CT.Expr
writeEnumDeclaration _ = cst "TODO:EnumDeclaration"

writeEqualityExpression :: Java.EqualityExpression -> CT.Expr
writeEqualityExpression e = case e of
  Java.EqualityExpressionUnary r -> writeRelationalExpression r
  Java.EqualityExpressionEqual (Java.EqualityExpression_Binary lhs rhs) ->
    ifx equalOp (writeEqualityExpression lhs) (writeRelationalExpression rhs)
  Java.EqualityExpressionNotEqual (Java.EqualityExpression_Binary lhs rhs) ->
    ifx notEqualOp (writeEqualityExpression lhs) (writeRelationalExpression rhs)

writeExclusiveOrExpression :: Java.ExclusiveOrExpression -> CT.Expr
writeExclusiveOrExpression (Java.ExclusiveOrExpression ands) = associateList bitXorOp (writeAndExpression <$> ands)

writeExpression :: Java.Expression -> CT.Expr
writeExpression e = case e of
  Java.ExpressionLambda l -> writeLambdaExpression l
  Java.ExpressionAssignment a -> writeAssignmentExpression a

writeExpressionName :: Java.ExpressionName -> CT.Expr
writeExpressionName _ = cst "TODO:ExpressionName"

writeExpressionStatement :: Java.ExpressionStatement -> CT.Expr
writeExpressionStatement _ = cst "TODO:ExpressionStatement"

writeFieldAccess :: Java.FieldAccess -> CT.Expr
writeFieldAccess _ = cst "TODO:FieldAccess"

writeFieldDeclaration :: Java.FieldDeclaration -> CT.Expr
writeFieldDeclaration (Java.FieldDeclaration mods typ vars) = suffixSemi $ spaceSep $ Y.catMaybes [
    if L.null mods then Nothing else Just $ spaceSep (writeFieldModifier <$> mods),
    Just $ writeUnannType typ,
    Just $ commaSep False (writeVariableDeclarator <$> vars)]

writeFieldModifier :: Java.FieldModifier -> CT.Expr
writeFieldModifier m = case m of
  Java.FieldModifierAnnotation ann -> writeAnnotation ann
  Java.FieldModifierPublic -> cst "public"
  Java.FieldModifierProtected -> cst "protected"
  Java.FieldModifierPrivate -> cst "private"
  Java.FieldModifierStatic -> cst "static"
  Java.FieldModifierFinal -> cst "final"
  Java.FieldModifierTransient -> cst "transient"
  Java.FieldModifierVolatile -> cst "volatile"

writeForStatement :: Java.ForStatement -> CT.Expr
writeForStatement _ = cst "TODO:ForStatement"

writeFormalParameter :: Java.FormalParameter -> CT.Expr
writeFormalParameter p = case p of
  Java.FormalParameterSimple s -> writeFormalParameter_Simple s
  Java.FormalParameterVariableArity v -> writeVariableArityParameter v

writeFormalParameter_Simple :: Java.FormalParameter_Simple -> CT.Expr
writeFormalParameter_Simple (Java.FormalParameter_Simple mods typ id) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeVariableModifier <$> mods),
  Just $ writeUnannType typ,
  Just $ writeVariableDeclaratorId id]

writeIdentifier :: Java.Identifier -> CT.Expr
writeIdentifier (Java.Identifier s) = cst s

writeIfThenStatement :: Java.IfThenStatement -> CT.Expr
writeIfThenStatement _ = cst "TODO:IfThenStatement"

writeIfThenElseStatement :: Java.IfThenElseStatement -> CT.Expr
writeIfThenElseStatement _ = cst "TODO:IfThenElseStatement"

writeImportDeclaration :: Java.ImportDeclaration -> CT.Expr
writeImportDeclaration imp = case imp of
  Java.ImportDeclarationSingleType d -> cst "TODO:ImportDeclarationSingleType"
  Java.ImportDeclarationTypeImportOnDemand d -> cst "TODO:ImportDeclarationTypeImportOnDemand"
  Java.ImportDeclarationSingleStaticImport d -> cst "TODO:ImportDeclarationSingleStaticImport"
  Java.ImportDeclarationStaticImportOnDemand d -> cst "TODO:ImportDeclarationStaticImportOnDemand"

writeInclusiveOrExpression :: Java.InclusiveOrExpression -> CT.Expr
writeInclusiveOrExpression (Java.InclusiveOrExpression ors)
  = associateList bitOrOp (writeExclusiveOrExpression <$> ors)

writeInstanceInitializer :: Java.InstanceInitializer -> CT.Expr
writeInstanceInitializer _ = cst "TODO:InstanceInitializer"

writeInterfaceDeclaration :: Java.InterfaceDeclaration -> CT.Expr
writeInterfaceDeclaration d = cst "TODO:InterfaceDeclaration"

writeInterfaceType :: Java.InterfaceType -> CT.Expr
writeInterfaceType (Java.InterfaceType ct) = writeClassType ct

writeLabeledStatement :: Java.LabeledStatement -> CT.Expr
writeLabeledStatement _ = cst "TODO:LabeledStatement"

writeLambdaBody :: Java.LambdaBody -> CT.Expr
writeLambdaBody b = case b of
  Java.LambdaBodyExpression e -> writeExpression e
  Java.LambdaBodyBlock b -> writeBlock b

writeLambdaExpression :: Java.LambdaExpression -> CT.Expr
writeLambdaExpression (Java.LambdaExpression params body) =
  ifx arrowOp (writeLambdaParameters params) (writeLambdaBody body)

writeLambdaParameters :: Java.LambdaParameters -> CT.Expr
writeLambdaParameters p = case p of
  Java.LambdaParametersTuple l -> parenList (writeLambdaParameters <$> l)
  Java.LambdaParametersSingle id -> writeIdentifier id

writeLiteral :: Java.Literal -> CT.Expr
writeLiteral _ = cst "TODO:Literal"

writeLocalVariableDeclarationStatement :: Java.LocalVariableDeclarationStatement -> CT.Expr
writeLocalVariableDeclarationStatement _ = cst "TODO:LocalVariableDeclarationStatement"

writeMarkerAnnotation :: Java.MarkerAnnotation -> CT.Expr
writeMarkerAnnotation (Java.MarkerAnnotation tname) = prefixAt $ writeTypeName tname

writeMethodBody :: Java.MethodBody -> CT.Expr
writeMethodBody (Java.MethodBody block) = writeBlock block

writeMethodHeader :: Java.MethodHeader -> CT.Expr
writeMethodHeader (Java.MethodHeader params anns result decl mthrows) = spaceSep $ Y.catMaybes [
  if L.null params then Nothing else Just $ angleBracesList False (writeTypeParameter <$> params),
  if L.null anns then Nothing else Just $ commaSep False (writeAnnotation <$> anns),
  Just $ writeResult result,
  Just $ writeMethodDeclarator decl,
  writeThrows <$> mthrows]

writeMethodDeclaration :: Java.MethodDeclaration -> CT.Expr
writeMethodDeclaration (Java.MethodDeclaration mods header body) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeMethodModifier <$> mods),
  Just $ writeMethodHeader header,
  Just $ writeMethodBody body]

writeMethodDeclarator :: Java.MethodDeclarator -> CT.Expr
writeMethodDeclarator (Java.MethodDeclarator id rparam params) = noSep [
  writeIdentifier id,
  -- Note: ignoring receiver param for now
  parenList (writeFormalParameter <$> params)]

writeMethodInvocation :: Java.MethodInvocation -> CT.Expr
writeMethodInvocation _ = cst "TODO:MethodInvocation"

writeMethodModifier :: Java.MethodModifier -> CT.Expr
writeMethodModifier m = case m of
  Java.MethodModifierAnnotation ann -> writeAnnotation ann
  Java.MethodModifierPublic -> cst "public"
  Java.MethodModifierProtected -> cst "protected"
  Java.MethodModifierPrivate -> cst "private"
  Java.MethodModifierAbstract -> cst "abstract"
  Java.MethodModifierFinal -> cst "final"
  Java.MethodModifierSynchronized -> cst "synchronized"
  Java.MethodModifierNative -> cst "native"
  Java.MethodModifierStrictfb -> cst "strictfb"

writeMethodReference :: Java.MethodReference -> CT.Expr
writeMethodReference _ = cst "TODO:MethodReference"

writeMultiplicativeExpression :: Java.MultiplicativeExpression -> CT.Expr
writeMultiplicativeExpression e = case e of
  Java.MultiplicativeExpressionUnary u -> writeUnaryExpression u
  Java.MultiplicativeExpressionTimes (Java.MultiplicativeExpression_Binary lhs rhs) ->
    ifx timesOp (writeMultiplicativeExpression lhs) (writeUnaryExpression rhs)
  Java.MultiplicativeExpressionDivide (Java.MultiplicativeExpression_Binary lhs rhs) ->
    ifx divideOp (writeMultiplicativeExpression lhs) (writeUnaryExpression rhs)
  Java.MultiplicativeExpressionMod (Java.MultiplicativeExpression_Binary lhs rhs) ->
    ifx modOp (writeMultiplicativeExpression lhs) (writeUnaryExpression rhs)

writeNormalAnnotation :: Java.NormalAnnotation -> CT.Expr
writeNormalAnnotation (Java.NormalAnnotation tname pairs) = prefixAt $ noSep [
  writeTypeName tname,
  commaSep False (writeElementValuePair <$> pairs)]

writeNormalClassDeclaration :: Java.NormalClassDeclaration -> CT.Expr
writeNormalClassDeclaration (Java.NormalClassDeclaration mods id tparams msuperc superi body) =
    spaceSep $ Y.catMaybes [modSec, classSec, idSec, extendsSec, implementsSec, bodySec]
  where
    modSec = if L.null mods
      then Nothing
      else Just $ spaceSep (writeClassModifier <$> mods)
    classSec = Just $ cst "class"
    idSec = Just $ noSep $ Y.catMaybes [Just $ writeTypeIdentifier id, params]
      where
        params = if L.null tparams
          then Nothing
          else Just $ angleBracesList False (writeTypeParameter <$> tparams)
    extendsSec = fmap (\c -> spaceSep [cst "extends", writeClassType c]) msuperc
    implementsSec = if L.null superi
      then Nothing
      else Just $ spaceSep [cst "implements", commaSep False (writeInterfaceType <$> superi)]
    bodySec = Just $ writeClassBody body

writePackageDeclaration :: Java.PackageDeclaration -> CT.Expr
writePackageDeclaration (Java.PackageDeclaration mods ids) = newlineSep $ modifierLines ++ [packageLine]
  where
    modifierLines = writePackageModifier <$> mods
    packageLine = suffixSemi $ spaceSep [cst "package", cst $ L.intercalate "." (Java.unIdentifier <$> ids)]

writePackageName :: Java.PackageName -> CT.Expr
writePackageName (Java.PackageName ids) = dotSep (writeIdentifier <$> ids)

writePackageOrTypeName :: Java.PackageOrTypeName -> CT.Expr
writePackageOrTypeName (Java.PackageOrTypeName ids) = dotSep (writeIdentifier <$> ids)

writePackageModifier :: Java.PackageModifier -> CT.Expr
writePackageModifier (Java.PackageModifier ann) = writeAnnotation ann

writePostDecrementExpression :: Java.PostDecrementExpression -> CT.Expr
writePostDecrementExpression _ = cst "TODO:PostDecrementExpression"

writePostIncrementExpression :: Java.PostIncrementExpression -> CT.Expr
writePostIncrementExpression _ = cst "TODO:PostIncrementExpression"

writePostfixExpression :: Java.PostfixExpression -> CT.Expr
writePostfixExpression e = case e of
  Java.PostfixExpressionPrimary p -> writePrimary p
  Java.PostfixExpressionName en -> writeExpressionName en
  Java.PostfixExpressionPostIncrement pi -> writePostIncrementExpression pi
  Java.PostfixExpressionPostDecrement pd -> writePostDecrementExpression pd

writePreDecrementExpression:: Java.PreDecrementExpression -> CT.Expr
writePreDecrementExpression _ = cst "TODO:PreDecrementExpression"

writePreIncrementExpression :: Java.PreIncrementExpression -> CT.Expr
writePreIncrementExpression _ = cst "TODO:PreIncrementExpression"

writePrimary :: Java.Primary -> CT.Expr
writePrimary p = case p of
  Java.PrimaryNoNewArray n -> writePrimaryNoNewArray n
  Java.PrimaryArrayCreation a -> writeArrayCreationExpression a

writePrimaryNoNewArray :: Java.PrimaryNoNewArray -> CT.Expr
writePrimaryNoNewArray p = case p of
  Java.PrimaryNoNewArrayLiteral l -> writeLiteral l
  Java.PrimaryNoNewArrayClassLiteral cl -> writeClassLiteral cl
  Java.PrimaryNoNewArrayThis -> cst "this"
  Java.PrimaryNoNewArrayDotThis n -> dotSep [writeTypeName n, cst "this"]
  Java.PrimaryNoNewArrayParens e -> parenList [writeExpression e]
  Java.PrimaryNoNewArrayClassInstance ci -> writeClassInstanceCreationExpression ci
  Java.PrimaryNoNewArrayFieldAccess fa -> writeFieldAccess fa
  Java.PrimaryNoNewArrayArrayAccess aa -> writeArrayAccess aa
  Java.PrimaryNoNewArrayMethodInvocation mi -> writeMethodInvocation mi
  Java.PrimaryNoNewArrayMethodReference mr -> writeMethodReference mr

writePrimitiveTypeWithAnnotations :: Java.PrimitiveTypeWithAnnotations -> CT.Expr
writePrimitiveTypeWithAnnotations _ = cst "TODO:PrimitiveTypeWithAnnotations" -- (Java.PrimitiveTypeWithAnnotations )

writeReferenceType :: Java.ReferenceType -> CT.Expr
writeReferenceType rt = case rt of
  Java.ReferenceTypeClassOrInterface cit -> writeClassOrInterfaceType cit
  Java.ReferenceTypeVariable v -> writeTypeVariable v
  Java.ReferenceTypeArray at -> writeArrayType at

writeRelationalExpression :: Java.RelationalExpression -> CT.Expr
writeRelationalExpression e = case e of
  Java.RelationalExpressionSimple s -> writeShiftExpression s
  Java.RelationalExpressionLessThan lt -> writeRelationalExpression_LessThan lt
  Java.RelationalExpressionGreaterThan gt -> writeRelationalExpression_GreaterThan gt
  Java.RelationalExpressionLessThanEqual lte -> writeRelationalExpression_LessThanEqual lte
  Java.RelationalExpressionGreaterThanEqual gte -> writeRelationalExpression_GreaterThanEqual gte
  Java.RelationalExpressionInstanceof i -> writeRelationalExpression_InstanceOf i

writeRelationalExpression_GreaterThan :: Java.RelationalExpression_GreaterThan -> CT.Expr
writeRelationalExpression_GreaterThan _ = cst "TODO:RelationalExpression_GreaterThan"

writeRelationalExpression_GreaterThanEqual :: Java.RelationalExpression_GreaterThanEqual -> CT.Expr
writeRelationalExpression_GreaterThanEqual _ = cst "TODO:RelationalExpression_GreaterThanEqual"

writeRelationalExpression_InstanceOf :: Java.RelationalExpression_InstanceOf -> CT.Expr
writeRelationalExpression_InstanceOf _ = cst "TODO:RelationalExpression_InstanceOf"

writeRelationalExpression_LessThan :: Java.RelationalExpression_LessThan -> CT.Expr
writeRelationalExpression_LessThan _ = cst "TODO:RelationalExpression_LessThan"

writeRelationalExpression_LessThanEqual :: Java.RelationalExpression_LessThanEqual -> CT.Expr
writeRelationalExpression_LessThanEqual _ = cst "TODO:RelationalExpression_LessThanEqual"

writeResult :: Java.Result -> CT.Expr
writeResult r = case r of
  Java.ResultType t -> writeUnannType t
  Java.ResultVoid -> cst "void"

writeReturnStatement :: Java.ReturnStatement -> CT.Expr
writeReturnStatement (Java.ReturnStatement mex) = suffixSemi $ spaceSep $ Y.catMaybes [
  Just $ cst "return",
  writeExpression <$> mex]

writeShiftExpression :: Java.ShiftExpression -> CT.Expr
writeShiftExpression e = case e of
  Java.ShiftExpressionUnary a -> writeAdditiveExpression a
  Java.ShiftExpressionShiftLeft (Java.ShiftExpression_Binary lhs rhs) ->
    ifx shiftLeftOp (writeShiftExpression lhs) (writeAdditiveExpression rhs)
  Java.ShiftExpressionShiftRight (Java.ShiftExpression_Binary lhs rhs) ->
    ifx shiftRightOp (writeShiftExpression lhs) (writeAdditiveExpression rhs)
  Java.ShiftExpressionShiftRightZeroFill (Java.ShiftExpression_Binary lhs rhs) ->
    ifx shiftRightZeroFillOp (writeShiftExpression lhs) (writeAdditiveExpression rhs)

writeSingleElementAnnotation :: Java.SingleElementAnnotation -> CT.Expr
writeSingleElementAnnotation (Java.SingleElementAnnotation tname mv) = case mv of
  Nothing -> writeMarkerAnnotation (Java.MarkerAnnotation tname)
  Just v -> prefixAt $ noSep [writeTypeName tname, parenList [writeElementValue v]]

writeStatement :: Java.Statement -> CT.Expr
writeStatement s = case s of
  Java.StatementWithoutTrailing s -> writeStatementWithoutTrailingSubstatement s
  Java.StatementLabeled l -> writeLabeledStatement l
  Java.StatementIfThen it -> writeIfThenStatement it
  Java.StatementIfThenElse ite -> writeIfThenElseStatement ite
  Java.StatementWhile w -> writeWhileStatement w
  Java.StatementFor f -> writeForStatement f

writeStatementWithoutTrailingSubstatement :: Java.StatementWithoutTrailingSubstatement -> CT.Expr
writeStatementWithoutTrailingSubstatement s = case s of
  Java.StatementWithoutTrailingSubstatementBlock b -> writeBlock b
  Java.StatementWithoutTrailingSubstatementEmpty e -> writeEmptyStatement e
  Java.StatementWithoutTrailingSubstatementExpression e -> writeExpressionStatement e
  Java.StatementWithoutTrailingSubstatementAssert a -> writeAssertStatement a
  Java.StatementWithoutTrailingSubstatementSwitch s -> writeSwitchStatement s
  Java.StatementWithoutTrailingSubstatementDo d -> writeDoStatement d
  Java.StatementWithoutTrailingSubstatementBreak b -> writeBreakStatement b
  Java.StatementWithoutTrailingSubstatementContinue c -> writeContinueStatement c
  Java.StatementWithoutTrailingSubstatementReturn r -> writeReturnStatement r
  Java.StatementWithoutTrailingSubstatementSynchronized s -> writeSynchronizedStatement s
  Java.StatementWithoutTrailingSubstatementThrow t -> writeThrowStatement t
  Java.StatementWithoutTrailingSubstatementTry t -> writeTryStatement t

writeStaticInitializer :: Java.StaticInitializer -> CT.Expr
writeStaticInitializer _ = cst "TODO:StaticInitializer"

writeSwitchStatement :: Java.SwitchStatement -> CT.Expr
writeSwitchStatement _ = cst "TODO:SwitchStatement"

writeSynchronizedStatement :: Java.SynchronizedStatement -> CT.Expr
writeSynchronizedStatement _ = cst "TODO:SynchronizedStatement"

writeThrowStatement :: Java.ThrowStatement -> CT.Expr
writeThrowStatement _ = cst "TODO:ThrowStatement"

writeThrows :: Java.Throws -> CT.Expr
writeThrows _ = cst "TODO:Throws"

writeTryStatement :: Java.TryStatement -> CT.Expr
writeTryStatement _ = cst "TODO:TryStatement"

writeType :: Java.Type -> CT.Expr
writeType t = case t of
  Java.TypePrimitive pt -> writePrimitiveTypeWithAnnotations pt
  Java.TypeReference rt -> writeReferenceType rt

writeTypeArgument :: Java.TypeArgument -> CT.Expr
writeTypeArgument a = case a of
  Java.TypeArgumentReference rt -> writeReferenceType rt
  Java.TypeArgumentWildcard w -> writeWildcard w

writeTypeArgumentsOrDiamond :: Java.TypeArgumentsOrDiamond -> CT.Expr
writeTypeArgumentsOrDiamond _ = cst "TODO:TypeArgumentsOrDiamond"
      
writeTypeBound :: Java.TypeBound -> CT.Expr
writeTypeBound _ = cst "TODO:TypeBound"

writeTypeDeclaration :: Java.TypeDeclaration -> CT.Expr
writeTypeDeclaration d = case d of
  Java.TypeDeclarationClass d -> writeClassDeclaration d
  Java.TypeDeclarationInterface d -> writeInterfaceDeclaration d

writeTypeIdentifier :: Java.TypeIdentifier -> CT.Expr
writeTypeIdentifier (Java.TypeIdentifier id) = writeIdentifier id

writeTypeName :: Java.TypeName -> CT.Expr
writeTypeName (Java.TypeName id mqual) = dotSep $ Y.catMaybes [
  writePackageOrTypeName <$> mqual,
  Just $ writeTypeIdentifier id]

writeTypeParameter :: Java.TypeParameter -> CT.Expr
writeTypeParameter (Java.TypeParameter mods id bound) = spaceSep $ Y.catMaybes [
  if L.null mods then Nothing else Just $ spaceSep (writeTypeParameterModifier <$> mods),
  Just $ writeTypeIdentifier id,
  fmap (\b -> spaceSep [cst "extends", writeTypeBound b]) bound]

writeTypeParameterModifier :: Java.TypeParameterModifier -> CT.Expr
writeTypeParameterModifier (Java.TypeParameterModifier ann) = writeAnnotation ann

writeTypeVariable :: Java.TypeVariable -> CT.Expr
writeTypeVariable _ = cst "TODO:TypeVariable"

writeUnannType :: Java.UnannType -> CT.Expr
writeUnannType (Java.UnannType t) = writeType t

writeUnaryExpression :: Java.UnaryExpression -> CT.Expr
writeUnaryExpression e = case e of
  Java.UnaryExpressionPreIncrement pi -> writePreIncrementExpression pi
  Java.UnaryExpressionPreDecrement pd -> writePreDecrementExpression pd
  Java.UnaryExpressionPlus p -> spaceSep [cst "+", writeUnaryExpression p]
  Java.UnaryExpressionMinus m -> spaceSep [cst "-", writeUnaryExpression m]
  Java.UnaryExpressionOther o -> writeUnaryExpressionNotPlusMinus o

writeUnaryExpressionNotPlusMinus :: Java.UnaryExpressionNotPlusMinus -> CT.Expr
writeUnaryExpressionNotPlusMinus e = case e of
  Java.UnaryExpressionNotPlusMinusPostfix p -> writePostfixExpression p
  Java.UnaryExpressionNotPlusMinusTilde u -> spaceSep [cst "~", writeUnaryExpression u]
  Java.UnaryExpressionNotPlusMinusNot u -> spaceSep [cst "!", writeUnaryExpression u]
  Java.UnaryExpressionNotPlusMinusCast c -> writeCastExpression c

writeUnqualifiedClassInstanceCreationExpression  :: Java.UnqualifiedClassInstanceCreationExpression -> CT.Expr
writeUnqualifiedClassInstanceCreationExpression (Java.UnqualifiedClassInstanceCreationExpression targs cit args mbody)
  = spaceSep $ Y.catMaybes [
    Just $ cst "new",
    if L.null targs then Nothing else Just $ angleBracesList False (writeTypeArgument <$> targs),
    Just $ noSep [writeClassOrInterfaceTypeToInstantiate cit, parenList (writeExpression <$> args)],
    writeClassBody <$> mbody]

writeVariableArityParameter :: Java.VariableArityParameter -> CT.Expr
writeVariableArityParameter _ = cst "TODO:VariableArityParameter"

writeVariableDeclarator :: Java.VariableDeclarator -> CT.Expr
writeVariableDeclarator (Java.VariableDeclarator id minit) = spaceSep $ Y.catMaybes [
  Just $ writeVariableDeclaratorId id,
  writeVariableInitializer <$> minit]

writeVariableDeclaratorId :: Java.VariableDeclaratorId -> CT.Expr
writeVariableDeclaratorId (Java.VariableDeclaratorId id mdims) = noSep $ Y.catMaybes [
  Just $ writeIdentifier id,
  writeDims <$> mdims]

writeVariableInitializer :: Java.VariableInitializer -> CT.Expr
writeVariableInitializer _ = cst "TODO:VariableInitializer"

writeVariableModifier :: Java.VariableModifier -> CT.Expr
writeVariableModifier m = case m of
  Java.VariableModifierAnnotation ann -> writeAnnotation ann
  Java.VariableModifierFinal -> cst "final"

writeWhileStatement :: Java.WhileStatement -> CT.Expr
writeWhileStatement _ = cst "TODO:WhileStatement"

writeWildcard :: Java.Wildcard -> CT.Expr
writeWildcard (Java.Wildcard anns mbounds) = spaceSep $ Y.catMaybes [
  if L.null anns then Nothing else Just $ commaSep False (writeAnnotation <$> anns),
  Just $ cst "*",
  writeWildcardBounds <$> mbounds]

writeWildcardBounds :: Java.WildcardBounds -> CT.Expr
writeWildcardBounds b = case b of
  Java.WildcardBoundsExtends rt -> spaceSep [cst "extends", writeReferenceType rt]
  Java.WildcardBoundsSuper rt -> spaceSep [cst "super", writeReferenceType rt]

prefixAt :: CT.Expr -> CT.Expr
prefixAt e = noSep [cst "@", e]

suffixSemi :: CT.Expr -> CT.Expr
suffixSemi e = noSep [e, cst ";"]
