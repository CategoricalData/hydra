package hydra.ext.java.syntax

import hydra.core.*

type Identifier = scala.Predef.String

type TypeIdentifier = hydra.ext.java.syntax.Identifier

enum Literal :
   case `null` extends Literal
   case integer(value: hydra.ext.java.syntax.IntegerLiteral) extends Literal
   case floatingPoint(value: hydra.ext.java.syntax.FloatingPointLiteral) extends Literal
   case boolean(value: Boolean) extends Literal
   case character(value: Int) extends Literal
   case string(value: hydra.ext.java.syntax.StringLiteral) extends Literal

type IntegerLiteral = BigInt

type FloatingPointLiteral = BigDecimal

type StringLiteral = scala.Predef.String

enum Type :
   case primitive(value: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations) extends Type
   case reference(value: hydra.ext.java.syntax.ReferenceType) extends Type

case class PrimitiveTypeWithAnnotations(`type`: hydra.ext.java.syntax.PrimitiveType, annotations: Seq[hydra.ext.java.syntax.Annotation])

enum PrimitiveType :
   case numeric(value: hydra.ext.java.syntax.NumericType) extends PrimitiveType
   case boolean extends PrimitiveType

enum NumericType :
   case integral(value: hydra.ext.java.syntax.IntegralType) extends NumericType
   case floatingPoint(value: hydra.ext.java.syntax.FloatingPointType) extends NumericType

enum IntegralType :
   case byte extends IntegralType
   case short extends IntegralType
   case int extends IntegralType
   case long extends IntegralType
   case char extends IntegralType

enum FloatingPointType :
   case float extends FloatingPointType
   case double extends FloatingPointType

enum ReferenceType :
   case classOrInterface(value: hydra.ext.java.syntax.ClassOrInterfaceType) extends ReferenceType
   case variable(value: hydra.ext.java.syntax.TypeVariable) extends ReferenceType
   case array(value: hydra.ext.java.syntax.ArrayType) extends ReferenceType

enum ClassOrInterfaceType :
   case `class`(value: hydra.ext.java.syntax.ClassType) extends ClassOrInterfaceType
   case interface(value: hydra.ext.java.syntax.InterfaceType) extends ClassOrInterfaceType

case class ClassType(annotations: Seq[hydra.ext.java.syntax.Annotation], qualifier: hydra.ext.java.syntax.ClassTypeQualifier,
   identifier: hydra.ext.java.syntax.TypeIdentifier, arguments: Seq[hydra.ext.java.syntax.TypeArgument])

enum ClassTypeQualifier :
   case none extends ClassTypeQualifier
   case `package`(value: hydra.ext.java.syntax.PackageName) extends ClassTypeQualifier
   case parent(value: hydra.ext.java.syntax.ClassOrInterfaceType) extends ClassTypeQualifier

type InterfaceType = hydra.ext.java.syntax.ClassType

case class TypeVariable(annotations: Seq[hydra.ext.java.syntax.Annotation], identifier: hydra.ext.java.syntax.TypeIdentifier)

case class ArrayType(dims: hydra.ext.java.syntax.Dims, variant: hydra.ext.java.syntax.ArrayType_Variant)

enum ArrayType_Variant :
   case primitive(value: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations) extends ArrayType_Variant
   case classOrInterface(value: hydra.ext.java.syntax.ClassOrInterfaceType) extends ArrayType_Variant
   case variable(value: hydra.ext.java.syntax.TypeVariable) extends ArrayType_Variant

type Dims = Seq[Seq[hydra.ext.java.syntax.Annotation]]

case class TypeParameter(modifiers: Seq[hydra.ext.java.syntax.TypeParameterModifier], identifier: hydra.ext.java.syntax.TypeIdentifier,
   bound: Option[hydra.ext.java.syntax.TypeBound])

type TypeParameterModifier = hydra.ext.java.syntax.Annotation

enum TypeBound :
   case variable(value: hydra.ext.java.syntax.TypeVariable) extends TypeBound
   case classOrInterface(value: hydra.ext.java.syntax.TypeBound_ClassOrInterface) extends TypeBound

case class TypeBound_ClassOrInterface(`type`: hydra.ext.java.syntax.ClassOrInterfaceType, additional: Seq[hydra.ext.java.syntax.AdditionalBound])

type AdditionalBound = hydra.ext.java.syntax.InterfaceType

enum TypeArgument :
   case reference(value: hydra.ext.java.syntax.ReferenceType) extends TypeArgument
   case wildcard(value: hydra.ext.java.syntax.Wildcard) extends TypeArgument

case class Wildcard(annotations: Seq[hydra.ext.java.syntax.Annotation], wildcard: Option[hydra.ext.java.syntax.WildcardBounds])

enum WildcardBounds :
   case `extends`(value: hydra.ext.java.syntax.ReferenceType) extends WildcardBounds
   case `super`(value: hydra.ext.java.syntax.ReferenceType) extends WildcardBounds

case class ModuleName(identifier: hydra.ext.java.syntax.Identifier, name: Option[hydra.ext.java.syntax.ModuleName])

type PackageName = Seq[hydra.ext.java.syntax.Identifier]

case class TypeName(identifier: hydra.ext.java.syntax.TypeIdentifier, qualifier: Option[hydra.ext.java.syntax.PackageOrTypeName])

case class ExpressionName(qualifier: Option[hydra.ext.java.syntax.AmbiguousName], identifier: hydra.ext.java.syntax.Identifier)

type MethodName = hydra.ext.java.syntax.Identifier

type PackageOrTypeName = Seq[hydra.ext.java.syntax.Identifier]

type AmbiguousName = Seq[hydra.ext.java.syntax.Identifier]

enum CompilationUnit :
   case ordinary(value: hydra.ext.java.syntax.OrdinaryCompilationUnit) extends CompilationUnit
   case modular(value: hydra.ext.java.syntax.ModularCompilationUnit) extends CompilationUnit

case class OrdinaryCompilationUnit(`package`: Option[hydra.ext.java.syntax.PackageDeclaration], imports: Seq[hydra.ext.java.syntax.ImportDeclaration],
   types: Seq[hydra.ext.java.syntax.TypeDeclarationWithComments])

case class ModularCompilationUnit(imports: Seq[hydra.ext.java.syntax.ImportDeclaration], module: hydra.ext.java.syntax.ModuleDeclaration)

case class PackageDeclaration(modifiers: Seq[hydra.ext.java.syntax.PackageModifier], identifiers: Seq[hydra.ext.java.syntax.Identifier])

type PackageModifier = hydra.ext.java.syntax.Annotation

enum ImportDeclaration :
   case singleType(value: hydra.ext.java.syntax.SingleTypeImportDeclaration) extends ImportDeclaration
   case typeImportOnDemand(value: hydra.ext.java.syntax.TypeImportOnDemandDeclaration) extends ImportDeclaration
   case singleStaticImport(value: hydra.ext.java.syntax.SingleStaticImportDeclaration) extends ImportDeclaration
   case staticImportOnDemand(value: hydra.ext.java.syntax.StaticImportOnDemandDeclaration) extends ImportDeclaration

type SingleTypeImportDeclaration = hydra.ext.java.syntax.TypeName

type TypeImportOnDemandDeclaration = hydra.ext.java.syntax.PackageOrTypeName

case class SingleStaticImportDeclaration(typeName: hydra.ext.java.syntax.TypeName, identifier: hydra.ext.java.syntax.Identifier)

type StaticImportOnDemandDeclaration = hydra.ext.java.syntax.TypeName

enum TypeDeclaration :
   case `class`(value: hydra.ext.java.syntax.ClassDeclaration) extends TypeDeclaration
   case interface(value: hydra.ext.java.syntax.InterfaceDeclaration) extends TypeDeclaration
   case none extends TypeDeclaration

case class TypeDeclarationWithComments(value: hydra.ext.java.syntax.TypeDeclaration, comments: Option[scala.Predef.String])

case class ModuleDeclaration(annotations: Seq[hydra.ext.java.syntax.Annotation], open: Boolean, identifiers: Seq[hydra.ext.java.syntax.Identifier],
   directives: Seq[Seq[hydra.ext.java.syntax.ModuleDirective]])

enum ModuleDirective :
   case requires(value: hydra.ext.java.syntax.ModuleDirective_Requires) extends ModuleDirective
   case exports(value: hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens) extends ModuleDirective
   case opens(value: hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens) extends ModuleDirective
   case uses(value: hydra.ext.java.syntax.TypeName) extends ModuleDirective
   case provides(value: hydra.ext.java.syntax.ModuleDirective_Provides) extends ModuleDirective

case class ModuleDirective_Requires(modifiers: Seq[hydra.ext.java.syntax.RequiresModifier], module: hydra.ext.java.syntax.ModuleName)

case class ModuleDirective_ExportsOrOpens(`package`: hydra.ext.java.syntax.PackageName, modules: Seq[hydra.ext.java.syntax.ModuleName])

case class ModuleDirective_Provides(to: hydra.ext.java.syntax.TypeName, `with`: Seq[hydra.ext.java.syntax.TypeName])

enum RequiresModifier :
   case transitive extends RequiresModifier
   case static extends RequiresModifier

enum ClassDeclaration :
   case normal(value: hydra.ext.java.syntax.NormalClassDeclaration) extends ClassDeclaration
   case `enum`(value: hydra.ext.java.syntax.EnumDeclaration) extends ClassDeclaration

case class NormalClassDeclaration(modifiers: Seq[hydra.ext.java.syntax.ClassModifier], identifier: hydra.ext.java.syntax.TypeIdentifier,
   parameters: Seq[hydra.ext.java.syntax.TypeParameter], `extends`: Option[hydra.ext.java.syntax.ClassType],
   implements: Seq[hydra.ext.java.syntax.InterfaceType], body: hydra.ext.java.syntax.ClassBody)

enum ClassModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends ClassModifier
   case public extends ClassModifier
   case `protected` extends ClassModifier
   case `private` extends ClassModifier
   case `abstract` extends ClassModifier
   case static extends ClassModifier
   case `final` extends ClassModifier
   case strictfp extends ClassModifier

type ClassBody = Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]

enum ClassBodyDeclaration :
   case classMember(value: hydra.ext.java.syntax.ClassMemberDeclaration) extends ClassBodyDeclaration
   case instanceInitializer(value: hydra.ext.java.syntax.InstanceInitializer) extends ClassBodyDeclaration
   case staticInitializer(value: hydra.ext.java.syntax.StaticInitializer) extends ClassBodyDeclaration
   case constructorDeclaration(value: hydra.ext.java.syntax.ConstructorDeclaration) extends ClassBodyDeclaration

case class ClassBodyDeclarationWithComments(value: hydra.ext.java.syntax.ClassBodyDeclaration, comments: Option[scala.Predef.String])

enum ClassMemberDeclaration :
   case field(value: hydra.ext.java.syntax.FieldDeclaration) extends ClassMemberDeclaration
   case method(value: hydra.ext.java.syntax.MethodDeclaration) extends ClassMemberDeclaration
   case `class`(value: hydra.ext.java.syntax.ClassDeclaration) extends ClassMemberDeclaration
   case interface(value: hydra.ext.java.syntax.InterfaceDeclaration) extends ClassMemberDeclaration
   case none extends ClassMemberDeclaration

case class FieldDeclaration(modifiers: Seq[hydra.ext.java.syntax.FieldModifier], unannType: hydra.ext.java.syntax.UnannType,
   variableDeclarators: Seq[hydra.ext.java.syntax.VariableDeclarator])

enum FieldModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends FieldModifier
   case public extends FieldModifier
   case `protected` extends FieldModifier
   case `private` extends FieldModifier
   case static extends FieldModifier
   case `final` extends FieldModifier
   case transient extends FieldModifier
   case volatile extends FieldModifier

case class VariableDeclarator(id: hydra.ext.java.syntax.VariableDeclaratorId, initializer: Option[hydra.ext.java.syntax.VariableInitializer])

case class VariableDeclaratorId(identifier: hydra.ext.java.syntax.Identifier, dims: Option[hydra.ext.java.syntax.Dims])

enum VariableInitializer :
   case expression(value: hydra.ext.java.syntax.Expression) extends VariableInitializer
   case arrayInitializer(value: hydra.ext.java.syntax.ArrayInitializer) extends VariableInitializer

type UnannType = hydra.ext.java.syntax.Type

type UnannClassType = hydra.ext.java.syntax.ClassType

case class MethodDeclaration(annotations: Seq[hydra.ext.java.syntax.Annotation], modifiers: Seq[hydra.ext.java.syntax.MethodModifier],
   header: hydra.ext.java.syntax.MethodHeader, body: hydra.ext.java.syntax.MethodBody)

enum MethodModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends MethodModifier
   case public extends MethodModifier
   case `protected` extends MethodModifier
   case `private` extends MethodModifier
   case `abstract` extends MethodModifier
   case static extends MethodModifier
   case `final` extends MethodModifier
   case synchronized extends MethodModifier
   case native extends MethodModifier
   case strictfb extends MethodModifier

case class MethodHeader(parameters: Seq[hydra.ext.java.syntax.TypeParameter], result: hydra.ext.java.syntax.Result,
   declarator: hydra.ext.java.syntax.MethodDeclarator, throws: Option[hydra.ext.java.syntax.Throws])

enum Result :
   case `type`(value: hydra.ext.java.syntax.UnannType) extends Result
   case void extends Result

case class MethodDeclarator(identifier: hydra.ext.java.syntax.Identifier, receiverParameter: Option[hydra.ext.java.syntax.ReceiverParameter],
   formalParameters: Seq[hydra.ext.java.syntax.FormalParameter])

case class ReceiverParameter(annotations: Seq[hydra.ext.java.syntax.Annotation], unannType: hydra.ext.java.syntax.UnannType,
   identifier: Option[hydra.ext.java.syntax.Identifier])

enum FormalParameter :
   case simple(value: hydra.ext.java.syntax.FormalParameter_Simple) extends FormalParameter
   case variableArity(value: hydra.ext.java.syntax.VariableArityParameter) extends FormalParameter

case class FormalParameter_Simple(modifiers: Seq[hydra.ext.java.syntax.VariableModifier], `type`: hydra.ext.java.syntax.UnannType,
   id: hydra.ext.java.syntax.VariableDeclaratorId)

case class VariableArityParameter(modifiers: hydra.ext.java.syntax.VariableModifier, `type`: hydra.ext.java.syntax.UnannType,
   annotations: Seq[hydra.ext.java.syntax.Annotation], identifier: hydra.ext.java.syntax.Identifier)

enum VariableModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends VariableModifier
   case `final` extends VariableModifier

type Throws = Seq[hydra.ext.java.syntax.ExceptionType]

enum ExceptionType :
   case `class`(value: hydra.ext.java.syntax.ClassType) extends ExceptionType
   case variable(value: hydra.ext.java.syntax.TypeVariable) extends ExceptionType

enum MethodBody :
   case block(value: hydra.ext.java.syntax.Block) extends MethodBody
   case none extends MethodBody

type InstanceInitializer = hydra.ext.java.syntax.Block

type StaticInitializer = hydra.ext.java.syntax.Block

case class ConstructorDeclaration(modifiers: Seq[hydra.ext.java.syntax.ConstructorModifier], constructor: hydra.ext.java.syntax.ConstructorDeclarator,
   throws: Option[hydra.ext.java.syntax.Throws], body: hydra.ext.java.syntax.ConstructorBody)

enum ConstructorModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends ConstructorModifier
   case public extends ConstructorModifier
   case `protected` extends ConstructorModifier
   case `private` extends ConstructorModifier

case class ConstructorDeclarator(parameters: Seq[hydra.ext.java.syntax.TypeParameter], name: hydra.ext.java.syntax.SimpleTypeName,
   receiverParameter: Option[hydra.ext.java.syntax.ReceiverParameter], formalParameters: Seq[hydra.ext.java.syntax.FormalParameter])

type SimpleTypeName = hydra.ext.java.syntax.TypeIdentifier

case class ConstructorBody(invocation: Option[hydra.ext.java.syntax.ExplicitConstructorInvocation], statements: Seq[hydra.ext.java.syntax.BlockStatement])

case class ExplicitConstructorInvocation(typeArguments: Seq[hydra.ext.java.syntax.TypeArgument], arguments: Seq[hydra.ext.java.syntax.Expression],
   variant: hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant)

enum ExplicitConstructorInvocation_Variant :
   case `this` extends ExplicitConstructorInvocation_Variant
   case `super`(value: Option[hydra.ext.java.syntax.ExpressionName]) extends ExplicitConstructorInvocation_Variant
   case primary(value: hydra.ext.java.syntax.Primary) extends ExplicitConstructorInvocation_Variant

case class EnumDeclaration(modifiers: Seq[hydra.ext.java.syntax.ClassModifier], identifier: hydra.ext.java.syntax.TypeIdentifier,
   implements: Seq[hydra.ext.java.syntax.InterfaceType], body: hydra.ext.java.syntax.EnumBody)

type EnumBody = Seq[hydra.ext.java.syntax.EnumBody_Element]

case class EnumBody_Element(constants: Seq[hydra.ext.java.syntax.EnumConstant], bodyDeclarations: Seq[hydra.ext.java.syntax.ClassBodyDeclaration])

case class EnumConstant(modifiers: Seq[hydra.ext.java.syntax.EnumConstantModifier], identifier: hydra.ext.java.syntax.Identifier,
   arguments: Seq[Seq[hydra.ext.java.syntax.Expression]], body: Option[hydra.ext.java.syntax.ClassBody])

type EnumConstantModifier = hydra.ext.java.syntax.Annotation

enum InterfaceDeclaration :
   case normalInterface(value: hydra.ext.java.syntax.NormalInterfaceDeclaration) extends InterfaceDeclaration
   case annotationType(value: hydra.ext.java.syntax.AnnotationTypeDeclaration) extends InterfaceDeclaration

case class NormalInterfaceDeclaration(modifiers: Seq[hydra.ext.java.syntax.InterfaceModifier], identifier: hydra.ext.java.syntax.TypeIdentifier,
   parameters: Seq[hydra.ext.java.syntax.TypeParameter], `extends`: Seq[hydra.ext.java.syntax.InterfaceType],
   body: hydra.ext.java.syntax.InterfaceBody)

enum InterfaceModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends InterfaceModifier
   case public extends InterfaceModifier
   case `protected` extends InterfaceModifier
   case `private` extends InterfaceModifier
   case `abstract` extends InterfaceModifier
   case static extends InterfaceModifier
   case strictfb extends InterfaceModifier

type InterfaceBody = Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration]

enum InterfaceMemberDeclaration :
   case constant(value: hydra.ext.java.syntax.ConstantDeclaration) extends InterfaceMemberDeclaration
   case interfaceMethod(value: hydra.ext.java.syntax.InterfaceMethodDeclaration) extends InterfaceMemberDeclaration
   case `class`(value: hydra.ext.java.syntax.ClassDeclaration) extends InterfaceMemberDeclaration
   case interface(value: hydra.ext.java.syntax.InterfaceDeclaration) extends InterfaceMemberDeclaration

case class ConstantDeclaration(modifiers: Seq[hydra.ext.java.syntax.ConstantModifier], `type`: hydra.ext.java.syntax.UnannType,
   variables: Seq[hydra.ext.java.syntax.VariableDeclarator])

enum ConstantModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends ConstantModifier
   case public extends ConstantModifier
   case static extends ConstantModifier
   case `final` extends ConstantModifier

case class InterfaceMethodDeclaration(modifiers: Seq[hydra.ext.java.syntax.InterfaceMethodModifier], header: hydra.ext.java.syntax.MethodHeader,
   body: hydra.ext.java.syntax.MethodBody)

enum InterfaceMethodModifier :
   case annotation(value: hydra.ext.java.syntax.Annotation) extends InterfaceMethodModifier
   case public extends InterfaceMethodModifier
   case `private` extends InterfaceMethodModifier
   case `abstract` extends InterfaceMethodModifier
   case default extends InterfaceMethodModifier
   case static extends InterfaceMethodModifier
   case strictfp extends InterfaceMethodModifier

case class AnnotationTypeDeclaration(modifiers: Seq[hydra.ext.java.syntax.InterfaceModifier], identifier: hydra.ext.java.syntax.TypeIdentifier,
   body: hydra.ext.java.syntax.AnnotationTypeBody)

type AnnotationTypeBody = Seq[Seq[hydra.ext.java.syntax.AnnotationTypeMemberDeclaration]]

enum AnnotationTypeMemberDeclaration :
   case annotationType(value: hydra.ext.java.syntax.AnnotationTypeElementDeclaration) extends AnnotationTypeMemberDeclaration
   case constant(value: hydra.ext.java.syntax.ConstantDeclaration) extends AnnotationTypeMemberDeclaration
   case `class`(value: hydra.ext.java.syntax.ClassDeclaration) extends AnnotationTypeMemberDeclaration
   case interface(value: hydra.ext.java.syntax.InterfaceDeclaration) extends AnnotationTypeMemberDeclaration

case class AnnotationTypeElementDeclaration(modifiers: Seq[hydra.ext.java.syntax.AnnotationTypeElementModifier],
   `type`: hydra.ext.java.syntax.UnannType, identifier: hydra.ext.java.syntax.Identifier, dims: Option[hydra.ext.java.syntax.Dims],
   default: Option[hydra.ext.java.syntax.DefaultValue])

enum AnnotationTypeElementModifier :
   case public(value: hydra.ext.java.syntax.Annotation) extends AnnotationTypeElementModifier
   case `abstract` extends AnnotationTypeElementModifier

type DefaultValue = hydra.ext.java.syntax.ElementValue

enum Annotation :
   case normal(value: hydra.ext.java.syntax.NormalAnnotation) extends Annotation
   case marker(value: hydra.ext.java.syntax.MarkerAnnotation) extends Annotation
   case singleElement(value: hydra.ext.java.syntax.SingleElementAnnotation) extends Annotation

case class NormalAnnotation(typeName: hydra.ext.java.syntax.TypeName, pairs: Seq[hydra.ext.java.syntax.ElementValuePair])

case class ElementValuePair(key: hydra.ext.java.syntax.Identifier, value: hydra.ext.java.syntax.ElementValue)

enum ElementValue :
   case conditionalExpression(value: hydra.ext.java.syntax.ConditionalExpression) extends ElementValue
   case elementValueArrayInitializer(value: hydra.ext.java.syntax.ElementValueArrayInitializer) extends ElementValue
   case annotation(value: hydra.ext.java.syntax.Annotation) extends ElementValue

type ElementValueArrayInitializer = Seq[hydra.ext.java.syntax.ElementValue]

type MarkerAnnotation = hydra.ext.java.syntax.TypeName

case class SingleElementAnnotation(name: hydra.ext.java.syntax.TypeName, value: Option[hydra.ext.java.syntax.ElementValue])

type ArrayInitializer = Seq[Seq[hydra.ext.java.syntax.VariableInitializer]]

type Block = Seq[hydra.ext.java.syntax.BlockStatement]

enum BlockStatement :
   case localVariableDeclaration(value: hydra.ext.java.syntax.LocalVariableDeclarationStatement) extends BlockStatement
   case `class`(value: hydra.ext.java.syntax.ClassDeclaration) extends BlockStatement
   case statement(value: hydra.ext.java.syntax.Statement) extends BlockStatement

type LocalVariableDeclarationStatement = hydra.ext.java.syntax.LocalVariableDeclaration

case class LocalVariableDeclaration(modifiers: Seq[hydra.ext.java.syntax.VariableModifier], `type`: hydra.ext.java.syntax.LocalVariableType,
   declarators: Seq[hydra.ext.java.syntax.VariableDeclarator])

enum LocalVariableType :
   case `type`(value: hydra.ext.java.syntax.UnannType) extends LocalVariableType
   case `var` extends LocalVariableType

enum Statement :
   case withoutTrailing(value: hydra.ext.java.syntax.StatementWithoutTrailingSubstatement) extends Statement
   case labeled(value: hydra.ext.java.syntax.LabeledStatement) extends Statement
   case ifThen(value: hydra.ext.java.syntax.IfThenStatement) extends Statement
   case ifThenElse(value: hydra.ext.java.syntax.IfThenElseStatement) extends Statement
   case `while`(value: hydra.ext.java.syntax.WhileStatement) extends Statement
   case `for`(value: hydra.ext.java.syntax.ForStatement) extends Statement

enum StatementNoShortIf :
   case withoutTrailing(value: hydra.ext.java.syntax.StatementWithoutTrailingSubstatement) extends StatementNoShortIf
   case labeled(value: hydra.ext.java.syntax.LabeledStatementNoShortIf) extends StatementNoShortIf
   case ifThenElse(value: hydra.ext.java.syntax.IfThenElseStatementNoShortIf) extends StatementNoShortIf
   case `while`(value: hydra.ext.java.syntax.WhileStatementNoShortIf) extends StatementNoShortIf
   case `for`(value: hydra.ext.java.syntax.ForStatementNoShortIf) extends StatementNoShortIf

enum StatementWithoutTrailingSubstatement :
   case block(value: hydra.ext.java.syntax.Block) extends StatementWithoutTrailingSubstatement
   case empty extends StatementWithoutTrailingSubstatement
   case expression(value: hydra.ext.java.syntax.ExpressionStatement) extends StatementWithoutTrailingSubstatement
   case assert(value: hydra.ext.java.syntax.AssertStatement) extends StatementWithoutTrailingSubstatement
   case switch(value: hydra.ext.java.syntax.SwitchStatement) extends StatementWithoutTrailingSubstatement
   case `do`(value: hydra.ext.java.syntax.DoStatement) extends StatementWithoutTrailingSubstatement
   case break(value: hydra.ext.java.syntax.BreakStatement) extends StatementWithoutTrailingSubstatement
   case continue(value: hydra.ext.java.syntax.ContinueStatement) extends StatementWithoutTrailingSubstatement
   case `return`(value: hydra.ext.java.syntax.ReturnStatement) extends StatementWithoutTrailingSubstatement
   case synchronized(value: hydra.ext.java.syntax.SynchronizedStatement) extends StatementWithoutTrailingSubstatement
   case `throw`(value: hydra.ext.java.syntax.ThrowStatement) extends StatementWithoutTrailingSubstatement
   case `try`(value: hydra.ext.java.syntax.TryStatement) extends StatementWithoutTrailingSubstatement

case class LabeledStatement(identifier: hydra.ext.java.syntax.Identifier, statement: hydra.ext.java.syntax.Statement)

case class LabeledStatementNoShortIf(identifier: hydra.ext.java.syntax.Identifier, statement: hydra.ext.java.syntax.StatementNoShortIf)

type ExpressionStatement = hydra.ext.java.syntax.StatementExpression

enum StatementExpression :
   case assignment(value: hydra.ext.java.syntax.Assignment) extends StatementExpression
   case preIncrement(value: hydra.ext.java.syntax.PreIncrementExpression) extends StatementExpression
   case preDecrement(value: hydra.ext.java.syntax.PreDecrementExpression) extends StatementExpression
   case postIncrement(value: hydra.ext.java.syntax.PostIncrementExpression) extends StatementExpression
   case postDecrement(value: hydra.ext.java.syntax.PostDecrementExpression) extends StatementExpression
   case methodInvocation(value: hydra.ext.java.syntax.MethodInvocation) extends StatementExpression
   case classInstanceCreation(value: hydra.ext.java.syntax.ClassInstanceCreationExpression) extends StatementExpression

case class IfThenStatement(expression: hydra.ext.java.syntax.Expression, statement: hydra.ext.java.syntax.Statement)

case class IfThenElseStatement(cond: Option[hydra.ext.java.syntax.Expression], `then`: hydra.ext.java.syntax.StatementNoShortIf,
   `else`: hydra.ext.java.syntax.Statement)

case class IfThenElseStatementNoShortIf(cond: Option[hydra.ext.java.syntax.Expression], `then`: hydra.ext.java.syntax.StatementNoShortIf,
   `else`: hydra.ext.java.syntax.StatementNoShortIf)

enum AssertStatement :
   case single(value: hydra.ext.java.syntax.Expression) extends AssertStatement
   case pair(value: hydra.ext.java.syntax.AssertStatement_Pair) extends AssertStatement

case class AssertStatement_Pair(first: hydra.ext.java.syntax.Expression, second: hydra.ext.java.syntax.Expression)

case class SwitchStatement(cond: hydra.ext.java.syntax.Expression, block: hydra.ext.java.syntax.SwitchBlock)

type SwitchBlock = Seq[hydra.ext.java.syntax.SwitchBlock_Pair]

case class SwitchBlock_Pair(statements: Seq[hydra.ext.java.syntax.SwitchBlockStatementGroup], labels: Seq[hydra.ext.java.syntax.SwitchLabel])

case class SwitchBlockStatementGroup(labels: Seq[hydra.ext.java.syntax.SwitchLabel], statements: Seq[hydra.ext.java.syntax.BlockStatement])

enum SwitchLabel :
   case constant(value: hydra.ext.java.syntax.ConstantExpression) extends SwitchLabel
   case enumConstant(value: hydra.ext.java.syntax.EnumConstantName) extends SwitchLabel
   case default extends SwitchLabel

type EnumConstantName = hydra.ext.java.syntax.Identifier

case class WhileStatement(cond: Option[hydra.ext.java.syntax.Expression], body: hydra.ext.java.syntax.Statement)

case class WhileStatementNoShortIf(cond: Option[hydra.ext.java.syntax.Expression], body: hydra.ext.java.syntax.StatementNoShortIf)

case class DoStatement(body: hydra.ext.java.syntax.Statement, conde: Option[hydra.ext.java.syntax.Expression])

enum ForStatement :
   case basic(value: hydra.ext.java.syntax.BasicForStatement) extends ForStatement
   case enhanced(value: hydra.ext.java.syntax.EnhancedForStatement) extends ForStatement

enum ForStatementNoShortIf :
   case basic(value: hydra.ext.java.syntax.BasicForStatementNoShortIf) extends ForStatementNoShortIf
   case enhanced(value: hydra.ext.java.syntax.EnhancedForStatementNoShortIf) extends ForStatementNoShortIf

case class BasicForStatement(cond: hydra.ext.java.syntax.ForCond, body: hydra.ext.java.syntax.Statement)

case class ForCond(init: Option[hydra.ext.java.syntax.ForInit], cond: Option[hydra.ext.java.syntax.Expression], update: Option[hydra.ext.java.syntax.ForUpdate])

case class BasicForStatementNoShortIf(cond: hydra.ext.java.syntax.ForCond, body: hydra.ext.java.syntax.StatementNoShortIf)

enum ForInit :
   case statements(value: Seq[hydra.ext.java.syntax.StatementExpression]) extends ForInit
   case localVariable(value: hydra.ext.java.syntax.LocalVariableDeclaration) extends ForInit

type ForUpdate = Seq[hydra.ext.java.syntax.StatementExpression]

case class EnhancedForStatement(cond: hydra.ext.java.syntax.EnhancedForCond, body: hydra.ext.java.syntax.Statement)

case class EnhancedForCond(modifiers: Seq[hydra.ext.java.syntax.VariableModifier], `type`: hydra.ext.java.syntax.LocalVariableType,
   id: hydra.ext.java.syntax.VariableDeclaratorId, expression: hydra.ext.java.syntax.Expression)

case class EnhancedForStatementNoShortIf(cond: hydra.ext.java.syntax.EnhancedForCond, body: hydra.ext.java.syntax.StatementNoShortIf)

type BreakStatement = Option[hydra.ext.java.syntax.Identifier]

type ContinueStatement = Option[hydra.ext.java.syntax.Identifier]

type ReturnStatement = Option[hydra.ext.java.syntax.Expression]

type ThrowStatement = hydra.ext.java.syntax.Expression

case class SynchronizedStatement(expression: hydra.ext.java.syntax.Expression, block: hydra.ext.java.syntax.Block)

enum TryStatement :
   case simple(value: hydra.ext.java.syntax.TryStatement_Simple) extends TryStatement
   case withFinally(value: hydra.ext.java.syntax.TryStatement_WithFinally) extends TryStatement
   case withResources(value: hydra.ext.java.syntax.TryWithResourcesStatement) extends TryStatement

case class TryStatement_Simple(block: hydra.ext.java.syntax.Block, catches: hydra.ext.java.syntax.Catches)

case class TryStatement_WithFinally(block: hydra.ext.java.syntax.Block, catches: Option[hydra.ext.java.syntax.Catches],
   `finally`: hydra.ext.java.syntax.Finally)

type Catches = Seq[hydra.ext.java.syntax.CatchClause]

case class CatchClause(parameter: Option[hydra.ext.java.syntax.CatchFormalParameter], block: hydra.ext.java.syntax.Block)

case class CatchFormalParameter(modifiers: Seq[hydra.ext.java.syntax.VariableModifier], `type`: hydra.ext.java.syntax.CatchType,
   id: hydra.ext.java.syntax.VariableDeclaratorId)

case class CatchType(`type`: hydra.ext.java.syntax.UnannClassType, types: Seq[hydra.ext.java.syntax.ClassType])

type Finally = hydra.ext.java.syntax.Block

case class TryWithResourcesStatement(resourceSpecification: hydra.ext.java.syntax.ResourceSpecification,
   block: hydra.ext.java.syntax.Block, catches: Option[hydra.ext.java.syntax.Catches], `finally`: Option[hydra.ext.java.syntax.Finally])

type ResourceSpecification = Seq[hydra.ext.java.syntax.Resource]

enum Resource :
   case local(value: hydra.ext.java.syntax.Resource_Local) extends Resource
   case variable(value: hydra.ext.java.syntax.VariableAccess) extends Resource

case class Resource_Local(modifiers: Seq[hydra.ext.java.syntax.VariableModifier], `type`: hydra.ext.java.syntax.LocalVariableType,
   identifier: hydra.ext.java.syntax.Identifier, expression: hydra.ext.java.syntax.Expression)

enum VariableAccess :
   case expressionName(value: hydra.ext.java.syntax.ExpressionName) extends VariableAccess
   case fieldAccess(value: hydra.ext.java.syntax.FieldAccess) extends VariableAccess

enum Primary :
   case noNewArray(value: hydra.ext.java.syntax.PrimaryNoNewArray) extends Primary
   case arrayCreation(value: hydra.ext.java.syntax.ArrayCreationExpression) extends Primary

enum PrimaryNoNewArray :
   case literal(value: hydra.ext.java.syntax.Literal) extends PrimaryNoNewArray
   case classLiteral(value: hydra.ext.java.syntax.ClassLiteral) extends PrimaryNoNewArray
   case `this` extends PrimaryNoNewArray
   case dotThis(value: hydra.ext.java.syntax.TypeName) extends PrimaryNoNewArray
   case parens(value: hydra.ext.java.syntax.Expression) extends PrimaryNoNewArray
   case classInstance(value: hydra.ext.java.syntax.ClassInstanceCreationExpression) extends PrimaryNoNewArray
   case fieldAccess(value: hydra.ext.java.syntax.FieldAccess) extends PrimaryNoNewArray
   case arrayAccess(value: hydra.ext.java.syntax.ArrayAccess) extends PrimaryNoNewArray
   case methodInvocation(value: hydra.ext.java.syntax.MethodInvocation) extends PrimaryNoNewArray
   case methodReference(value: hydra.ext.java.syntax.MethodReference) extends PrimaryNoNewArray

enum ClassLiteral :
   case `type`(value: hydra.ext.java.syntax.TypeNameArray) extends ClassLiteral
   case numericType(value: hydra.ext.java.syntax.NumericTypeArray) extends ClassLiteral
   case boolean(value: hydra.ext.java.syntax.BooleanArray) extends ClassLiteral
   case void extends ClassLiteral

enum TypeNameArray :
   case simple(value: hydra.ext.java.syntax.TypeName) extends TypeNameArray
   case array(value: hydra.ext.java.syntax.TypeNameArray) extends TypeNameArray

enum NumericTypeArray :
   case simple(value: hydra.ext.java.syntax.NumericType) extends NumericTypeArray
   case array(value: hydra.ext.java.syntax.NumericTypeArray) extends NumericTypeArray

enum BooleanArray :
   case simple extends BooleanArray
   case array(value: hydra.ext.java.syntax.BooleanArray) extends BooleanArray

case class ClassInstanceCreationExpression(qualifier: Option[hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier],
   expression: hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression)

enum ClassInstanceCreationExpression_Qualifier :
   case expression(value: hydra.ext.java.syntax.ExpressionName) extends ClassInstanceCreationExpression_Qualifier
   case primary(value: hydra.ext.java.syntax.Primary) extends ClassInstanceCreationExpression_Qualifier

case class UnqualifiedClassInstanceCreationExpression(typeArguments: Seq[hydra.ext.java.syntax.TypeArgument],
   classOrInterface: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate, arguments: Seq[hydra.ext.java.syntax.Expression],
   body: Option[hydra.ext.java.syntax.ClassBody])

case class ClassOrInterfaceTypeToInstantiate(identifiers: Seq[hydra.ext.java.syntax.AnnotatedIdentifier],
   typeArguments: Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond])

case class AnnotatedIdentifier(annotations: Seq[hydra.ext.java.syntax.Annotation], identifier: hydra.ext.java.syntax.Identifier)

enum TypeArgumentsOrDiamond :
   case arguments(value: Seq[hydra.ext.java.syntax.TypeArgument]) extends TypeArgumentsOrDiamond
   case diamond extends TypeArgumentsOrDiamond

case class FieldAccess(qualifier: hydra.ext.java.syntax.FieldAccess_Qualifier, identifier: hydra.ext.java.syntax.Identifier)

enum FieldAccess_Qualifier :
   case primary(value: hydra.ext.java.syntax.Primary) extends FieldAccess_Qualifier
   case `super` extends FieldAccess_Qualifier
   case typed(value: hydra.ext.java.syntax.TypeName) extends FieldAccess_Qualifier

case class ArrayAccess(expression: Option[hydra.ext.java.syntax.Expression], variant: hydra.ext.java.syntax.ArrayAccess_Variant)

enum ArrayAccess_Variant :
   case name(value: hydra.ext.java.syntax.ExpressionName) extends ArrayAccess_Variant
   case primary(value: hydra.ext.java.syntax.PrimaryNoNewArray) extends ArrayAccess_Variant

case class MethodInvocation(header: hydra.ext.java.syntax.MethodInvocation_Header, arguments: Seq[hydra.ext.java.syntax.Expression])

enum MethodInvocation_Header :
   case simple(value: hydra.ext.java.syntax.MethodName) extends MethodInvocation_Header
   case complex(value: hydra.ext.java.syntax.MethodInvocation_Complex) extends MethodInvocation_Header

case class MethodInvocation_Complex(variant: hydra.ext.java.syntax.MethodInvocation_Variant, typeArguments: Seq[hydra.ext.java.syntax.TypeArgument],
   identifier: hydra.ext.java.syntax.Identifier)

enum MethodInvocation_Variant :
   case `type`(value: hydra.ext.java.syntax.TypeName) extends MethodInvocation_Variant
   case expression(value: hydra.ext.java.syntax.ExpressionName) extends MethodInvocation_Variant
   case primary(value: hydra.ext.java.syntax.Primary) extends MethodInvocation_Variant
   case `super` extends MethodInvocation_Variant
   case typeSuper(value: hydra.ext.java.syntax.TypeName) extends MethodInvocation_Variant

enum MethodReference :
   case expression(value: hydra.ext.java.syntax.MethodReference_Expression) extends MethodReference
   case primary(value: hydra.ext.java.syntax.MethodReference_Primary) extends MethodReference
   case referenceType(value: hydra.ext.java.syntax.MethodReference_ReferenceType) extends MethodReference
   case `super`(value: hydra.ext.java.syntax.MethodReference_Super) extends MethodReference
   case `new`(value: hydra.ext.java.syntax.MethodReference_New) extends MethodReference
   case array(value: hydra.ext.java.syntax.MethodReference_Array) extends MethodReference

case class MethodReference_Expression(name: hydra.ext.java.syntax.ExpressionName, typeArguments: Seq[hydra.ext.java.syntax.TypeArgument],
   identifier: hydra.ext.java.syntax.Identifier)

case class MethodReference_Primary(primary: hydra.ext.java.syntax.Primary, typeArguments: Seq[hydra.ext.java.syntax.TypeArgument],
   identifier: hydra.ext.java.syntax.Identifier)

case class MethodReference_ReferenceType(referenceType: hydra.ext.java.syntax.ReferenceType, typeArguments: Seq[hydra.ext.java.syntax.TypeArgument],
   identifier: hydra.ext.java.syntax.Identifier)

case class MethodReference_Super(typeArguments: Seq[hydra.ext.java.syntax.TypeArgument], identifier: hydra.ext.java.syntax.Identifier, `super`: Boolean)

case class MethodReference_New(classType: hydra.ext.java.syntax.ClassType, typeArguments: Seq[hydra.ext.java.syntax.TypeArgument])

type MethodReference_Array = hydra.ext.java.syntax.ArrayType

enum ArrayCreationExpression :
   case primitive(value: hydra.ext.java.syntax.ArrayCreationExpression_Primitive) extends ArrayCreationExpression
   case classOrInterface(value: hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface) extends ArrayCreationExpression
   case primitiveArray(value: hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray) extends ArrayCreationExpression
   case classOrInterfaceArray(value: hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray) extends ArrayCreationExpression

case class ArrayCreationExpression_Primitive(`type`: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations,
   dimExprs: Seq[hydra.ext.java.syntax.DimExpr], dims: Option[hydra.ext.java.syntax.Dims])

case class ArrayCreationExpression_ClassOrInterface(`type`: hydra.ext.java.syntax.ClassOrInterfaceType,
   dimExprs: Seq[hydra.ext.java.syntax.DimExpr], dims: Option[hydra.ext.java.syntax.Dims])

case class ArrayCreationExpression_PrimitiveArray(`type`: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations,
   dims: Seq[hydra.ext.java.syntax.Dims], array: hydra.ext.java.syntax.ArrayInitializer)

case class ArrayCreationExpression_ClassOrInterfaceArray(`type`: hydra.ext.java.syntax.ClassOrInterfaceType,
   dims: Seq[hydra.ext.java.syntax.Dims], array: hydra.ext.java.syntax.ArrayInitializer)

case class DimExpr(annotations: Seq[hydra.ext.java.syntax.Annotation], expression: Option[hydra.ext.java.syntax.Expression])

enum Expression :
   case lambda(value: hydra.ext.java.syntax.LambdaExpression) extends Expression
   case assignment(value: hydra.ext.java.syntax.AssignmentExpression) extends Expression

case class LambdaExpression(parameters: hydra.ext.java.syntax.LambdaParameters, body: hydra.ext.java.syntax.LambdaBody)

enum LambdaParameters :
   case tuple(value: Seq[hydra.ext.java.syntax.LambdaParameters]) extends LambdaParameters
   case single(value: hydra.ext.java.syntax.Identifier) extends LambdaParameters

enum LambdaParameter :
   case normal(value: hydra.ext.java.syntax.LambdaParameter_Normal) extends LambdaParameter
   case variableArity(value: hydra.ext.java.syntax.VariableArityParameter) extends LambdaParameter

case class LambdaParameter_Normal(modifiers: Seq[hydra.ext.java.syntax.VariableModifier], `type`: hydra.ext.java.syntax.LambdaParameterType,
   id: hydra.ext.java.syntax.VariableDeclaratorId)

enum LambdaParameterType :
   case `type`(value: hydra.ext.java.syntax.UnannType) extends LambdaParameterType
   case `var` extends LambdaParameterType

enum LambdaBody :
   case expression(value: hydra.ext.java.syntax.Expression) extends LambdaBody
   case block(value: hydra.ext.java.syntax.Block) extends LambdaBody

enum AssignmentExpression :
   case conditional(value: hydra.ext.java.syntax.ConditionalExpression) extends AssignmentExpression
   case assignment(value: hydra.ext.java.syntax.Assignment) extends AssignmentExpression

case class Assignment(lhs: hydra.ext.java.syntax.LeftHandSide, op: hydra.ext.java.syntax.AssignmentOperator, expression: hydra.ext.java.syntax.Expression)

enum LeftHandSide :
   case expressionName(value: hydra.ext.java.syntax.ExpressionName) extends LeftHandSide
   case fieldAccess(value: hydra.ext.java.syntax.FieldAccess) extends LeftHandSide
   case arrayAccess(value: hydra.ext.java.syntax.ArrayAccess) extends LeftHandSide

enum AssignmentOperator :
   case simple extends AssignmentOperator
   case times extends AssignmentOperator
   case div extends AssignmentOperator
   case mod extends AssignmentOperator
   case plus extends AssignmentOperator
   case minus extends AssignmentOperator
   case shiftLeft extends AssignmentOperator
   case shiftRight extends AssignmentOperator
   case shiftRightZeroFill extends AssignmentOperator
   case and extends AssignmentOperator
   case xor extends AssignmentOperator
   case or extends AssignmentOperator

enum ConditionalExpression :
   case simple(value: hydra.ext.java.syntax.ConditionalOrExpression) extends ConditionalExpression
   case ternaryCond(value: hydra.ext.java.syntax.ConditionalExpression_TernaryCond) extends ConditionalExpression
   case ternaryLambda(value: hydra.ext.java.syntax.ConditionalExpression_TernaryLambda) extends ConditionalExpression

case class ConditionalExpression_TernaryCond(cond: hydra.ext.java.syntax.ConditionalOrExpression, ifTrue: hydra.ext.java.syntax.Expression,
   ifFalse: hydra.ext.java.syntax.ConditionalExpression)

case class ConditionalExpression_TernaryLambda(cond: hydra.ext.java.syntax.ConditionalOrExpression, ifTrue: hydra.ext.java.syntax.Expression,
   ifFalse: hydra.ext.java.syntax.LambdaExpression)

type ConditionalOrExpression = Seq[hydra.ext.java.syntax.ConditionalAndExpression]

type ConditionalAndExpression = Seq[hydra.ext.java.syntax.InclusiveOrExpression]

type InclusiveOrExpression = Seq[hydra.ext.java.syntax.ExclusiveOrExpression]

type ExclusiveOrExpression = Seq[hydra.ext.java.syntax.AndExpression]

type AndExpression = Seq[hydra.ext.java.syntax.EqualityExpression]

enum EqualityExpression :
   case unary(value: hydra.ext.java.syntax.RelationalExpression) extends EqualityExpression
   case equal(value: hydra.ext.java.syntax.EqualityExpression_Binary) extends EqualityExpression
   case notEqual(value: hydra.ext.java.syntax.EqualityExpression_Binary) extends EqualityExpression

case class EqualityExpression_Binary(lhs: hydra.ext.java.syntax.EqualityExpression, rhs: hydra.ext.java.syntax.RelationalExpression)

enum RelationalExpression :
   case simple(value: hydra.ext.java.syntax.ShiftExpression) extends RelationalExpression
   case lessThan(value: hydra.ext.java.syntax.RelationalExpression_LessThan) extends RelationalExpression
   case greaterThan(value: hydra.ext.java.syntax.RelationalExpression_GreaterThan) extends RelationalExpression
   case lessThanEqual(value: hydra.ext.java.syntax.RelationalExpression_LessThanEqual) extends RelationalExpression
   case greaterThanEqual(value: hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual) extends RelationalExpression
   case instanceof(value: hydra.ext.java.syntax.RelationalExpression_InstanceOf) extends RelationalExpression

case class RelationalExpression_LessThan(lhs: hydra.ext.java.syntax.RelationalExpression, rhs: hydra.ext.java.syntax.ShiftExpression)

case class RelationalExpression_GreaterThan(lhs: hydra.ext.java.syntax.RelationalExpression, rhs: hydra.ext.java.syntax.ShiftExpression)

case class RelationalExpression_LessThanEqual(lhs: hydra.ext.java.syntax.RelationalExpression, rhs: hydra.ext.java.syntax.ShiftExpression)

case class RelationalExpression_GreaterThanEqual(lhs: hydra.ext.java.syntax.RelationalExpression, rhs: hydra.ext.java.syntax.ShiftExpression)

case class RelationalExpression_InstanceOf(lhs: hydra.ext.java.syntax.RelationalExpression, rhs: hydra.ext.java.syntax.ReferenceType)

enum ShiftExpression :
   case unary(value: hydra.ext.java.syntax.AdditiveExpression) extends ShiftExpression
   case shiftLeft(value: hydra.ext.java.syntax.ShiftExpression_Binary) extends ShiftExpression
   case shiftRight(value: hydra.ext.java.syntax.ShiftExpression_Binary) extends ShiftExpression
   case shiftRightZeroFill(value: hydra.ext.java.syntax.ShiftExpression_Binary) extends ShiftExpression

case class ShiftExpression_Binary(lhs: hydra.ext.java.syntax.ShiftExpression, rhs: hydra.ext.java.syntax.AdditiveExpression)

enum AdditiveExpression :
   case unary(value: hydra.ext.java.syntax.MultiplicativeExpression) extends AdditiveExpression
   case plus(value: hydra.ext.java.syntax.AdditiveExpression_Binary) extends AdditiveExpression
   case minus(value: hydra.ext.java.syntax.AdditiveExpression_Binary) extends AdditiveExpression

case class AdditiveExpression_Binary(lhs: hydra.ext.java.syntax.AdditiveExpression, rhs: hydra.ext.java.syntax.MultiplicativeExpression)

enum MultiplicativeExpression :
   case unary(value: hydra.ext.java.syntax.UnaryExpression) extends MultiplicativeExpression
   case times(value: hydra.ext.java.syntax.MultiplicativeExpression_Binary) extends MultiplicativeExpression
   case divide(value: hydra.ext.java.syntax.MultiplicativeExpression_Binary) extends MultiplicativeExpression
   case mod(value: hydra.ext.java.syntax.MultiplicativeExpression_Binary) extends MultiplicativeExpression

case class MultiplicativeExpression_Binary(lhs: hydra.ext.java.syntax.MultiplicativeExpression, rhs: hydra.ext.java.syntax.UnaryExpression)

enum UnaryExpression :
   case preIncrement(value: hydra.ext.java.syntax.PreIncrementExpression) extends UnaryExpression
   case preDecrement(value: hydra.ext.java.syntax.PreDecrementExpression) extends UnaryExpression
   case plus(value: hydra.ext.java.syntax.UnaryExpression) extends UnaryExpression
   case minus(value: hydra.ext.java.syntax.UnaryExpression) extends UnaryExpression
   case other(value: hydra.ext.java.syntax.UnaryExpressionNotPlusMinus) extends UnaryExpression

type PreIncrementExpression = hydra.ext.java.syntax.UnaryExpression

type PreDecrementExpression = hydra.ext.java.syntax.UnaryExpression

enum UnaryExpressionNotPlusMinus :
   case postfix(value: hydra.ext.java.syntax.PostfixExpression) extends UnaryExpressionNotPlusMinus
   case tilde(value: hydra.ext.java.syntax.UnaryExpression) extends UnaryExpressionNotPlusMinus
   case not(value: hydra.ext.java.syntax.UnaryExpression) extends UnaryExpressionNotPlusMinus
   case cast(value: hydra.ext.java.syntax.CastExpression) extends UnaryExpressionNotPlusMinus

enum PostfixExpression :
   case primary(value: hydra.ext.java.syntax.Primary) extends PostfixExpression
   case name(value: hydra.ext.java.syntax.ExpressionName) extends PostfixExpression
   case postIncrement(value: hydra.ext.java.syntax.PostIncrementExpression) extends PostfixExpression
   case postDecrement(value: hydra.ext.java.syntax.PostDecrementExpression) extends PostfixExpression

type PostIncrementExpression = hydra.ext.java.syntax.PostfixExpression

type PostDecrementExpression = hydra.ext.java.syntax.PostfixExpression

enum CastExpression :
   case primitive(value: hydra.ext.java.syntax.CastExpression_Primitive) extends CastExpression
   case notPlusMinus(value: hydra.ext.java.syntax.CastExpression_NotPlusMinus) extends CastExpression
   case lambda(value: hydra.ext.java.syntax.CastExpression_Lambda) extends CastExpression

case class CastExpression_Primitive(`type`: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations, expression: hydra.ext.java.syntax.UnaryExpression)

case class CastExpression_NotPlusMinus(refAndBounds: hydra.ext.java.syntax.CastExpression_RefAndBounds, expression: hydra.ext.java.syntax.UnaryExpression)

case class CastExpression_Lambda(refAndBounds: hydra.ext.java.syntax.CastExpression_RefAndBounds, expression: hydra.ext.java.syntax.LambdaExpression)

case class CastExpression_RefAndBounds(`type`: hydra.ext.java.syntax.ReferenceType, bounds: Seq[hydra.ext.java.syntax.AdditionalBound])

type ConstantExpression = hydra.ext.java.syntax.Expression
