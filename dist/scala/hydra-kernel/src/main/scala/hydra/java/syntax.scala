package hydra.java.syntax

import hydra.core.*

type Identifier = scala.Predef.String

type TypeIdentifier = hydra.java.syntax.Identifier

enum Literal :
   case `null` extends Literal
   case integer(value: hydra.java.syntax.IntegerLiteral) extends Literal
   case floatingPoint(value: hydra.java.syntax.FloatingPointLiteral) extends Literal
   case boolean(value: Boolean) extends Literal
   case character(value: Int) extends Literal
   case string(value: hydra.java.syntax.StringLiteral) extends Literal

type IntegerLiteral = BigInt

type FloatingPointLiteral = BigDecimal

type StringLiteral = scala.Predef.String

enum Type :
   case primitive(value: hydra.java.syntax.PrimitiveTypeWithAnnotations) extends Type
   case reference(value: hydra.java.syntax.ReferenceType) extends Type

case class PrimitiveTypeWithAnnotations(`type`: hydra.java.syntax.PrimitiveType, annotations: Seq[hydra.java.syntax.Annotation])

enum PrimitiveType :
   case numeric(value: hydra.java.syntax.NumericType) extends PrimitiveType
   case boolean extends PrimitiveType

enum NumericType :
   case integral(value: hydra.java.syntax.IntegralType) extends NumericType
   case floatingPoint(value: hydra.java.syntax.FloatingPointType) extends NumericType

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
   case classOrInterface(value: hydra.java.syntax.ClassOrInterfaceType) extends ReferenceType
   case variable(value: hydra.java.syntax.TypeVariable) extends ReferenceType
   case array(value: hydra.java.syntax.ArrayType) extends ReferenceType

enum ClassOrInterfaceType :
   case `class`(value: hydra.java.syntax.ClassType) extends ClassOrInterfaceType
   case interface(value: hydra.java.syntax.InterfaceType) extends ClassOrInterfaceType

case class ClassType(annotations: Seq[hydra.java.syntax.Annotation], qualifier: hydra.java.syntax.ClassTypeQualifier,
   identifier: hydra.java.syntax.TypeIdentifier, arguments: Seq[hydra.java.syntax.TypeArgument])

enum ClassTypeQualifier :
   case none extends ClassTypeQualifier
   case `package`(value: hydra.java.syntax.PackageName) extends ClassTypeQualifier
   case parent(value: hydra.java.syntax.ClassOrInterfaceType) extends ClassTypeQualifier

type InterfaceType = hydra.java.syntax.ClassType

case class TypeVariable(annotations: Seq[hydra.java.syntax.Annotation], identifier: hydra.java.syntax.TypeIdentifier)

case class ArrayType(dims: hydra.java.syntax.Dims, variant: hydra.java.syntax.ArrayType_Variant)

enum ArrayType_Variant :
   case primitive(value: hydra.java.syntax.PrimitiveTypeWithAnnotations) extends ArrayType_Variant
   case classOrInterface(value: hydra.java.syntax.ClassOrInterfaceType) extends ArrayType_Variant
   case variable(value: hydra.java.syntax.TypeVariable) extends ArrayType_Variant

type Dims = Seq[Seq[hydra.java.syntax.Annotation]]

case class TypeParameter(modifiers: Seq[hydra.java.syntax.TypeParameterModifier], identifier: hydra.java.syntax.TypeIdentifier,
   bound: Option[hydra.java.syntax.TypeBound])

type TypeParameterModifier = hydra.java.syntax.Annotation

enum TypeBound :
   case variable(value: hydra.java.syntax.TypeVariable) extends TypeBound
   case classOrInterface(value: hydra.java.syntax.TypeBound_ClassOrInterface) extends TypeBound

case class TypeBound_ClassOrInterface(`type`: hydra.java.syntax.ClassOrInterfaceType, additional: Seq[hydra.java.syntax.AdditionalBound])

type AdditionalBound = hydra.java.syntax.InterfaceType

enum TypeArgument :
   case reference(value: hydra.java.syntax.ReferenceType) extends TypeArgument
   case wildcard(value: hydra.java.syntax.Wildcard) extends TypeArgument

case class Wildcard(annotations: Seq[hydra.java.syntax.Annotation], wildcard: Option[hydra.java.syntax.WildcardBounds])

enum WildcardBounds :
   case `extends`(value: hydra.java.syntax.ReferenceType) extends WildcardBounds
   case `super`(value: hydra.java.syntax.ReferenceType) extends WildcardBounds

case class ModuleName(identifier: hydra.java.syntax.Identifier, name: Option[hydra.java.syntax.ModuleName])

type PackageName = Seq[hydra.java.syntax.Identifier]

case class TypeName(identifier: hydra.java.syntax.TypeIdentifier, qualifier: Option[hydra.java.syntax.PackageOrTypeName])

case class ExpressionName(qualifier: Option[hydra.java.syntax.AmbiguousName], identifier: hydra.java.syntax.Identifier)

type MethodName = hydra.java.syntax.Identifier

type PackageOrTypeName = Seq[hydra.java.syntax.Identifier]

type AmbiguousName = Seq[hydra.java.syntax.Identifier]

enum CompilationUnit :
   case ordinary(value: hydra.java.syntax.OrdinaryCompilationUnit) extends CompilationUnit
   case modular(value: hydra.java.syntax.ModularCompilationUnit) extends CompilationUnit

case class OrdinaryCompilationUnit(`package`: Option[hydra.java.syntax.PackageDeclaration], imports: Seq[hydra.java.syntax.ImportDeclaration],
   types: Seq[hydra.java.syntax.TypeDeclarationWithComments])

case class ModularCompilationUnit(imports: Seq[hydra.java.syntax.ImportDeclaration], module: hydra.java.syntax.ModuleDeclaration)

case class PackageDeclaration(modifiers: Seq[hydra.java.syntax.PackageModifier], identifiers: Seq[hydra.java.syntax.Identifier])

type PackageModifier = hydra.java.syntax.Annotation

enum ImportDeclaration :
   case singleType(value: hydra.java.syntax.SingleTypeImportDeclaration) extends ImportDeclaration
   case typeImportOnDemand(value: hydra.java.syntax.TypeImportOnDemandDeclaration) extends ImportDeclaration
   case singleStaticImport(value: hydra.java.syntax.SingleStaticImportDeclaration) extends ImportDeclaration
   case staticImportOnDemand(value: hydra.java.syntax.StaticImportOnDemandDeclaration) extends ImportDeclaration

type SingleTypeImportDeclaration = hydra.java.syntax.TypeName

type TypeImportOnDemandDeclaration = hydra.java.syntax.PackageOrTypeName

case class SingleStaticImportDeclaration(typeName: hydra.java.syntax.TypeName, identifier: hydra.java.syntax.Identifier)

type StaticImportOnDemandDeclaration = hydra.java.syntax.TypeName

enum TypeDeclaration :
   case `class`(value: hydra.java.syntax.ClassDeclaration) extends TypeDeclaration
   case interface(value: hydra.java.syntax.InterfaceDeclaration) extends TypeDeclaration
   case none extends TypeDeclaration

case class TypeDeclarationWithComments(value: hydra.java.syntax.TypeDeclaration, comments: Option[scala.Predef.String])

case class ModuleDeclaration(annotations: Seq[hydra.java.syntax.Annotation], open: Boolean, identifiers: Seq[hydra.java.syntax.Identifier],
   directives: Seq[Seq[hydra.java.syntax.ModuleDirective]])

enum ModuleDirective :
   case requires(value: hydra.java.syntax.ModuleDirective_Requires) extends ModuleDirective
   case exports(value: hydra.java.syntax.ModuleDirective_ExportsOrOpens) extends ModuleDirective
   case opens(value: hydra.java.syntax.ModuleDirective_ExportsOrOpens) extends ModuleDirective
   case uses(value: hydra.java.syntax.TypeName) extends ModuleDirective
   case provides(value: hydra.java.syntax.ModuleDirective_Provides) extends ModuleDirective

case class ModuleDirective_Requires(modifiers: Seq[hydra.java.syntax.RequiresModifier], module: hydra.java.syntax.ModuleName)

case class ModuleDirective_ExportsOrOpens(`package`: hydra.java.syntax.PackageName, modules: Seq[hydra.java.syntax.ModuleName])

case class ModuleDirective_Provides(to: hydra.java.syntax.TypeName, `with`: Seq[hydra.java.syntax.TypeName])

enum RequiresModifier :
   case transitive extends RequiresModifier
   case static extends RequiresModifier

enum ClassDeclaration :
   case normal(value: hydra.java.syntax.NormalClassDeclaration) extends ClassDeclaration
   case `enum`(value: hydra.java.syntax.EnumDeclaration) extends ClassDeclaration

case class NormalClassDeclaration(modifiers: Seq[hydra.java.syntax.ClassModifier], identifier: hydra.java.syntax.TypeIdentifier,
   parameters: Seq[hydra.java.syntax.TypeParameter], `extends`: Option[hydra.java.syntax.ClassType], implements: Seq[hydra.java.syntax.InterfaceType],
   body: hydra.java.syntax.ClassBody)

enum ClassModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends ClassModifier
   case public extends ClassModifier
   case `protected` extends ClassModifier
   case `private` extends ClassModifier
   case `abstract` extends ClassModifier
   case static extends ClassModifier
   case `final` extends ClassModifier
   case strictfp extends ClassModifier

type ClassBody = Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]

enum ClassBodyDeclaration :
   case classMember(value: hydra.java.syntax.ClassMemberDeclaration) extends ClassBodyDeclaration
   case instanceInitializer(value: hydra.java.syntax.InstanceInitializer) extends ClassBodyDeclaration
   case staticInitializer(value: hydra.java.syntax.StaticInitializer) extends ClassBodyDeclaration
   case constructorDeclaration(value: hydra.java.syntax.ConstructorDeclaration) extends ClassBodyDeclaration

case class ClassBodyDeclarationWithComments(value: hydra.java.syntax.ClassBodyDeclaration, comments: Option[scala.Predef.String])

enum ClassMemberDeclaration :
   case field(value: hydra.java.syntax.FieldDeclaration) extends ClassMemberDeclaration
   case method(value: hydra.java.syntax.MethodDeclaration) extends ClassMemberDeclaration
   case `class`(value: hydra.java.syntax.ClassDeclaration) extends ClassMemberDeclaration
   case interface(value: hydra.java.syntax.InterfaceDeclaration) extends ClassMemberDeclaration
   case none extends ClassMemberDeclaration

case class FieldDeclaration(modifiers: Seq[hydra.java.syntax.FieldModifier], unannType: hydra.java.syntax.UnannType,
   variableDeclarators: Seq[hydra.java.syntax.VariableDeclarator])

enum FieldModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends FieldModifier
   case public extends FieldModifier
   case `protected` extends FieldModifier
   case `private` extends FieldModifier
   case static extends FieldModifier
   case `final` extends FieldModifier
   case transient extends FieldModifier
   case volatile extends FieldModifier

case class VariableDeclarator(id: hydra.java.syntax.VariableDeclaratorId, initializer: Option[hydra.java.syntax.VariableInitializer])

case class VariableDeclaratorId(identifier: hydra.java.syntax.Identifier, dims: Option[hydra.java.syntax.Dims])

enum VariableInitializer :
   case expression(value: hydra.java.syntax.Expression) extends VariableInitializer
   case arrayInitializer(value: hydra.java.syntax.ArrayInitializer) extends VariableInitializer

type UnannType = hydra.java.syntax.Type

type UnannClassType = hydra.java.syntax.ClassType

case class MethodDeclaration(annotations: Seq[hydra.java.syntax.Annotation], modifiers: Seq[hydra.java.syntax.MethodModifier],
   header: hydra.java.syntax.MethodHeader, body: hydra.java.syntax.MethodBody)

enum MethodModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends MethodModifier
   case public extends MethodModifier
   case `protected` extends MethodModifier
   case `private` extends MethodModifier
   case `abstract` extends MethodModifier
   case static extends MethodModifier
   case `final` extends MethodModifier
   case synchronized extends MethodModifier
   case native extends MethodModifier
   case strictfb extends MethodModifier

case class MethodHeader(parameters: Seq[hydra.java.syntax.TypeParameter], result: hydra.java.syntax.Result,
   declarator: hydra.java.syntax.MethodDeclarator, throws: Option[hydra.java.syntax.Throws])

enum Result :
   case `type`(value: hydra.java.syntax.UnannType) extends Result
   case void extends Result

case class MethodDeclarator(identifier: hydra.java.syntax.Identifier, receiverParameter: Option[hydra.java.syntax.ReceiverParameter],
   formalParameters: Seq[hydra.java.syntax.FormalParameter])

case class ReceiverParameter(annotations: Seq[hydra.java.syntax.Annotation], unannType: hydra.java.syntax.UnannType,
   identifier: Option[hydra.java.syntax.Identifier])

enum FormalParameter :
   case simple(value: hydra.java.syntax.FormalParameter_Simple) extends FormalParameter
   case variableArity(value: hydra.java.syntax.VariableArityParameter) extends FormalParameter

case class FormalParameter_Simple(modifiers: Seq[hydra.java.syntax.VariableModifier], `type`: hydra.java.syntax.UnannType,
   id: hydra.java.syntax.VariableDeclaratorId)

case class VariableArityParameter(modifiers: hydra.java.syntax.VariableModifier, `type`: hydra.java.syntax.UnannType,
   annotations: Seq[hydra.java.syntax.Annotation], identifier: hydra.java.syntax.Identifier)

enum VariableModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends VariableModifier
   case `final` extends VariableModifier

type Throws = Seq[hydra.java.syntax.ExceptionType]

enum ExceptionType :
   case `class`(value: hydra.java.syntax.ClassType) extends ExceptionType
   case variable(value: hydra.java.syntax.TypeVariable) extends ExceptionType

enum MethodBody :
   case block(value: hydra.java.syntax.Block) extends MethodBody
   case none extends MethodBody

type InstanceInitializer = hydra.java.syntax.Block

type StaticInitializer = hydra.java.syntax.Block

case class ConstructorDeclaration(modifiers: Seq[hydra.java.syntax.ConstructorModifier], constructor: hydra.java.syntax.ConstructorDeclarator,
   throws: Option[hydra.java.syntax.Throws], body: hydra.java.syntax.ConstructorBody)

enum ConstructorModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends ConstructorModifier
   case public extends ConstructorModifier
   case `protected` extends ConstructorModifier
   case `private` extends ConstructorModifier

case class ConstructorDeclarator(parameters: Seq[hydra.java.syntax.TypeParameter], name: hydra.java.syntax.SimpleTypeName,
   receiverParameter: Option[hydra.java.syntax.ReceiverParameter], formalParameters: Seq[hydra.java.syntax.FormalParameter])

type SimpleTypeName = hydra.java.syntax.TypeIdentifier

case class ConstructorBody(invocation: Option[hydra.java.syntax.ExplicitConstructorInvocation], statements: Seq[hydra.java.syntax.BlockStatement])

case class ExplicitConstructorInvocation(typeArguments: Seq[hydra.java.syntax.TypeArgument], arguments: Seq[hydra.java.syntax.Expression],
   variant: hydra.java.syntax.ExplicitConstructorInvocation_Variant)

enum ExplicitConstructorInvocation_Variant :
   case `this` extends ExplicitConstructorInvocation_Variant
   case `super`(value: Option[hydra.java.syntax.ExpressionName]) extends ExplicitConstructorInvocation_Variant
   case primary(value: hydra.java.syntax.Primary) extends ExplicitConstructorInvocation_Variant

case class EnumDeclaration(modifiers: Seq[hydra.java.syntax.ClassModifier], identifier: hydra.java.syntax.TypeIdentifier,
   implements: Seq[hydra.java.syntax.InterfaceType], body: hydra.java.syntax.EnumBody)

type EnumBody = Seq[hydra.java.syntax.EnumBody_Element]

case class EnumBody_Element(constants: Seq[hydra.java.syntax.EnumConstant], bodyDeclarations: Seq[hydra.java.syntax.ClassBodyDeclaration])

case class EnumConstant(modifiers: Seq[hydra.java.syntax.EnumConstantModifier], identifier: hydra.java.syntax.Identifier,
   arguments: Seq[Seq[hydra.java.syntax.Expression]], body: Option[hydra.java.syntax.ClassBody])

type EnumConstantModifier = hydra.java.syntax.Annotation

enum InterfaceDeclaration :
   case normalInterface(value: hydra.java.syntax.NormalInterfaceDeclaration) extends InterfaceDeclaration
   case annotationType(value: hydra.java.syntax.AnnotationTypeDeclaration) extends InterfaceDeclaration

case class NormalInterfaceDeclaration(modifiers: Seq[hydra.java.syntax.InterfaceModifier], identifier: hydra.java.syntax.TypeIdentifier,
   parameters: Seq[hydra.java.syntax.TypeParameter], `extends`: Seq[hydra.java.syntax.InterfaceType],
   body: hydra.java.syntax.InterfaceBody)

enum InterfaceModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends InterfaceModifier
   case public extends InterfaceModifier
   case `protected` extends InterfaceModifier
   case `private` extends InterfaceModifier
   case `abstract` extends InterfaceModifier
   case static extends InterfaceModifier
   case strictfb extends InterfaceModifier

type InterfaceBody = Seq[hydra.java.syntax.InterfaceMemberDeclaration]

enum InterfaceMemberDeclaration :
   case constant(value: hydra.java.syntax.ConstantDeclaration) extends InterfaceMemberDeclaration
   case interfaceMethod(value: hydra.java.syntax.InterfaceMethodDeclaration) extends InterfaceMemberDeclaration
   case `class`(value: hydra.java.syntax.ClassDeclaration) extends InterfaceMemberDeclaration
   case interface(value: hydra.java.syntax.InterfaceDeclaration) extends InterfaceMemberDeclaration

case class ConstantDeclaration(modifiers: Seq[hydra.java.syntax.ConstantModifier], `type`: hydra.java.syntax.UnannType,
   variables: Seq[hydra.java.syntax.VariableDeclarator])

enum ConstantModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends ConstantModifier
   case public extends ConstantModifier
   case static extends ConstantModifier
   case `final` extends ConstantModifier

case class InterfaceMethodDeclaration(modifiers: Seq[hydra.java.syntax.InterfaceMethodModifier], header: hydra.java.syntax.MethodHeader,
   body: hydra.java.syntax.MethodBody)

enum InterfaceMethodModifier :
   case annotation(value: hydra.java.syntax.Annotation) extends InterfaceMethodModifier
   case public extends InterfaceMethodModifier
   case `private` extends InterfaceMethodModifier
   case `abstract` extends InterfaceMethodModifier
   case default extends InterfaceMethodModifier
   case static extends InterfaceMethodModifier
   case strictfp extends InterfaceMethodModifier

case class AnnotationTypeDeclaration(modifiers: Seq[hydra.java.syntax.InterfaceModifier], identifier: hydra.java.syntax.TypeIdentifier,
   body: hydra.java.syntax.AnnotationTypeBody)

type AnnotationTypeBody = Seq[Seq[hydra.java.syntax.AnnotationTypeMemberDeclaration]]

enum AnnotationTypeMemberDeclaration :
   case annotationType(value: hydra.java.syntax.AnnotationTypeElementDeclaration) extends AnnotationTypeMemberDeclaration
   case constant(value: hydra.java.syntax.ConstantDeclaration) extends AnnotationTypeMemberDeclaration
   case `class`(value: hydra.java.syntax.ClassDeclaration) extends AnnotationTypeMemberDeclaration
   case interface(value: hydra.java.syntax.InterfaceDeclaration) extends AnnotationTypeMemberDeclaration

case class AnnotationTypeElementDeclaration(modifiers: Seq[hydra.java.syntax.AnnotationTypeElementModifier],
   `type`: hydra.java.syntax.UnannType, identifier: hydra.java.syntax.Identifier, dims: Option[hydra.java.syntax.Dims],
   default: Option[hydra.java.syntax.DefaultValue])

enum AnnotationTypeElementModifier :
   case public(value: hydra.java.syntax.Annotation) extends AnnotationTypeElementModifier
   case `abstract` extends AnnotationTypeElementModifier

type DefaultValue = hydra.java.syntax.ElementValue

enum Annotation :
   case normal(value: hydra.java.syntax.NormalAnnotation) extends Annotation
   case marker(value: hydra.java.syntax.MarkerAnnotation) extends Annotation
   case singleElement(value: hydra.java.syntax.SingleElementAnnotation) extends Annotation

case class NormalAnnotation(typeName: hydra.java.syntax.TypeName, pairs: Seq[hydra.java.syntax.ElementValuePair])

case class ElementValuePair(key: hydra.java.syntax.Identifier, value: hydra.java.syntax.ElementValue)

enum ElementValue :
   case conditionalExpression(value: hydra.java.syntax.ConditionalExpression) extends ElementValue
   case elementValueArrayInitializer(value: hydra.java.syntax.ElementValueArrayInitializer) extends ElementValue
   case annotation(value: hydra.java.syntax.Annotation) extends ElementValue

type ElementValueArrayInitializer = Seq[hydra.java.syntax.ElementValue]

type MarkerAnnotation = hydra.java.syntax.TypeName

case class SingleElementAnnotation(name: hydra.java.syntax.TypeName, value: Option[hydra.java.syntax.ElementValue])

type ArrayInitializer = Seq[Seq[hydra.java.syntax.VariableInitializer]]

type Block = Seq[hydra.java.syntax.BlockStatement]

enum BlockStatement :
   case localVariableDeclaration(value: hydra.java.syntax.LocalVariableDeclarationStatement) extends BlockStatement
   case `class`(value: hydra.java.syntax.ClassDeclaration) extends BlockStatement
   case statement(value: hydra.java.syntax.Statement) extends BlockStatement

type LocalVariableDeclarationStatement = hydra.java.syntax.LocalVariableDeclaration

case class LocalVariableDeclaration(modifiers: Seq[hydra.java.syntax.VariableModifier], `type`: hydra.java.syntax.LocalVariableType,
   declarators: Seq[hydra.java.syntax.VariableDeclarator])

enum LocalVariableType :
   case `type`(value: hydra.java.syntax.UnannType) extends LocalVariableType
   case `var` extends LocalVariableType

enum Statement :
   case withoutTrailing(value: hydra.java.syntax.StatementWithoutTrailingSubstatement) extends Statement
   case labeled(value: hydra.java.syntax.LabeledStatement) extends Statement
   case ifThen(value: hydra.java.syntax.IfThenStatement) extends Statement
   case ifThenElse(value: hydra.java.syntax.IfThenElseStatement) extends Statement
   case `while`(value: hydra.java.syntax.WhileStatement) extends Statement
   case `for`(value: hydra.java.syntax.ForStatement) extends Statement

enum StatementNoShortIf :
   case withoutTrailing(value: hydra.java.syntax.StatementWithoutTrailingSubstatement) extends StatementNoShortIf
   case labeled(value: hydra.java.syntax.LabeledStatementNoShortIf) extends StatementNoShortIf
   case ifThenElse(value: hydra.java.syntax.IfThenElseStatementNoShortIf) extends StatementNoShortIf
   case `while`(value: hydra.java.syntax.WhileStatementNoShortIf) extends StatementNoShortIf
   case `for`(value: hydra.java.syntax.ForStatementNoShortIf) extends StatementNoShortIf

enum StatementWithoutTrailingSubstatement :
   case block(value: hydra.java.syntax.Block) extends StatementWithoutTrailingSubstatement
   case empty extends StatementWithoutTrailingSubstatement
   case expression(value: hydra.java.syntax.ExpressionStatement) extends StatementWithoutTrailingSubstatement
   case assert(value: hydra.java.syntax.AssertStatement) extends StatementWithoutTrailingSubstatement
   case switch(value: hydra.java.syntax.SwitchStatement) extends StatementWithoutTrailingSubstatement
   case `do`(value: hydra.java.syntax.DoStatement) extends StatementWithoutTrailingSubstatement
   case break(value: hydra.java.syntax.BreakStatement) extends StatementWithoutTrailingSubstatement
   case continue(value: hydra.java.syntax.ContinueStatement) extends StatementWithoutTrailingSubstatement
   case `return`(value: hydra.java.syntax.ReturnStatement) extends StatementWithoutTrailingSubstatement
   case synchronized(value: hydra.java.syntax.SynchronizedStatement) extends StatementWithoutTrailingSubstatement
   case `throw`(value: hydra.java.syntax.ThrowStatement) extends StatementWithoutTrailingSubstatement
   case `try`(value: hydra.java.syntax.TryStatement) extends StatementWithoutTrailingSubstatement

case class LabeledStatement(identifier: hydra.java.syntax.Identifier, statement: hydra.java.syntax.Statement)

case class LabeledStatementNoShortIf(identifier: hydra.java.syntax.Identifier, statement: hydra.java.syntax.StatementNoShortIf)

type ExpressionStatement = hydra.java.syntax.StatementExpression

enum StatementExpression :
   case assignment(value: hydra.java.syntax.Assignment) extends StatementExpression
   case preIncrement(value: hydra.java.syntax.PreIncrementExpression) extends StatementExpression
   case preDecrement(value: hydra.java.syntax.PreDecrementExpression) extends StatementExpression
   case postIncrement(value: hydra.java.syntax.PostIncrementExpression) extends StatementExpression
   case postDecrement(value: hydra.java.syntax.PostDecrementExpression) extends StatementExpression
   case methodInvocation(value: hydra.java.syntax.MethodInvocation) extends StatementExpression
   case classInstanceCreation(value: hydra.java.syntax.ClassInstanceCreationExpression) extends StatementExpression

case class IfThenStatement(expression: hydra.java.syntax.Expression, statement: hydra.java.syntax.Statement)

case class IfThenElseStatement(cond: Option[hydra.java.syntax.Expression], `then`: hydra.java.syntax.StatementNoShortIf, `else`: hydra.java.syntax.Statement)

case class IfThenElseStatementNoShortIf(cond: Option[hydra.java.syntax.Expression], `then`: hydra.java.syntax.StatementNoShortIf,
   `else`: hydra.java.syntax.StatementNoShortIf)

enum AssertStatement :
   case single(value: hydra.java.syntax.Expression) extends AssertStatement
   case pair(value: hydra.java.syntax.AssertStatement_Pair) extends AssertStatement

case class AssertStatement_Pair(first: hydra.java.syntax.Expression, second: hydra.java.syntax.Expression)

case class SwitchStatement(cond: hydra.java.syntax.Expression, block: hydra.java.syntax.SwitchBlock)

type SwitchBlock = Seq[hydra.java.syntax.SwitchBlock_Pair]

case class SwitchBlock_Pair(statements: Seq[hydra.java.syntax.SwitchBlockStatementGroup], labels: Seq[hydra.java.syntax.SwitchLabel])

case class SwitchBlockStatementGroup(labels: Seq[hydra.java.syntax.SwitchLabel], statements: Seq[hydra.java.syntax.BlockStatement])

enum SwitchLabel :
   case constant(value: hydra.java.syntax.ConstantExpression) extends SwitchLabel
   case enumConstant(value: hydra.java.syntax.EnumConstantName) extends SwitchLabel
   case default extends SwitchLabel

type EnumConstantName = hydra.java.syntax.Identifier

case class WhileStatement(cond: Option[hydra.java.syntax.Expression], body: hydra.java.syntax.Statement)

case class WhileStatementNoShortIf(cond: Option[hydra.java.syntax.Expression], body: hydra.java.syntax.StatementNoShortIf)

case class DoStatement(body: hydra.java.syntax.Statement, conde: Option[hydra.java.syntax.Expression])

enum ForStatement :
   case basic(value: hydra.java.syntax.BasicForStatement) extends ForStatement
   case enhanced(value: hydra.java.syntax.EnhancedForStatement) extends ForStatement

enum ForStatementNoShortIf :
   case basic(value: hydra.java.syntax.BasicForStatementNoShortIf) extends ForStatementNoShortIf
   case enhanced(value: hydra.java.syntax.EnhancedForStatementNoShortIf) extends ForStatementNoShortIf

case class BasicForStatement(cond: hydra.java.syntax.ForCond, body: hydra.java.syntax.Statement)

case class ForCond(init: Option[hydra.java.syntax.ForInit], cond: Option[hydra.java.syntax.Expression], update: Option[hydra.java.syntax.ForUpdate])

case class BasicForStatementNoShortIf(cond: hydra.java.syntax.ForCond, body: hydra.java.syntax.StatementNoShortIf)

enum ForInit :
   case statements(value: Seq[hydra.java.syntax.StatementExpression]) extends ForInit
   case localVariable(value: hydra.java.syntax.LocalVariableDeclaration) extends ForInit

type ForUpdate = Seq[hydra.java.syntax.StatementExpression]

case class EnhancedForStatement(cond: hydra.java.syntax.EnhancedForCond, body: hydra.java.syntax.Statement)

case class EnhancedForCond(modifiers: Seq[hydra.java.syntax.VariableModifier], `type`: hydra.java.syntax.LocalVariableType,
   id: hydra.java.syntax.VariableDeclaratorId, expression: hydra.java.syntax.Expression)

case class EnhancedForStatementNoShortIf(cond: hydra.java.syntax.EnhancedForCond, body: hydra.java.syntax.StatementNoShortIf)

type BreakStatement = Option[hydra.java.syntax.Identifier]

type ContinueStatement = Option[hydra.java.syntax.Identifier]

type ReturnStatement = Option[hydra.java.syntax.Expression]

type ThrowStatement = hydra.java.syntax.Expression

case class SynchronizedStatement(expression: hydra.java.syntax.Expression, block: hydra.java.syntax.Block)

enum TryStatement :
   case simple(value: hydra.java.syntax.TryStatement_Simple) extends TryStatement
   case withFinally(value: hydra.java.syntax.TryStatement_WithFinally) extends TryStatement
   case withResources(value: hydra.java.syntax.TryWithResourcesStatement) extends TryStatement

case class TryStatement_Simple(block: hydra.java.syntax.Block, catches: hydra.java.syntax.Catches)

case class TryStatement_WithFinally(block: hydra.java.syntax.Block, catches: Option[hydra.java.syntax.Catches], `finally`: hydra.java.syntax.Finally)

type Catches = Seq[hydra.java.syntax.CatchClause]

case class CatchClause(parameter: Option[hydra.java.syntax.CatchFormalParameter], block: hydra.java.syntax.Block)

case class CatchFormalParameter(modifiers: Seq[hydra.java.syntax.VariableModifier], `type`: hydra.java.syntax.CatchType,
   id: hydra.java.syntax.VariableDeclaratorId)

case class CatchType(`type`: hydra.java.syntax.UnannClassType, types: Seq[hydra.java.syntax.ClassType])

type Finally = hydra.java.syntax.Block

case class TryWithResourcesStatement(resourceSpecification: hydra.java.syntax.ResourceSpecification, block: hydra.java.syntax.Block,
   catches: Option[hydra.java.syntax.Catches], `finally`: Option[hydra.java.syntax.Finally])

type ResourceSpecification = Seq[hydra.java.syntax.Resource]

enum Resource :
   case local(value: hydra.java.syntax.Resource_Local) extends Resource
   case variable(value: hydra.java.syntax.VariableAccess) extends Resource

case class Resource_Local(modifiers: Seq[hydra.java.syntax.VariableModifier], `type`: hydra.java.syntax.LocalVariableType,
   identifier: hydra.java.syntax.Identifier, expression: hydra.java.syntax.Expression)

enum VariableAccess :
   case expressionName(value: hydra.java.syntax.ExpressionName) extends VariableAccess
   case fieldAccess(value: hydra.java.syntax.FieldAccess) extends VariableAccess

enum Primary :
   case noNewArray(value: hydra.java.syntax.PrimaryNoNewArrayExpression) extends Primary
   case arrayCreation(value: hydra.java.syntax.ArrayCreationExpression) extends Primary

enum PrimaryNoNewArrayExpression :
   case literal(value: hydra.java.syntax.Literal) extends PrimaryNoNewArrayExpression
   case classLiteral(value: hydra.java.syntax.ClassLiteral) extends PrimaryNoNewArrayExpression
   case `this` extends PrimaryNoNewArrayExpression
   case dotThis(value: hydra.java.syntax.TypeName) extends PrimaryNoNewArrayExpression
   case parens(value: hydra.java.syntax.Expression) extends PrimaryNoNewArrayExpression
   case classInstance(value: hydra.java.syntax.ClassInstanceCreationExpression) extends PrimaryNoNewArrayExpression
   case fieldAccess(value: hydra.java.syntax.FieldAccess) extends PrimaryNoNewArrayExpression
   case arrayAccess(value: hydra.java.syntax.ArrayAccess) extends PrimaryNoNewArrayExpression
   case methodInvocation(value: hydra.java.syntax.MethodInvocation) extends PrimaryNoNewArrayExpression
   case methodReference(value: hydra.java.syntax.MethodReference) extends PrimaryNoNewArrayExpression

enum ClassLiteral :
   case `type`(value: hydra.java.syntax.TypeNameArray) extends ClassLiteral
   case numericType(value: hydra.java.syntax.NumericTypeArray) extends ClassLiteral
   case boolean(value: hydra.java.syntax.BooleanArray) extends ClassLiteral
   case void extends ClassLiteral

enum TypeNameArray :
   case simple(value: hydra.java.syntax.TypeName) extends TypeNameArray
   case array(value: hydra.java.syntax.TypeNameArray) extends TypeNameArray

enum NumericTypeArray :
   case simple(value: hydra.java.syntax.NumericType) extends NumericTypeArray
   case array(value: hydra.java.syntax.NumericTypeArray) extends NumericTypeArray

enum BooleanArray :
   case simple extends BooleanArray
   case array(value: hydra.java.syntax.BooleanArray) extends BooleanArray

case class ClassInstanceCreationExpression(qualifier: Option[hydra.java.syntax.ClassInstanceCreationExpression_Qualifier],
   expression: hydra.java.syntax.UnqualifiedClassInstanceCreationExpression)

enum ClassInstanceCreationExpression_Qualifier :
   case expression(value: hydra.java.syntax.ExpressionName) extends ClassInstanceCreationExpression_Qualifier
   case primary(value: hydra.java.syntax.Primary) extends ClassInstanceCreationExpression_Qualifier

case class UnqualifiedClassInstanceCreationExpression(typeArguments: Seq[hydra.java.syntax.TypeArgument],
   classOrInterface: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate, arguments: Seq[hydra.java.syntax.Expression],
   body: Option[hydra.java.syntax.ClassBody])

case class ClassOrInterfaceTypeToInstantiate(identifiers: Seq[hydra.java.syntax.AnnotatedIdentifier],
   typeArguments: Option[hydra.java.syntax.TypeArgumentsOrDiamond])

case class AnnotatedIdentifier(annotations: Seq[hydra.java.syntax.Annotation], identifier: hydra.java.syntax.Identifier)

enum TypeArgumentsOrDiamond :
   case arguments(value: Seq[hydra.java.syntax.TypeArgument]) extends TypeArgumentsOrDiamond
   case diamond extends TypeArgumentsOrDiamond

case class FieldAccess(qualifier: hydra.java.syntax.FieldAccess_Qualifier, identifier: hydra.java.syntax.Identifier)

enum FieldAccess_Qualifier :
   case primary(value: hydra.java.syntax.Primary) extends FieldAccess_Qualifier
   case `super` extends FieldAccess_Qualifier
   case typed(value: hydra.java.syntax.TypeName) extends FieldAccess_Qualifier

case class ArrayAccess(expression: Option[hydra.java.syntax.Expression], variant: hydra.java.syntax.ArrayAccess_Variant)

enum ArrayAccess_Variant :
   case name(value: hydra.java.syntax.ExpressionName) extends ArrayAccess_Variant
   case primary(value: hydra.java.syntax.PrimaryNoNewArrayExpression) extends ArrayAccess_Variant

case class MethodInvocation(header: hydra.java.syntax.MethodInvocation_Header, arguments: Seq[hydra.java.syntax.Expression])

enum MethodInvocation_Header :
   case simple(value: hydra.java.syntax.MethodName) extends MethodInvocation_Header
   case complex(value: hydra.java.syntax.MethodInvocation_Complex) extends MethodInvocation_Header

case class MethodInvocation_Complex(variant: hydra.java.syntax.MethodInvocation_Variant, typeArguments: Seq[hydra.java.syntax.TypeArgument],
   identifier: hydra.java.syntax.Identifier)

enum MethodInvocation_Variant :
   case `type`(value: hydra.java.syntax.TypeName) extends MethodInvocation_Variant
   case expression(value: hydra.java.syntax.ExpressionName) extends MethodInvocation_Variant
   case primary(value: hydra.java.syntax.Primary) extends MethodInvocation_Variant
   case `super` extends MethodInvocation_Variant
   case typeSuper(value: hydra.java.syntax.TypeName) extends MethodInvocation_Variant

enum MethodReference :
   case expression(value: hydra.java.syntax.MethodReference_Expression) extends MethodReference
   case primary(value: hydra.java.syntax.MethodReference_Primary) extends MethodReference
   case referenceType(value: hydra.java.syntax.MethodReference_ReferenceType) extends MethodReference
   case `super`(value: hydra.java.syntax.MethodReference_Super) extends MethodReference
   case `new`(value: hydra.java.syntax.MethodReference_New) extends MethodReference
   case array(value: hydra.java.syntax.MethodReference_Array) extends MethodReference

case class MethodReference_Expression(name: hydra.java.syntax.ExpressionName, typeArguments: Seq[hydra.java.syntax.TypeArgument],
   identifier: hydra.java.syntax.Identifier)

case class MethodReference_Primary(primary: hydra.java.syntax.Primary, typeArguments: Seq[hydra.java.syntax.TypeArgument],
   identifier: hydra.java.syntax.Identifier)

case class MethodReference_ReferenceType(referenceType: hydra.java.syntax.ReferenceType, typeArguments: Seq[hydra.java.syntax.TypeArgument],
   identifier: hydra.java.syntax.Identifier)

case class MethodReference_Super(typeArguments: Seq[hydra.java.syntax.TypeArgument], identifier: hydra.java.syntax.Identifier, `super`: Boolean)

case class MethodReference_New(classType: hydra.java.syntax.ClassType, typeArguments: Seq[hydra.java.syntax.TypeArgument])

type MethodReference_Array = hydra.java.syntax.ArrayType

enum ArrayCreationExpression :
   case primitive(value: hydra.java.syntax.ArrayCreationExpression_Primitive) extends ArrayCreationExpression
   case classOrInterface(value: hydra.java.syntax.ArrayCreationExpression_ClassOrInterface) extends ArrayCreationExpression
   case primitiveArray(value: hydra.java.syntax.ArrayCreationExpression_PrimitiveArray) extends ArrayCreationExpression
   case classOrInterfaceArray(value: hydra.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray) extends ArrayCreationExpression

case class ArrayCreationExpression_Primitive(`type`: hydra.java.syntax.PrimitiveTypeWithAnnotations, dimExprs: Seq[hydra.java.syntax.DimExpr],
   dims: Option[hydra.java.syntax.Dims])

case class ArrayCreationExpression_ClassOrInterface(`type`: hydra.java.syntax.ClassOrInterfaceType, dimExprs: Seq[hydra.java.syntax.DimExpr],
   dims: Option[hydra.java.syntax.Dims])

case class ArrayCreationExpression_PrimitiveArray(`type`: hydra.java.syntax.PrimitiveTypeWithAnnotations,
   dims: Seq[hydra.java.syntax.Dims], array: hydra.java.syntax.ArrayInitializer)

case class ArrayCreationExpression_ClassOrInterfaceArray(`type`: hydra.java.syntax.ClassOrInterfaceType,
   dims: Seq[hydra.java.syntax.Dims], array: hydra.java.syntax.ArrayInitializer)

case class DimExpr(annotations: Seq[hydra.java.syntax.Annotation], expression: Option[hydra.java.syntax.Expression])

enum Expression :
   case lambda(value: hydra.java.syntax.LambdaExpression) extends Expression
   case assignment(value: hydra.java.syntax.AssignmentExpression) extends Expression

case class LambdaExpression(parameters: hydra.java.syntax.LambdaParameters, body: hydra.java.syntax.LambdaBody)

enum LambdaParameters :
   case tuple(value: Seq[hydra.java.syntax.LambdaParameters]) extends LambdaParameters
   case single(value: hydra.java.syntax.Identifier) extends LambdaParameters

enum LambdaParameter :
   case normal(value: hydra.java.syntax.LambdaParameter_Normal) extends LambdaParameter
   case variableArity(value: hydra.java.syntax.VariableArityParameter) extends LambdaParameter

case class LambdaParameter_Normal(modifiers: Seq[hydra.java.syntax.VariableModifier], `type`: hydra.java.syntax.LambdaParameterType,
   id: hydra.java.syntax.VariableDeclaratorId)

enum LambdaParameterType :
   case `type`(value: hydra.java.syntax.UnannType) extends LambdaParameterType
   case `var` extends LambdaParameterType

enum LambdaBody :
   case expression(value: hydra.java.syntax.Expression) extends LambdaBody
   case block(value: hydra.java.syntax.Block) extends LambdaBody

enum AssignmentExpression :
   case conditional(value: hydra.java.syntax.ConditionalExpression) extends AssignmentExpression
   case assignment(value: hydra.java.syntax.Assignment) extends AssignmentExpression

case class Assignment(lhs: hydra.java.syntax.LeftHandSide, op: hydra.java.syntax.AssignmentOperator, expression: hydra.java.syntax.Expression)

enum LeftHandSide :
   case expressionName(value: hydra.java.syntax.ExpressionName) extends LeftHandSide
   case fieldAccess(value: hydra.java.syntax.FieldAccess) extends LeftHandSide
   case arrayAccess(value: hydra.java.syntax.ArrayAccess) extends LeftHandSide

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
   case simple(value: hydra.java.syntax.ConditionalOrExpression) extends ConditionalExpression
   case ternaryCond(value: hydra.java.syntax.ConditionalExpression_TernaryCond) extends ConditionalExpression
   case ternaryLambda(value: hydra.java.syntax.ConditionalExpression_TernaryLambda) extends ConditionalExpression

case class ConditionalExpression_TernaryCond(cond: hydra.java.syntax.ConditionalOrExpression, ifTrue: hydra.java.syntax.Expression,
   ifFalse: hydra.java.syntax.ConditionalExpression)

case class ConditionalExpression_TernaryLambda(cond: hydra.java.syntax.ConditionalOrExpression, ifTrue: hydra.java.syntax.Expression,
   ifFalse: hydra.java.syntax.LambdaExpression)

type ConditionalOrExpression = Seq[hydra.java.syntax.ConditionalAndExpression]

type ConditionalAndExpression = Seq[hydra.java.syntax.InclusiveOrExpression]

type InclusiveOrExpression = Seq[hydra.java.syntax.ExclusiveOrExpression]

type ExclusiveOrExpression = Seq[hydra.java.syntax.AndExpression]

type AndExpression = Seq[hydra.java.syntax.EqualityExpression]

enum EqualityExpression :
   case unary(value: hydra.java.syntax.RelationalExpression) extends EqualityExpression
   case equal(value: hydra.java.syntax.EqualityExpression_Binary) extends EqualityExpression
   case notEqual(value: hydra.java.syntax.EqualityExpression_Binary) extends EqualityExpression

case class EqualityExpression_Binary(lhs: hydra.java.syntax.EqualityExpression, rhs: hydra.java.syntax.RelationalExpression)

enum RelationalExpression :
   case simple(value: hydra.java.syntax.ShiftExpression) extends RelationalExpression
   case lessThan(value: hydra.java.syntax.RelationalExpression_LessThan) extends RelationalExpression
   case greaterThan(value: hydra.java.syntax.RelationalExpression_GreaterThan) extends RelationalExpression
   case lessThanEqual(value: hydra.java.syntax.RelationalExpression_LessThanEqual) extends RelationalExpression
   case greaterThanEqual(value: hydra.java.syntax.RelationalExpression_GreaterThanEqual) extends RelationalExpression
   case instanceof(value: hydra.java.syntax.RelationalExpression_InstanceOf) extends RelationalExpression

case class RelationalExpression_LessThan(lhs: hydra.java.syntax.RelationalExpression, rhs: hydra.java.syntax.ShiftExpression)

case class RelationalExpression_GreaterThan(lhs: hydra.java.syntax.RelationalExpression, rhs: hydra.java.syntax.ShiftExpression)

case class RelationalExpression_LessThanEqual(lhs: hydra.java.syntax.RelationalExpression, rhs: hydra.java.syntax.ShiftExpression)

case class RelationalExpression_GreaterThanEqual(lhs: hydra.java.syntax.RelationalExpression, rhs: hydra.java.syntax.ShiftExpression)

case class RelationalExpression_InstanceOf(lhs: hydra.java.syntax.RelationalExpression, rhs: hydra.java.syntax.ReferenceType)

enum ShiftExpression :
   case unary(value: hydra.java.syntax.AdditiveExpression) extends ShiftExpression
   case shiftLeft(value: hydra.java.syntax.ShiftExpression_Binary) extends ShiftExpression
   case shiftRight(value: hydra.java.syntax.ShiftExpression_Binary) extends ShiftExpression
   case shiftRightZeroFill(value: hydra.java.syntax.ShiftExpression_Binary) extends ShiftExpression

case class ShiftExpression_Binary(lhs: hydra.java.syntax.ShiftExpression, rhs: hydra.java.syntax.AdditiveExpression)

enum AdditiveExpression :
   case unary(value: hydra.java.syntax.MultiplicativeExpression) extends AdditiveExpression
   case plus(value: hydra.java.syntax.AdditiveExpression_Binary) extends AdditiveExpression
   case minus(value: hydra.java.syntax.AdditiveExpression_Binary) extends AdditiveExpression

case class AdditiveExpression_Binary(lhs: hydra.java.syntax.AdditiveExpression, rhs: hydra.java.syntax.MultiplicativeExpression)

enum MultiplicativeExpression :
   case unary(value: hydra.java.syntax.UnaryExpression) extends MultiplicativeExpression
   case times(value: hydra.java.syntax.MultiplicativeExpression_Binary) extends MultiplicativeExpression
   case divide(value: hydra.java.syntax.MultiplicativeExpression_Binary) extends MultiplicativeExpression
   case mod(value: hydra.java.syntax.MultiplicativeExpression_Binary) extends MultiplicativeExpression

case class MultiplicativeExpression_Binary(lhs: hydra.java.syntax.MultiplicativeExpression, rhs: hydra.java.syntax.UnaryExpression)

enum UnaryExpression :
   case preIncrement(value: hydra.java.syntax.PreIncrementExpression) extends UnaryExpression
   case preDecrement(value: hydra.java.syntax.PreDecrementExpression) extends UnaryExpression
   case plus(value: hydra.java.syntax.UnaryExpression) extends UnaryExpression
   case minus(value: hydra.java.syntax.UnaryExpression) extends UnaryExpression
   case other(value: hydra.java.syntax.UnaryExpressionNotPlusMinus) extends UnaryExpression

type PreIncrementExpression = hydra.java.syntax.UnaryExpression

type PreDecrementExpression = hydra.java.syntax.UnaryExpression

enum UnaryExpressionNotPlusMinus :
   case postfix(value: hydra.java.syntax.PostfixExpression) extends UnaryExpressionNotPlusMinus
   case tilde(value: hydra.java.syntax.UnaryExpression) extends UnaryExpressionNotPlusMinus
   case not(value: hydra.java.syntax.UnaryExpression) extends UnaryExpressionNotPlusMinus
   case cast(value: hydra.java.syntax.CastExpression) extends UnaryExpressionNotPlusMinus

enum PostfixExpression :
   case primary(value: hydra.java.syntax.Primary) extends PostfixExpression
   case name(value: hydra.java.syntax.ExpressionName) extends PostfixExpression
   case postIncrement(value: hydra.java.syntax.PostIncrementExpression) extends PostfixExpression
   case postDecrement(value: hydra.java.syntax.PostDecrementExpression) extends PostfixExpression

type PostIncrementExpression = hydra.java.syntax.PostfixExpression

type PostDecrementExpression = hydra.java.syntax.PostfixExpression

enum CastExpression :
   case primitive(value: hydra.java.syntax.CastExpression_Primitive) extends CastExpression
   case notPlusMinus(value: hydra.java.syntax.CastExpression_NotPlusMinus) extends CastExpression
   case lambda(value: hydra.java.syntax.CastExpression_Lambda) extends CastExpression

case class CastExpression_Primitive(`type`: hydra.java.syntax.PrimitiveTypeWithAnnotations, expression: hydra.java.syntax.UnaryExpression)

case class CastExpression_NotPlusMinus(refAndBounds: hydra.java.syntax.CastExpression_RefAndBounds, expression: hydra.java.syntax.UnaryExpression)

case class CastExpression_Lambda(refAndBounds: hydra.java.syntax.CastExpression_RefAndBounds, expression: hydra.java.syntax.LambdaExpression)

case class CastExpression_RefAndBounds(`type`: hydra.java.syntax.ReferenceType, bounds: Seq[hydra.java.syntax.AdditionalBound])

type ConstantExpression = hydra.java.syntax.Expression
