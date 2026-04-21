// Note: this is an automatically generated file. Do not edit.

/**
 * A Java syntax module. Based on the Oracle Java SE 12 BNF:
 *   https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html
 * Note: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments.
 */



import * as Core from "../core.js";

export type Identifier = string & { readonly __brand: "Identifier" };

export type TypeIdentifier = Identifier & { readonly __brand: "TypeIdentifier" };

export type Literal =
  | { readonly tag: "null" }
  | { readonly tag: "integer"; readonly value: IntegerLiteral }
  | { readonly tag: "floatingPoint"; readonly value: FloatingPointLiteral }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "character"; readonly value: number }
  | { readonly tag: "string"; readonly value: StringLiteral };

export type IntegerLiteral = bigint & { readonly __brand: "IntegerLiteral" };

export type FloatingPointLiteral = number & { readonly __brand: "FloatingPointLiteral" };

export type StringLiteral = string & { readonly __brand: "StringLiteral" };

export type Type =
  | { readonly tag: "primitive"; readonly value: PrimitiveTypeWithAnnotations }
  | { readonly tag: "reference"; readonly value: ReferenceType };

export interface PrimitiveTypeWithAnnotations {
  readonly type: PrimitiveType;
  readonly annotations: ReadonlyArray<Annotation>;
}

export type PrimitiveType =
  | { readonly tag: "numeric"; readonly value: NumericType }
  | { readonly tag: "boolean" };

export type NumericType =
  | { readonly tag: "integral"; readonly value: IntegralType }
  | { readonly tag: "floatingPoint"; readonly value: FloatingPointType };

export type IntegralType =
  | { readonly tag: "byte" }
  | { readonly tag: "short" }
  | { readonly tag: "int" }
  | { readonly tag: "long" }
  | { readonly tag: "char" };

export type FloatingPointType =
  | { readonly tag: "float" }
  | { readonly tag: "double" };

export type ReferenceType =
  | { readonly tag: "classOrInterface"; readonly value: ClassOrInterfaceType }
  | { readonly tag: "variable"; readonly value: TypeVariable }
  | { readonly tag: "array"; readonly value: ArrayType };

export type ClassOrInterfaceType =
  | { readonly tag: "class"; readonly value: ClassType }
  | { readonly tag: "interface"; readonly value: InterfaceType };

export interface ClassType {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly qualifier: ClassTypeQualifier;
  readonly identifier: TypeIdentifier;
  readonly arguments: ReadonlyArray<TypeArgument>;
}

export type ClassTypeQualifier =
  | { readonly tag: "none" }
  | { readonly tag: "package"; readonly value: PackageName }
  | { readonly tag: "parent"; readonly value: ClassOrInterfaceType };

export type InterfaceType = ClassType & { readonly __brand: "InterfaceType" };

export interface TypeVariable {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly identifier: TypeIdentifier;
}

export interface ArrayType {
  readonly dims: Dims;
  readonly variant: ArrayType_Variant;
}

export type ArrayType_Variant =
  | { readonly tag: "primitive"; readonly value: PrimitiveTypeWithAnnotations }
  | { readonly tag: "classOrInterface"; readonly value: ClassOrInterfaceType }
  | { readonly tag: "variable"; readonly value: TypeVariable };

export type Dims = ReadonlyArray<ReadonlyArray<Annotation>> & { readonly __brand: "Dims" };

export interface TypeParameter {
  readonly modifiers: ReadonlyArray<TypeParameterModifier>;
  readonly identifier: TypeIdentifier;
  readonly bound: TypeBound | null;
}

export type TypeParameterModifier = Annotation & { readonly __brand: "TypeParameterModifier" };

export type TypeBound =
  | { readonly tag: "variable"; readonly value: TypeVariable }
  | { readonly tag: "classOrInterface"; readonly value: TypeBound_ClassOrInterface };

export interface TypeBound_ClassOrInterface {
  readonly type: ClassOrInterfaceType;
  readonly additional: ReadonlyArray<AdditionalBound>;
}

export type AdditionalBound = InterfaceType & { readonly __brand: "AdditionalBound" };

export type TypeArgument =
  | { readonly tag: "reference"; readonly value: ReferenceType }
  | { readonly tag: "wildcard"; readonly value: Wildcard };

export interface Wildcard {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly wildcard: WildcardBounds | null;
}

export type WildcardBounds =
  | { readonly tag: "extends"; readonly value: ReferenceType }
  | { readonly tag: "super"; readonly value: ReferenceType };

export interface ModuleName {
  readonly identifier: Identifier;
  readonly name: ModuleName | null;
}

export type PackageName = ReadonlyArray<Identifier> & { readonly __brand: "PackageName" };

export interface TypeName {
  readonly identifier: TypeIdentifier;
  readonly qualifier: PackageOrTypeName | null;
}

export interface ExpressionName {
  readonly qualifier: AmbiguousName | null;
  readonly identifier: Identifier;
}

export type MethodName = Identifier & { readonly __brand: "MethodName" };

export type PackageOrTypeName = ReadonlyArray<Identifier> & { readonly __brand: "PackageOrTypeName" };

export type AmbiguousName = ReadonlyArray<Identifier> & { readonly __brand: "AmbiguousName" };

export type CompilationUnit =
  | { readonly tag: "ordinary"; readonly value: OrdinaryCompilationUnit }
  | { readonly tag: "modular"; readonly value: ModularCompilationUnit };

export interface OrdinaryCompilationUnit {
  readonly package: PackageDeclaration | null;
  readonly imports: ReadonlyArray<ImportDeclaration>;
  readonly types: ReadonlyArray<TypeDeclarationWithComments>;
}

export interface ModularCompilationUnit {
  readonly imports: ReadonlyArray<ImportDeclaration>;
  readonly module: ModuleDeclaration;
}

export interface PackageDeclaration {
  readonly modifiers: ReadonlyArray<PackageModifier>;
  readonly identifiers: ReadonlyArray<Identifier>;
}

export type PackageModifier = Annotation & { readonly __brand: "PackageModifier" };

export type ImportDeclaration =
  | { readonly tag: "singleType"; readonly value: SingleTypeImportDeclaration }
  | { readonly tag: "typeImportOnDemand"; readonly value: TypeImportOnDemandDeclaration }
  | { readonly tag: "singleStaticImport"; readonly value: SingleStaticImportDeclaration }
  | { readonly tag: "staticImportOnDemand"; readonly value: StaticImportOnDemandDeclaration };

export type SingleTypeImportDeclaration = TypeName & { readonly __brand: "SingleTypeImportDeclaration" };

export type TypeImportOnDemandDeclaration = PackageOrTypeName & { readonly __brand: "TypeImportOnDemandDeclaration" };

export interface SingleStaticImportDeclaration {
  readonly typeName: TypeName;
  readonly identifier: Identifier;
}

export type StaticImportOnDemandDeclaration = TypeName & { readonly __brand: "StaticImportOnDemandDeclaration" };

export type TypeDeclaration =
  | { readonly tag: "class"; readonly value: ClassDeclaration }
  | { readonly tag: "interface"; readonly value: InterfaceDeclaration }
  | { readonly tag: "none" };

export interface TypeDeclarationWithComments {
  readonly value: TypeDeclaration;
  readonly comments: string | null;
}

export interface ModuleDeclaration {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly open: boolean;
  readonly identifiers: ReadonlyArray<Identifier>;
  readonly directives: ReadonlyArray<ReadonlyArray<ModuleDirective>>;
}

export type ModuleDirective =
  | { readonly tag: "requires"; readonly value: ModuleDirective_Requires }
  | { readonly tag: "exports"; readonly value: ModuleDirective_ExportsOrOpens }
  | { readonly tag: "opens"; readonly value: ModuleDirective_ExportsOrOpens }
  | { readonly tag: "uses"; readonly value: TypeName }
  | { readonly tag: "provides"; readonly value: ModuleDirective_Provides };

export interface ModuleDirective_Requires {
  readonly modifiers: ReadonlyArray<RequiresModifier>;
  readonly module: ModuleName;
}

export interface ModuleDirective_ExportsOrOpens {
  readonly package: PackageName;
  readonly modules: ReadonlyArray<ModuleName>;
}

export interface ModuleDirective_Provides {
  readonly to: TypeName;
  readonly with: ReadonlyArray<TypeName>;
}

export type RequiresModifier =
  | { readonly tag: "transitive" }
  | { readonly tag: "static" };

export type ClassDeclaration =
  | { readonly tag: "normal"; readonly value: NormalClassDeclaration }
  | { readonly tag: "enum"; readonly value: EnumDeclaration };

export interface NormalClassDeclaration {
  readonly modifiers: ReadonlyArray<ClassModifier>;
  readonly identifier: TypeIdentifier;
  readonly parameters: ReadonlyArray<TypeParameter>;
  readonly extends: ClassType | null;
  readonly implements: ReadonlyArray<InterfaceType>;
  readonly body: ClassBody;
}

export type ClassModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "protected" }
  | { readonly tag: "private" }
  | { readonly tag: "abstract" }
  | { readonly tag: "static" }
  | { readonly tag: "final" }
  | { readonly tag: "strictfp" };

export type ClassBody = ReadonlyArray<ClassBodyDeclarationWithComments> & { readonly __brand: "ClassBody" };

export type ClassBodyDeclaration =
  | { readonly tag: "classMember"; readonly value: ClassMemberDeclaration }
  | { readonly tag: "instanceInitializer"; readonly value: InstanceInitializer }
  | { readonly tag: "staticInitializer"; readonly value: StaticInitializer }
  | { readonly tag: "constructorDeclaration"; readonly value: ConstructorDeclaration };

export interface ClassBodyDeclarationWithComments {
  readonly value: ClassBodyDeclaration;
  readonly comments: string | null;
}

export type ClassMemberDeclaration =
  | { readonly tag: "field"; readonly value: FieldDeclaration }
  | { readonly tag: "method"; readonly value: MethodDeclaration }
  | { readonly tag: "class"; readonly value: ClassDeclaration }
  | { readonly tag: "interface"; readonly value: InterfaceDeclaration }
  | { readonly tag: "none" };

export interface FieldDeclaration {
  readonly modifiers: ReadonlyArray<FieldModifier>;
  readonly unannType: UnannType;
  readonly variableDeclarators: ReadonlyArray<VariableDeclarator>;
}

export type FieldModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "protected" }
  | { readonly tag: "private" }
  | { readonly tag: "static" }
  | { readonly tag: "final" }
  | { readonly tag: "transient" }
  | { readonly tag: "volatile" };

export interface VariableDeclarator {
  readonly id: VariableDeclaratorId;
  readonly initializer: VariableInitializer | null;
}

export interface VariableDeclaratorId {
  readonly identifier: Identifier;
  readonly dims: Dims | null;
}

export type VariableInitializer =
  | { readonly tag: "expression"; readonly value: Expression }
  | { readonly tag: "arrayInitializer"; readonly value: ArrayInitializer };

export type UnannType = Type & { readonly __brand: "UnannType" };

export type UnannClassType = ClassType & { readonly __brand: "UnannClassType" };

export interface MethodDeclaration {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly modifiers: ReadonlyArray<MethodModifier>;
  readonly header: MethodHeader;
  readonly body: MethodBody;
}

export type MethodModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "protected" }
  | { readonly tag: "private" }
  | { readonly tag: "abstract" }
  | { readonly tag: "static" }
  | { readonly tag: "final" }
  | { readonly tag: "synchronized" }
  | { readonly tag: "native" }
  | { readonly tag: "strictfb" };

export interface MethodHeader {
  readonly parameters: ReadonlyArray<TypeParameter>;
  readonly result: Result;
  readonly declarator: MethodDeclarator;
  readonly throws: Throws | null;
}

export type Result =
  | { readonly tag: "type"; readonly value: UnannType }
  | { readonly tag: "void" };

export interface MethodDeclarator {
  readonly identifier: Identifier;
  readonly receiverParameter: ReceiverParameter | null;
  readonly formalParameters: ReadonlyArray<FormalParameter>;
}

export interface ReceiverParameter {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly unannType: UnannType;
  readonly identifier: Identifier | null;
}

export type FormalParameter =
  | { readonly tag: "simple"; readonly value: FormalParameter_Simple }
  | { readonly tag: "variableArity"; readonly value: VariableArityParameter };

export interface FormalParameter_Simple {
  readonly modifiers: ReadonlyArray<VariableModifier>;
  readonly type: UnannType;
  readonly id: VariableDeclaratorId;
}

export interface VariableArityParameter {
  readonly modifiers: VariableModifier;
  readonly type: UnannType;
  readonly annotations: ReadonlyArray<Annotation>;
  readonly identifier: Identifier;
}

export type VariableModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "final" };

export type Throws = ReadonlyArray<ExceptionType> & { readonly __brand: "Throws" };

export type ExceptionType =
  | { readonly tag: "class"; readonly value: ClassType }
  | { readonly tag: "variable"; readonly value: TypeVariable };

export type MethodBody =
  | { readonly tag: "block"; readonly value: Block }
  | { readonly tag: "none" };

export type InstanceInitializer = Block & { readonly __brand: "InstanceInitializer" };

export type StaticInitializer = Block & { readonly __brand: "StaticInitializer" };

export interface ConstructorDeclaration {
  readonly modifiers: ReadonlyArray<ConstructorModifier>;
  readonly constructor: ConstructorDeclarator;
  readonly throws: Throws | null;
  readonly body: ConstructorBody;
}

export type ConstructorModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "protected" }
  | { readonly tag: "private" };

export interface ConstructorDeclarator {
  readonly parameters: ReadonlyArray<TypeParameter>;
  readonly name: SimpleTypeName;
  readonly receiverParameter: ReceiverParameter | null;
  readonly formalParameters: ReadonlyArray<FormalParameter>;
}

export type SimpleTypeName = TypeIdentifier & { readonly __brand: "SimpleTypeName" };

export interface ConstructorBody {
  readonly invocation: ExplicitConstructorInvocation | null;
  readonly statements: ReadonlyArray<BlockStatement>;
}

export interface ExplicitConstructorInvocation {
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly arguments: ReadonlyArray<Expression>;
  readonly variant: ExplicitConstructorInvocation_Variant;
}

export type ExplicitConstructorInvocation_Variant =
  | { readonly tag: "this" }
  | { readonly tag: "super"; readonly value: ExpressionName | null }
  | { readonly tag: "primary"; readonly value: Primary };

export interface EnumDeclaration {
  readonly modifiers: ReadonlyArray<ClassModifier>;
  readonly identifier: TypeIdentifier;
  readonly implements: ReadonlyArray<InterfaceType>;
  readonly body: EnumBody;
}

export type EnumBody = ReadonlyArray<EnumBody_Element> & { readonly __brand: "EnumBody" };

export interface EnumBody_Element {
  readonly constants: ReadonlyArray<EnumConstant>;
  readonly bodyDeclarations: ReadonlyArray<ClassBodyDeclaration>;
}

export interface EnumConstant {
  readonly modifiers: ReadonlyArray<EnumConstantModifier>;
  readonly identifier: Identifier;
  readonly arguments: ReadonlyArray<ReadonlyArray<Expression>>;
  readonly body: ClassBody | null;
}

export type EnumConstantModifier = Annotation & { readonly __brand: "EnumConstantModifier" };

export type InterfaceDeclaration =
  | { readonly tag: "normalInterface"; readonly value: NormalInterfaceDeclaration }
  | { readonly tag: "annotationType"; readonly value: AnnotationTypeDeclaration };

export interface NormalInterfaceDeclaration {
  readonly modifiers: ReadonlyArray<InterfaceModifier>;
  readonly identifier: TypeIdentifier;
  readonly parameters: ReadonlyArray<TypeParameter>;
  readonly extends: ReadonlyArray<InterfaceType>;
  readonly body: InterfaceBody;
}

export type InterfaceModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "protected" }
  | { readonly tag: "private" }
  | { readonly tag: "abstract" }
  | { readonly tag: "static" }
  | { readonly tag: "strictfb" };

export type InterfaceBody = ReadonlyArray<InterfaceMemberDeclaration> & { readonly __brand: "InterfaceBody" };

export type InterfaceMemberDeclaration =
  | { readonly tag: "constant"; readonly value: ConstantDeclaration }
  | { readonly tag: "interfaceMethod"; readonly value: InterfaceMethodDeclaration }
  | { readonly tag: "class"; readonly value: ClassDeclaration }
  | { readonly tag: "interface"; readonly value: InterfaceDeclaration };

export interface ConstantDeclaration {
  readonly modifiers: ReadonlyArray<ConstantModifier>;
  readonly type: UnannType;
  readonly variables: ReadonlyArray<VariableDeclarator>;
}

export type ConstantModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "static" }
  | { readonly tag: "final" };

export interface InterfaceMethodDeclaration {
  readonly modifiers: ReadonlyArray<InterfaceMethodModifier>;
  readonly header: MethodHeader;
  readonly body: MethodBody;
}

export type InterfaceMethodModifier =
  | { readonly tag: "annotation"; readonly value: Annotation }
  | { readonly tag: "public" }
  | { readonly tag: "private" }
  | { readonly tag: "abstract" }
  | { readonly tag: "default" }
  | { readonly tag: "static" }
  | { readonly tag: "strictfp" };

export interface AnnotationTypeDeclaration {
  readonly modifiers: ReadonlyArray<InterfaceModifier>;
  readonly identifier: TypeIdentifier;
  readonly body: AnnotationTypeBody;
}

export type AnnotationTypeBody = ReadonlyArray<ReadonlyArray<AnnotationTypeMemberDeclaration>> & { readonly __brand: "AnnotationTypeBody" };

export type AnnotationTypeMemberDeclaration =
  | { readonly tag: "annotationType"; readonly value: AnnotationTypeElementDeclaration }
  | { readonly tag: "constant"; readonly value: ConstantDeclaration }
  | { readonly tag: "class"; readonly value: ClassDeclaration }
  | { readonly tag: "interface"; readonly value: InterfaceDeclaration };

export interface AnnotationTypeElementDeclaration {
  readonly modifiers: ReadonlyArray<AnnotationTypeElementModifier>;
  readonly type: UnannType;
  readonly identifier: Identifier;
  readonly dims: Dims | null;
  readonly default: DefaultValue | null;
}

export type AnnotationTypeElementModifier =
  | { readonly tag: "public"; readonly value: Annotation }
  | { readonly tag: "abstract" };

export type DefaultValue = ElementValue & { readonly __brand: "DefaultValue" };

export type Annotation =
  | { readonly tag: "normal"; readonly value: NormalAnnotation }
  | { readonly tag: "marker"; readonly value: MarkerAnnotation }
  | { readonly tag: "singleElement"; readonly value: SingleElementAnnotation };

export interface NormalAnnotation {
  readonly typeName: TypeName;
  readonly pairs: ReadonlyArray<ElementValuePair>;
}

export interface ElementValuePair {
  readonly key: Identifier;
  readonly value: ElementValue;
}

export type ElementValue =
  | { readonly tag: "conditionalExpression"; readonly value: ConditionalExpression }
  | { readonly tag: "elementValueArrayInitializer"; readonly value: ElementValueArrayInitializer }
  | { readonly tag: "annotation"; readonly value: Annotation };

export type ElementValueArrayInitializer = ReadonlyArray<ElementValue> & { readonly __brand: "ElementValueArrayInitializer" };

export type MarkerAnnotation = TypeName & { readonly __brand: "MarkerAnnotation" };

export interface SingleElementAnnotation {
  readonly name: TypeName;
  readonly value: ElementValue | null;
}

export type ArrayInitializer = ReadonlyArray<ReadonlyArray<VariableInitializer>> & { readonly __brand: "ArrayInitializer" };

export type Block = ReadonlyArray<BlockStatement> & { readonly __brand: "Block" };

export type BlockStatement =
  | { readonly tag: "localVariableDeclaration"; readonly value: LocalVariableDeclarationStatement }
  | { readonly tag: "class"; readonly value: ClassDeclaration }
  | { readonly tag: "statement"; readonly value: Statement };

export type LocalVariableDeclarationStatement = LocalVariableDeclaration & { readonly __brand: "LocalVariableDeclarationStatement" };

export interface LocalVariableDeclaration {
  readonly modifiers: ReadonlyArray<VariableModifier>;
  readonly type: LocalVariableType;
  readonly declarators: ReadonlyArray<VariableDeclarator>;
}

export type LocalVariableType =
  | { readonly tag: "type"; readonly value: UnannType }
  | { readonly tag: "var" };

export type Statement =
  | { readonly tag: "withoutTrailing"; readonly value: StatementWithoutTrailingSubstatement }
  | { readonly tag: "labeled"; readonly value: LabeledStatement }
  | { readonly tag: "ifThen"; readonly value: IfThenStatement }
  | { readonly tag: "ifThenElse"; readonly value: IfThenElseStatement }
  | { readonly tag: "while"; readonly value: WhileStatement }
  | { readonly tag: "for"; readonly value: ForStatement };

export type StatementNoShortIf =
  | { readonly tag: "withoutTrailing"; readonly value: StatementWithoutTrailingSubstatement }
  | { readonly tag: "labeled"; readonly value: LabeledStatementNoShortIf }
  | { readonly tag: "ifThenElse"; readonly value: IfThenElseStatementNoShortIf }
  | { readonly tag: "while"; readonly value: WhileStatementNoShortIf }
  | { readonly tag: "for"; readonly value: ForStatementNoShortIf };

export type StatementWithoutTrailingSubstatement =
  | { readonly tag: "block"; readonly value: Block }
  | { readonly tag: "empty" }
  | { readonly tag: "expression"; readonly value: ExpressionStatement }
  | { readonly tag: "assert"; readonly value: AssertStatement }
  | { readonly tag: "switch"; readonly value: SwitchStatement }
  | { readonly tag: "do"; readonly value: DoStatement }
  | { readonly tag: "break"; readonly value: BreakStatement }
  | { readonly tag: "continue"; readonly value: ContinueStatement }
  | { readonly tag: "return"; readonly value: ReturnStatement }
  | { readonly tag: "synchronized"; readonly value: SynchronizedStatement }
  | { readonly tag: "throw"; readonly value: ThrowStatement }
  | { readonly tag: "try"; readonly value: TryStatement };

export interface LabeledStatement {
  readonly identifier: Identifier;
  readonly statement: Statement;
}

export interface LabeledStatementNoShortIf {
  readonly identifier: Identifier;
  readonly statement: StatementNoShortIf;
}

export type ExpressionStatement = StatementExpression & { readonly __brand: "ExpressionStatement" };

export type StatementExpression =
  | { readonly tag: "assignment"; readonly value: Assignment }
  | { readonly tag: "preIncrement"; readonly value: PreIncrementExpression }
  | { readonly tag: "preDecrement"; readonly value: PreDecrementExpression }
  | { readonly tag: "postIncrement"; readonly value: PostIncrementExpression }
  | { readonly tag: "postDecrement"; readonly value: PostDecrementExpression }
  | { readonly tag: "methodInvocation"; readonly value: MethodInvocation }
  | { readonly tag: "classInstanceCreation"; readonly value: ClassInstanceCreationExpression };

export interface IfThenStatement {
  readonly expression: Expression;
  readonly statement: Statement;
}

export interface IfThenElseStatement {
  readonly cond: Expression | null;
  readonly then: StatementNoShortIf;
  readonly else: Statement;
}

export interface IfThenElseStatementNoShortIf {
  readonly cond: Expression | null;
  readonly then: StatementNoShortIf;
  readonly else: StatementNoShortIf;
}

export type AssertStatement =
  | { readonly tag: "single"; readonly value: Expression }
  | { readonly tag: "pair"; readonly value: AssertStatement_Pair };

export interface AssertStatement_Pair {
  readonly first: Expression;
  readonly second: Expression;
}

export interface SwitchStatement {
  readonly cond: Expression;
  readonly block: SwitchBlock;
}

export type SwitchBlock = ReadonlyArray<SwitchBlock_Pair> & { readonly __brand: "SwitchBlock" };

export interface SwitchBlock_Pair {
  readonly statements: ReadonlyArray<SwitchBlockStatementGroup>;
  readonly labels: ReadonlyArray<SwitchLabel>;
}

export interface SwitchBlockStatementGroup {
  readonly labels: ReadonlyArray<SwitchLabel>;
  readonly statements: ReadonlyArray<BlockStatement>;
}

export type SwitchLabel =
  | { readonly tag: "constant"; readonly value: ConstantExpression }
  | { readonly tag: "enumConstant"; readonly value: EnumConstantName }
  | { readonly tag: "default" };

export type EnumConstantName = Identifier & { readonly __brand: "EnumConstantName" };

export interface WhileStatement {
  readonly cond: Expression | null;
  readonly body: Statement;
}

export interface WhileStatementNoShortIf {
  readonly cond: Expression | null;
  readonly body: StatementNoShortIf;
}

export interface DoStatement {
  readonly body: Statement;
  readonly conde: Expression | null;
}

export type ForStatement =
  | { readonly tag: "basic"; readonly value: BasicForStatement }
  | { readonly tag: "enhanced"; readonly value: EnhancedForStatement };

export type ForStatementNoShortIf =
  | { readonly tag: "basic"; readonly value: BasicForStatementNoShortIf }
  | { readonly tag: "enhanced"; readonly value: EnhancedForStatementNoShortIf };

export interface BasicForStatement {
  readonly cond: ForCond;
  readonly body: Statement;
}

export interface ForCond {
  readonly init: ForInit | null;
  readonly cond: Expression | null;
  readonly update: ForUpdate | null;
}

export interface BasicForStatementNoShortIf {
  readonly cond: ForCond;
  readonly body: StatementNoShortIf;
}

export type ForInit =
  | { readonly tag: "statements"; readonly value: ReadonlyArray<StatementExpression> }
  | { readonly tag: "localVariable"; readonly value: LocalVariableDeclaration };

export type ForUpdate = ReadonlyArray<StatementExpression> & { readonly __brand: "ForUpdate" };

export interface EnhancedForStatement {
  readonly cond: EnhancedForCond;
  readonly body: Statement;
}

export interface EnhancedForCond {
  readonly modifiers: ReadonlyArray<VariableModifier>;
  readonly type: LocalVariableType;
  readonly id: VariableDeclaratorId;
  readonly expression: Expression;
}

export interface EnhancedForStatementNoShortIf {
  readonly cond: EnhancedForCond;
  readonly body: StatementNoShortIf;
}

export type BreakStatement = Identifier | null & { readonly __brand: "BreakStatement" };

export type ContinueStatement = Identifier | null & { readonly __brand: "ContinueStatement" };

export type ReturnStatement = Expression | null & { readonly __brand: "ReturnStatement" };

export type ThrowStatement = Expression & { readonly __brand: "ThrowStatement" };

export interface SynchronizedStatement {
  readonly expression: Expression;
  readonly block: Block;
}

export type TryStatement =
  | { readonly tag: "simple"; readonly value: TryStatement_Simple }
  | { readonly tag: "withFinally"; readonly value: TryStatement_WithFinally }
  | { readonly tag: "withResources"; readonly value: TryWithResourcesStatement };

export interface TryStatement_Simple {
  readonly block: Block;
  readonly catches: Catches;
}

export interface TryStatement_WithFinally {
  readonly block: Block;
  readonly catches: Catches | null;
  readonly finally: Finally;
}

export type Catches = ReadonlyArray<CatchClause> & { readonly __brand: "Catches" };

export interface CatchClause {
  readonly parameter: CatchFormalParameter | null;
  readonly block: Block;
}

export interface CatchFormalParameter {
  readonly modifiers: ReadonlyArray<VariableModifier>;
  readonly type: CatchType;
  readonly id: VariableDeclaratorId;
}

export interface CatchType {
  readonly type: UnannClassType;
  readonly types: ReadonlyArray<ClassType>;
}

export type Finally = Block & { readonly __brand: "Finally" };

export interface TryWithResourcesStatement {
  readonly resourceSpecification: ResourceSpecification;
  readonly block: Block;
  readonly catches: Catches | null;
  readonly finally: Finally | null;
}

export type ResourceSpecification = ReadonlyArray<Resource> & { readonly __brand: "ResourceSpecification" };

export type Resource =
  | { readonly tag: "local"; readonly value: Resource_Local }
  | { readonly tag: "variable"; readonly value: VariableAccess };

export interface Resource_Local {
  readonly modifiers: ReadonlyArray<VariableModifier>;
  readonly type: LocalVariableType;
  readonly identifier: Identifier;
  readonly expression: Expression;
}

export type VariableAccess =
  | { readonly tag: "expressionName"; readonly value: ExpressionName }
  | { readonly tag: "fieldAccess"; readonly value: FieldAccess };

export type Primary =
  | { readonly tag: "noNewArray"; readonly value: PrimaryNoNewArrayExpression }
  | { readonly tag: "arrayCreation"; readonly value: ArrayCreationExpression };

export type PrimaryNoNewArrayExpression =
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "classLiteral"; readonly value: ClassLiteral }
  | { readonly tag: "this" }
  | { readonly tag: "dotThis"; readonly value: TypeName }
  | { readonly tag: "parens"; readonly value: Expression }
  | { readonly tag: "classInstance"; readonly value: ClassInstanceCreationExpression }
  | { readonly tag: "fieldAccess"; readonly value: FieldAccess }
  | { readonly tag: "arrayAccess"; readonly value: ArrayAccess }
  | { readonly tag: "methodInvocation"; readonly value: MethodInvocation }
  | { readonly tag: "methodReference"; readonly value: MethodReference };

export type ClassLiteral =
  | { readonly tag: "type"; readonly value: TypeNameArray }
  | { readonly tag: "numericType"; readonly value: NumericTypeArray }
  | { readonly tag: "boolean"; readonly value: BooleanArray }
  | { readonly tag: "void" };

export type TypeNameArray =
  | { readonly tag: "simple"; readonly value: TypeName }
  | { readonly tag: "array"; readonly value: TypeNameArray };

export type NumericTypeArray =
  | { readonly tag: "simple"; readonly value: NumericType }
  | { readonly tag: "array"; readonly value: NumericTypeArray };

export type BooleanArray =
  | { readonly tag: "simple" }
  | { readonly tag: "array"; readonly value: BooleanArray };

export interface ClassInstanceCreationExpression {
  readonly qualifier: ClassInstanceCreationExpression_Qualifier | null;
  readonly expression: UnqualifiedClassInstanceCreationExpression;
}

export type ClassInstanceCreationExpression_Qualifier =
  | { readonly tag: "expression"; readonly value: ExpressionName }
  | { readonly tag: "primary"; readonly value: Primary };

export interface UnqualifiedClassInstanceCreationExpression {
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly classOrInterface: ClassOrInterfaceTypeToInstantiate;
  readonly arguments: ReadonlyArray<Expression>;
  readonly body: ClassBody | null;
}

export interface ClassOrInterfaceTypeToInstantiate {
  readonly identifiers: ReadonlyArray<AnnotatedIdentifier>;
  readonly typeArguments: TypeArgumentsOrDiamond | null;
}

export interface AnnotatedIdentifier {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly identifier: Identifier;
}

export type TypeArgumentsOrDiamond =
  | { readonly tag: "arguments"; readonly value: ReadonlyArray<TypeArgument> }
  | { readonly tag: "diamond" };

export interface FieldAccess {
  readonly qualifier: FieldAccess_Qualifier;
  readonly identifier: Identifier;
}

export type FieldAccess_Qualifier =
  | { readonly tag: "primary"; readonly value: Primary }
  | { readonly tag: "super" }
  | { readonly tag: "typed"; readonly value: TypeName };

export interface ArrayAccess {
  readonly expression: Expression | null;
  readonly variant: ArrayAccess_Variant;
}

export type ArrayAccess_Variant =
  | { readonly tag: "name"; readonly value: ExpressionName }
  | { readonly tag: "primary"; readonly value: PrimaryNoNewArrayExpression };

export interface MethodInvocation {
  readonly header: MethodInvocation_Header;
  readonly arguments: ReadonlyArray<Expression>;
}

export type MethodInvocation_Header =
  | { readonly tag: "simple"; readonly value: MethodName }
  | { readonly tag: "complex"; readonly value: MethodInvocation_Complex };

export interface MethodInvocation_Complex {
  readonly variant: MethodInvocation_Variant;
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly identifier: Identifier;
}

export type MethodInvocation_Variant =
  | { readonly tag: "type"; readonly value: TypeName }
  | { readonly tag: "expression"; readonly value: ExpressionName }
  | { readonly tag: "primary"; readonly value: Primary }
  | { readonly tag: "super" }
  | { readonly tag: "typeSuper"; readonly value: TypeName };

export type MethodReference =
  | { readonly tag: "expression"; readonly value: MethodReference_Expression }
  | { readonly tag: "primary"; readonly value: MethodReference_Primary }
  | { readonly tag: "referenceType"; readonly value: MethodReference_ReferenceType }
  | { readonly tag: "super"; readonly value: MethodReference_Super }
  | { readonly tag: "new"; readonly value: MethodReference_New }
  | { readonly tag: "array"; readonly value: MethodReference_Array };

export interface MethodReference_Expression {
  readonly name: ExpressionName;
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly identifier: Identifier;
}

export interface MethodReference_Primary {
  readonly primary: Primary;
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly identifier: Identifier;
}

export interface MethodReference_ReferenceType {
  readonly referenceType: ReferenceType;
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly identifier: Identifier;
}

export interface MethodReference_Super {
  readonly typeArguments: ReadonlyArray<TypeArgument>;
  readonly identifier: Identifier;
  readonly super: boolean;
}

export interface MethodReference_New {
  readonly classType: ClassType;
  readonly typeArguments: ReadonlyArray<TypeArgument>;
}

export type MethodReference_Array = ArrayType & { readonly __brand: "MethodReference_Array" };

export type ArrayCreationExpression =
  | { readonly tag: "primitive"; readonly value: ArrayCreationExpression_Primitive }
  | { readonly tag: "classOrInterface"; readonly value: ArrayCreationExpression_ClassOrInterface }
  | { readonly tag: "primitiveArray"; readonly value: ArrayCreationExpression_PrimitiveArray }
  | { readonly tag: "classOrInterfaceArray"; readonly value: ArrayCreationExpression_ClassOrInterfaceArray };

export interface ArrayCreationExpression_Primitive {
  readonly type: PrimitiveTypeWithAnnotations;
  readonly dimExprs: ReadonlyArray<DimExpr>;
  readonly dims: Dims | null;
}

export interface ArrayCreationExpression_ClassOrInterface {
  readonly type: ClassOrInterfaceType;
  readonly dimExprs: ReadonlyArray<DimExpr>;
  readonly dims: Dims | null;
}

export interface ArrayCreationExpression_PrimitiveArray {
  readonly type: PrimitiveTypeWithAnnotations;
  readonly dims: ReadonlyArray<Dims>;
  readonly array: ArrayInitializer;
}

export interface ArrayCreationExpression_ClassOrInterfaceArray {
  readonly type: ClassOrInterfaceType;
  readonly dims: ReadonlyArray<Dims>;
  readonly array: ArrayInitializer;
}

export interface DimExpr {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly expression: Expression | null;
}

export type Expression =
  | { readonly tag: "lambda"; readonly value: LambdaExpression }
  | { readonly tag: "assignment"; readonly value: AssignmentExpression };

export interface LambdaExpression {
  readonly parameters: LambdaParameters;
  readonly body: LambdaBody;
}

export type LambdaParameters =
  | { readonly tag: "tuple"; readonly value: ReadonlyArray<LambdaParameters> }
  | { readonly tag: "single"; readonly value: Identifier };

export type LambdaParameter =
  | { readonly tag: "normal"; readonly value: LambdaParameter_Normal }
  | { readonly tag: "variableArity"; readonly value: VariableArityParameter };

export interface LambdaParameter_Normal {
  readonly modifiers: ReadonlyArray<VariableModifier>;
  readonly type: LambdaParameterType;
  readonly id: VariableDeclaratorId;
}

export type LambdaParameterType =
  | { readonly tag: "type"; readonly value: UnannType }
  | { readonly tag: "var" };

export type LambdaBody =
  | { readonly tag: "expression"; readonly value: Expression }
  | { readonly tag: "block"; readonly value: Block };

export type AssignmentExpression =
  | { readonly tag: "conditional"; readonly value: ConditionalExpression }
  | { readonly tag: "assignment"; readonly value: Assignment };

export interface Assignment {
  readonly lhs: LeftHandSide;
  readonly op: AssignmentOperator;
  readonly expression: Expression;
}

export type LeftHandSide =
  | { readonly tag: "expressionName"; readonly value: ExpressionName }
  | { readonly tag: "fieldAccess"; readonly value: FieldAccess }
  | { readonly tag: "arrayAccess"; readonly value: ArrayAccess };

export type AssignmentOperator =
  | { readonly tag: "simple" }
  | { readonly tag: "times" }
  | { readonly tag: "div" }
  | { readonly tag: "mod" }
  | { readonly tag: "plus" }
  | { readonly tag: "minus" }
  | { readonly tag: "shiftLeft" }
  | { readonly tag: "shiftRight" }
  | { readonly tag: "shiftRightZeroFill" }
  | { readonly tag: "and" }
  | { readonly tag: "xor" }
  | { readonly tag: "or" };

export type ConditionalExpression =
  | { readonly tag: "simple"; readonly value: ConditionalOrExpression }
  | { readonly tag: "ternaryCond"; readonly value: ConditionalExpression_TernaryCond }
  | { readonly tag: "ternaryLambda"; readonly value: ConditionalExpression_TernaryLambda };

export interface ConditionalExpression_TernaryCond {
  readonly cond: ConditionalOrExpression;
  readonly ifTrue: Expression;
  readonly ifFalse: ConditionalExpression;
}

export interface ConditionalExpression_TernaryLambda {
  readonly cond: ConditionalOrExpression;
  readonly ifTrue: Expression;
  readonly ifFalse: LambdaExpression;
}

export type ConditionalOrExpression = ReadonlyArray<ConditionalAndExpression> & { readonly __brand: "ConditionalOrExpression" };

export type ConditionalAndExpression = ReadonlyArray<InclusiveOrExpression> & { readonly __brand: "ConditionalAndExpression" };

export type InclusiveOrExpression = ReadonlyArray<ExclusiveOrExpression> & { readonly __brand: "InclusiveOrExpression" };

export type ExclusiveOrExpression = ReadonlyArray<AndExpression> & { readonly __brand: "ExclusiveOrExpression" };

export type AndExpression = ReadonlyArray<EqualityExpression> & { readonly __brand: "AndExpression" };

export type EqualityExpression =
  | { readonly tag: "unary"; readonly value: RelationalExpression }
  | { readonly tag: "equal"; readonly value: EqualityExpression_Binary }
  | { readonly tag: "notEqual"; readonly value: EqualityExpression_Binary };

export interface EqualityExpression_Binary {
  readonly lhs: EqualityExpression;
  readonly rhs: RelationalExpression;
}

export type RelationalExpression =
  | { readonly tag: "simple"; readonly value: ShiftExpression }
  | { readonly tag: "lessThan"; readonly value: RelationalExpression_LessThan }
  | { readonly tag: "greaterThan"; readonly value: RelationalExpression_GreaterThan }
  | { readonly tag: "lessThanEqual"; readonly value: RelationalExpression_LessThanEqual }
  | { readonly tag: "greaterThanEqual"; readonly value: RelationalExpression_GreaterThanEqual }
  | { readonly tag: "instanceof"; readonly value: RelationalExpression_InstanceOf };

export interface RelationalExpression_LessThan {
  readonly lhs: RelationalExpression;
  readonly rhs: ShiftExpression;
}

export interface RelationalExpression_GreaterThan {
  readonly lhs: RelationalExpression;
  readonly rhs: ShiftExpression;
}

export interface RelationalExpression_LessThanEqual {
  readonly lhs: RelationalExpression;
  readonly rhs: ShiftExpression;
}

export interface RelationalExpression_GreaterThanEqual {
  readonly lhs: RelationalExpression;
  readonly rhs: ShiftExpression;
}

export interface RelationalExpression_InstanceOf {
  readonly lhs: RelationalExpression;
  readonly rhs: ReferenceType;
}

export type ShiftExpression =
  | { readonly tag: "unary"; readonly value: AdditiveExpression }
  | { readonly tag: "shiftLeft"; readonly value: ShiftExpression_Binary }
  | { readonly tag: "shiftRight"; readonly value: ShiftExpression_Binary }
  | { readonly tag: "shiftRightZeroFill"; readonly value: ShiftExpression_Binary };

export interface ShiftExpression_Binary {
  readonly lhs: ShiftExpression;
  readonly rhs: AdditiveExpression;
}

export type AdditiveExpression =
  | { readonly tag: "unary"; readonly value: MultiplicativeExpression }
  | { readonly tag: "plus"; readonly value: AdditiveExpression_Binary }
  | { readonly tag: "minus"; readonly value: AdditiveExpression_Binary };

export interface AdditiveExpression_Binary {
  readonly lhs: AdditiveExpression;
  readonly rhs: MultiplicativeExpression;
}

export type MultiplicativeExpression =
  | { readonly tag: "unary"; readonly value: UnaryExpression }
  | { readonly tag: "times"; readonly value: MultiplicativeExpression_Binary }
  | { readonly tag: "divide"; readonly value: MultiplicativeExpression_Binary }
  | { readonly tag: "mod"; readonly value: MultiplicativeExpression_Binary };

export interface MultiplicativeExpression_Binary {
  readonly lhs: MultiplicativeExpression;
  readonly rhs: UnaryExpression;
}

export type UnaryExpression =
  | { readonly tag: "preIncrement"; readonly value: PreIncrementExpression }
  | { readonly tag: "preDecrement"; readonly value: PreDecrementExpression }
  | { readonly tag: "plus"; readonly value: UnaryExpression }
  | { readonly tag: "minus"; readonly value: UnaryExpression }
  | { readonly tag: "other"; readonly value: UnaryExpressionNotPlusMinus };

export type PreIncrementExpression = UnaryExpression & { readonly __brand: "PreIncrementExpression" };

export type PreDecrementExpression = UnaryExpression & { readonly __brand: "PreDecrementExpression" };

export type UnaryExpressionNotPlusMinus =
  | { readonly tag: "postfix"; readonly value: PostfixExpression }
  | { readonly tag: "tilde"; readonly value: UnaryExpression }
  | { readonly tag: "not"; readonly value: UnaryExpression }
  | { readonly tag: "cast"; readonly value: CastExpression };

export type PostfixExpression =
  | { readonly tag: "primary"; readonly value: Primary }
  | { readonly tag: "name"; readonly value: ExpressionName }
  | { readonly tag: "postIncrement"; readonly value: PostIncrementExpression }
  | { readonly tag: "postDecrement"; readonly value: PostDecrementExpression };

export type PostIncrementExpression = PostfixExpression & { readonly __brand: "PostIncrementExpression" };

export type PostDecrementExpression = PostfixExpression & { readonly __brand: "PostDecrementExpression" };

export type CastExpression =
  | { readonly tag: "primitive"; readonly value: CastExpression_Primitive }
  | { readonly tag: "notPlusMinus"; readonly value: CastExpression_NotPlusMinus }
  | { readonly tag: "lambda"; readonly value: CastExpression_Lambda };

export interface CastExpression_Primitive {
  readonly type: PrimitiveTypeWithAnnotations;
  readonly expression: UnaryExpression;
}

export interface CastExpression_NotPlusMinus {
  readonly refAndBounds: CastExpression_RefAndBounds;
  readonly expression: UnaryExpression;
}

export interface CastExpression_Lambda {
  readonly refAndBounds: CastExpression_RefAndBounds;
  readonly expression: LambdaExpression;
}

export interface CastExpression_RefAndBounds {
  readonly type: ReferenceType;
  readonly bounds: ReadonlyArray<AdditionalBound>;
}

export type ConstantExpression = Expression & { readonly __brand: "ConstantExpression" };
