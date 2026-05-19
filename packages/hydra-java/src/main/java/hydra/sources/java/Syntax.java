package hydra.sources.java;

import hydra.core.Type;
import hydra.dsl.Types;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.Namespace;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;

import static hydra.dsl.java.Helpers.doc;
import static hydra.dsl.java.Helpers.typeDef;
import static hydra.dsl.java.Helpers.typeref;

/**
 * Java syntax — Hydra type definitions matching the Oracle Java SE 21 BNF.
 * Mirror of packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Syntax.hs.
 */
public class Syntax {
    public static final Namespace NS = new Namespace("hydra.java.syntax");
    private static final Namespace CORE_NS = new Namespace("hydra.core");

    private static Type java(String local) { return typeref(NS, local); }

    private static Definition typeDefHere(String localName, Type t) {
        return typeDef(NS, localName, t);
    }

    private static Definition identifierDef() {
        return typeDefHere("Identifier", Types.wrap(Types.string()));
    }

    private static Definition typeIdentifierDef() {
        return typeDefHere("TypeIdentifier", Types.wrap(java("Identifier")));
    }

    private static Definition literalDef() {
        return typeDefHere("Literal", Types.union(Types.field("null", Types.unit()), Types.field("integer", java("IntegerLiteral")), Types.field("floatingPoint", java("FloatingPointLiteral")), Types.field("boolean", Types.boolean_()), Types.field("character", Types.uint16()), Types.field("string", java("StringLiteral")), Types.field("textBlock", java("TextBlock"))));
    }

    private static Definition integerLiteralDef() {
        return typeDefHere("IntegerLiteral", doc("Note: this is an approximation which ignores encoding", Types.wrap(Types.bigint())));
    }

    private static Definition floatingPointLiteralDef() {
        return typeDefHere("FloatingPointLiteral", doc("Note: this is an approximation which ignores encoding", Types.wrap(Types.float64())));
    }

    private static Definition stringLiteralDef() {
        return typeDefHere("StringLiteral", doc("Note: this is an approximation which ignores encoding", Types.wrap(Types.string())));
    }

    private static Definition textBlockDef() {
        return typeDefHere("TextBlock", doc("Note: this is an approximation which ignores encoding and incidental whitespace stripping", Types.wrap(Types.string())));
    }

    private static Definition type_Def() {
        return typeDefHere("Type", Types.union(Types.field("primitive", java("PrimitiveTypeWithAnnotations")), Types.field("reference", java("ReferenceType"))));
    }

    private static Definition primitiveTypeWithAnnotationsDef() {
        return typeDefHere("PrimitiveTypeWithAnnotations", Types.record(Types.field("type", java("PrimitiveType")), Types.field("annotations", Types.list(java("Annotation")))));
    }

    private static Definition primitiveType_Def() {
        return typeDefHere("PrimitiveType", Types.union(Types.field("numeric", java("NumericType")), Types.field("boolean", Types.unit())));
    }

    private static Definition numericTypeDef() {
        return typeDefHere("NumericType", Types.union(Types.field("integral", java("IntegralType")), Types.field("floatingPoint", java("FloatingPointType"))));
    }

    private static Definition integralTypeDef() {
        return typeDefHere("IntegralType", Types.union(Types.field("byte", Types.unit()), Types.field("short", Types.unit()), Types.field("int", Types.unit()), Types.field("long", Types.unit()), Types.field("char", Types.unit())));
    }

    private static Definition floatingPointTypeDef() {
        return typeDefHere("FloatingPointType", Types.union(Types.field("float", Types.unit()), Types.field("double", Types.unit())));
    }

    private static Definition referenceTypeDef() {
        return typeDefHere("ReferenceType", Types.union(Types.field("classOrInterface", java("ClassOrInterfaceType")), Types.field("variable", java("TypeVariable")), Types.field("array", java("ArrayType"))));
    }

    private static Definition classOrInterfaceTypeDef() {
        return typeDefHere("ClassOrInterfaceType", Types.union(Types.field("class", java("ClassType")), Types.field("interface", java("InterfaceType"))));
    }

    private static Definition classTypeDef() {
        return typeDefHere("ClassType", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("qualifier", java("ClassTypeQualifier")), Types.field("identifier", java("TypeIdentifier")), Types.field("arguments", Types.list(java("TypeArgument")))));
    }

    private static Definition classTypeQualifierDef() {
        return typeDefHere("ClassTypeQualifier", Types.union(Types.field("none", Types.unit()), Types.field("package", java("PackageName")), Types.field("parent", java("ClassOrInterfaceType"))));
    }

    private static Definition interfaceTypeDef() {
        return typeDefHere("InterfaceType", Types.wrap(java("ClassType")));
    }

    private static Definition typeVariableDef() {
        return typeDefHere("TypeVariable", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("identifier", java("TypeIdentifier"))));
    }

    private static Definition arrayTypeDef() {
        return typeDefHere("ArrayType", Types.record(Types.field("dims", java("Dims")), Types.field("variant", java("ArrayType_Variant"))));
    }

    private static Definition arrayType_VariantDef() {
        return typeDefHere("ArrayType_Variant", Types.union(Types.field("primitive", java("PrimitiveTypeWithAnnotations")), Types.field("classOrInterface", java("ClassOrInterfaceType")), Types.field("variable", java("TypeVariable"))));
    }

    private static Definition dimsDef() {
        return typeDefHere("Dims", Types.wrap(Types.list(Types.list(java("Annotation")))));
    }

    private static Definition typeParameterDef() {
        return typeDefHere("TypeParameter", Types.record(Types.field("modifiers", Types.list(java("TypeParameterModifier"))), Types.field("identifier", java("TypeIdentifier")), Types.field("bound", Types.optional(java("TypeBound")))));
    }

    private static Definition typeParameterModifierDef() {
        return typeDefHere("TypeParameterModifier", Types.wrap(java("Annotation")));
    }

    private static Definition typeBoundDef() {
        return typeDefHere("TypeBound", Types.union(Types.field("variable", java("TypeVariable")), Types.field("classOrInterface", java("TypeBound_ClassOrInterface"))));
    }

    private static Definition typeBound_ClassOrInterfaceDef() {
        return typeDefHere("TypeBound_ClassOrInterface", Types.record(Types.field("type", java("ClassOrInterfaceType")), Types.field("additional", Types.list(java("AdditionalBound")))));
    }

    private static Definition additionalBoundDef() {
        return typeDefHere("AdditionalBound", Types.wrap(java("InterfaceType")));
    }

    private static Definition typeArgumentDef() {
        return typeDefHere("TypeArgument", Types.union(Types.field("reference", java("ReferenceType")), Types.field("wildcard", java("Wildcard"))));
    }

    private static Definition wildcardDef() {
        return typeDefHere("Wildcard", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("wildcard", Types.optional(java("WildcardBounds")))));
    }

    private static Definition wildcardBoundsDef() {
        return typeDefHere("WildcardBounds", Types.union(Types.field("extends", java("ReferenceType")), Types.field("super", java("ReferenceType"))));
    }

    private static Definition moduleNameDef() {
        return typeDefHere("ModuleName", Types.record(Types.field("identifier", java("Identifier")), Types.field("name", Types.optional(java("ModuleName")))));
    }

    private static Definition packageNameDef() {
        return typeDefHere("PackageName", Types.wrap(Types.list(java("Identifier"))));
    }

    private static Definition typeNameDef() {
        return typeDefHere("TypeName", Types.record(Types.field("identifier", java("TypeIdentifier")), Types.field("qualifier", Types.optional(java("PackageOrTypeName")))));
    }

    private static Definition expressionNameDef() {
        return typeDefHere("ExpressionName", Types.record(Types.field("qualifier", Types.optional(java("AmbiguousName"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition methodNameDef() {
        return typeDefHere("MethodName", Types.wrap(java("Identifier")));
    }

    private static Definition packageOrTypeNameDef() {
        return typeDefHere("PackageOrTypeName", Types.wrap(Types.list(java("Identifier"))));
    }

    private static Definition ambiguousNameDef() {
        return typeDefHere("AmbiguousName", Types.wrap(Types.list(java("Identifier"))));
    }

    private static Definition compilationUnitDef() {
        return typeDefHere("CompilationUnit", Types.union(Types.field("ordinary", java("OrdinaryCompilationUnit")), Types.field("modular", java("ModularCompilationUnit"))));
    }

    private static Definition ordinaryCompilationUnitDef() {
        return typeDefHere("OrdinaryCompilationUnit", Types.record(Types.field("package", Types.optional(java("PackageDeclaration"))), Types.field("imports", Types.list(java("ImportDeclaration"))), Types.field("types", Types.list(java("TopLevelClassOrInterfaceDeclarationWithComments")))));
    }

    private static Definition modularCompilationUnitDef() {
        return typeDefHere("ModularCompilationUnit", Types.record(Types.field("imports", Types.list(java("ImportDeclaration"))), Types.field("module", java("ModuleDeclaration"))));
    }

    private static Definition packageDeclarationDef() {
        return typeDefHere("PackageDeclaration", Types.record(Types.field("modifiers", Types.list(java("PackageModifier"))), Types.field("identifiers", Types.list(java("Identifier")))));
    }

    private static Definition packageModifierDef() {
        return typeDefHere("PackageModifier", Types.wrap(java("Annotation")));
    }

    private static Definition importDeclarationDef() {
        return typeDefHere("ImportDeclaration", Types.union(Types.field("singleType", java("SingleTypeImportDeclaration")), Types.field("typeImportOnDemand", java("TypeImportOnDemandDeclaration")), Types.field("singleStaticImport", java("SingleStaticImportDeclaration")), Types.field("staticImportOnDemand", java("StaticImportOnDemandDeclaration"))));
    }

    private static Definition singleTypeImportDeclarationDef() {
        return typeDefHere("SingleTypeImportDeclaration", Types.wrap(java("TypeName")));
    }

    private static Definition typeImportOnDemandDeclarationDef() {
        return typeDefHere("TypeImportOnDemandDeclaration", Types.wrap(java("PackageOrTypeName")));
    }

    private static Definition singleStaticImportDeclarationDef() {
        return typeDefHere("SingleStaticImportDeclaration", Types.record(Types.field("typeName", java("TypeName")), Types.field("identifier", java("Identifier"))));
    }

    private static Definition staticImportOnDemandDeclarationDef() {
        return typeDefHere("StaticImportOnDemandDeclaration", Types.wrap(java("TypeName")));
    }

    private static Definition topLevelClassOrInterfaceDeclarationDef() {
        return typeDefHere("TopLevelClassOrInterfaceDeclaration", Types.union(Types.field("class", java("ClassDeclaration")), Types.field("interface", java("InterfaceDeclaration")), Types.field("none", Types.unit())));
    }

    private static Definition topLevelClassOrInterfaceDeclarationWithCommentsDef() {
        return typeDefHere("TopLevelClassOrInterfaceDeclarationWithComments", Types.record(Types.field("value", java("TopLevelClassOrInterfaceDeclaration")), Types.field("comments", Types.optional(Types.string()))));
    }

    private static Definition moduleDeclarationDef() {
        return typeDefHere("ModuleDeclaration", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("open", Types.boolean_()), Types.field("identifiers", Types.list(java("Identifier"))), Types.field("directives", Types.list(java("ModuleDirective")))));
    }

    private static Definition moduleDirectiveDef() {
        return typeDefHere("ModuleDirective", Types.union(Types.field("requires", java("ModuleDirective_Requires")), Types.field("exports", java("ModuleDirective_ExportsOrOpens")), Types.field("opens", java("ModuleDirective_ExportsOrOpens")), Types.field("uses", java("TypeName")), Types.field("provides", java("ModuleDirective_Provides"))));
    }

    private static Definition moduleDirective_RequiresDef() {
        return typeDefHere("ModuleDirective_Requires", Types.record(Types.field("modifiers", Types.list(java("RequiresModifier"))), Types.field("module", java("ModuleName"))));
    }

    private static Definition moduleDirective_ExportsOrOpensDef() {
        return typeDefHere("ModuleDirective_ExportsOrOpens", Types.record(Types.field("package", java("PackageName")), Types.field("modules", doc("At least one module", Types.list(java("ModuleName"))))));
    }

    private static Definition moduleDirective_ProvidesDef() {
        return typeDefHere("ModuleDirective_Provides", Types.record(Types.field("to", java("TypeName")), Types.field("with", doc("At least one type", Types.list(java("TypeName"))))));
    }

    private static Definition requiresModifierDef() {
        return typeDefHere("RequiresModifier", Types.union(Types.field("transitive", Types.unit()), Types.field("static", Types.unit())));
    }

    private static Definition classDeclarationDef() {
        return typeDefHere("ClassDeclaration", Types.union(Types.field("normal", java("NormalClassDeclaration")), Types.field("enum", java("EnumDeclaration")), Types.field("record", java("RecordDeclaration"))));
    }

    private static Definition normalClassDeclarationDef() {
        return typeDefHere("NormalClassDeclaration", Types.record(Types.field("modifiers", Types.list(java("ClassModifier"))), Types.field("identifier", java("TypeIdentifier")), Types.field("parameters", Types.list(java("TypeParameter"))), Types.field("extends", Types.optional(java("ClassType"))), Types.field("implements", Types.list(java("InterfaceType"))), Types.field("permits", Types.list(java("TypeName"))), Types.field("body", java("ClassBody"))));
    }

    private static Definition classModifierDef() {
        return typeDefHere("ClassModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("protected", Types.unit()), Types.field("private", Types.unit()), Types.field("abstract", Types.unit()), Types.field("static", Types.unit()), Types.field("final", Types.unit()), Types.field("sealed", Types.unit()), Types.field("nonSealed", Types.unit()), Types.field("strictfp", Types.unit())));
    }

    private static Definition classBodyDef() {
        return typeDefHere("ClassBody", Types.wrap(Types.list(java("ClassBodyDeclarationWithComments"))));
    }

    private static Definition classBodyDeclarationDef() {
        return typeDefHere("ClassBodyDeclaration", Types.union(Types.field("classMember", java("ClassMemberDeclaration")), Types.field("instanceInitializer", java("InstanceInitializer")), Types.field("staticInitializer", java("StaticInitializer")), Types.field("constructorDeclaration", java("ConstructorDeclaration"))));
    }

    private static Definition classBodyDeclarationWithCommentsDef() {
        return typeDefHere("ClassBodyDeclarationWithComments", Types.record(Types.field("value", java("ClassBodyDeclaration")), Types.field("comments", Types.optional(Types.string()))));
    }

    private static Definition classMemberDeclarationDef() {
        return typeDefHere("ClassMemberDeclaration", Types.union(Types.field("field", java("FieldDeclaration")), Types.field("method", java("MethodDeclaration")), Types.field("class", java("ClassDeclaration")), Types.field("interface", java("InterfaceDeclaration")), Types.field("none", Types.unit())));
    }

    private static Definition fieldDeclarationDef() {
        return typeDefHere("FieldDeclaration", Types.record(Types.field("modifiers", Types.list(java("FieldModifier"))), Types.field("unannType", java("UnannType")), Types.field("variableDeclarators", Types.list(java("VariableDeclarator")))));
    }

    private static Definition fieldModifierDef() {
        return typeDefHere("FieldModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("protected", Types.unit()), Types.field("private", Types.unit()), Types.field("static", Types.unit()), Types.field("final", Types.unit()), Types.field("transient", Types.unit()), Types.field("volatile", Types.unit())));
    }

    private static Definition variableDeclaratorDef() {
        return typeDefHere("VariableDeclarator", Types.record(Types.field("id", java("VariableDeclaratorId")), Types.field("initializer", Types.optional(java("VariableInitializer")))));
    }

    private static Definition variableDeclaratorIdDef() {
        return typeDefHere("VariableDeclaratorId", Types.record(Types.field("identifier", java("Identifier")), Types.field("dims", Types.optional(java("Dims")))));
    }

    private static Definition variableInitializerDef() {
        return typeDefHere("VariableInitializer", Types.union(Types.field("expression", java("Expression")), Types.field("arrayInitializer", java("ArrayInitializer"))));
    }

    private static Definition unannTypeDef() {
        return typeDefHere("UnannType", doc("A Type which does not allow annotations", Types.wrap(java("Type"))));
    }

    private static Definition unannClassTypeDef() {
        return typeDefHere("UnannClassType", doc("A ClassType which does not allow annotations", Types.wrap(java("ClassType"))));
    }

    private static Definition methodDeclarationDef() {
        return typeDefHere("MethodDeclaration", Types.record(Types.field("annotations", doc("Note: simple methods cannot have annotations", Types.list(java("Annotation")))), Types.field("modifiers", Types.list(java("MethodModifier"))), Types.field("header", java("MethodHeader")), Types.field("body", java("MethodBody"))));
    }

    private static Definition methodModifierDef() {
        return typeDefHere("MethodModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("protected", Types.unit()), Types.field("private", Types.unit()), Types.field("abstract", Types.unit()), Types.field("static", Types.unit()), Types.field("final", Types.unit()), Types.field("synchronized", Types.unit()), Types.field("native", Types.unit()), Types.field("strictfp", Types.unit())));
    }

    private static Definition methodHeaderDef() {
        return typeDefHere("MethodHeader", Types.record(Types.field("parameters", Types.list(java("TypeParameter"))), Types.field("result", java("Result")), Types.field("declarator", java("MethodDeclarator")), Types.field("throws", Types.optional(java("Throws")))));
    }

    private static Definition resultDef() {
        return typeDefHere("Result", Types.union(Types.field("type", java("UnannType")), Types.field("void", Types.unit())));
    }

    private static Definition methodDeclaratorDef() {
        return typeDefHere("MethodDeclarator", Types.record(Types.field("identifier", java("Identifier")), Types.field("receiverParameter", Types.optional(java("ReceiverParameter"))), Types.field("formalParameters", Types.list(java("FormalParameter")))));
    }

    private static Definition receiverParameterDef() {
        return typeDefHere("ReceiverParameter", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("unannType", java("UnannType")), Types.field("identifier", Types.optional(java("Identifier")))));
    }

    private static Definition formalParameterDef() {
        return typeDefHere("FormalParameter", Types.union(Types.field("simple", java("FormalParameter_Simple")), Types.field("variableArity", java("VariableArityParameter"))));
    }

    private static Definition formalParameter_SimpleDef() {
        return typeDefHere("FormalParameter_Simple", Types.record(Types.field("modifiers", Types.list(java("VariableModifier"))), Types.field("type", java("UnannType")), Types.field("id", java("VariableDeclaratorId"))));
    }

    private static Definition variableArityParameterDef() {
        return typeDefHere("VariableArityParameter", Types.record(Types.field("modifiers", Types.list(java("VariableModifier"))), Types.field("type", java("UnannType")), Types.field("annotations", Types.list(java("Annotation"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition variableModifierDef() {
        return typeDefHere("VariableModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("final", Types.unit())));
    }

    private static Definition throwsDef() {
        return typeDefHere("Throws", Types.wrap(Types.list(java("ExceptionType"))));
    }

    private static Definition exceptionTypeDef() {
        return typeDefHere("ExceptionType", Types.union(Types.field("class", java("ClassType")), Types.field("variable", java("TypeVariable"))));
    }

    private static Definition methodBodyDef() {
        return typeDefHere("MethodBody", Types.union(Types.field("block", java("Block")), Types.field("none", Types.unit())));
    }

    private static Definition instanceInitializerDef() {
        return typeDefHere("InstanceInitializer", Types.wrap(java("Block")));
    }

    private static Definition staticInitializerDef() {
        return typeDefHere("StaticInitializer", Types.wrap(java("Block")));
    }

    private static Definition constructorDeclarationDef() {
        return typeDefHere("ConstructorDeclaration", Types.record(Types.field("modifiers", Types.list(java("ConstructorModifier"))), Types.field("constructor", java("ConstructorDeclarator")), Types.field("throws", Types.optional(java("Throws"))), Types.field("body", java("ConstructorBody"))));
    }

    private static Definition constructorModifierDef() {
        return typeDefHere("ConstructorModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("protected", Types.unit()), Types.field("private", Types.unit())));
    }

    private static Definition constructorDeclaratorDef() {
        return typeDefHere("ConstructorDeclarator", Types.record(Types.field("parameters", Types.list(java("TypeParameter"))), Types.field("name", java("SimpleTypeName")), Types.field("receiverParameter", Types.optional(java("ReceiverParameter"))), Types.field("formalParameters", Types.list(java("FormalParameter")))));
    }

    private static Definition simpleTypeNameDef() {
        return typeDefHere("SimpleTypeName", Types.wrap(java("TypeIdentifier")));
    }

    private static Definition constructorBodyDef() {
        return typeDefHere("ConstructorBody", Types.record(Types.field("invocation", Types.optional(java("ExplicitConstructorInvocation"))), Types.field("statements", Types.list(java("BlockStatement")))));
    }

    private static Definition explicitConstructorInvocationDef() {
        return typeDefHere("ExplicitConstructorInvocation", Types.record(Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("arguments", Types.list(java("Expression"))), Types.field("variant", java("ExplicitConstructorInvocation_Variant"))));
    }

    private static Definition explicitConstructorInvocation_VariantDef() {
        return typeDefHere("ExplicitConstructorInvocation_Variant", Types.union(Types.field("this", Types.unit()), Types.field("super", Types.optional(java("ExpressionName"))), Types.field("primary", java("Primary"))));
    }

    private static Definition enumDeclarationDef() {
        return typeDefHere("EnumDeclaration", Types.record(Types.field("modifiers", Types.list(java("ClassModifier"))), Types.field("identifier", java("TypeIdentifier")), Types.field("implements", Types.list(java("InterfaceType"))), Types.field("body", java("EnumBody"))));
    }

    private static Definition enumBodyDef() {
        return typeDefHere("EnumBody", Types.wrap(Types.list(java("EnumBody_Element"))));
    }

    private static Definition enumBody_ElementDef() {
        return typeDefHere("EnumBody_Element", Types.record(Types.field("constants", Types.list(java("EnumConstant"))), Types.field("bodyDeclarations", Types.list(java("ClassBodyDeclaration")))));
    }

    private static Definition enumConstantDef() {
        return typeDefHere("EnumConstant", Types.record(Types.field("modifiers", Types.list(java("EnumConstantModifier"))), Types.field("identifier", java("Identifier")), Types.field("arguments", Types.optional(Types.list(java("Expression")))), Types.field("body", Types.optional(java("ClassBody")))));
    }

    private static Definition enumConstantModifierDef() {
        return typeDefHere("EnumConstantModifier", Types.wrap(java("Annotation")));
    }

    private static Definition recordDeclarationDef() {
        return typeDefHere("RecordDeclaration", Types.record(Types.field("modifiers", Types.list(java("ClassModifier"))), Types.field("identifier", java("TypeIdentifier")), Types.field("parameters", Types.list(java("TypeParameter"))), Types.field("header", java("RecordHeader")), Types.field("implements", Types.list(java("InterfaceType"))), Types.field("body", java("RecordBody"))));
    }

    private static Definition recordHeaderDef() {
        return typeDefHere("RecordHeader", Types.wrap(Types.list(java("RecordComponent"))));
    }

    private static Definition recordComponentDef() {
        return typeDefHere("RecordComponent", Types.union(Types.field("simple", java("RecordComponent_Simple")), Types.field("variableArity", java("VariableArityRecordComponent"))));
    }

    private static Definition recordComponent_SimpleDef() {
        return typeDefHere("RecordComponent_Simple", Types.record(Types.field("modifiers", Types.list(java("RecordComponentModifier"))), Types.field("type", java("UnannType")), Types.field("identifier", java("Identifier"))));
    }

    private static Definition variableArityRecordComponentDef() {
        return typeDefHere("VariableArityRecordComponent", Types.record(Types.field("modifiers", Types.list(java("RecordComponentModifier"))), Types.field("type", java("UnannType")), Types.field("annotations", Types.list(java("Annotation"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition recordComponentModifierDef() {
        return typeDefHere("RecordComponentModifier", Types.wrap(java("Annotation")));
    }

    private static Definition recordBodyDef() {
        return typeDefHere("RecordBody", Types.wrap(Types.list(java("RecordBodyDeclaration"))));
    }

    private static Definition recordBodyDeclarationDef() {
        return typeDefHere("RecordBodyDeclaration", Types.union(Types.field("classBody", java("ClassBodyDeclaration")), Types.field("compactConstructor", java("CompactConstructorDeclaration"))));
    }

    private static Definition compactConstructorDeclarationDef() {
        return typeDefHere("CompactConstructorDeclaration", Types.record(Types.field("modifiers", Types.list(java("ConstructorModifier"))), Types.field("name", java("SimpleTypeName")), Types.field("body", java("ConstructorBody"))));
    }

    private static Definition interfaceDeclarationDef() {
        return typeDefHere("InterfaceDeclaration", Types.union(Types.field("normalInterface", java("NormalInterfaceDeclaration")), Types.field("annotationInterface", java("AnnotationInterfaceDeclaration"))));
    }

    private static Definition normalInterfaceDeclarationDef() {
        return typeDefHere("NormalInterfaceDeclaration", Types.record(Types.field("modifiers", Types.list(java("InterfaceModifier"))), Types.field("identifier", java("TypeIdentifier")), Types.field("parameters", Types.list(java("TypeParameter"))), Types.field("extends", Types.list(java("InterfaceType"))), Types.field("permits", Types.list(java("TypeName"))), Types.field("body", java("InterfaceBody"))));
    }

    private static Definition interfaceModifierDef() {
        return typeDefHere("InterfaceModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("protected", Types.unit()), Types.field("private", Types.unit()), Types.field("abstract", Types.unit()), Types.field("static", Types.unit()), Types.field("sealed", Types.unit()), Types.field("nonSealed", Types.unit()), Types.field("strictfp", Types.unit())));
    }

    private static Definition interfaceBodyDef() {
        return typeDefHere("InterfaceBody", Types.wrap(Types.list(java("InterfaceMemberDeclarationWithComments"))));
    }

    private static Definition interfaceMemberDeclarationDef() {
        return typeDefHere("InterfaceMemberDeclaration", Types.union(Types.field("constant", java("ConstantDeclaration")), Types.field("interfaceMethod", java("InterfaceMethodDeclaration")), Types.field("class", java("ClassDeclaration")), Types.field("interface", java("InterfaceDeclaration"))));
    }

    private static Definition interfaceMemberDeclarationWithCommentsDef() {
        return typeDefHere("InterfaceMemberDeclarationWithComments", Types.record(Types.field("value", java("InterfaceMemberDeclaration")), Types.field("comments", Types.optional(Types.string()))));
    }

    private static Definition constantDeclarationDef() {
        return typeDefHere("ConstantDeclaration", Types.record(Types.field("modifiers", Types.list(java("ConstantModifier"))), Types.field("type", java("UnannType")), Types.field("variables", Types.list(java("VariableDeclarator")))));
    }

    private static Definition constantModifierDef() {
        return typeDefHere("ConstantModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("static", Types.unit()), Types.field("final", Types.unit())));
    }

    private static Definition interfaceMethodDeclarationDef() {
        return typeDefHere("InterfaceMethodDeclaration", Types.record(Types.field("modifiers", Types.list(java("InterfaceMethodModifier"))), Types.field("header", java("MethodHeader")), Types.field("body", java("MethodBody"))));
    }

    private static Definition interfaceMethodModifierDef() {
        return typeDefHere("InterfaceMethodModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("private", Types.unit()), Types.field("abstract", Types.unit()), Types.field("default", Types.unit()), Types.field("static", Types.unit()), Types.field("strictfp", Types.unit())));
    }

    private static Definition annotationInterfaceDeclarationDef() {
        return typeDefHere("AnnotationInterfaceDeclaration", Types.record(Types.field("modifiers", Types.list(java("InterfaceModifier"))), Types.field("identifier", java("TypeIdentifier")), Types.field("body", java("AnnotationInterfaceBody"))));
    }

    private static Definition annotationInterfaceBodyDef() {
        return typeDefHere("AnnotationInterfaceBody", Types.wrap(Types.list(java("AnnotationInterfaceMemberDeclaration"))));
    }

    private static Definition annotationInterfaceMemberDeclarationDef() {
        return typeDefHere("AnnotationInterfaceMemberDeclaration", Types.union(Types.field("annotationInterface", java("AnnotationInterfaceElementDeclaration")), Types.field("constant", java("ConstantDeclaration")), Types.field("class", java("ClassDeclaration")), Types.field("interface", java("InterfaceDeclaration"))));
    }

    private static Definition annotationInterfaceElementDeclarationDef() {
        return typeDefHere("AnnotationInterfaceElementDeclaration", Types.record(Types.field("modifiers", Types.list(java("AnnotationInterfaceElementModifier"))), Types.field("type", java("UnannType")), Types.field("identifier", java("Identifier")), Types.field("dims", Types.optional(java("Dims"))), Types.field("default", Types.optional(java("DefaultValue")))));
    }

    private static Definition annotationInterfaceElementModifierDef() {
        return typeDefHere("AnnotationInterfaceElementModifier", Types.union(Types.field("annotation", java("Annotation")), Types.field("public", Types.unit()), Types.field("abstract", Types.unit())));
    }

    private static Definition defaultValueDef() {
        return typeDefHere("DefaultValue", Types.wrap(java("ElementValue")));
    }

    private static Definition annotationDef() {
        return typeDefHere("Annotation", Types.union(Types.field("normal", java("NormalAnnotation")), Types.field("marker", java("MarkerAnnotation")), Types.field("singleElement", java("SingleElementAnnotation"))));
    }

    private static Definition normalAnnotationDef() {
        return typeDefHere("NormalAnnotation", Types.record(Types.field("typeName", java("TypeName")), Types.field("pairs", Types.list(java("ElementValuePair")))));
    }

    private static Definition elementValuePairDef() {
        return typeDefHere("ElementValuePair", Types.record(Types.field("key", java("Identifier")), Types.field("value", java("ElementValue"))));
    }

    private static Definition elementValueDef() {
        return typeDefHere("ElementValue", Types.union(Types.field("conditionalExpression", java("ConditionalExpression")), Types.field("elementValueArrayInitializer", java("ElementValueArrayInitializer")), Types.field("annotation", java("Annotation"))));
    }

    private static Definition elementValueArrayInitializerDef() {
        return typeDefHere("ElementValueArrayInitializer", Types.wrap(Types.list(java("ElementValue"))));
    }

    private static Definition markerAnnotationDef() {
        return typeDefHere("MarkerAnnotation", Types.wrap(java("TypeName")));
    }

    private static Definition singleElementAnnotationDef() {
        return typeDefHere("SingleElementAnnotation", Types.record(Types.field("name", java("TypeName")), Types.field("value", Types.optional(java("ElementValue")))));
    }

    private static Definition arrayInitializerDef() {
        return typeDefHere("ArrayInitializer", Types.wrap(Types.list(Types.list(java("VariableInitializer")))));
    }

    private static Definition blockDef() {
        return typeDefHere("Block", Types.wrap(Types.list(java("BlockStatement"))));
    }

    private static Definition blockStatementDef() {
        return typeDefHere("BlockStatement", Types.union(Types.field("localVariableDeclaration", java("LocalVariableDeclarationStatement")), Types.field("localClassOrInterface", java("LocalClassOrInterfaceDeclaration")), Types.field("statement", java("Statement"))));
    }

    private static Definition localClassOrInterfaceDeclarationDef() {
        return typeDefHere("LocalClassOrInterfaceDeclaration", Types.union(Types.field("class", java("ClassDeclaration")), Types.field("normalInterface", java("NormalInterfaceDeclaration"))));
    }

    private static Definition localVariableDeclarationStatementDef() {
        return typeDefHere("LocalVariableDeclarationStatement", Types.wrap(java("LocalVariableDeclaration")));
    }

    private static Definition localVariableDeclarationDef() {
        return typeDefHere("LocalVariableDeclaration", Types.record(Types.field("modifiers", Types.list(java("VariableModifier"))), Types.field("type", java("LocalVariableType")), Types.field("declarators", Types.list(java("VariableDeclarator")))));
    }

    private static Definition localVariableTypeDef() {
        return typeDefHere("LocalVariableType", Types.union(Types.field("type", java("UnannType")), Types.field("var", Types.unit())));
    }

    private static Definition statementDef() {
        return typeDefHere("Statement", Types.union(Types.field("withoutTrailing", java("StatementWithoutTrailingSubstatement")), Types.field("labeled", java("LabeledStatement")), Types.field("ifThen", java("IfThenStatement")), Types.field("ifThenElse", java("IfThenElseStatement")), Types.field("while", java("WhileStatement")), Types.field("for", java("ForStatement"))));
    }

    private static Definition statementNoShortIfDef() {
        return typeDefHere("StatementNoShortIf", Types.union(Types.field("withoutTrailing", java("StatementWithoutTrailingSubstatement")), Types.field("labeled", java("LabeledStatementNoShortIf")), Types.field("ifThenElse", java("IfThenElseStatementNoShortIf")), Types.field("while", java("WhileStatementNoShortIf")), Types.field("for", java("ForStatementNoShortIf"))));
    }

    private static Definition statementWithoutTrailingSubstatementDef() {
        return typeDefHere("StatementWithoutTrailingSubstatement", Types.union(Types.field("block", java("Block")), Types.field("empty", Types.unit()), Types.field("expression", java("ExpressionStatement")), Types.field("assert", java("AssertStatement")), Types.field("switch", java("SwitchStatement")), Types.field("do", java("DoStatement")), Types.field("break", java("BreakStatement")), Types.field("continue", java("ContinueStatement")), Types.field("return", java("ReturnStatement")), Types.field("synchronized", java("SynchronizedStatement")), Types.field("throw", java("ThrowStatement")), Types.field("try", java("TryStatement")), Types.field("yield", java("YieldStatement"))));
    }

    private static Definition labeledStatementDef() {
        return typeDefHere("LabeledStatement", Types.record(Types.field("identifier", java("Identifier")), Types.field("statement", java("Statement"))));
    }

    private static Definition labeledStatementNoShortIfDef() {
        return typeDefHere("LabeledStatementNoShortIf", Types.record(Types.field("identifier", java("Identifier")), Types.field("statement", java("StatementNoShortIf"))));
    }

    private static Definition expressionStatementDef() {
        return typeDefHere("ExpressionStatement", Types.wrap(java("StatementExpression")));
    }

    private static Definition statementExpressionDef() {
        return typeDefHere("StatementExpression", Types.union(Types.field("assignment", java("Assignment")), Types.field("preIncrement", java("PreIncrementExpression")), Types.field("preDecrement", java("PreDecrementExpression")), Types.field("postIncrement", java("PostIncrementExpression")), Types.field("postDecrement", java("PostDecrementExpression")), Types.field("methodInvocation", java("MethodInvocation")), Types.field("classInstanceCreation", java("ClassInstanceCreationExpression"))));
    }

    private static Definition ifThenStatementDef() {
        return typeDefHere("IfThenStatement", Types.record(Types.field("expression", java("Expression")), Types.field("statement", java("Statement"))));
    }

    private static Definition ifThenElseStatementDef() {
        return typeDefHere("IfThenElseStatement", Types.record(Types.field("cond", Types.optional(java("Expression"))), Types.field("then", java("StatementNoShortIf")), Types.field("else", java("Statement"))));
    }

    private static Definition ifThenElseStatementNoShortIfDef() {
        return typeDefHere("IfThenElseStatementNoShortIf", Types.record(Types.field("cond", Types.optional(java("Expression"))), Types.field("then", java("StatementNoShortIf")), Types.field("else", java("StatementNoShortIf"))));
    }

    private static Definition assertStatementDef() {
        return typeDefHere("AssertStatement", Types.union(Types.field("single", java("Expression")), Types.field("pair", java("AssertStatement_Pair"))));
    }

    private static Definition assertStatement_PairDef() {
        return typeDefHere("AssertStatement_Pair", Types.record(Types.field("first", java("Expression")), Types.field("second", java("Expression"))));
    }

    private static Definition switchStatementDef() {
        return typeDefHere("SwitchStatement", Types.record(Types.field("cond", java("Expression")), Types.field("block", java("SwitchBlock"))));
    }

    private static Definition switchBlockDef() {
        return typeDefHere("SwitchBlock", Types.union(Types.field("rules", Types.list(java("SwitchRule"))), Types.field("legacy", java("SwitchBlock_Legacy"))));
    }

    private static Definition switchBlock_LegacyDef() {
        return typeDefHere("SwitchBlock_Legacy", Types.record(Types.field("groups", Types.list(java("SwitchBlockStatementGroup"))), Types.field("trailingLabels", Types.list(java("SwitchLabel")))));
    }

    private static Definition switchRuleDef() {
        return typeDefHere("SwitchRule", Types.record(Types.field("label", java("SwitchLabel")), Types.field("body", java("SwitchRule_Body"))));
    }

    private static Definition switchRule_BodyDef() {
        return typeDefHere("SwitchRule_Body", Types.union(Types.field("expression", java("Expression")), Types.field("block", java("Block")), Types.field("throw", java("ThrowStatement"))));
    }

    private static Definition switchBlockStatementGroupDef() {
        return typeDefHere("SwitchBlockStatementGroup", Types.record(Types.field("labels", Types.list(java("SwitchLabel"))), Types.field("statements", Types.list(java("BlockStatement")))));
    }

    private static Definition switchLabelDef() {
        return typeDefHere("SwitchLabel", Types.union(Types.field("case", Types.list(java("CaseConstant"))), Types.field("null", Types.boolean_()), Types.field("casePattern", java("CasePattern")), Types.field("default", Types.unit())));
    }

    private static Definition caseConstantDef() {
        return typeDefHere("CaseConstant", Types.wrap(java("ConditionalExpression")));
    }

    private static Definition casePatternDef() {
        return typeDefHere("CasePattern", Types.record(Types.field("pattern", java("Pattern")), Types.field("guard", Types.optional(java("Guard")))));
    }

    private static Definition guardDef() {
        return typeDefHere("Guard", Types.wrap(java("Expression")));
    }

    private static Definition pattern_Def() {
        return typeDefHere("Pattern", Types.union(Types.field("type", java("TypePattern")), Types.field("record", java("RecordPattern"))));
    }

    private static Definition typePatternDef() {
        return typeDefHere("TypePattern", Types.wrap(java("LocalVariableDeclaration")));
    }

    private static Definition recordPatternDef() {
        return typeDefHere("RecordPattern", Types.record(Types.field("type", java("ReferenceType")), Types.field("patterns", Types.list(java("Pattern")))));
    }

    private static Definition whileStatementDef() {
        return typeDefHere("WhileStatement", Types.record(Types.field("cond", Types.optional(java("Expression"))), Types.field("body", java("Statement"))));
    }

    private static Definition whileStatementNoShortIfDef() {
        return typeDefHere("WhileStatementNoShortIf", Types.record(Types.field("cond", Types.optional(java("Expression"))), Types.field("body", java("StatementNoShortIf"))));
    }

    private static Definition doStatementDef() {
        return typeDefHere("DoStatement", Types.record(Types.field("body", java("Statement")), Types.field("cond", java("Expression"))));
    }

    private static Definition forStatementDef() {
        return typeDefHere("ForStatement", Types.union(Types.field("basic", java("BasicForStatement")), Types.field("enhanced", java("EnhancedForStatement"))));
    }

    private static Definition forStatementNoShortIfDef() {
        return typeDefHere("ForStatementNoShortIf", Types.union(Types.field("basic", java("BasicForStatementNoShortIf")), Types.field("enhanced", java("EnhancedForStatementNoShortIf"))));
    }

    private static Definition basicForStatementDef() {
        return typeDefHere("BasicForStatement", Types.record(Types.field("cond", java("ForCond")), Types.field("body", java("Statement"))));
    }

    private static Definition forCondDef() {
        return typeDefHere("ForCond", Types.record(Types.field("init", Types.optional(java("ForInit"))), Types.field("cond", Types.optional(java("Expression"))), Types.field("update", Types.optional(java("ForUpdate")))));
    }

    private static Definition basicForStatementNoShortIfDef() {
        return typeDefHere("BasicForStatementNoShortIf", Types.record(Types.field("cond", java("ForCond")), Types.field("body", java("StatementNoShortIf"))));
    }

    private static Definition forInitDef() {
        return typeDefHere("ForInit", Types.union(Types.field("statements", Types.list(java("StatementExpression"))), Types.field("localVariable", java("LocalVariableDeclaration"))));
    }

    private static Definition forUpdateDef() {
        return typeDefHere("ForUpdate", Types.wrap(Types.list(java("StatementExpression"))));
    }

    private static Definition enhancedForStatementDef() {
        return typeDefHere("EnhancedForStatement", Types.record(Types.field("cond", java("EnhancedForCond")), Types.field("body", java("Statement"))));
    }

    private static Definition enhancedForCondDef() {
        return typeDefHere("EnhancedForCond", Types.record(Types.field("declaration", java("LocalVariableDeclaration")), Types.field("expression", java("Expression"))));
    }

    private static Definition enhancedForStatementNoShortIfDef() {
        return typeDefHere("EnhancedForStatementNoShortIf", Types.record(Types.field("cond", java("EnhancedForCond")), Types.field("body", java("StatementNoShortIf"))));
    }

    private static Definition breakStatementDef() {
        return typeDefHere("BreakStatement", Types.wrap(Types.optional(java("Identifier"))));
    }

    private static Definition yieldStatementDef() {
        return typeDefHere("YieldStatement", Types.wrap(java("Expression")));
    }

    private static Definition continueStatementDef() {
        return typeDefHere("ContinueStatement", Types.wrap(Types.optional(java("Identifier"))));
    }

    private static Definition returnStatementDef() {
        return typeDefHere("ReturnStatement", Types.wrap(Types.optional(java("Expression"))));
    }

    private static Definition throwStatementDef() {
        return typeDefHere("ThrowStatement", Types.wrap(java("Expression")));
    }

    private static Definition synchronizedStatementDef() {
        return typeDefHere("SynchronizedStatement", Types.record(Types.field("expression", java("Expression")), Types.field("block", java("Block"))));
    }

    private static Definition tryStatementDef() {
        return typeDefHere("TryStatement", Types.union(Types.field("simple", java("TryStatement_Simple")), Types.field("withFinally", java("TryStatement_WithFinally")), Types.field("withResources", java("TryWithResourcesStatement"))));
    }

    private static Definition tryStatement_SimpleDef() {
        return typeDefHere("TryStatement_Simple", Types.record(Types.field("block", java("Block")), Types.field("catches", java("Catches"))));
    }

    private static Definition tryStatement_WithFinallyDef() {
        return typeDefHere("TryStatement_WithFinally", Types.record(Types.field("block", java("Block")), Types.field("catches", Types.optional(java("Catches"))), Types.field("finally", java("Finally"))));
    }

    private static Definition catchesDef() {
        return typeDefHere("Catches", Types.wrap(Types.list(java("CatchClause"))));
    }

    private static Definition catchClauseDef() {
        return typeDefHere("CatchClause", Types.record(Types.field("parameter", Types.optional(java("CatchFormalParameter"))), Types.field("block", java("Block"))));
    }

    private static Definition catchFormalParameterDef() {
        return typeDefHere("CatchFormalParameter", Types.record(Types.field("modifiers", Types.list(java("VariableModifier"))), Types.field("type", java("CatchType")), Types.field("id", java("VariableDeclaratorId"))));
    }

    private static Definition catchTypeDef() {
        return typeDefHere("CatchType", Types.record(Types.field("type", java("UnannClassType")), Types.field("types", Types.list(java("ClassType")))));
    }

    private static Definition finally_Def() {
        return typeDefHere("Finally", Types.wrap(java("Block")));
    }

    private static Definition tryWithResourcesStatementDef() {
        return typeDefHere("TryWithResourcesStatement", Types.record(Types.field("resourceSpecification", java("ResourceSpecification")), Types.field("block", java("Block")), Types.field("catches", Types.optional(java("Catches"))), Types.field("finally", Types.optional(java("Finally")))));
    }

    private static Definition resourceSpecificationDef() {
        return typeDefHere("ResourceSpecification", Types.wrap(Types.list(java("Resource"))));
    }

    private static Definition resourceDef() {
        return typeDefHere("Resource", Types.union(Types.field("local", java("Resource_Local")), Types.field("variable", java("VariableAccess"))));
    }

    private static Definition resource_LocalDef() {
        return typeDefHere("Resource_Local", Types.record(Types.field("modifiers", Types.list(java("VariableModifier"))), Types.field("type", java("LocalVariableType")), Types.field("identifier", java("Identifier")), Types.field("expression", java("Expression"))));
    }

    private static Definition variableAccessDef() {
        return typeDefHere("VariableAccess", Types.union(Types.field("expressionName", java("ExpressionName")), Types.field("fieldAccess", java("FieldAccess"))));
    }

    private static Definition primaryDef() {
        return typeDefHere("Primary", Types.union(Types.field("noNewArray", java("PrimaryNoNewArrayExpression")), Types.field("arrayCreation", java("ArrayCreationExpression"))));
    }

    private static Definition primaryNoNewArrayExpressionDef() {
        return typeDefHere("PrimaryNoNewArrayExpression", Types.union(Types.field("literal", java("Literal")), Types.field("classLiteral", java("ClassLiteral")), Types.field("this", Types.unit()), Types.field("dotThis", java("TypeName")), Types.field("parens", java("Expression")), Types.field("classInstance", java("ClassInstanceCreationExpression")), Types.field("fieldAccess", java("FieldAccess")), Types.field("arrayAccess", java("ArrayAccess")), Types.field("methodInvocation", java("MethodInvocation")), Types.field("methodReference", java("MethodReference"))));
    }

    private static Definition classLiteralDef() {
        return typeDefHere("ClassLiteral", Types.union(Types.field("type", java("TypeNameArray")), Types.field("numericType", java("NumericTypeArray")), Types.field("boolean", java("BooleanArray")), Types.field("void", Types.unit())));
    }

    private static Definition typeNameArrayDef() {
        return typeDefHere("TypeNameArray", Types.union(Types.field("simple", java("TypeName")), Types.field("array", java("TypeNameArray"))));
    }

    private static Definition numericTypeArrayDef() {
        return typeDefHere("NumericTypeArray", Types.union(Types.field("simple", java("NumericType")), Types.field("array", java("NumericTypeArray"))));
    }

    private static Definition booleanArrayDef() {
        return typeDefHere("BooleanArray", Types.union(Types.field("simple", Types.unit()), Types.field("array", java("BooleanArray"))));
    }

    private static Definition classInstanceCreationExpressionDef() {
        return typeDefHere("ClassInstanceCreationExpression", Types.record(Types.field("qualifier", Types.optional(java("ClassInstanceCreationExpression_Qualifier"))), Types.field("expression", java("UnqualifiedClassInstanceCreationExpression"))));
    }

    private static Definition classInstanceCreationExpression_QualifierDef() {
        return typeDefHere("ClassInstanceCreationExpression_Qualifier", Types.union(Types.field("expression", java("ExpressionName")), Types.field("primary", java("Primary"))));
    }

    private static Definition unqualifiedClassInstanceCreationExpressionDef() {
        return typeDefHere("UnqualifiedClassInstanceCreationExpression", Types.record(Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("classOrInterface", java("ClassOrInterfaceTypeToInstantiate")), Types.field("arguments", Types.list(java("Expression"))), Types.field("body", Types.optional(java("ClassBody")))));
    }

    private static Definition classOrInterfaceTypeToInstantiateDef() {
        return typeDefHere("ClassOrInterfaceTypeToInstantiate", Types.record(Types.field("identifiers", Types.list(java("AnnotatedIdentifier"))), Types.field("typeArguments", Types.optional(java("TypeArgumentsOrDiamond")))));
    }

    private static Definition annotatedIdentifierDef() {
        return typeDefHere("AnnotatedIdentifier", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition typeArgumentsOrDiamondDef() {
        return typeDefHere("TypeArgumentsOrDiamond", Types.union(Types.field("arguments", Types.list(java("TypeArgument"))), Types.field("diamond", Types.unit())));
    }

    private static Definition fieldAccessDef() {
        return typeDefHere("FieldAccess", Types.record(Types.field("qualifier", java("FieldAccess_Qualifier")), Types.field("identifier", java("Identifier"))));
    }

    private static Definition fieldAccess_QualifierDef() {
        return typeDefHere("FieldAccess_Qualifier", Types.union(Types.field("primary", java("Primary")), Types.field("super", Types.unit()), Types.field("typed", java("TypeName"))));
    }

    private static Definition arrayAccessDef() {
        return typeDefHere("ArrayAccess", Types.record(Types.field("expression", Types.optional(java("Expression"))), Types.field("variant", java("ArrayAccess_Variant"))));
    }

    private static Definition arrayAccess_VariantDef() {
        return typeDefHere("ArrayAccess_Variant", Types.union(Types.field("name", java("ExpressionName")), Types.field("primary", java("PrimaryNoNewArrayExpression")), Types.field("arrayCreationWithInitializer", java("ArrayCreationExpressionWithInitializer"))));
    }

    private static Definition methodInvocationDef() {
        return typeDefHere("MethodInvocation", Types.record(Types.field("header", java("MethodInvocation_Header")), Types.field("arguments", Types.list(java("Expression")))));
    }

    private static Definition methodInvocation_HeaderDef() {
        return typeDefHere("MethodInvocation_Header", Types.union(Types.field("simple", java("MethodName")), Types.field("complex", java("MethodInvocation_Complex"))));
    }

    private static Definition methodInvocation_ComplexDef() {
        return typeDefHere("MethodInvocation_Complex", Types.record(Types.field("variant", java("MethodInvocation_Variant")), Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition methodInvocation_VariantDef() {
        return typeDefHere("MethodInvocation_Variant", Types.union(Types.field("type", java("TypeName")), Types.field("expression", java("ExpressionName")), Types.field("primary", java("Primary")), Types.field("super", Types.unit()), Types.field("typeSuper", java("TypeName"))));
    }

    private static Definition methodReferenceDef() {
        return typeDefHere("MethodReference", Types.union(Types.field("expression", java("MethodReference_Expression")), Types.field("primary", java("MethodReference_Primary")), Types.field("referenceType", java("MethodReference_ReferenceType")), Types.field("super", java("MethodReference_Super")), Types.field("new", java("MethodReference_New")), Types.field("array", java("MethodReference_Array"))));
    }

    private static Definition methodReference_ExpressionDef() {
        return typeDefHere("MethodReference_Expression", Types.record(Types.field("name", java("ExpressionName")), Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition methodReference_PrimaryDef() {
        return typeDefHere("MethodReference_Primary", Types.record(Types.field("primary", java("Primary")), Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition methodReference_ReferenceTypeDef() {
        return typeDefHere("MethodReference_ReferenceType", Types.record(Types.field("referenceType", java("ReferenceType")), Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("identifier", java("Identifier"))));
    }

    private static Definition methodReference_SuperDef() {
        return typeDefHere("MethodReference_Super", Types.record(Types.field("typeArguments", Types.list(java("TypeArgument"))), Types.field("identifier", java("Identifier")), Types.field("super", Types.boolean_())));
    }

    private static Definition methodReference_NewDef() {
        return typeDefHere("MethodReference_New", Types.record(Types.field("classType", java("ClassType")), Types.field("typeArguments", Types.list(java("TypeArgument")))));
    }

    private static Definition methodReference_ArrayDef() {
        return typeDefHere("MethodReference_Array", Types.wrap(java("ArrayType")));
    }

    private static Definition arrayCreationExpressionDef() {
        return typeDefHere("ArrayCreationExpression", Types.union(Types.field("withoutInitializer", java("ArrayCreationExpressionWithoutInitializer")), Types.field("withInitializer", java("ArrayCreationExpressionWithInitializer"))));
    }

    private static Definition arrayCreationExpressionWithoutInitializerDef() {
        return typeDefHere("ArrayCreationExpressionWithoutInitializer", Types.union(Types.field("primitive", java("ArrayCreationExpressionWithoutInitializer_Primitive")), Types.field("classOrInterface", java("ArrayCreationExpressionWithoutInitializer_ClassOrInterface"))));
    }

    private static Definition arrayCreationExpressionWithoutInitializer_PrimitiveDef() {
        return typeDefHere("ArrayCreationExpressionWithoutInitializer_Primitive", Types.record(Types.field("type", java("PrimitiveTypeWithAnnotations")), Types.field("dimExprs", Types.list(java("DimExpr"))), Types.field("dims", Types.optional(java("Dims")))));
    }

    private static Definition arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDef() {
        return typeDefHere("ArrayCreationExpressionWithoutInitializer_ClassOrInterface", Types.record(Types.field("type", java("ClassOrInterfaceType")), Types.field("dimExprs", Types.list(java("DimExpr"))), Types.field("dims", Types.optional(java("Dims")))));
    }

    private static Definition arrayCreationExpressionWithInitializerDef() {
        return typeDefHere("ArrayCreationExpressionWithInitializer", Types.union(Types.field("primitive", java("ArrayCreationExpressionWithInitializer_Primitive")), Types.field("classOrInterface", java("ArrayCreationExpressionWithInitializer_ClassOrInterface"))));
    }

    private static Definition arrayCreationExpressionWithInitializer_PrimitiveDef() {
        return typeDefHere("ArrayCreationExpressionWithInitializer_Primitive", Types.record(Types.field("type", java("PrimitiveTypeWithAnnotations")), Types.field("dims", Types.list(java("Dims"))), Types.field("array", java("ArrayInitializer"))));
    }

    private static Definition arrayCreationExpressionWithInitializer_ClassOrInterfaceDef() {
        return typeDefHere("ArrayCreationExpressionWithInitializer_ClassOrInterface", Types.record(Types.field("type", java("ClassOrInterfaceType")), Types.field("dims", Types.list(java("Dims"))), Types.field("array", java("ArrayInitializer"))));
    }

    private static Definition dimExprDef() {
        return typeDefHere("DimExpr", Types.record(Types.field("annotations", Types.list(java("Annotation"))), Types.field("expression", Types.optional(java("Expression")))));
    }

    private static Definition expressionDef() {
        return typeDefHere("Expression", Types.union(Types.field("lambda", java("LambdaExpression")), Types.field("assignment", java("AssignmentExpression"))));
    }

    private static Definition lambdaExpressionDef() {
        return typeDefHere("LambdaExpression", Types.record(Types.field("parameters", java("LambdaParameters")), Types.field("body", java("LambdaBody"))));
    }

    private static Definition lambdaParametersDef() {
        return typeDefHere("LambdaParameters", Types.union(Types.field("tuple", Types.list(java("LambdaParameters"))), Types.field("single", java("Identifier"))));
    }

    private static Definition lambdaParameter_Def() {
        return typeDefHere("LambdaParameter", Types.union(Types.field("normal", java("LambdaParameter_Normal")), Types.field("variableArity", java("VariableArityParameter"))));
    }

    private static Definition lambdaParameter_NormalDef() {
        return typeDefHere("LambdaParameter_Normal", Types.record(Types.field("modifiers", Types.list(java("VariableModifier"))), Types.field("type", java("LambdaParameterType")), Types.field("id", java("VariableDeclaratorId"))));
    }

    private static Definition lambdaParameterTypeDef() {
        return typeDefHere("LambdaParameterType", Types.union(Types.field("type", java("UnannType")), Types.field("var", Types.unit())));
    }

    private static Definition lambdaBody_Def() {
        return typeDefHere("LambdaBody", Types.union(Types.field("expression", java("Expression")), Types.field("block", java("Block"))));
    }

    private static Definition assignmentExpressionDef() {
        return typeDefHere("AssignmentExpression", Types.union(Types.field("conditional", java("ConditionalExpression")), Types.field("assignment", java("Assignment"))));
    }

    private static Definition assignmentDef() {
        return typeDefHere("Assignment", Types.record(Types.field("lhs", java("LeftHandSide")), Types.field("op", java("AssignmentOperator")), Types.field("expression", java("Expression"))));
    }

    private static Definition leftHandSideDef() {
        return typeDefHere("LeftHandSide", Types.union(Types.field("expressionName", java("ExpressionName")), Types.field("fieldAccess", java("FieldAccess")), Types.field("arrayAccess", java("ArrayAccess"))));
    }

    private static Definition assignmentOperatorDef() {
        return typeDefHere("AssignmentOperator", Types.union(Types.field("simple", Types.unit()), Types.field("times", Types.unit()), Types.field("div", Types.unit()), Types.field("mod", Types.unit()), Types.field("plus", Types.unit()), Types.field("minus", Types.unit()), Types.field("shiftLeft", Types.unit()), Types.field("shiftRight", Types.unit()), Types.field("shiftRightZeroFill", Types.unit()), Types.field("and", Types.unit()), Types.field("xor", Types.unit()), Types.field("or", Types.unit())));
    }

    private static Definition conditionalExpressionDef() {
        return typeDefHere("ConditionalExpression", Types.union(Types.field("simple", java("ConditionalOrExpression")), Types.field("ternaryCond", java("ConditionalExpression_TernaryCond")), Types.field("ternaryLambda", java("ConditionalExpression_TernaryLambda"))));
    }

    private static Definition conditionalExpression_TernaryCondDef() {
        return typeDefHere("ConditionalExpression_TernaryCond", Types.record(Types.field("cond", java("ConditionalOrExpression")), Types.field("ifTrue", java("Expression")), Types.field("ifFalse", java("ConditionalExpression"))));
    }

    private static Definition conditionalExpression_TernaryLambdaDef() {
        return typeDefHere("ConditionalExpression_TernaryLambda", Types.record(Types.field("cond", java("ConditionalOrExpression")), Types.field("ifTrue", java("Expression")), Types.field("ifFalse", java("LambdaExpression"))));
    }

    private static Definition conditionalOrExpressionDef() {
        return typeDefHere("ConditionalOrExpression", Types.wrap(Types.list(java("ConditionalAndExpression"))));
    }

    private static Definition conditionalAndExpressionDef() {
        return typeDefHere("ConditionalAndExpression", Types.wrap(Types.list(java("InclusiveOrExpression"))));
    }

    private static Definition inclusiveOrExpressionDef() {
        return typeDefHere("InclusiveOrExpression", Types.wrap(Types.list(java("ExclusiveOrExpression"))));
    }

    private static Definition exclusiveOrExpressionDef() {
        return typeDefHere("ExclusiveOrExpression", Types.wrap(Types.list(java("AndExpression"))));
    }

    private static Definition andExpressionDef() {
        return typeDefHere("AndExpression", Types.wrap(Types.list(java("EqualityExpression"))));
    }

    private static Definition equalityExpressionDef() {
        return typeDefHere("EqualityExpression", Types.union(Types.field("unary", java("RelationalExpression")), Types.field("equal", java("EqualityExpression_Binary")), Types.field("notEqual", java("EqualityExpression_Binary"))));
    }

    private static Definition equalityExpression_BinaryDef() {
        return typeDefHere("EqualityExpression_Binary", Types.record(Types.field("lhs", java("EqualityExpression")), Types.field("rhs", java("RelationalExpression"))));
    }

    private static Definition relationalExpressionDef() {
        return typeDefHere("RelationalExpression", Types.union(Types.field("simple", java("ShiftExpression")), Types.field("lessThan", java("RelationalExpression_LessThan")), Types.field("greaterThan", java("RelationalExpression_GreaterThan")), Types.field("lessThanEqual", java("RelationalExpression_LessThanEqual")), Types.field("greaterThanEqual", java("RelationalExpression_GreaterThanEqual")), Types.field("instanceofExpression", java("InstanceofExpression"))));
    }

    private static Definition relationalExpression_LessThanDef() {
        return typeDefHere("RelationalExpression_LessThan", Types.record(Types.field("lhs", java("RelationalExpression")), Types.field("rhs", java("ShiftExpression"))));
    }

    private static Definition relationalExpression_GreaterThanDef() {
        return typeDefHere("RelationalExpression_GreaterThan", Types.record(Types.field("lhs", java("RelationalExpression")), Types.field("rhs", java("ShiftExpression"))));
    }

    private static Definition relationalExpression_LessThanEqualDef() {
        return typeDefHere("RelationalExpression_LessThanEqual", Types.record(Types.field("lhs", java("RelationalExpression")), Types.field("rhs", java("ShiftExpression"))));
    }

    private static Definition relationalExpression_GreaterThanEqualDef() {
        return typeDefHere("RelationalExpression_GreaterThanEqual", Types.record(Types.field("lhs", java("RelationalExpression")), Types.field("rhs", java("ShiftExpression"))));
    }

    private static Definition instanceofExpressionDef() {
        return typeDefHere("InstanceofExpression", Types.record(Types.field("lhs", java("RelationalExpression")), Types.field("rhs", java("InstanceofExpression_Rhs"))));
    }

    private static Definition instanceofExpression_RhsDef() {
        return typeDefHere("InstanceofExpression_Rhs", Types.union(Types.field("referenceType", java("ReferenceType")), Types.field("pattern", java("Pattern"))));
    }

    private static Definition shiftExpressionDef() {
        return typeDefHere("ShiftExpression", Types.union(Types.field("unary", java("AdditiveExpression")), Types.field("shiftLeft", java("ShiftExpression_Binary")), Types.field("shiftRight", java("ShiftExpression_Binary")), Types.field("shiftRightZeroFill", java("ShiftExpression_Binary"))));
    }

    private static Definition shiftExpression_BinaryDef() {
        return typeDefHere("ShiftExpression_Binary", Types.record(Types.field("lhs", java("ShiftExpression")), Types.field("rhs", java("AdditiveExpression"))));
    }

    private static Definition additiveExpressionDef() {
        return typeDefHere("AdditiveExpression", Types.union(Types.field("unary", java("MultiplicativeExpression")), Types.field("plus", java("AdditiveExpression_Binary")), Types.field("minus", java("AdditiveExpression_Binary"))));
    }

    private static Definition additiveExpression_BinaryDef() {
        return typeDefHere("AdditiveExpression_Binary", Types.record(Types.field("lhs", java("AdditiveExpression")), Types.field("rhs", java("MultiplicativeExpression"))));
    }

    private static Definition multiplicativeExpressionDef() {
        return typeDefHere("MultiplicativeExpression", Types.union(Types.field("unary", java("UnaryExpression")), Types.field("times", java("MultiplicativeExpression_Binary")), Types.field("divide", java("MultiplicativeExpression_Binary")), Types.field("mod", java("MultiplicativeExpression_Binary"))));
    }

    private static Definition multiplicativeExpression_BinaryDef() {
        return typeDefHere("MultiplicativeExpression_Binary", Types.record(Types.field("lhs", java("MultiplicativeExpression")), Types.field("rhs", java("UnaryExpression"))));
    }

    private static Definition unaryExpressionDef() {
        return typeDefHere("UnaryExpression", Types.union(Types.field("preIncrement", java("PreIncrementExpression")), Types.field("preDecrement", java("PreDecrementExpression")), Types.field("plus", java("UnaryExpression")), Types.field("minus", java("UnaryExpression")), Types.field("other", java("UnaryExpressionNotPlusMinus"))));
    }

    private static Definition preIncrementExpressionDef() {
        return typeDefHere("PreIncrementExpression", Types.wrap(java("UnaryExpression")));
    }

    private static Definition preDecrementExpressionDef() {
        return typeDefHere("PreDecrementExpression", Types.wrap(java("UnaryExpression")));
    }

    private static Definition unaryExpressionNotPlusMinusDef() {
        return typeDefHere("UnaryExpressionNotPlusMinus", Types.union(Types.field("postfix", java("PostfixExpression")), Types.field("tilde", java("UnaryExpression")), Types.field("not", java("UnaryExpression")), Types.field("cast", java("CastExpression")), Types.field("switchExpression", java("SwitchExpression"))));
    }

    private static Definition postfixExpressionDef() {
        return typeDefHere("PostfixExpression", Types.union(Types.field("primary", java("Primary")), Types.field("name", java("ExpressionName")), Types.field("postIncrement", java("PostIncrementExpression")), Types.field("postDecrement", java("PostDecrementExpression"))));
    }

    private static Definition postIncrementExpressionDef() {
        return typeDefHere("PostIncrementExpression", Types.wrap(java("PostfixExpression")));
    }

    private static Definition postDecrementExpressionDef() {
        return typeDefHere("PostDecrementExpression", Types.wrap(java("PostfixExpression")));
    }

    private static Definition castExpressionDef() {
        return typeDefHere("CastExpression", Types.union(Types.field("primitive", java("CastExpression_Primitive")), Types.field("notPlusMinus", java("CastExpression_NotPlusMinus")), Types.field("lambda", java("CastExpression_Lambda"))));
    }

    private static Definition castExpression_PrimitiveDef() {
        return typeDefHere("CastExpression_Primitive", Types.record(Types.field("type", java("PrimitiveTypeWithAnnotations")), Types.field("expression", java("UnaryExpression"))));
    }

    private static Definition castExpression_NotPlusMinusDef() {
        return typeDefHere("CastExpression_NotPlusMinus", Types.record(Types.field("refAndBounds", java("CastExpression_RefAndBounds")), Types.field("expression", java("UnaryExpression"))));
    }

    private static Definition castExpression_LambdaDef() {
        return typeDefHere("CastExpression_Lambda", Types.record(Types.field("refAndBounds", java("CastExpression_RefAndBounds")), Types.field("expression", java("LambdaExpression"))));
    }

    private static Definition castExpression_RefAndBoundsDef() {
        return typeDefHere("CastExpression_RefAndBounds", Types.record(Types.field("type", java("ReferenceType")), Types.field("bounds", Types.list(java("AdditionalBound")))));
    }

    private static Definition switchExpressionDef() {
        return typeDefHere("SwitchExpression", Types.record(Types.field("cond", java("Expression")), Types.field("block", java("SwitchBlock"))));
    }

    private static Definition constantExpressionDef() {
        return typeDefHere("ConstantExpression", Types.wrap(java("Expression")));
    }

    private static final List<Definition> DEFINITIONS = Arrays.asList(
        identifierDef(),
        typeIdentifierDef(),
        literalDef(),
        integerLiteralDef(),
        floatingPointLiteralDef(),
        stringLiteralDef(),
        textBlockDef(),
        type_Def(),
        primitiveTypeWithAnnotationsDef(),
        primitiveType_Def(),
        numericTypeDef(),
        integralTypeDef(),
        floatingPointTypeDef(),
        referenceTypeDef(),
        classOrInterfaceTypeDef(),
        classTypeDef(),
        classTypeQualifierDef(),
        interfaceTypeDef(),
        typeVariableDef(),
        arrayTypeDef(),
        arrayType_VariantDef(),
        dimsDef(),
        typeParameterDef(),
        typeParameterModifierDef(),
        typeBoundDef(),
        typeBound_ClassOrInterfaceDef(),
        additionalBoundDef(),
        typeArgumentDef(),
        wildcardDef(),
        wildcardBoundsDef(),
        moduleNameDef(),
        packageNameDef(),
        typeNameDef(),
        expressionNameDef(),
        methodNameDef(),
        packageOrTypeNameDef(),
        ambiguousNameDef(),
        compilationUnitDef(),
        ordinaryCompilationUnitDef(),
        modularCompilationUnitDef(),
        packageDeclarationDef(),
        packageModifierDef(),
        importDeclarationDef(),
        singleTypeImportDeclarationDef(),
        typeImportOnDemandDeclarationDef(),
        singleStaticImportDeclarationDef(),
        staticImportOnDemandDeclarationDef(),
        topLevelClassOrInterfaceDeclarationDef(),
        topLevelClassOrInterfaceDeclarationWithCommentsDef(),
        moduleDeclarationDef(),
        moduleDirectiveDef(),
        moduleDirective_RequiresDef(),
        moduleDirective_ExportsOrOpensDef(),
        moduleDirective_ProvidesDef(),
        requiresModifierDef(),
        classDeclarationDef(),
        normalClassDeclarationDef(),
        classModifierDef(),
        classBodyDef(),
        classBodyDeclarationDef(),
        classBodyDeclarationWithCommentsDef(),
        classMemberDeclarationDef(),
        fieldDeclarationDef(),
        fieldModifierDef(),
        variableDeclaratorDef(),
        variableDeclaratorIdDef(),
        variableInitializerDef(),
        unannTypeDef(),
        unannClassTypeDef(),
        methodDeclarationDef(),
        methodModifierDef(),
        methodHeaderDef(),
        resultDef(),
        methodDeclaratorDef(),
        receiverParameterDef(),
        formalParameterDef(),
        formalParameter_SimpleDef(),
        variableArityParameterDef(),
        variableModifierDef(),
        throwsDef(),
        exceptionTypeDef(),
        methodBodyDef(),
        instanceInitializerDef(),
        staticInitializerDef(),
        constructorDeclarationDef(),
        constructorModifierDef(),
        constructorDeclaratorDef(),
        simpleTypeNameDef(),
        constructorBodyDef(),
        explicitConstructorInvocationDef(),
        explicitConstructorInvocation_VariantDef(),
        enumDeclarationDef(),
        enumBodyDef(),
        enumBody_ElementDef(),
        enumConstantDef(),
        enumConstantModifierDef(),
        recordDeclarationDef(),
        recordHeaderDef(),
        recordComponentDef(),
        recordComponent_SimpleDef(),
        variableArityRecordComponentDef(),
        recordComponentModifierDef(),
        recordBodyDef(),
        recordBodyDeclarationDef(),
        compactConstructorDeclarationDef(),
        interfaceDeclarationDef(),
        normalInterfaceDeclarationDef(),
        interfaceModifierDef(),
        interfaceBodyDef(),
        interfaceMemberDeclarationDef(),
        interfaceMemberDeclarationWithCommentsDef(),
        constantDeclarationDef(),
        constantModifierDef(),
        interfaceMethodDeclarationDef(),
        interfaceMethodModifierDef(),
        annotationInterfaceDeclarationDef(),
        annotationInterfaceBodyDef(),
        annotationInterfaceMemberDeclarationDef(),
        annotationInterfaceElementDeclarationDef(),
        annotationInterfaceElementModifierDef(),
        defaultValueDef(),
        annotationDef(),
        normalAnnotationDef(),
        elementValuePairDef(),
        elementValueDef(),
        elementValueArrayInitializerDef(),
        markerAnnotationDef(),
        singleElementAnnotationDef(),
        arrayInitializerDef(),
        blockDef(),
        blockStatementDef(),
        localClassOrInterfaceDeclarationDef(),
        localVariableDeclarationStatementDef(),
        localVariableDeclarationDef(),
        localVariableTypeDef(),
        statementDef(),
        statementNoShortIfDef(),
        statementWithoutTrailingSubstatementDef(),
        labeledStatementDef(),
        labeledStatementNoShortIfDef(),
        expressionStatementDef(),
        statementExpressionDef(),
        ifThenStatementDef(),
        ifThenElseStatementDef(),
        ifThenElseStatementNoShortIfDef(),
        assertStatementDef(),
        assertStatement_PairDef(),
        switchStatementDef(),
        switchBlockDef(),
        switchBlock_LegacyDef(),
        switchRuleDef(),
        switchRule_BodyDef(),
        switchBlockStatementGroupDef(),
        switchLabelDef(),
        caseConstantDef(),
        casePatternDef(),
        guardDef(),
        pattern_Def(),
        typePatternDef(),
        recordPatternDef(),
        whileStatementDef(),
        whileStatementNoShortIfDef(),
        doStatementDef(),
        forStatementDef(),
        forStatementNoShortIfDef(),
        basicForStatementDef(),
        forCondDef(),
        basicForStatementNoShortIfDef(),
        forInitDef(),
        forUpdateDef(),
        enhancedForStatementDef(),
        enhancedForCondDef(),
        enhancedForStatementNoShortIfDef(),
        breakStatementDef(),
        yieldStatementDef(),
        continueStatementDef(),
        returnStatementDef(),
        throwStatementDef(),
        synchronizedStatementDef(),
        tryStatementDef(),
        tryStatement_SimpleDef(),
        tryStatement_WithFinallyDef(),
        catchesDef(),
        catchClauseDef(),
        catchFormalParameterDef(),
        catchTypeDef(),
        finally_Def(),
        tryWithResourcesStatementDef(),
        resourceSpecificationDef(),
        resourceDef(),
        resource_LocalDef(),
        variableAccessDef(),
        primaryDef(),
        primaryNoNewArrayExpressionDef(),
        classLiteralDef(),
        typeNameArrayDef(),
        numericTypeArrayDef(),
        booleanArrayDef(),
        classInstanceCreationExpressionDef(),
        classInstanceCreationExpression_QualifierDef(),
        unqualifiedClassInstanceCreationExpressionDef(),
        classOrInterfaceTypeToInstantiateDef(),
        annotatedIdentifierDef(),
        typeArgumentsOrDiamondDef(),
        fieldAccessDef(),
        fieldAccess_QualifierDef(),
        arrayAccessDef(),
        arrayAccess_VariantDef(),
        methodInvocationDef(),
        methodInvocation_HeaderDef(),
        methodInvocation_ComplexDef(),
        methodInvocation_VariantDef(),
        methodReferenceDef(),
        methodReference_ExpressionDef(),
        methodReference_PrimaryDef(),
        methodReference_ReferenceTypeDef(),
        methodReference_SuperDef(),
        methodReference_NewDef(),
        methodReference_ArrayDef(),
        arrayCreationExpressionDef(),
        arrayCreationExpressionWithoutInitializerDef(),
        arrayCreationExpressionWithoutInitializer_PrimitiveDef(),
        arrayCreationExpressionWithoutInitializer_ClassOrInterfaceDef(),
        arrayCreationExpressionWithInitializerDef(),
        arrayCreationExpressionWithInitializer_PrimitiveDef(),
        arrayCreationExpressionWithInitializer_ClassOrInterfaceDef(),
        dimExprDef(),
        expressionDef(),
        lambdaExpressionDef(),
        lambdaParametersDef(),
        lambdaParameter_Def(),
        lambdaParameter_NormalDef(),
        lambdaParameterTypeDef(),
        lambdaBody_Def(),
        assignmentExpressionDef(),
        assignmentDef(),
        leftHandSideDef(),
        assignmentOperatorDef(),
        conditionalExpressionDef(),
        conditionalExpression_TernaryCondDef(),
        conditionalExpression_TernaryLambdaDef(),
        conditionalOrExpressionDef(),
        conditionalAndExpressionDef(),
        inclusiveOrExpressionDef(),
        exclusiveOrExpressionDef(),
        andExpressionDef(),
        equalityExpressionDef(),
        equalityExpression_BinaryDef(),
        relationalExpressionDef(),
        relationalExpression_LessThanDef(),
        relationalExpression_GreaterThanDef(),
        relationalExpression_LessThanEqualDef(),
        relationalExpression_GreaterThanEqualDef(),
        instanceofExpressionDef(),
        instanceofExpression_RhsDef(),
        shiftExpressionDef(),
        shiftExpression_BinaryDef(),
        additiveExpressionDef(),
        additiveExpression_BinaryDef(),
        multiplicativeExpressionDef(),
        multiplicativeExpression_BinaryDef(),
        unaryExpressionDef(),
        preIncrementExpressionDef(),
        preDecrementExpressionDef(),
        unaryExpressionNotPlusMinusDef(),
        postfixExpressionDef(),
        postIncrementExpressionDef(),
        postDecrementExpressionDef(),
        castExpressionDef(),
        castExpression_PrimitiveDef(),
        castExpression_NotPlusMinusDef(),
        castExpression_LambdaDef(),
        castExpression_RefAndBoundsDef(),
        switchExpressionDef(),
        constantExpressionDef()
    );

    private static final List<Namespace> DEPENDENCIES = Arrays.asList(CORE_NS);

    public static final Module module_ = new Module(
        Maybe.just("A Java syntax module. Tracks the Oracle Java SE 21 BNF:\n  https://docs.oracle.com/javase/specs/jls/se21/html/jls-19.html\nNote: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments."),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
