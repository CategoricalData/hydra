package hydra.sources.java;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Core;
import hydra.dsl.Packaging;
import hydra.dsl.Types;
import hydra.dsl.java.Environment;
import hydra.dsl.java.Syntax;
import hydra.dsl.meta.lib.Eithers;
import hydra.dsl.meta.lib.Equality;
import hydra.dsl.meta.lib.Lists;
import hydra.dsl.meta.lib.Literals;
import hydra.dsl.meta.lib.Logic;
import hydra.dsl.meta.lib.Maps;
import hydra.dsl.meta.lib.Math_;
import hydra.dsl.meta.lib.Maybes;
import hydra.dsl.meta.lib.Pairs;
import hydra.dsl.meta.lib.Sets;
import hydra.dsl.meta.lib.Strings;
import hydra.packaging.Definition;
import hydra.packaging.EntityMetadata;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.ModuleDependency;
import hydra.typed.TypedTerm;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;

import static hydra.dsl.meta.Phantoms.*;
import hydra.dsl.meta.Defs.Def;
import static hydra.dsl.meta.Defs.define;
import static hydra.dsl.meta.Defs.unqualifiedDeps;
import static hydra.dsl.meta.Defs.ref;
import static hydra.dsl.meta.Defs.definitionsOf;
import java.util.function.Supplier;
import hydra.java.syntax.AdditionalBound;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AdditiveExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AdditiveExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AmbiguousName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AndExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AnnotatedIdentifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Annotation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayCreationExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayCreationExpressionWithInitializer;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayInitializer;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayType_Variant;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Assignment;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AssignmentExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AssignmentOperator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Block;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.BlockStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.BreakStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression_NotPlusMinus;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression_Primitive;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression_RefAndBounds;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBodyDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBodyDeclarationWithComments;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassInstanceCreationExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassInstanceCreationExpression_Qualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassMemberDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassOrInterfaceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassOrInterfaceTypeToInstantiate;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassTypeQualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CompilationUnit;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalAndExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalOrExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstantDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorDeclarator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ContinueStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Dims;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ElementValue;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ElementValueArrayInitializer;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ElementValuePair;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.EqualityExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.EqualityExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExclusiveOrExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Expression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExpressionName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExpressionStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldAccess;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldAccess_Qualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FloatingPointLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FloatingPointType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FormalParameter;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FormalParameter_Simple;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Identifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IfThenStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ImportDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InclusiveOrExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InstanceofExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InstanceofExpression_Rhs;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IntegerLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IntegralType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMemberDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMemberDeclarationWithComments;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMethodDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMethodModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaParameters;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LeftHandSide;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Literal;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LocalClassOrInterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LocalVariableDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LocalVariableDeclarationStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LocalVariableType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MarkerAnnotation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodDeclarator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodHeader;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation_Complex;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation_Header;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation_Variant;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MultiplicativeExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MultiplicativeExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NormalAnnotation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NormalClassDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NormalInterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NumericType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.OrdinaryCompilationUnit;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageOrTypeName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PostfixExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Primary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimaryNoNewArrayExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimitiveType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimitiveTypeWithAnnotations;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ReferenceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.RelationalExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.RelationalExpression_GreaterThan;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.RelationalExpression_GreaterThanEqual;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.RelationalExpression_LessThan;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.RelationalExpression_LessThanEqual;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Result;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ReturnStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ShiftExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ShiftExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.SimpleTypeName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.SingleElementAnnotation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.SingleTypeImportDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Statement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StatementExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StatementWithoutTrailingSubstatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StringLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ThrowStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TopLevelClassOrInterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeArgument;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeArgumentsOrDiamond;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeBound;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeBound_ClassOrInterface;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeIdentifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeParameter;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeParameterModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeVariable;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnannType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnaryExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnaryExpressionNotPlusMinus;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnqualifiedClassInstanceCreationExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableDeclarator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableDeclaratorId;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableInitializer;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.WhileStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Wildcard;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.WildcardBounds;  // AUTO-IMPORT (hydra-java DSL)

/**
 * Java serializer: converts Java AST to concrete syntax.
 *
 * <p>Mirror of {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Serde.hs}.</p>
 *
 * <p>Partial port: ~33 defs for Java statement/expression constructs (e.g. {@code switch},
 * {@code try}, {@code for}, {@code methodReference}, pre/post-increment) emit a
 * {@code "STUB:..."} placeholder rather than real concrete syntax. These constructs lie
 * <em>outside the kernel-codegen surface</em> — Hydra's generated Java never produces them —
 * so the stubs are not reachable in practice (the Java self-host bootstrap is green, and no
 * {@code STUB:} string appears in any generated kernel/test output). They remain for
 * completeness of the Java serializer as a general tool. The cross-coder dependency the Scala
 * coder relies on, {@code escapeJavaString}, IS fully implemented here (not a stub).</p>
 */
public class Serde {
    public static final ModuleName NS = new ModuleName("hydra.java.serde");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def additionalBoundToExpr = def(
        "additionalBoundToExpr",
        () -> lambda("ab",
                apply(
                    var("hydra.serialization.spaceSep"),
                    list(
                        apply(var("hydra.serialization.cst"), string("&")),
                        apply(
                            ref(Serde.interfaceTypeToExpr),
                            apply(unwrap(AdditionalBound.TYPE_), var("ab")))))));

    public static final Def additiveExpressionToExpr = def(
        "additiveExpressionToExpr",
        () -> lambda("e",
                cases(AdditiveExpression.TYPE_,
                    var("e"),
                    field(
                        AdditiveExpression.UNARY,
                        lambda("m", apply(ref(Serde.multiplicativeExpressionToExpr), var("m")))),
                    field(
                        AdditiveExpression.PLUS,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("+"),
                                apply(
                                    ref(Serde.additiveExpressionToExpr),
                                    proj(AdditiveExpression_Binary.TYPE_, AdditiveExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.multiplicativeExpressionToExpr),
                                    proj(AdditiveExpression_Binary.TYPE_, AdditiveExpression_Binary.RHS, "b"))))),
                    field(
                        AdditiveExpression.MINUS,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("-"),
                                apply(
                                    ref(Serde.additiveExpressionToExpr),
                                    proj(AdditiveExpression_Binary.TYPE_, AdditiveExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.multiplicativeExpressionToExpr),
                                    proj(AdditiveExpression_Binary.TYPE_, AdditiveExpression_Binary.RHS, "b"))))))));

    public static final Def ambiguousNameToExpr = def(
        "ambiguousNameToExpr",
        () -> lambda("an",
                apply(
                    var("hydra.serialization.dotSep"),
                    Lists.map(
                        ref(Serde.identifierToExpr),
                        apply(unwrap(AmbiguousName.TYPE_), var("an"))))));

    public static final Def andExpressionToExpr = def(
        "andExpressionToExpr",
        () -> lambda("ae",
                apply(
                    var("hydra.serialization.infixWsList"),
                    string("&"),
                    Lists.map(
                        ref(Serde.equalityExpressionToExpr),
                        apply(unwrap(AndExpression.TYPE_), var("ae"))))));

    public static final Def annotatedIdentifierToExpr = def(
        "annotatedIdentifierToExpr",
        () -> lambda("ai",
                apply(
                    ref(Serde.identifierToExpr),
                    proj(AnnotatedIdentifier.TYPE_, AnnotatedIdentifier.IDENTIFIER, "ai"))));

    public static final Def annotationToExpr = def(
        "annotationToExpr",
        () -> lambda("ann",
                cases(Annotation.TYPE_,
                    var("ann"),
                    field(
                        Annotation.NORMAL,
                        lambda("n", apply(ref(Serde.normalAnnotationToExpr), var("n")))),
                    field(
                        Annotation.MARKER,
                        lambda("m", apply(ref(Serde.markerAnnotationToExpr), var("m")))),
                    field(
                        Annotation.SINGLE_ELEMENT,
                        lambda("s", apply(ref(Serde.singleElementAnnotationToExpr), var("s")))))));

    public static final Def annotationTypeDeclarationToExpr = def(
        "annotationTypeDeclarationToExpr",
        () -> constant(
                apply(var("hydra.serialization.cst"), string("STUB:AnnotationInterfaceDeclaration"))));

    public static final Def arrayAccessToExpr = def(
        "arrayAccessToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:ArrayAccess"))));

    public static final Def arrayCreationExpressionToExpr = def(
        "arrayCreationExpressionToExpr",
        () -> lambda("ace",
                cases(ArrayCreationExpression.TYPE_,
                    var("ace"),
                    field(
                        ArrayCreationExpression.WITHOUT_INITIALIZER,
                        constant(
                            apply(
                                var("hydra.serialization.cst"),
                                string("STUB:ArrayCreationExpression")))),
                    field(
                        ArrayCreationExpression.WITH_INITIALIZER,
                        lambda("wi",
                            cases(ArrayCreationExpressionWithInitializer.TYPE_,
                                var("wi"),
                                field(
                                    ArrayCreationExpressionWithInitializer.PRIMITIVE,
                                    lambda("pa",
                                        let(
                                            field("pt",
                                                proj(ArrayCreationExpressionWithInitializer_Primitive.TYPE_, ArrayCreationExpressionWithInitializer_Primitive.TYPE, "pa")),
                                            field("ai",
                                                proj(ArrayCreationExpressionWithInitializer_Primitive.TYPE_, ArrayCreationExpressionWithInitializer_Primitive.ARRAY, "pa")),
                                            apply(
                                                var("hydra.serialization.spaceSep"),
                                                list(
                                                    apply(
                                                        var("hydra.serialization.cst"),
                                                        string("new")),
                                                    apply(
                                                        var("hydra.serialization.noSep"),
                                                        list(
                                                            apply(
                                                                ref(Serde.primitiveTypeWithAnnotationsToExpr),
                                                                var("pt")),
                                                            apply(
                                                                var("hydra.serialization.cst"),
                                                                string("[]")))),
                                                    apply(
                                                        ref(Serde.arrayInitializerToExpr),
                                                        var("ai"))))))),
                                field(
                                    ArrayCreationExpressionWithInitializer.CLASS_OR_INTERFACE,
                                    constant(
                                        apply(
                                            var("hydra.serialization.cst"),
                                            string("STUB:ArrayCreationExpression"))))))))));

    public static final Def arrayInitializerToExpr = def(
        "arrayInitializerToExpr",
        () -> lambda("ai",
                let("groups",
                    apply(unwrap(ArrayInitializer.TYPE_), var("ai")),
                    Maybes.fromMaybe(
                        apply(var("hydra.serialization.cst"), string("{}")),
                        Maybes.map(
                            lambda("firstGroup",
                                Logic.ifElse(
                                    Equality.equal(Lists.length(var("groups")), int32(1)),
                                    apply(
                                        var("hydra.serialization.noSep"),
                                        list(
                                            apply(var("hydra.serialization.cst"), string("{")),
                                            apply(
                                                var("hydra.serialization.commaSep"),
                                                var("hydra.serialization.inlineStyle"),
                                                Lists.map(
                                                    ref(Serde.variableInitializerToExpr),
                                                    var("firstGroup"))),
                                            apply(var("hydra.serialization.cst"), string("}")))),
                                    apply(var("hydra.serialization.cst"), string("{}")))),
                            Lists.maybeHead(var("groups")))))));

    public static final Def arrayTypeToExpr = def(
        "arrayTypeToExpr",
        () -> lambda("at",
                let(
                    field("dims",
                        proj(ArrayType.TYPE_, ArrayType.DIMS, "at")),
                    field("variant",
                        proj(ArrayType.TYPE_, ArrayType.VARIANT, "at")),
                    field("varExpr",
                        cases(ArrayType_Variant.TYPE_,
                            var("variant"),
                            field(
                                ArrayType_Variant.PRIMITIVE,
                                lambda("pt",
                                    apply(ref(Serde.primitiveTypeWithAnnotationsToExpr), var("pt")))),
                            field(
                                ArrayType_Variant.CLASS_OR_INTERFACE,
                                lambda("cit",
                                    apply(ref(Serde.classOrInterfaceTypeToExpr), var("cit")))),
                            field(
                                ArrayType_Variant.VARIABLE,
                                lambda("tv", apply(ref(Serde.typeVariableToExpr), var("tv")))))),
                    apply(
                        var("hydra.serialization.noSep"),
                        list(var("varExpr"), apply(ref(Serde.dimsToExpr), var("dims")))))));

    public static final Def assertStatementToExpr = def(
        "assertStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:AssertStatement"))));

    public static final Def assignmentExpressionToExpr = def(
        "assignmentExpressionToExpr",
        () -> lambda("e",
                cases(AssignmentExpression.TYPE_,
                    var("e"),
                    field(
                        AssignmentExpression.CONDITIONAL,
                        lambda("c", apply(ref(Serde.conditionalExpressionToExpr), var("c")))),
                    field(
                        AssignmentExpression.ASSIGNMENT,
                        lambda("a", apply(ref(Serde.assignmentToExpr), var("a")))))));

    public static final Def assignmentToExpr = def(
        "assignmentToExpr",
        () -> lambda("a",
                let(
                    field("lhs",
                        proj(Assignment.TYPE_, Assignment.LHS, "a")),
                    field("op",
                        proj(Assignment.TYPE_, Assignment.OP, "a")),
                    field("rhs",
                        proj(Assignment.TYPE_, Assignment.EXPRESSION, "a")),
                    field("ctop",
                        cases(AssignmentOperator.TYPE_,
                            var("op"),
                            field(
                                AssignmentOperator.SIMPLE,
                                constant(string("="))),
                            field(
                                AssignmentOperator.TIMES,
                                constant(string("*="))),
                            field(AssignmentOperator.DIV, constant(string("/="))),
                            field(AssignmentOperator.MOD, constant(string("%="))),
                            field(AssignmentOperator.PLUS, constant(string("+="))),
                            field(
                                AssignmentOperator.MINUS,
                                constant(string("-="))),
                            field(
                                AssignmentOperator.SHIFT_LEFT,
                                constant(string("<<="))),
                            field(
                                AssignmentOperator.SHIFT_RIGHT,
                                constant(string(">>="))),
                            field(
                                AssignmentOperator.SHIFT_RIGHT_ZERO_FILL,
                                constant(string(">>>="))),
                            field(AssignmentOperator.AND, constant(string("&="))),
                            field(AssignmentOperator.XOR, constant(string("^="))),
                            field(AssignmentOperator.OR, constant(string("|="))))),
                    apply(
                        var("hydra.serialization.infixWs"),
                        var("ctop"),
                        apply(ref(Serde.leftHandSideToExpr), var("lhs")),
                        apply(ref(Serde.expressionToExpr), var("rhs"))))));

    public static final Def blockStatementToExpr = def(
        "blockStatementToExpr",
        () -> lambda("s",
                cases(BlockStatement.TYPE_,
                    var("s"),
                    field(
                        BlockStatement.LOCAL_VARIABLE_DECLARATION,
                        lambda("d",
                            apply(ref(Serde.localVariableDeclarationStatementToExpr), var("d")))),
                    field(
                        BlockStatement.LOCAL_CLASS_OR_INTERFACE,
                        lambda("lcid",
                            cases(LocalClassOrInterfaceDeclaration.TYPE_,
                                var("lcid"),
                                field(
                                    LocalClassOrInterfaceDeclaration.CLASS,
                                    lambda("cd",
                                        apply(ref(Serde.classDeclarationToExpr), var("cd")))),
                                field(
                                    LocalClassOrInterfaceDeclaration.NORMAL_INTERFACE,
                                    lambda("nid",
                                        apply(
                                            ref(Serde.interfaceDeclarationToExpr),
                                            inject(InterfaceDeclaration.TYPE_,
                                                InterfaceDeclaration.NORMAL_INTERFACE,
                                                var("nid")))))))),
                    field(
                        BlockStatement.STATEMENT,
                        lambda("s", apply(ref(Serde.statementToExpr), var("s")))))));

    public static final Def blockToExpr = def(
        "blockToExpr",
        () -> lambda("b",
                apply(
                    var("hydra.serialization.curlyBlock"),
                    var("hydra.serialization.fullBlockStyle"),
                    apply(
                        var("hydra.serialization.newlineSep"),
                        Lists.map(
                            ref(Serde.blockStatementToExpr),
                            apply(unwrap(Block.TYPE_), var("b")))))));

    public static final Def breakStatementToExpr = def(
        "breakStatementToExpr",
        () -> lambda("bs",
                let("mlabel",
                    apply(unwrap(BreakStatement.TYPE_), var("bs")),
                    apply(
                        var("hydra.serialization.withSemi"),
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    just(apply(var("hydra.serialization.cst"), string("break"))),
                                    Maybes.map(ref(Serde.identifierToExpr), var("mlabel")))))))));

    public static final Def castExpressionLambdaToExpr = def(
        "castExpressionLambdaToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:CastExpression_Lambda"))));

    public static final Def castExpressionNotPlusMinusToExpr = def(
        "castExpressionNotPlusMinusToExpr",
        () -> lambda("npm",
                let(
                    field("rb",
                        proj(CastExpression_NotPlusMinus.TYPE_, CastExpression_NotPlusMinus.REF_AND_BOUNDS, "npm")),
                    field("ex",
                        proj(CastExpression_NotPlusMinus.TYPE_, CastExpression_NotPlusMinus.EXPRESSION, "npm")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        list(
                            apply(ref(Serde.castExpressionRefAndBoundsToExpr), var("rb")),
                            apply(ref(Serde.unaryExpressionToExpr), var("ex")))))));

    public static final Def castExpressionPrimitiveToExpr = def(
        "castExpressionPrimitiveToExpr",
        () -> lambda("cp",
                let(
                    field("pt",
                        proj(CastExpression_Primitive.TYPE_, CastExpression_Primitive.TYPE, "cp")),
                    field("ex",
                        proj(CastExpression_Primitive.TYPE_, CastExpression_Primitive.EXPRESSION, "cp")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        list(
                            apply(
                                var("hydra.serialization.parenList"),
                                bool(false),
                                list(
                                    apply(ref(Serde.primitiveTypeWithAnnotationsToExpr), var("pt")))),
                            apply(ref(Serde.unaryExpressionToExpr), var("ex")))))));

    public static final Def castExpressionRefAndBoundsToExpr = def(
        "castExpressionRefAndBoundsToExpr",
        () -> lambda("rab",
                let(
                    field("rt",
                        proj(CastExpression_RefAndBounds.TYPE_, CastExpression_RefAndBounds.TYPE, "rab")),
                    field("adds",
                        proj(CastExpression_RefAndBounds.TYPE_, CastExpression_RefAndBounds.BOUNDS, "rab")),
                    apply(
                        var("hydra.serialization.parenList"),
                        bool(false),
                        list(
                            apply(
                                var("hydra.serialization.spaceSep"),
                                Maybes.cat(
                                    list(
                                        just(apply(ref(Serde.referenceTypeToExpr), var("rt"))),
                                        Logic.ifElse(
                                            Lists.null_(var("adds")),
                                            nothing(),
                                            just(
                                                apply(
                                                    var("hydra.serialization.spaceSep"),
                                                    Lists.map(
                                                        ref(Serde.additionalBoundToExpr),
                                                        var("adds")))))))))))));

    public static final Def castExpressionToExpr = def(
        "castExpressionToExpr",
        () -> lambda("e",
                cases(CastExpression.TYPE_,
                    var("e"),
                    field(
                        CastExpression.PRIMITIVE,
                        lambda("p", apply(ref(Serde.castExpressionPrimitiveToExpr), var("p")))),
                    field(
                        CastExpression.NOT_PLUS_MINUS,
                        lambda("npm",
                            apply(ref(Serde.castExpressionNotPlusMinusToExpr), var("npm")))),
                    field(
                        CastExpression.LAMBDA,
                        lambda("l", apply(ref(Serde.castExpressionLambdaToExpr), var("l")))))));

    public static final Def classBodyDeclarationToExpr = def(
        "classBodyDeclarationToExpr",
        () -> lambda("d",
                cases(ClassBodyDeclaration.TYPE_,
                    var("d"),
                    field(
                        ClassBodyDeclaration.CLASS_MEMBER,
                        lambda("d", apply(ref(Serde.classMemberDeclarationToExpr), var("d")))),
                    field(
                        ClassBodyDeclaration.INSTANCE_INITIALIZER,
                        lambda("i", apply(ref(Serde.instanceInitializerToExpr), var("i")))),
                    field(
                        ClassBodyDeclaration.STATIC_INITIALIZER,
                        lambda("i", apply(ref(Serde.staticInitializerToExpr), var("i")))),
                    field(
                        ClassBodyDeclaration.CONSTRUCTOR_DECLARATION,
                        lambda("d", apply(ref(Serde.constructorDeclarationToExpr), var("d")))))));

    public static final Def classBodyDeclarationWithCommentsToExpr = def(
        "classBodyDeclarationWithCommentsToExpr",
        () -> lambda("cbdwc",
                let(
                    field("d",
                        proj(ClassBodyDeclarationWithComments.TYPE_, ClassBodyDeclarationWithComments.VALUE, "cbdwc")),
                    field("mc",
                        proj(ClassBodyDeclarationWithComments.TYPE_, ClassBodyDeclarationWithComments.COMMENTS, "cbdwc")),
                    apply(
                        ref(Serde.withComments),
                        var("mc"),
                        apply(ref(Serde.classBodyDeclarationToExpr), var("d"))))));

    public static final Def classBodyToExpr = def(
        "classBodyToExpr",
        () -> lambda("cb",
                apply(
                    var("hydra.serialization.curlyBlock"),
                    var("hydra.serialization.fullBlockStyle"),
                    apply(
                        var("hydra.serialization.doubleNewlineSep"),
                        Lists.map(
                            ref(Serde.classBodyDeclarationWithCommentsToExpr),
                            apply(unwrap(ClassBody.TYPE_), var("cb")))))));

    public static final Def classDeclarationToExpr = def(
        "classDeclarationToExpr",
        () -> lambda("d",
                cases(ClassDeclaration.TYPE_,
                    var("d"),
                    field(
                        ClassDeclaration.NORMAL,
                        lambda("nd", apply(ref(Serde.normalClassDeclarationToExpr), var("nd")))),
                    field(
                        ClassDeclaration.ENUM,
                        lambda("ed", apply(ref(Serde.enumDeclarationToExpr), var("ed")))))));

    public static final Def classInstanceCreationExpressionQualifierToExpr = def(
        "classInstanceCreationExpressionQualifierToExpr",
        () -> lambda("q",
                cases(ClassInstanceCreationExpression_Qualifier.TYPE_,
                    var("q"),
                    field(
                        ClassInstanceCreationExpression_Qualifier.EXPRESSION,
                        lambda("en", apply(ref(Serde.expressionNameToExpr), var("en")))),
                    field(
                        ClassInstanceCreationExpression_Qualifier.PRIMARY,
                        lambda("p", apply(ref(Serde.primaryToExpr), var("p")))))));

    public static final Def classInstanceCreationExpressionToExpr = def(
        "classInstanceCreationExpressionToExpr",
        () -> lambda("cice",
                let(
                    field("mqual",
                        proj(ClassInstanceCreationExpression.TYPE_, ClassInstanceCreationExpression.QUALIFIER, "cice")),
                    field("e",
                        proj(ClassInstanceCreationExpression.TYPE_, ClassInstanceCreationExpression.EXPRESSION, "cice")),
                    Maybes.cases(var("mqual"), apply(ref(Serde.unqualifiedClassInstanceCreationExpressionToExpr), var("e")), lambda("q",
                            apply(
                                var("hydra.serialization.dotSep"),
                                list(
                                    apply(
                                        ref(Serde.classInstanceCreationExpressionQualifierToExpr),
                                        var("q")),
                                    apply(
                                        ref(Serde.unqualifiedClassInstanceCreationExpressionToExpr),
                                        var("e")))))))));

    public static final Def classLiteralToExpr = def(
        "classLiteralToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:ClassLiteral"))));

    public static final Def classMemberDeclarationToExpr = def(
        "classMemberDeclarationToExpr",
        () -> lambda("d",
                cases(ClassMemberDeclaration.TYPE_,
                    var("d"),
                    field(
                        ClassMemberDeclaration.FIELD,
                        lambda("fd", apply(ref(Serde.fieldDeclarationToExpr), var("fd")))),
                    field(
                        ClassMemberDeclaration.METHOD,
                        lambda("md", apply(ref(Serde.methodDeclarationToExpr), var("md")))),
                    field(
                        ClassMemberDeclaration.CLASS,
                        lambda("cd", apply(ref(Serde.classDeclarationToExpr), var("cd")))),
                    field(
                        ClassMemberDeclaration.INTERFACE,
                        lambda("id", apply(ref(Serde.interfaceDeclarationToExpr), var("id")))),
                    field(
                        ClassMemberDeclaration.NONE,
                        constant(apply(var("hydra.serialization.cst"), string(";")))))));

    public static final Def classModifierToExpr = def(
        "classModifierToExpr",
        () -> lambda("m",
                cases(ClassModifier.TYPE_,
                    var("m"),
                    field(
                        ClassModifier.ANNOTATION,
                        lambda("ann", apply(ref(Serde.annotationToExpr), var("ann")))),
                    field(
                        ClassModifier.PUBLIC,
                        constant(apply(var("hydra.serialization.cst"), string("public")))),
                    field(
                        ClassModifier.PROTECTED,
                        constant(apply(var("hydra.serialization.cst"), string("protected")))),
                    field(
                        ClassModifier.PRIVATE,
                        constant(apply(var("hydra.serialization.cst"), string("private")))),
                    field(
                        ClassModifier.ABSTRACT,
                        constant(apply(var("hydra.serialization.cst"), string("abstract")))),
                    field(
                        ClassModifier.STATIC,
                        constant(apply(var("hydra.serialization.cst"), string("static")))),
                    field(
                        ClassModifier.FINAL,
                        constant(apply(var("hydra.serialization.cst"), string("final")))),
                    field(
                        ClassModifier.STRICTFP,
                        constant(apply(var("hydra.serialization.cst"), string("strictfp")))))));

    public static final Def classOrInterfaceTypeToExpr = def(
        "classOrInterfaceTypeToExpr",
        () -> lambda("cit",
                cases(ClassOrInterfaceType.TYPE_,
                    var("cit"),
                    field(
                        ClassOrInterfaceType.CLASS,
                        lambda("ct", apply(ref(Serde.classTypeToExpr), var("ct")))),
                    field(
                        ClassOrInterfaceType.INTERFACE,
                        lambda("it", apply(ref(Serde.interfaceTypeToExpr), var("it")))))));

    public static final Def classOrInterfaceTypeToInstantiateToExpr = def(
        "classOrInterfaceTypeToInstantiateToExpr",
        () -> lambda("coitti",
                let(
                    field("ids",
                        proj(ClassOrInterfaceTypeToInstantiate.TYPE_, ClassOrInterfaceTypeToInstantiate.IDENTIFIERS, "coitti")),
                    field("margs",
                        proj(ClassOrInterfaceTypeToInstantiate.TYPE_, ClassOrInterfaceTypeToInstantiate.TYPE_ARGUMENTS, "coitti")),
                    apply(
                        var("hydra.serialization.noSep"),
                        Maybes.cat(
                            list(
                                just(
                                    apply(
                                        var("hydra.serialization.dotSep"),
                                        Lists.map(ref(Serde.annotatedIdentifierToExpr), var("ids")))),
                                Maybes.map(ref(Serde.typeArgumentsOrDiamondToExpr), var("margs"))))))));

    public static final Def classTypeToExpr = def(
        "classTypeToExpr",
        () -> lambda("ct",
                let(
                    field("anns",
                        proj(ClassType.TYPE_, ClassType.ANNOTATIONS, "ct")),
                    field("qual",
                        proj(ClassType.TYPE_, ClassType.QUALIFIER, "ct")),
                    field("id",
                        proj(ClassType.TYPE_, ClassType.IDENTIFIER, "ct")),
                    field("args",
                        proj(ClassType.TYPE_, ClassType.ARGUMENTS, "ct")),
                    field("qualifiedId",
                        cases(ClassTypeQualifier.TYPE_,
                            var("qual"),
                            field(
                                ClassTypeQualifier.NONE,
                                constant(apply(ref(Serde.typeIdentifierToExpr), var("id")))),
                            field(
                                ClassTypeQualifier.PACKAGE,
                                lambda("pkg",
                                    apply(
                                        var("hydra.serialization.dotSep"),
                                        list(
                                            apply(ref(Serde.packageNameToExpr), var("pkg")),
                                            apply(ref(Serde.typeIdentifierToExpr), var("id")))))),
                            field(
                                ClassTypeQualifier.PARENT,
                                lambda("cit",
                                    apply(
                                        var("hydra.serialization.dotSep"),
                                        list(
                                            apply(ref(Serde.classOrInterfaceTypeToExpr), var("cit")),
                                            apply(ref(Serde.typeIdentifierToExpr), var("id")))))))),
                    apply(
                        var("hydra.serialization.noSep"),
                        Maybes.cat(
                            list(
                                just(
                                    apply(
                                        var("hydra.serialization.spaceSep"),
                                        Maybes.cat(
                                            list(
                                                Logic.ifElse(
                                                    Lists.null_(var("anns")),
                                                    nothing(),
                                                    just(
                                                        apply(
                                                            var("hydra.serialization.commaSep"),
                                                            var("hydra.serialization.inlineStyle"),
                                                            Lists.map(
                                                                ref(Serde.annotationToExpr),
                                                                var("anns"))))),
                                                just(var("qualifiedId")))))),
                                Logic.ifElse(
                                    Lists.null_(var("args")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.angleBracesList"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(ref(Serde.typeArgumentToExpr), var("args")))))))))));

    public static final Def compilationUnitToExpr = def(
        "compilationUnitToExpr",
        () -> lambda("u",
                cases(CompilationUnit.TYPE_,
                    var("u"),
                    field(
                        CompilationUnit.ORDINARY,
                        lambda("ocu",
                            let(
                                field("mpkg",
                                    proj(OrdinaryCompilationUnit.TYPE_, OrdinaryCompilationUnit.PACKAGE, "ocu")),
                                field("imports",
                                    proj(OrdinaryCompilationUnit.TYPE_, OrdinaryCompilationUnit.IMPORTS, "ocu")),
                                field("types",
                                    proj(OrdinaryCompilationUnit.TYPE_, OrdinaryCompilationUnit.TYPES, "ocu")),
                                field("warning",
                                    just(
                                        apply(
                                            ref(Serde.singleLineComment),
                                            var("hydra.constants.warningAutoGeneratedFile")))),
                                field("pkgSec",
                                    Maybes.map(ref(Serde.packageDeclarationToExpr), var("mpkg"))),
                                field("importsSec",
                                    Logic.ifElse(
                                        Lists.null_(var("imports")),
                                        nothing(),
                                        just(
                                            apply(
                                                var("hydra.serialization.newlineSep"),
                                                Lists.map(
                                                    ref(Serde.importDeclarationToExpr),
                                                    var("imports")))))),
                                field("typesSec",
                                    Logic.ifElse(
                                        Lists.null_(var("types")),
                                        nothing(),
                                        just(
                                            apply(
                                                var("hydra.serialization.doubleNewlineSep"),
                                                Lists.map(
                                                    ref(Serde.typeDeclarationWithCommentsToExpr),
                                                    var("types")))))),
                                apply(
                                    var("hydra.serialization.doubleNewlineSep"),
                                    Maybes.cat(
                                        list(
                                            var("warning"),
                                            var("pkgSec"),
                                            var("importsSec"),
                                            var("typesSec"))))))))));

    public static final Def conditionalAndExpressionToExpr = def(
        "conditionalAndExpressionToExpr",
        () -> lambda("cae",
                apply(
                    var("hydra.serialization.infixWsList"),
                    string("&&"),
                    Lists.map(
                        ref(Serde.inclusiveOrExpressionToExpr),
                        apply(unwrap(ConditionalAndExpression.TYPE_), var("cae"))))));

    public static final Def conditionalExpressionTernaryCondToExpr = def(
        "conditionalExpressionTernaryCondToExpr",
        () -> constant(
                apply(
                    var("hydra.serialization.cst"),
                    string("STUB:ConditionalExpression_TernaryCond"))));

    public static final Def conditionalExpressionTernaryLambdaToExpr = def(
        "conditionalExpressionTernaryLambdaToExpr",
        () -> constant(
                apply(
                    var("hydra.serialization.cst"),
                    string("STUB:ConditionalExpression_TernaryLambda"))));

    public static final Def conditionalExpressionToExpr = def(
        "conditionalExpressionToExpr",
        () -> lambda("c",
                cases(ConditionalExpression.TYPE_,
                    var("c"),
                    field(
                        ConditionalExpression.SIMPLE,
                        lambda("co", apply(ref(Serde.conditionalOrExpressionToExpr), var("co")))),
                    field(
                        ConditionalExpression.TERNARY_COND,
                        lambda("tc",
                            apply(ref(Serde.conditionalExpressionTernaryCondToExpr), var("tc")))),
                    field(
                        ConditionalExpression.TERNARY_LAMBDA,
                        lambda("tl",
                            apply(ref(Serde.conditionalExpressionTernaryLambdaToExpr), var("tl")))))));

    public static final Def conditionalOrExpressionToExpr = def(
        "conditionalOrExpressionToExpr",
        () -> lambda("coe",
                apply(
                    var("hydra.serialization.infixWsList"),
                    string("||"),
                    Lists.map(
                        ref(Serde.conditionalAndExpressionToExpr),
                        apply(unwrap(ConditionalOrExpression.TYPE_), var("coe"))))));

    public static final Def constantDeclarationToExpr = def(
        "constantDeclarationToExpr",
        () -> lambda("cd",
                let(
                    field("mods",
                        proj(ConstantDeclaration.TYPE_, ConstantDeclaration.MODIFIERS, "cd")),
                    field("typ",
                        proj(ConstantDeclaration.TYPE_, ConstantDeclaration.TYPE, "cd")),
                    field("vars",
                        proj(ConstantDeclaration.TYPE_, ConstantDeclaration.VARIABLES, "cd")),
                    apply(
                        var("hydra.serialization.withSemi"),
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    Logic.ifElse(
                                        Lists.null_(var("mods")),
                                        nothing(),
                                        just(
                                            apply(
                                                var("hydra.serialization.spaceSep"),
                                                Lists.map(
                                                    ref(Serde.constantModifierToExpr),
                                                    var("mods"))))),
                                    just(apply(ref(Serde.unannTypeToExpr), var("typ"))),
                                    just(
                                        apply(
                                            var("hydra.serialization.commaSep"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(
                                                ref(Serde.variableDeclaratorToExpr),
                                                var("vars")))))))))));

    public static final Def constantModifierToExpr = def(
        "constantModifierToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:ConstantModifier"))));

    public static final Def constructorBodyToExpr = def(
        "constructorBodyToExpr",
        () -> lambda("cb",
                let(
                    field("minvoc",
                        proj(ConstructorBody.TYPE_, ConstructorBody.INVOCATION, "cb")),
                    field("stmts",
                        proj(ConstructorBody.TYPE_, ConstructorBody.STATEMENTS, "cb")),
                    apply(
                        var("hydra.serialization.curlyBlock"),
                        var("hydra.serialization.fullBlockStyle"),
                        apply(
                            var("hydra.serialization.doubleNewlineSep"),
                            Maybes.cat(
                                list(
                                    Maybes.map(
                                        ref(Serde.explicitConstructorInvocationToExpr),
                                        var("minvoc")),
                                    just(
                                        apply(
                                            var("hydra.serialization.newlineSep"),
                                            Lists.map(ref(Serde.blockStatementToExpr), var("stmts")))))))))));

    public static final Def constructorDeclarationToExpr = def(
        "constructorDeclarationToExpr",
        () -> lambda("cd",
                let(
                    field("mods",
                        proj(ConstructorDeclaration.TYPE_, ConstructorDeclaration.MODIFIERS, "cd")),
                    field("cons",
                        proj(ConstructorDeclaration.TYPE_, ConstructorDeclaration.CONSTRUCTOR, "cd")),
                    field("mthrows",
                        proj(ConstructorDeclaration.TYPE_, ConstructorDeclaration.THROWS, "cd")),
                    field("body",
                        proj(ConstructorDeclaration.TYPE_, ConstructorDeclaration.BODY, "cd")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(
                                                ref(Serde.constructorModifierToExpr),
                                                var("mods"))))),
                                just(apply(ref(Serde.constructorDeclaratorToExpr), var("cons"))),
                                Maybes.map(ref(Serde.throwsToExpr), var("mthrows")),
                                just(apply(ref(Serde.constructorBodyToExpr), var("body")))))))));

    public static final Def constructorDeclaratorToExpr = def(
        "constructorDeclaratorToExpr",
        () -> lambda("cd",
                let(
                    field("tparams",
                        proj(ConstructorDeclarator.TYPE_, ConstructorDeclarator.PARAMETERS, "cd")),
                    field("name",
                        proj(ConstructorDeclarator.TYPE_, ConstructorDeclarator.NAME, "cd")),
                    field("fparams",
                        proj(ConstructorDeclarator.TYPE_, ConstructorDeclarator.FORMAL_PARAMETERS, "cd")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("tparams")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.angleBracesList"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(
                                                ref(Serde.typeParameterToExpr),
                                                var("tparams"))))),
                                just(apply(ref(Serde.simpleTypeNameToExpr), var("name"))),
                                just(
                                    apply(
                                        var("hydra.serialization.parenListAdaptive"),
                                        Lists.map(ref(Serde.formalParameterToExpr), var("fparams"))))))))));

    public static final Def constructorModifierToExpr = def(
        "constructorModifierToExpr",
        () -> lambda("m",
                cases(ConstructorModifier.TYPE_,
                    var("m"),
                    field(
                        ConstructorModifier.ANNOTATION,
                        lambda("ann", apply(ref(Serde.annotationToExpr), var("ann")))),
                    field(
                        ConstructorModifier.PUBLIC,
                        constant(apply(var("hydra.serialization.cst"), string("public")))),
                    field(
                        ConstructorModifier.PROTECTED,
                        constant(apply(var("hydra.serialization.cst"), string("protected")))),
                    field(
                        ConstructorModifier.PRIVATE,
                        constant(apply(var("hydra.serialization.cst"), string("private")))))));

    public static final Def continueStatementToExpr = def(
        "continueStatementToExpr",
        () -> lambda("cs",
                let("mlabel",
                    apply(unwrap(ContinueStatement.TYPE_), var("cs")),
                    apply(
                        var("hydra.serialization.withSemi"),
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    just(apply(var("hydra.serialization.cst"), string("continue"))),
                                    Maybes.map(ref(Serde.identifierToExpr), var("mlabel")))))))));

    public static final Def dimsToExpr = def(
        "dimsToExpr",
        () -> lambda("d",
                apply(
                    var("hydra.serialization.noSep"),
                    Lists.map(
                        constant(apply(var("hydra.serialization.cst"), string("[]"))),
                        apply(unwrap(Dims.TYPE_), var("d"))))));

    public static final Def doStatementToExpr = def(
        "doStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:DoStatement"))));

    public static final Def elementValuePairToExpr = def(
        "elementValuePairToExpr",
        () -> lambda("evp",
                let(
                    field("k",
                        proj(ElementValuePair.TYPE_, ElementValuePair.KEY, "evp")),
                    field("v",
                        proj(ElementValuePair.TYPE_, ElementValuePair.VALUE, "evp")),
                    apply(
                        var("hydra.serialization.infixWs"),
                        string("="),
                        apply(ref(Serde.identifierToExpr), var("k")),
                        apply(ref(Serde.elementValueToExpr), var("v"))))));

    public static final Def elementValueToExpr = def(
        "elementValueToExpr",
        () -> lambda("ev",
                cases(ElementValue.TYPE_,
                    var("ev"),
                    field(
                        ElementValue.CONDITIONAL_EXPRESSION,
                        lambda("c", apply(ref(Serde.conditionalExpressionToExpr), var("c")))),
                    field(
                        ElementValue.ELEMENT_VALUE_ARRAY_INITIALIZER,
                        lambda("evai",
                            apply(
                                var("hydra.serialization.commaSep"),
                                var("hydra.serialization.inlineStyle"),
                                Lists.map(
                                    ref(Serde.elementValueToExpr),
                                    apply(
                                        unwrap(ElementValueArrayInitializer.TYPE_),
                                        var("evai")))))),
                    field(
                        ElementValue.ANNOTATION,
                        lambda("ann", apply(ref(Serde.annotationToExpr), var("ann")))))));

    public static final Def enumDeclarationToExpr = def(
        "enumDeclarationToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:EnumDeclaration"))));

    public static final Def equalityExpressionToExpr = def(
        "equalityExpressionToExpr",
        () -> lambda("e",
                cases(EqualityExpression.TYPE_,
                    var("e"),
                    field(
                        EqualityExpression.UNARY,
                        lambda("r", apply(ref(Serde.relationalExpressionToExpr), var("r")))),
                    field(
                        EqualityExpression.EQUAL,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("=="),
                                apply(
                                    ref(Serde.equalityExpressionToExpr),
                                    proj(EqualityExpression_Binary.TYPE_, EqualityExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.relationalExpressionToExpr),
                                    proj(EqualityExpression_Binary.TYPE_, EqualityExpression_Binary.RHS, "b"))))),
                    field(
                        EqualityExpression.NOT_EQUAL,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("!="),
                                apply(
                                    ref(Serde.equalityExpressionToExpr),
                                    proj(EqualityExpression_Binary.TYPE_, EqualityExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.relationalExpressionToExpr),
                                    proj(EqualityExpression_Binary.TYPE_, EqualityExpression_Binary.RHS, "b"))))))));

    public static final Def escapeJavaChar = def(
        "escapeJavaChar",
        () -> lambda("c",
                Logic.ifElse(
                    Equality.equal(var("c"), int32(34)),
                    string("\\\""),
                    Logic.ifElse(
                        Equality.equal(var("c"), int32(92)),
                        string("\\\\"),
                        Logic.ifElse(
                            Equality.equal(var("c"), int32(10)),
                            string("\\n"),
                            Logic.ifElse(
                                Equality.equal(var("c"), int32(13)),
                                string("\\r"),
                                Logic.ifElse(
                                    Equality.equal(var("c"), int32(9)),
                                    string("\\t"),
                                    Logic.ifElse(
                                        Equality.equal(var("c"), int32(8)),
                                        string("\\b"),
                                        Logic.ifElse(
                                            Equality.equal(var("c"), int32(12)),
                                            string("\\f"),
                                            Logic.ifElse(
                                                Logic.and_(
                                                    Equality.gte(var("c"), int32(32)),
                                                    Equality.lt(var("c"), int32(127))),
                                                Strings.fromList(list(var("c"))),
                                                apply(ref(Serde.javaUnicodeEscape), var("c"))))))))))));

    public static final Def escapeJavaString = def(
        "escapeJavaString",
        () -> lambda("s",
                Strings.cat(
                    Lists.map(
                        lambda("c", apply(ref(Serde.escapeJavaChar), var("c"))),
                        Strings.toList(var("s"))))));

    public static final Def exclusiveOrExpressionToExpr = def(
        "exclusiveOrExpressionToExpr",
        () -> lambda("eoe",
                apply(
                    var("hydra.serialization.infixWsList"),
                    string("^"),
                    Lists.map(
                        ref(Serde.andExpressionToExpr),
                        apply(unwrap(ExclusiveOrExpression.TYPE_), var("eoe"))))));

    public static final Def explicitConstructorInvocationToExpr = def(
        "explicitConstructorInvocationToExpr",
        () -> constant(
                apply(var("hydra.serialization.cst"), string("STUB:ExplicitConstructorInvocation"))));

    public static final Def expressionNameToExpr = def(
        "expressionNameToExpr",
        () -> lambda("en",
                let(
                    field("mqual",
                        proj(ExpressionName.TYPE_, ExpressionName.QUALIFIER, "en")),
                    field("id",
                        proj(ExpressionName.TYPE_, ExpressionName.IDENTIFIER, "en")),
                    apply(
                        var("hydra.serialization.dotSep"),
                        Maybes.cat(
                            list(
                                Maybes.map(ref(Serde.ambiguousNameToExpr), var("mqual")),
                                just(apply(ref(Serde.identifierToExpr), var("id")))))))));

    public static final Def expressionStatementToExpr = def(
        "expressionStatementToExpr",
        () -> lambda("es",
                apply(
                    var("hydra.serialization.withSemi"),
                    apply(
                        ref(Serde.statementExpressionToExpr),
                        apply(unwrap(ExpressionStatement.TYPE_), var("es"))))));

    public static final Def expressionToExpr = def(
        "expressionToExpr",
        () -> lambda("e",
                cases(Expression.TYPE_,
                    var("e"),
                    field(
                        Expression.LAMBDA,
                        lambda("l", apply(ref(Serde.lambdaExpressionToExpr), var("l")))),
                    field(
                        Expression.ASSIGNMENT,
                        lambda("a", apply(ref(Serde.assignmentExpressionToExpr), var("a")))))));

    public static final Def fieldAccessToExpr = def(
        "fieldAccessToExpr",
        () -> lambda("fa",
                let(
                    field("qual",
                        proj(FieldAccess.TYPE_, FieldAccess.QUALIFIER, "fa")),
                    field("id",
                        proj(FieldAccess.TYPE_, FieldAccess.IDENTIFIER, "fa")),
                    cases(FieldAccess_Qualifier.TYPE_,
                        var("qual"),
                        field(
                            FieldAccess_Qualifier.PRIMARY,
                            lambda("p",
                                apply(
                                    var("hydra.serialization.dotSep"),
                                    list(
                                        apply(ref(Serde.primaryToExpr), var("p")),
                                        apply(ref(Serde.identifierToExpr), var("id")))))),
                        field(
                            FieldAccess_Qualifier.SUPER,
                            constant(
                                apply(
                                    var("hydra.serialization.dotSep"),
                                    list(
                                        apply(var("hydra.serialization.cst"), string("super")),
                                        apply(ref(Serde.identifierToExpr), var("id")))))),
                        field(
                            FieldAccess_Qualifier.TYPED,
                            lambda("tn",
                                apply(
                                    var("hydra.serialization.dotSep"),
                                    list(
                                        apply(ref(Serde.typeNameToExpr), var("tn")),
                                        apply(var("hydra.serialization.cst"), string("super")),
                                        apply(ref(Serde.identifierToExpr), var("id"))))))))));

    public static final Def fieldDeclarationToExpr = def(
        "fieldDeclarationToExpr",
        () -> lambda("fd",
                let(
                    field("mods",
                        proj(FieldDeclaration.TYPE_, FieldDeclaration.MODIFIERS, "fd")),
                    field("typ",
                        proj(FieldDeclaration.TYPE_, FieldDeclaration.UNANN_TYPE, "fd")),
                    field("vars",
                        proj(FieldDeclaration.TYPE_, FieldDeclaration.VARIABLE_DECLARATORS, "fd")),
                    apply(
                        var("hydra.serialization.withSemi"),
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    Logic.ifElse(
                                        Lists.null_(var("mods")),
                                        nothing(),
                                        just(
                                            apply(
                                                var("hydra.serialization.spaceSep"),
                                                Lists.map(
                                                    ref(Serde.fieldModifierToExpr),
                                                    var("mods"))))),
                                    just(apply(ref(Serde.unannTypeToExpr), var("typ"))),
                                    just(
                                        apply(
                                            var("hydra.serialization.commaSep"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(
                                                ref(Serde.variableDeclaratorToExpr),
                                                var("vars")))))))))));

    public static final Def fieldModifierToExpr = def(
        "fieldModifierToExpr",
        () -> lambda("m",
                cases(FieldModifier.TYPE_,
                    var("m"),
                    field(
                        FieldModifier.ANNOTATION,
                        lambda("ann", apply(ref(Serde.annotationToExpr), var("ann")))),
                    field(
                        FieldModifier.PUBLIC,
                        constant(apply(var("hydra.serialization.cst"), string("public")))),
                    field(
                        FieldModifier.PROTECTED,
                        constant(apply(var("hydra.serialization.cst"), string("protected")))),
                    field(
                        FieldModifier.PRIVATE,
                        constant(apply(var("hydra.serialization.cst"), string("private")))),
                    field(
                        FieldModifier.STATIC,
                        constant(apply(var("hydra.serialization.cst"), string("static")))),
                    field(
                        FieldModifier.FINAL,
                        constant(apply(var("hydra.serialization.cst"), string("final")))),
                    field(
                        FieldModifier.TRANSIENT,
                        constant(apply(var("hydra.serialization.cst"), string("transient")))),
                    field(
                        FieldModifier.VOLATILE,
                        constant(apply(var("hydra.serialization.cst"), string("volatile")))))));

    public static final Def floatingPointLiteralToExpr = def(
        "floatingPointLiteralToExpr",
        () -> lambda("fl",
                apply(
                    var("hydra.serialization.cst"),
                    apply(
                        ref(Serde.javaFloatLiteralText),
                        Literals.showFloat64(
                            apply(unwrap(FloatingPointLiteral.TYPE_), var("fl")))))));

    public static final Def floatingPointTypeToExpr = def(
        "floatingPointTypeToExpr",
        () -> lambda("ft",
                cases(FloatingPointType.TYPE_,
                    var("ft"),
                    field(
                        FloatingPointType.FLOAT,
                        constant(apply(var("hydra.serialization.cst"), string("float")))),
                    field(
                        FloatingPointType.DOUBLE,
                        constant(apply(var("hydra.serialization.cst"), string("double")))))));

    public static final Def forStatementToExpr = def(
        "forStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:ForStatement"))));

    public static final Def formalParameterSimpleToExpr = def(
        "formalParameterSimpleToExpr",
        () -> lambda("fps",
                let(
                    field("mods",
                        proj(FormalParameter_Simple.TYPE_, FormalParameter_Simple.MODIFIERS, "fps")),
                    field("typ",
                        proj(FormalParameter_Simple.TYPE_, FormalParameter_Simple.TYPE, "fps")),
                    field("id",
                        proj(FormalParameter_Simple.TYPE_, FormalParameter_Simple.ID, "fps")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(
                                                ref(Serde.variableModifierToExpr),
                                                var("mods"))))),
                                just(apply(ref(Serde.unannTypeToExpr), var("typ"))),
                                just(apply(ref(Serde.variableDeclaratorIdToExpr), var("id")))))))));

    public static final Def formalParameterToExpr = def(
        "formalParameterToExpr",
        () -> lambda("p",
                cases(FormalParameter.TYPE_,
                    var("p"),
                    field(
                        FormalParameter.SIMPLE,
                        lambda("s", apply(ref(Serde.formalParameterSimpleToExpr), var("s")))),
                    field(
                        FormalParameter.VARIABLE_ARITY,
                        lambda("v", apply(ref(Serde.variableArityParameterToExpr), var("v")))))));

    public static final Def hexDigit = def(
        "hexDigit",
        () -> lambda("n",
                Logic.ifElse(
                    Equality.lt(var("n"), int32(10)),
                    Math_.add(var("n"), int32(48)),
                    Math_.add(Math_.sub(var("n"), int32(10)), int32(65)))));

    public static final Def identifierToExpr = def(
        "identifierToExpr",
        () -> lambda("id",
                apply(
                    var("hydra.serialization.cst"),
                    apply(unwrap(Identifier.TYPE_), var("id")))));

    public static final Def ifThenElseStatementToExpr = def(
        "ifThenElseStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:IfThenElseStatement"))));

    public static final Def ifThenStatementToExpr = def(
        "ifThenStatementToExpr",
        () -> lambda("its",
                let(
                    field("cond",
                        proj(IfThenStatement.TYPE_, IfThenStatement.EXPRESSION, "its")),
                    field("thn",
                        proj(IfThenStatement.TYPE_, IfThenStatement.STATEMENT, "its")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        list(
                            apply(var("hydra.serialization.cst"), string("if")),
                            apply(
                                var("hydra.serialization.parenList"),
                                bool(false),
                                list(apply(ref(Serde.expressionToExpr), var("cond")))),
                            apply(
                                var("hydra.serialization.curlyBlock"),
                                var("hydra.serialization.fullBlockStyle"),
                                apply(ref(Serde.statementToExpr), var("thn"))))))));

    public static final Def importDeclarationToExpr = def(
        "importDeclarationToExpr",
        () -> lambda("imp",
                cases(ImportDeclaration.TYPE_,
                    var("imp"),
                    field(
                        ImportDeclaration.SINGLE_TYPE,
                        lambda("st",
                            apply(
                                var("hydra.serialization.withSemi"),
                                apply(
                                    var("hydra.serialization.spaceSep"),
                                    list(
                                        apply(var("hydra.serialization.cst"), string("import")),
                                        apply(
                                            ref(Serde.typeNameToExpr),
                                            apply(
                                                unwrap(SingleTypeImportDeclaration.TYPE_),
                                                var("st")))))))),
                    field(
                        ImportDeclaration.TYPE_IMPORT_ON_DEMAND,
                        constant(
                            apply(
                                var("hydra.serialization.cst"),
                                string("STUB:ImportDeclarationTypeImportOnDemand")))),
                    field(
                        ImportDeclaration.SINGLE_STATIC_IMPORT,
                        constant(
                            apply(
                                var("hydra.serialization.cst"),
                                string("STUB:ImportDeclarationSingleStaticImport")))),
                    field(
                        ImportDeclaration.STATIC_IMPORT_ON_DEMAND,
                        constant(
                            apply(
                                var("hydra.serialization.cst"),
                                string("STUB:ImportDeclarationStaticImportOnDemand")))))));

    public static final Def inclusiveOrExpressionToExpr = def(
        "inclusiveOrExpressionToExpr",
        () -> lambda("ioe",
                apply(
                    var("hydra.serialization.infixWsList"),
                    string("|"),
                    Lists.map(
                        ref(Serde.exclusiveOrExpressionToExpr),
                        apply(unwrap(InclusiveOrExpression.TYPE_), var("ioe"))))));

    public static final Def instanceInitializerToExpr = def(
        "instanceInitializerToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:InstanceInitializer"))));

    public static final Def integerLiteralToExpr = def(
        "integerLiteralToExpr",
        () -> lambda("il",
                let(
                    field("i",
                        apply(unwrap(IntegerLiteral.TYPE_), var("il"))),
                    field("suffix",
                        Logic.ifElse(
                            Logic.or_(
                                Equality.gt(
                                    var("i"),
                                    bigint(java.math.BigInteger.valueOf(2147483647L))),
                                Equality.lt(
                                    var("i"),
                                    bigint(java.math.BigInteger.valueOf(-2147483648L)))),
                            string("L"),
                            string(""))),
                    apply(
                        var("hydra.serialization.cst"),
                        Strings.cat2(Literals.showBigint(var("i")), var("suffix"))))));

    public static final Def integralTypeToExpr = def(
        "integralTypeToExpr",
        () -> lambda("t",
                cases(IntegralType.TYPE_,
                    var("t"),
                    field(
                        IntegralType.BYTE,
                        constant(apply(var("hydra.serialization.cst"), string("byte")))),
                    field(
                        IntegralType.SHORT,
                        constant(apply(var("hydra.serialization.cst"), string("short")))),
                    field(
                        IntegralType.INT,
                        constant(apply(var("hydra.serialization.cst"), string("int")))),
                    field(
                        IntegralType.LONG,
                        constant(apply(var("hydra.serialization.cst"), string("long")))),
                    field(
                        IntegralType.CHAR,
                        constant(apply(var("hydra.serialization.cst"), string("char")))))));

    public static final Def interfaceBodyToExpr = def(
        "interfaceBodyToExpr",
        () -> lambda("ib",
                apply(
                    var("hydra.serialization.curlyBlock"),
                    var("hydra.serialization.fullBlockStyle"),
                    apply(
                        var("hydra.serialization.doubleNewlineSep"),
                        Lists.map(
                            ref(Serde.interfaceMemberDeclarationWithCommentsToExpr),
                            apply(unwrap(InterfaceBody.TYPE_), var("ib")))))));

    public static final Def interfaceDeclarationToExpr = def(
        "interfaceDeclarationToExpr",
        () -> lambda("d",
                cases(InterfaceDeclaration.TYPE_,
                    var("d"),
                    field(
                        InterfaceDeclaration.NORMAL_INTERFACE,
                        lambda("n", apply(ref(Serde.normalInterfaceDeclarationToExpr), var("n")))),
                    field(
                        InterfaceDeclaration.ANNOTATION_INTERFACE,
                        lambda("a", apply(ref(Serde.annotationTypeDeclarationToExpr), var("a")))))));

    public static final Def interfaceMemberDeclarationToExpr = def(
        "interfaceMemberDeclarationToExpr",
        () -> lambda("d",
                cases(InterfaceMemberDeclaration.TYPE_,
                    var("d"),
                    field(
                        InterfaceMemberDeclaration.CONSTANT,
                        lambda("c", apply(ref(Serde.constantDeclarationToExpr), var("c")))),
                    field(
                        InterfaceMemberDeclaration.INTERFACE_METHOD,
                        lambda("im", apply(ref(Serde.interfaceMethodDeclarationToExpr), var("im")))),
                    field(
                        InterfaceMemberDeclaration.CLASS,
                        lambda("cd", apply(ref(Serde.classDeclarationToExpr), var("cd")))),
                    field(
                        InterfaceMemberDeclaration.INTERFACE,
                        lambda("id", apply(ref(Serde.interfaceDeclarationToExpr), var("id")))))));

    public static final Def interfaceMemberDeclarationWithCommentsToExpr = def(
        "interfaceMemberDeclarationWithCommentsToExpr",
        () -> lambda("imdwc",
                let(
                    field("d",
                        proj(InterfaceMemberDeclarationWithComments.TYPE_, InterfaceMemberDeclarationWithComments.VALUE, "imdwc")),
                    field("mc",
                        proj(InterfaceMemberDeclarationWithComments.TYPE_, InterfaceMemberDeclarationWithComments.COMMENTS, "imdwc")),
                    apply(
                        ref(Serde.withComments),
                        var("mc"),
                        apply(ref(Serde.interfaceMemberDeclarationToExpr), var("d"))))));

    public static final Def interfaceMethodDeclarationToExpr = def(
        "interfaceMethodDeclarationToExpr",
        () -> lambda("imd",
                let(
                    field("mods",
                        proj(InterfaceMethodDeclaration.TYPE_, InterfaceMethodDeclaration.MODIFIERS, "imd")),
                    field("header",
                        proj(InterfaceMethodDeclaration.TYPE_, InterfaceMethodDeclaration.HEADER, "imd")),
                    field("body",
                        proj(InterfaceMethodDeclaration.TYPE_, InterfaceMethodDeclaration.BODY, "imd")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(
                                                ref(Serde.interfaceMethodModifierToExpr),
                                                var("mods"))))),
                                just(apply(ref(Serde.methodHeaderToExpr), var("header"))),
                                just(apply(ref(Serde.methodBodyToExpr), var("body")))))))));

    public static final Def interfaceMethodModifierToExpr = def(
        "interfaceMethodModifierToExpr",
        () -> lambda("m",
                cases(InterfaceMethodModifier.TYPE_,
                    var("m"),
                    field(
                        InterfaceMethodModifier.ANNOTATION,
                        lambda("a", apply(ref(Serde.annotationToExpr), var("a")))),
                    field(
                        InterfaceMethodModifier.PUBLIC,
                        constant(apply(var("hydra.serialization.cst"), string("public")))),
                    field(
                        InterfaceMethodModifier.PRIVATE,
                        constant(apply(var("hydra.serialization.cst"), string("private")))),
                    field(
                        InterfaceMethodModifier.ABSTRACT,
                        constant(apply(var("hydra.serialization.cst"), string("abstract")))),
                    field(
                        InterfaceMethodModifier.DEFAULT,
                        constant(apply(var("hydra.serialization.cst"), string("default")))),
                    field(
                        InterfaceMethodModifier.STATIC,
                        constant(apply(var("hydra.serialization.cst"), string("static")))),
                    field(
                        InterfaceMethodModifier.STRICTFP,
                        constant(apply(var("hydra.serialization.cst"), string("strictfp")))))));

    public static final Def interfaceModifierToExpr = def(
        "interfaceModifierToExpr",
        () -> lambda("m",
                cases(InterfaceModifier.TYPE_,
                    var("m"),
                    field(
                        InterfaceModifier.ANNOTATION,
                        lambda("a", apply(ref(Serde.annotationToExpr), var("a")))),
                    field(
                        InterfaceModifier.PUBLIC,
                        constant(apply(var("hydra.serialization.cst"), string("public")))),
                    field(
                        InterfaceModifier.PROTECTED,
                        constant(apply(var("hydra.serialization.cst"), string("protected")))),
                    field(
                        InterfaceModifier.PRIVATE,
                        constant(apply(var("hydra.serialization.cst"), string("private")))),
                    field(
                        InterfaceModifier.ABSTRACT,
                        constant(apply(var("hydra.serialization.cst"), string("abstract")))),
                    field(
                        InterfaceModifier.STATIC,
                        constant(apply(var("hydra.serialization.cst"), string("static")))),
                    field(
                        InterfaceModifier.STRICTFP,
                        constant(apply(var("hydra.serialization.cst"), string("strictfp")))))));

    public static final Def interfaceTypeToExpr = def(
        "interfaceTypeToExpr",
        () -> lambda("it",
                apply(
                    ref(Serde.classTypeToExpr),
                    apply(unwrap(InterfaceType.TYPE_), var("it")))));

    public static final Def javaFloatLiteralText = def(
        "javaFloatLiteralText",
        () -> lambda("s",
                Logic.ifElse(
                    Equality.equal(var("s"), string("NaN")),
                    string("Double.NaN"),
                    Logic.ifElse(
                        Equality.equal(var("s"), string("Infinity")),
                        string("Double.POSITIVE_INFINITY"),
                        Logic.ifElse(
                            Equality.equal(var("s"), string("-Infinity")),
                            string("Double.NEGATIVE_INFINITY"),
                            var("s"))))));

    public static final Def javaUnicodeEscape = def(
        "javaUnicodeEscape",
        () -> lambda("n",
                Logic.ifElse(
                    Equality.gt(var("n"), int32(65535)),
                    let(
                        field("n'",
                            Math_.sub(var("n"), int32(65536))),
                        field("hi",
                            Math_.add(
                                int32(55296),
                                Maybes.fromMaybe(int32(0), Math_.maybeDiv(var("n'"), int32(1024))))),
                        field("lo",
                            Math_.add(
                                int32(56320),
                                Maybes.fromMaybe(int32(0), Math_.maybeMod(var("n'"), int32(1024))))),
                        Strings.cat2(
                            Strings.cat2(string("\\u"), apply(ref(Serde.padHex4), var("hi"))),
                            Strings.cat2(string("\\u"), apply(ref(Serde.padHex4), var("lo"))))),
                    Strings.cat2(string("\\u"), apply(ref(Serde.padHex4), var("n"))))));

    public static final Def labeledStatementToExpr = def(
        "labeledStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:LabeledStatement"))));

    public static final Def lambdaBodyToExpr = def(
        "lambdaBodyToExpr",
        () -> lambda("b",
                cases(LambdaBody.TYPE_,
                    var("b"),
                    field(
                        LambdaBody.EXPRESSION,
                        lambda("e", apply(ref(Serde.expressionToExpr), var("e")))),
                    field(
                        LambdaBody.BLOCK,
                        lambda("b", apply(ref(Serde.blockToExpr), var("b")))))));

    public static final Def lambdaExpressionToExpr = def(
        "lambdaExpressionToExpr",
        () -> lambda("le",
                let(
                    field("params",
                        proj(LambdaExpression.TYPE_, LambdaExpression.PARAMETERS, "le")),
                    field("body",
                        proj(LambdaExpression.TYPE_, LambdaExpression.BODY, "le")),
                    apply(
                        var("hydra.serialization.infixWs"),
                        string("->"),
                        apply(ref(Serde.lambdaParametersToExpr), var("params")),
                        apply(ref(Serde.lambdaBodyToExpr), var("body"))))));

    public static final Def lambdaParametersToExpr = def(
        "lambdaParametersToExpr",
        () -> lambda("p",
                cases(LambdaParameters.TYPE_,
                    var("p"),
                    field(
                        LambdaParameters.TUPLE,
                        lambda("l",
                            apply(
                                var("hydra.serialization.parenList"),
                                bool(false),
                                Lists.map(ref(Serde.lambdaParametersToExpr), var("l"))))),
                    field(
                        LambdaParameters.SINGLE,
                        lambda("id", apply(ref(Serde.identifierToExpr), var("id")))))));

    public static final Def leftHandSideToExpr = def(
        "leftHandSideToExpr",
        () -> lambda("lhs",
                cases(LeftHandSide.TYPE_,
                    var("lhs"),
                    field(
                        LeftHandSide.EXPRESSION_NAME,
                        lambda("en", apply(ref(Serde.expressionNameToExpr), var("en")))),
                    field(
                        LeftHandSide.FIELD_ACCESS,
                        lambda("fa", apply(ref(Serde.fieldAccessToExpr), var("fa")))),
                    field(
                        LeftHandSide.ARRAY_ACCESS,
                        lambda("aa", apply(ref(Serde.arrayAccessToExpr), var("aa")))))));

    public static final Def literalToExpr = def(
        "literalToExpr",
        () -> lambda("l",
                cases(Literal.TYPE_,
                    var("l"),
                    field(
                        Literal.NULL,
                        constant(apply(var("hydra.serialization.cst"), string("null")))),
                    field(
                        Literal.INTEGER,
                        lambda("il", apply(ref(Serde.integerLiteralToExpr), var("il")))),
                    field(
                        Literal.FLOATING_POINT,
                        lambda("fl", apply(ref(Serde.floatingPointLiteralToExpr), var("fl")))),
                    field(
                        Literal.BOOLEAN,
                        lambda("b",
                            apply(
                                var("hydra.serialization.cst"),
                                Logic.ifElse(var("b"), string("true"), string("false"))))),
                    field(
                        Literal.CHARACTER,
                        lambda("c",
                            let("ci",
                                Literals.bigintToInt32(Literals.uint16ToBigint(var("c"))),
                                apply(
                                    var("hydra.serialization.cst"),
                                    Strings.cat2(
                                        string("'"),
                                        Strings.cat2(
                                            Logic.ifElse(
                                                Equality.equal(var("ci"), int32(39)),
                                                string("\\'"),
                                                Logic.ifElse(
                                                    Equality.equal(var("ci"), int32(92)),
                                                    string("\\\\"),
                                                    Logic.ifElse(
                                                        Equality.equal(var("ci"), int32(10)),
                                                        string("\\n"),
                                                        Logic.ifElse(
                                                            Equality.equal(var("ci"), int32(13)),
                                                            string("\\r"),
                                                            Logic.ifElse(
                                                                Equality.equal(var("ci"), int32(9)),
                                                                string("\\t"),
                                                                Logic.ifElse(
                                                                    Logic.and_(
                                                                        Equality.gte(
                                                                            var("ci"),
                                                                            int32(32)),
                                                                        Equality.lt(
                                                                            var("ci"),
                                                                            int32(127))),
                                                                    Strings.fromList(
                                                                        list(var("ci"))),
                                                                    apply(
                                                                        ref(Serde.javaUnicodeEscape),
                                                                        var("ci")))))))),
                                            string("'"))))))),
                    field(
                        Literal.STRING,
                        lambda("sl", apply(ref(Serde.stringLiteralToExpr), var("sl")))))));

    public static final Def localNameToExpr = def(
        "localNameToExpr",
        () -> lambda("t",
                cases(LocalVariableType.TYPE_,
                    var("t"),
                    field(
                        LocalVariableType.TYPE,
                        lambda("ut", apply(ref(Serde.unannTypeToExpr), var("ut")))),
                    field(
                        LocalVariableType.VAR,
                        constant(apply(var("hydra.serialization.cst"), string("var")))))));

    public static final Def localVariableDeclarationStatementToExpr = def(
        "localVariableDeclarationStatementToExpr",
        () -> lambda("lvds",
                apply(
                    var("hydra.serialization.withSemi"),
                    apply(
                        ref(Serde.localVariableDeclarationToExpr),
                        apply(
                            unwrap(LocalVariableDeclarationStatement.TYPE_),
                            var("lvds"))))));

    public static final Def localVariableDeclarationToExpr = def(
        "localVariableDeclarationToExpr",
        () -> lambda("lvd",
                let(
                    field("mods",
                        proj(LocalVariableDeclaration.TYPE_, LocalVariableDeclaration.MODIFIERS, "lvd")),
                    field("t",
                        proj(LocalVariableDeclaration.TYPE_, LocalVariableDeclaration.TYPE, "lvd")),
                    field("decls",
                        proj(LocalVariableDeclaration.TYPE_, LocalVariableDeclaration.DECLARATORS, "lvd")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(
                                                ref(Serde.variableModifierToExpr),
                                                var("mods"))))),
                                just(apply(ref(Serde.localNameToExpr), var("t"))),
                                just(
                                    apply(
                                        var("hydra.serialization.commaSep"),
                                        var("hydra.serialization.inlineStyle"),
                                        Lists.map(ref(Serde.variableDeclaratorToExpr), var("decls"))))))))));

    public static final Def markerAnnotationToExpr = def(
        "markerAnnotationToExpr",
        () -> lambda("ma",
                apply(
                    var("hydra.serialization.prefix"),
                    string("@"),
                    apply(
                        ref(Serde.typeNameToExpr),
                        apply(unwrap(MarkerAnnotation.TYPE_), var("ma"))))));

    public static final Def methodBodyToExpr = def(
        "methodBodyToExpr",
        () -> lambda("b",
                cases(MethodBody.TYPE_,
                    var("b"),
                    field(
                        MethodBody.BLOCK,
                        lambda("block", apply(ref(Serde.blockToExpr), var("block")))),
                    field(
                        MethodBody.NONE,
                        constant(apply(var("hydra.serialization.cst"), string(";")))))));

    public static final Def methodDeclarationToExpr = def(
        "methodDeclarationToExpr",
        () -> lambda("md",
                let(
                    field("anns",
                        proj(MethodDeclaration.TYPE_, MethodDeclaration.ANNOTATIONS, "md")),
                    field("mods",
                        proj(MethodDeclaration.TYPE_, MethodDeclaration.MODIFIERS, "md")),
                    field("header",
                        proj(MethodDeclaration.TYPE_, MethodDeclaration.HEADER, "md")),
                    field("body",
                        proj(MethodDeclaration.TYPE_, MethodDeclaration.BODY, "md")),
                    field("headerAndBody",
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    Logic.ifElse(
                                        Lists.null_(var("mods")),
                                        nothing(),
                                        just(
                                            apply(
                                                var("hydra.serialization.spaceSep"),
                                                Lists.map(
                                                    ref(Serde.methodModifierToExpr),
                                                    var("mods"))))),
                                    just(apply(ref(Serde.methodHeaderToExpr), var("header"))),
                                    just(apply(ref(Serde.methodBodyToExpr), var("body"))))))),
                    apply(
                        var("hydra.serialization.newlineSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("anns")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.newlineSep"),
                                            Lists.map(ref(Serde.annotationToExpr), var("anns"))))),
                                just(var("headerAndBody"))))))));

    public static final Def methodDeclaratorToExpr = def(
        "methodDeclaratorToExpr",
        () -> lambda("md",
                let(
                    field("id",
                        proj(MethodDeclarator.TYPE_, MethodDeclarator.IDENTIFIER, "md")),
                    field("params",
                        proj(MethodDeclarator.TYPE_, MethodDeclarator.FORMAL_PARAMETERS, "md")),
                    apply(
                        var("hydra.serialization.noSep"),
                        list(
                            apply(ref(Serde.identifierToExpr), var("id")),
                            apply(
                                var("hydra.serialization.parenListAdaptive"),
                                Lists.map(ref(Serde.formalParameterToExpr), var("params"))))))));

    public static final Def methodHeaderToExpr = def(
        "methodHeaderToExpr",
        () -> lambda("mh",
                let(
                    field("params",
                        proj(MethodHeader.TYPE_, MethodHeader.PARAMETERS, "mh")),
                    field("result",
                        proj(MethodHeader.TYPE_, MethodHeader.RESULT, "mh")),
                    field("decl",
                        proj(MethodHeader.TYPE_, MethodHeader.DECLARATOR, "mh")),
                    field("mthrows",
                        proj(MethodHeader.TYPE_, MethodHeader.THROWS, "mh")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("params")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.angleBracesList"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(ref(Serde.typeParameterToExpr), var("params"))))),
                                just(apply(ref(Serde.resultToExpr), var("result"))),
                                just(apply(ref(Serde.methodDeclaratorToExpr), var("decl"))),
                                Maybes.map(ref(Serde.throwsToExpr), var("mthrows"))))))));

    public static final Def methodInvocationToExpr = def(
        "methodInvocationToExpr",
        () -> lambda("mi",
                let(
                    field("header",
                        proj(MethodInvocation.TYPE_, MethodInvocation.HEADER, "mi")),
                    field("args",
                        proj(MethodInvocation.TYPE_, MethodInvocation.ARGUMENTS, "mi")),
                    field("argSec",
                        apply(
                            var("hydra.serialization.parenListAdaptive"),
                            Lists.map(ref(Serde.expressionToExpr), var("args")))),
                    field("headerSec",
                        cases(MethodInvocation_Header.TYPE_,
                            var("header"),
                            field(
                                MethodInvocation_Header.SIMPLE,
                                lambda("mname", apply(ref(Serde.methodNameToExpr), var("mname")))),
                            field(
                                MethodInvocation_Header.COMPLEX,
                                lambda("cx",
                                    let(
                                        field("cvar",
                                            proj(MethodInvocation_Complex.TYPE_, MethodInvocation_Complex.VARIANT, "cx")),
                                        field("targs",
                                            proj(MethodInvocation_Complex.TYPE_, MethodInvocation_Complex.TYPE_ARGUMENTS, "cx")),
                                        field("cid",
                                            proj(MethodInvocation_Complex.TYPE_, MethodInvocation_Complex.IDENTIFIER, "cx")),
                                        field("idSec",
                                            apply(
                                                var("hydra.serialization.noSep"),
                                                Maybes.cat(
                                                    list(
                                                        Logic.ifElse(
                                                            Lists.null_(var("targs")),
                                                            nothing(),
                                                            just(
                                                                apply(
                                                                    var("hydra.serialization.angleBracesList"),
                                                                    var("hydra.serialization.inlineStyle"),
                                                                    Lists.map(
                                                                        ref(Serde.typeArgumentToExpr),
                                                                        var("targs"))))),
                                                        just(
                                                            apply(
                                                                ref(Serde.identifierToExpr),
                                                                var("cid"))))))),
                                        cases(MethodInvocation_Variant.TYPE_,
                                            var("cvar"),
                                            field(
                                                MethodInvocation_Variant.TYPE,
                                                lambda("tname",
                                                    apply(
                                                        var("hydra.serialization.dotSep"),
                                                        list(
                                                            apply(
                                                                ref(Serde.typeNameToExpr),
                                                                var("tname")),
                                                            var("idSec"))))),
                                            field(
                                                MethodInvocation_Variant.EXPRESSION,
                                                lambda("en",
                                                    apply(
                                                        var("hydra.serialization.dotSep"),
                                                        list(
                                                            apply(
                                                                ref(Serde.expressionNameToExpr),
                                                                var("en")),
                                                            var("idSec"))))),
                                            field(
                                                MethodInvocation_Variant.PRIMARY,
                                                lambda("p",
                                                    apply(
                                                        var("hydra.serialization.dotSep"),
                                                        list(
                                                            apply(
                                                                ref(Serde.primaryToExpr),
                                                                var("p")),
                                                            var("idSec"))))),
                                            field(
                                                MethodInvocation_Variant.SUPER,
                                                constant(
                                                    apply(
                                                        var("hydra.serialization.dotSep"),
                                                        list(
                                                            apply(
                                                                var("hydra.serialization.cst"),
                                                                string("super")),
                                                            var("idSec"))))),
                                            field(
                                                MethodInvocation_Variant.TYPE_SUPER,
                                                lambda("tname",
                                                    apply(
                                                        var("hydra.serialization.dotSep"),
                                                        list(
                                                            apply(
                                                                ref(Serde.typeNameToExpr),
                                                                var("tname")),
                                                            apply(
                                                                var("hydra.serialization.cst"),
                                                                string("super")),
                                                            var("idSec"))))))))))),
                    apply(var("hydra.serialization.noSep"), list(var("headerSec"), var("argSec"))))));

    public static final Def methodModifierToExpr = def(
        "methodModifierToExpr",
        () -> lambda("m",
                cases(MethodModifier.TYPE_,
                    var("m"),
                    field(
                        MethodModifier.ANNOTATION,
                        lambda("ann", apply(ref(Serde.annotationToExpr), var("ann")))),
                    field(
                        MethodModifier.PUBLIC,
                        constant(apply(var("hydra.serialization.cst"), string("public")))),
                    field(
                        MethodModifier.PROTECTED,
                        constant(apply(var("hydra.serialization.cst"), string("protected")))),
                    field(
                        MethodModifier.PRIVATE,
                        constant(apply(var("hydra.serialization.cst"), string("private")))),
                    field(
                        MethodModifier.ABSTRACT,
                        constant(apply(var("hydra.serialization.cst"), string("abstract")))),
                    field(
                        MethodModifier.FINAL,
                        constant(apply(var("hydra.serialization.cst"), string("final")))),
                    field(
                        MethodModifier.SYNCHRONIZED,
                        constant(apply(var("hydra.serialization.cst"), string("synchronized")))),
                    field(
                        MethodModifier.NATIVE,
                        constant(apply(var("hydra.serialization.cst"), string("native")))),
                    field(
                        MethodModifier.STRICTFP,
                        constant(apply(var("hydra.serialization.cst"), string("strictfp")))))));

    public static final Def methodNameToExpr = def(
        "methodNameToExpr",
        () -> lambda("mn",
                apply(
                    ref(Serde.identifierToExpr),
                    apply(unwrap(MethodName.TYPE_), var("mn")))));

    public static final Def methodReferenceToExpr = def(
        "methodReferenceToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:MethodReference"))));

    public static final Def multiplicativeExpressionToExpr = def(
        "multiplicativeExpressionToExpr",
        () -> lambda("e",
                cases(MultiplicativeExpression.TYPE_,
                    var("e"),
                    field(
                        MultiplicativeExpression.UNARY,
                        lambda("u", apply(ref(Serde.unaryExpressionToExpr), var("u")))),
                    field(
                        MultiplicativeExpression.TIMES,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("*"),
                                apply(
                                    ref(Serde.multiplicativeExpressionToExpr),
                                    proj(MultiplicativeExpression_Binary.TYPE_, MultiplicativeExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.unaryExpressionToExpr),
                                    proj(MultiplicativeExpression_Binary.TYPE_, MultiplicativeExpression_Binary.RHS, "b"))))),
                    field(
                        MultiplicativeExpression.DIVIDE,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("/"),
                                apply(
                                    ref(Serde.multiplicativeExpressionToExpr),
                                    proj(MultiplicativeExpression_Binary.TYPE_, MultiplicativeExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.unaryExpressionToExpr),
                                    proj(MultiplicativeExpression_Binary.TYPE_, MultiplicativeExpression_Binary.RHS, "b"))))),
                    field(
                        MultiplicativeExpression.MOD,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("%"),
                                apply(
                                    ref(Serde.multiplicativeExpressionToExpr),
                                    proj(MultiplicativeExpression_Binary.TYPE_, MultiplicativeExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.unaryExpressionToExpr),
                                    proj(MultiplicativeExpression_Binary.TYPE_, MultiplicativeExpression_Binary.RHS, "b"))))))));

    public static final Def normalAnnotationToExpr = def(
        "normalAnnotationToExpr",
        () -> lambda("na",
                let(
                    field("tname",
                        proj(NormalAnnotation.TYPE_, NormalAnnotation.TYPE_NAME, "na")),
                    field("pairs",
                        proj(NormalAnnotation.TYPE_, NormalAnnotation.PAIRS, "na")),
                    apply(
                        var("hydra.serialization.prefix"),
                        string("@"),
                        apply(
                            var("hydra.serialization.noSep"),
                            list(
                                apply(ref(Serde.typeNameToExpr), var("tname")),
                                apply(
                                    var("hydra.serialization.commaSep"),
                                    var("hydra.serialization.inlineStyle"),
                                    Lists.map(ref(Serde.elementValuePairToExpr), var("pairs")))))))));

    public static final Def normalClassDeclarationToExpr = def(
        "normalClassDeclarationToExpr",
        () -> lambda("ncd",
                let(
                    field("mods",
                        proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.MODIFIERS, "ncd")),
                    field("id",
                        proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.IDENTIFIER, "ncd")),
                    field("tparams",
                        proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.PARAMETERS, "ncd")),
                    field("msuperc",
                        proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.EXTENDS, "ncd")),
                    field("superi",
                        proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.IMPLEMENTS, "ncd")),
                    field("body",
                        proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.BODY, "ncd")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(ref(Serde.classModifierToExpr), var("mods"))))),
                                just(apply(var("hydra.serialization.cst"), string("class"))),
                                just(
                                    apply(
                                        var("hydra.serialization.noSep"),
                                        Maybes.cat(
                                            list(
                                                just(
                                                    apply(
                                                        ref(Serde.typeIdentifierToExpr),
                                                        var("id"))),
                                                Logic.ifElse(
                                                    Lists.null_(var("tparams")),
                                                    nothing(),
                                                    just(
                                                        apply(
                                                            var("hydra.serialization.angleBracesList"),
                                                            var("hydra.serialization.inlineStyle"),
                                                            Lists.map(
                                                                ref(Serde.typeParameterToExpr),
                                                                var("tparams"))))))))),
                                Maybes.map(
                                    lambda("c",
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            list(
                                                apply(
                                                    var("hydra.serialization.cst"),
                                                    string("extends")),
                                                apply(ref(Serde.classTypeToExpr), var("c"))))),
                                    var("msuperc")),
                                Logic.ifElse(
                                    Lists.null_(var("superi")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            list(
                                                apply(
                                                    var("hydra.serialization.cst"),
                                                    string("implements")),
                                                apply(
                                                    var("hydra.serialization.commaSep"),
                                                    var("hydra.serialization.inlineStyle"),
                                                    Lists.map(
                                                        ref(Serde.interfaceTypeToExpr),
                                                        var("superi"))))))),
                                just(apply(ref(Serde.classBodyToExpr), var("body")))))))));

    public static final Def normalInterfaceDeclarationToExpr = def(
        "normalInterfaceDeclarationToExpr",
        () -> lambda("nid",
                let(
                    field("mods",
                        proj(NormalInterfaceDeclaration.TYPE_, NormalInterfaceDeclaration.MODIFIERS, "nid")),
                    field("id",
                        proj(NormalInterfaceDeclaration.TYPE_, NormalInterfaceDeclaration.IDENTIFIER, "nid")),
                    field("tparams",
                        proj(NormalInterfaceDeclaration.TYPE_, NormalInterfaceDeclaration.PARAMETERS, "nid")),
                    field("extends",
                        proj(NormalInterfaceDeclaration.TYPE_, NormalInterfaceDeclaration.EXTENDS, "nid")),
                    field("body",
                        proj(NormalInterfaceDeclaration.TYPE_, NormalInterfaceDeclaration.BODY, "nid")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(
                                                ref(Serde.interfaceModifierToExpr),
                                                var("mods"))))),
                                just(apply(var("hydra.serialization.cst"), string("interface"))),
                                just(
                                    apply(
                                        var("hydra.serialization.noSep"),
                                        Maybes.cat(
                                            list(
                                                just(
                                                    apply(
                                                        ref(Serde.typeIdentifierToExpr),
                                                        var("id"))),
                                                Logic.ifElse(
                                                    Lists.null_(var("tparams")),
                                                    nothing(),
                                                    just(
                                                        apply(
                                                            var("hydra.serialization.angleBracesList"),
                                                            var("hydra.serialization.inlineStyle"),
                                                            Lists.map(
                                                                ref(Serde.typeParameterToExpr),
                                                                var("tparams"))))))))),
                                Logic.ifElse(
                                    Lists.null_(var("extends")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            list(
                                                apply(
                                                    var("hydra.serialization.cst"),
                                                    string("extends")),
                                                apply(
                                                    var("hydra.serialization.commaSep"),
                                                    var("hydra.serialization.inlineStyle"),
                                                    Lists.map(
                                                        ref(Serde.interfaceTypeToExpr),
                                                        var("extends"))))))),
                                just(apply(ref(Serde.interfaceBodyToExpr), var("body")))))))));

    public static final Def numericTypeToExpr = def(
        "numericTypeToExpr",
        () -> lambda("nt",
                cases(NumericType.TYPE_,
                    var("nt"),
                    field(
                        NumericType.INTEGRAL,
                        lambda("it", apply(ref(Serde.integralTypeToExpr), var("it")))),
                    field(
                        NumericType.FLOATING_POINT,
                        lambda("ft", apply(ref(Serde.floatingPointTypeToExpr), var("ft")))))));

    public static final Def packageDeclarationToExpr = def(
        "packageDeclarationToExpr",
        () -> lambda("pd",
                let(
                    field("mods",
                        proj(PackageDeclaration.TYPE_, PackageDeclaration.MODIFIERS, "pd")),
                    field("ids",
                        proj(PackageDeclaration.TYPE_, PackageDeclaration.IDENTIFIERS, "pd")),
                    apply(
                        var("hydra.serialization.withSemi"),
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    Logic.ifElse(
                                        Lists.null_(var("mods")),
                                        nothing(),
                                        just(
                                            apply(
                                                var("hydra.serialization.spaceSep"),
                                                Lists.map(
                                                    ref(Serde.packageModifierToExpr),
                                                    var("mods"))))),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            list(
                                                apply(
                                                    var("hydra.serialization.cst"),
                                                    string("package")),
                                                apply(
                                                    var("hydra.serialization.cst"),
                                                    Strings.intercalate(
                                                        string("."),
                                                        Lists.map(
                                                            lambda("id",
                                                                apply(
                                                                    unwrap(Identifier.TYPE_),
                                                                    var("id"))),
                                                            var("ids"))))))))))))));

    public static final Def packageModifierToExpr = def(
        "packageModifierToExpr",
        () -> lambda("pm",
                apply(
                    ref(Serde.annotationToExpr),
                    apply(unwrap(PackageModifier.TYPE_), var("pm")))));

    public static final Def packageNameToExpr = def(
        "packageNameToExpr",
        () -> lambda("pn",
                apply(
                    var("hydra.serialization.dotSep"),
                    Lists.map(
                        ref(Serde.identifierToExpr),
                        apply(unwrap(PackageName.TYPE_), var("pn"))))));

    public static final Def packageOrTypeNameToExpr = def(
        "packageOrTypeNameToExpr",
        () -> lambda("potn",
                apply(
                    var("hydra.serialization.dotSep"),
                    Lists.map(
                        ref(Serde.identifierToExpr),
                        apply(unwrap(PackageOrTypeName.TYPE_), var("potn"))))));

    public static final Def padHex4 = def(
        "padHex4",
        () -> lambda("n",
                let(
                    field("d3",
                        Maybes.fromMaybe(int32(0), Math_.maybeDiv(var("n"), int32(4096)))),
                    field("r3",
                        Maybes.fromMaybe(int32(0), Math_.maybeMod(var("n"), int32(4096)))),
                    field("d2",
                        Maybes.fromMaybe(int32(0), Math_.maybeDiv(var("r3"), int32(256)))),
                    field("r2",
                        Maybes.fromMaybe(int32(0), Math_.maybeMod(var("r3"), int32(256)))),
                    field("d1",
                        Maybes.fromMaybe(int32(0), Math_.maybeDiv(var("r2"), int32(16)))),
                    field("d0",
                        Maybes.fromMaybe(int32(0), Math_.maybeMod(var("r2"), int32(16)))),
                    Strings.fromList(
                        list(
                            apply(ref(Serde.hexDigit), var("d3")),
                            apply(ref(Serde.hexDigit), var("d2")),
                            apply(ref(Serde.hexDigit), var("d1")),
                            apply(ref(Serde.hexDigit), var("d0")))))));

    public static final Def postDecrementExpressionToExpr = def(
        "postDecrementExpressionToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:PostDecrementExpression"))));

    public static final Def postIncrementExpressionToExpr = def(
        "postIncrementExpressionToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:PostIncrementExpression"))));

    public static final Def postfixExpressionToExpr = def(
        "postfixExpressionToExpr",
        () -> lambda("e",
                cases(PostfixExpression.TYPE_,
                    var("e"),
                    field(
                        PostfixExpression.PRIMARY,
                        lambda("p", apply(ref(Serde.primaryToExpr), var("p")))),
                    field(
                        PostfixExpression.NAME,
                        lambda("en", apply(ref(Serde.expressionNameToExpr), var("en")))),
                    field(
                        PostfixExpression.POST_INCREMENT,
                        lambda("pi", apply(ref(Serde.postIncrementExpressionToExpr), var("pi")))),
                    field(
                        PostfixExpression.POST_DECREMENT,
                        lambda("pd", apply(ref(Serde.postDecrementExpressionToExpr), var("pd")))))));

    public static final Def preDecrementExpressionToExpr = def(
        "preDecrementExpressionToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:PreDecrementExpression"))));

    public static final Def preIncrementExpressionToExpr = def(
        "preIncrementExpressionToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:PreIncrementExpression"))));

    public static final Def primaryNoNewArrayExpressionExpressionToExpr = def(
        "primaryNoNewArrayExpressionExpressionToExpr",
        () -> lambda("p",
                cases(PrimaryNoNewArrayExpression.TYPE_,
                    var("p"),
                    field(
                        PrimaryNoNewArrayExpression.LITERAL,
                        lambda("l", apply(ref(Serde.literalToExpr), var("l")))),
                    field(
                        PrimaryNoNewArrayExpression.CLASS_LITERAL,
                        lambda("cl", apply(ref(Serde.classLiteralToExpr), var("cl")))),
                    field(
                        PrimaryNoNewArrayExpression.THIS,
                        constant(apply(var("hydra.serialization.cst"), string("this")))),
                    field(
                        PrimaryNoNewArrayExpression.DOT_THIS,
                        lambda("n",
                            apply(
                                var("hydra.serialization.dotSep"),
                                list(
                                    apply(ref(Serde.typeNameToExpr), var("n")),
                                    apply(var("hydra.serialization.cst"), string("this")))))),
                    field(
                        PrimaryNoNewArrayExpression.PARENS,
                        lambda("e",
                            apply(
                                var("hydra.serialization.parenList"),
                                bool(false),
                                list(apply(ref(Serde.expressionToExpr), var("e")))))),
                    field(
                        PrimaryNoNewArrayExpression.CLASS_INSTANCE,
                        lambda("ci",
                            apply(ref(Serde.classInstanceCreationExpressionToExpr), var("ci")))),
                    field(
                        PrimaryNoNewArrayExpression.FIELD_ACCESS,
                        lambda("fa", apply(ref(Serde.fieldAccessToExpr), var("fa")))),
                    field(
                        PrimaryNoNewArrayExpression.ARRAY_ACCESS,
                        lambda("aa", apply(ref(Serde.arrayAccessToExpr), var("aa")))),
                    field(
                        PrimaryNoNewArrayExpression.METHOD_INVOCATION,
                        lambda("mi", apply(ref(Serde.methodInvocationToExpr), var("mi")))),
                    field(
                        PrimaryNoNewArrayExpression.METHOD_REFERENCE,
                        lambda("mr", apply(ref(Serde.methodReferenceToExpr), var("mr")))))));

    public static final Def primaryToExpr = def(
        "primaryToExpr",
        () -> lambda("p",
                cases(Primary.TYPE_,
                    var("p"),
                    field(
                        Primary.NO_NEW_ARRAY,
                        lambda("n",
                            apply(ref(Serde.primaryNoNewArrayExpressionExpressionToExpr), var("n")))),
                    field(
                        Primary.ARRAY_CREATION,
                        lambda("a", apply(ref(Serde.arrayCreationExpressionToExpr), var("a")))))));

    public static final Def primitiveTypeToExpr = def(
        "primitiveTypeToExpr",
        () -> lambda("pt",
                cases(PrimitiveType.TYPE_,
                    var("pt"),
                    field(
                        PrimitiveType.NUMERIC,
                        lambda("nt", apply(ref(Serde.numericTypeToExpr), var("nt")))),
                    field(
                        PrimitiveType.BOOLEAN,
                        constant(apply(var("hydra.serialization.cst"), string("boolean")))))));

    public static final Def primitiveTypeWithAnnotationsToExpr = def(
        "primitiveTypeWithAnnotationsToExpr",
        () -> lambda("ptwa",
                let(
                    field("pt",
                        proj(PrimitiveTypeWithAnnotations.TYPE_, PrimitiveTypeWithAnnotations.TYPE, "ptwa")),
                    field("anns",
                        proj(PrimitiveTypeWithAnnotations.TYPE_, PrimitiveTypeWithAnnotations.ANNOTATIONS, "ptwa")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("anns")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(ref(Serde.annotationToExpr), var("anns"))))),
                                just(apply(ref(Serde.primitiveTypeToExpr), var("pt")))))))));

    public static final Def receiverParameterToExpr = def(
        "receiverParameterToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:ReceiverParameter"))));

    public static final Def referenceTypeToExpr = def(
        "referenceTypeToExpr",
        () -> lambda("rt",
                cases(ReferenceType.TYPE_,
                    var("rt"),
                    field(
                        ReferenceType.CLASS_OR_INTERFACE,
                        lambda("cit", apply(ref(Serde.classOrInterfaceTypeToExpr), var("cit")))),
                    field(
                        ReferenceType.VARIABLE,
                        lambda("v", apply(ref(Serde.typeVariableToExpr), var("v")))),
                    field(
                        ReferenceType.ARRAY,
                        lambda("at", apply(ref(Serde.arrayTypeToExpr), var("at")))))));

    public static final Def relationalExpressionGreaterThanEqualToExpr = def(
        "relationalExpressionGreaterThanEqualToExpr",
        () -> lambda("gte",
                apply(
                    var("hydra.serialization.infixWs"),
                    string(">="),
                    apply(
                        ref(Serde.relationalExpressionToExpr),
                        proj(RelationalExpression_GreaterThanEqual.TYPE_, RelationalExpression_GreaterThanEqual.LHS, "gte")),
                    apply(
                        ref(Serde.shiftExpressionToExpr),
                        proj(RelationalExpression_GreaterThanEqual.TYPE_, RelationalExpression_GreaterThanEqual.RHS, "gte")))));

    public static final Def relationalExpressionGreaterThanToExpr = def(
        "relationalExpressionGreaterThanToExpr",
        () -> lambda("gt",
                apply(
                    var("hydra.serialization.infixWs"),
                    string(">"),
                    apply(
                        ref(Serde.relationalExpressionToExpr),
                        proj(RelationalExpression_GreaterThan.TYPE_, RelationalExpression_GreaterThan.LHS, "gt")),
                    apply(
                        ref(Serde.shiftExpressionToExpr),
                        proj(RelationalExpression_GreaterThan.TYPE_, RelationalExpression_GreaterThan.RHS, "gt")))));

    public static final Def relationalExpressionInstanceOfToExpr = def(
        "relationalExpressionInstanceOfToExpr",
        () -> lambda("io",
                let("rhsExpr",
                    cases(InstanceofExpression_Rhs.TYPE_,
                        proj(InstanceofExpression.TYPE_, InstanceofExpression.RHS, "io"),
                        field(
                            InstanceofExpression_Rhs.REFERENCE_TYPE,
                            lambda("rt", apply(ref(Serde.referenceTypeToExpr), var("rt")))),
                        field(
                            InstanceofExpression_Rhs.PATTERN,
                            constant(apply(var("hydra.serialization.cst"), string("STUB:Pattern"))))),
                    apply(
                        var("hydra.serialization.infixWs"),
                        string("instanceof"),
                        apply(
                            ref(Serde.relationalExpressionToExpr),
                            proj(InstanceofExpression.TYPE_, InstanceofExpression.LHS, "io")),
                        var("rhsExpr")))));

    public static final Def relationalExpressionLessThanEqualToExpr = def(
        "relationalExpressionLessThanEqualToExpr",
        () -> lambda("lte",
                apply(
                    var("hydra.serialization.infixWs"),
                    string("<="),
                    apply(
                        ref(Serde.relationalExpressionToExpr),
                        proj(RelationalExpression_LessThanEqual.TYPE_, RelationalExpression_LessThanEqual.LHS, "lte")),
                    apply(
                        ref(Serde.shiftExpressionToExpr),
                        proj(RelationalExpression_LessThanEqual.TYPE_, RelationalExpression_LessThanEqual.RHS, "lte")))));

    public static final Def relationalExpressionLessThanToExpr = def(
        "relationalExpressionLessThanToExpr",
        () -> lambda("lt",
                apply(
                    var("hydra.serialization.infixWs"),
                    string("<"),
                    apply(
                        ref(Serde.relationalExpressionToExpr),
                        proj(RelationalExpression_LessThan.TYPE_, RelationalExpression_LessThan.LHS, "lt")),
                    apply(
                        ref(Serde.shiftExpressionToExpr),
                        proj(RelationalExpression_LessThan.TYPE_, RelationalExpression_LessThan.RHS, "lt")))));

    public static final Def relationalExpressionToExpr = def(
        "relationalExpressionToExpr",
        () -> lambda("e",
                cases(RelationalExpression.TYPE_,
                    var("e"),
                    field(
                        RelationalExpression.SIMPLE,
                        lambda("s", apply(ref(Serde.shiftExpressionToExpr), var("s")))),
                    field(
                        RelationalExpression.LESS_THAN,
                        lambda("lt",
                            apply(ref(Serde.relationalExpressionLessThanToExpr), var("lt")))),
                    field(
                        RelationalExpression.GREATER_THAN,
                        lambda("gt",
                            apply(ref(Serde.relationalExpressionGreaterThanToExpr), var("gt")))),
                    field(
                        RelationalExpression.LESS_THAN_EQUAL,
                        lambda("lte",
                            apply(ref(Serde.relationalExpressionLessThanEqualToExpr), var("lte")))),
                    field(
                        RelationalExpression.GREATER_THAN_EQUAL,
                        lambda("gte",
                            apply(ref(Serde.relationalExpressionGreaterThanEqualToExpr), var("gte")))),
                    field(
                        RelationalExpression.INSTANCEOF_EXPRESSION,
                        lambda("i",
                            apply(ref(Serde.relationalExpressionInstanceOfToExpr), var("i")))))));

    public static final Def resultToExpr = def(
        "resultToExpr",
        () -> lambda("r",
                cases(Result.TYPE_,
                    var("r"),
                    field(
                        Result.TYPE,
                        lambda("t", apply(ref(Serde.unannTypeToExpr), var("t")))),
                    field(
                        Result.VOID,
                        constant(apply(var("hydra.serialization.cst"), string("void")))))));

    public static final Def returnStatementToExpr = def(
        "returnStatementToExpr",
        () -> lambda("rs",
                let("mex",
                    apply(unwrap(ReturnStatement.TYPE_), var("rs")),
                    apply(
                        var("hydra.serialization.withSemi"),
                        apply(
                            var("hydra.serialization.spaceSep"),
                            Maybes.cat(
                                list(
                                    just(apply(var("hydra.serialization.cst"), string("return"))),
                                    Maybes.map(ref(Serde.expressionToExpr), var("mex")))))))));

    public static final Def sanitizeJavaComment = def(
        "sanitizeJavaComment",
        () -> doc("Sanitize a string for use in a Java comment",
                lambda("s",
                    Strings.intercalate(
                        string("&gt;"),
                        Strings.splitOn(
                            string(">"),
                            Strings.intercalate(
                                string("&lt;"),
                                Strings.splitOn(string("<"), var("s"))))))));

    public static final Def shiftExpressionToExpr = def(
        "shiftExpressionToExpr",
        () -> lambda("e",
                cases(ShiftExpression.TYPE_,
                    var("e"),
                    field(
                        ShiftExpression.UNARY,
                        lambda("a", apply(ref(Serde.additiveExpressionToExpr), var("a")))),
                    field(
                        ShiftExpression.SHIFT_LEFT,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("<<"),
                                apply(
                                    ref(Serde.shiftExpressionToExpr),
                                    proj(ShiftExpression_Binary.TYPE_, ShiftExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.additiveExpressionToExpr),
                                    proj(ShiftExpression_Binary.TYPE_, ShiftExpression_Binary.RHS, "b"))))),
                    field(
                        ShiftExpression.SHIFT_RIGHT,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string(">>"),
                                apply(
                                    ref(Serde.shiftExpressionToExpr),
                                    proj(ShiftExpression_Binary.TYPE_, ShiftExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.additiveExpressionToExpr),
                                    proj(ShiftExpression_Binary.TYPE_, ShiftExpression_Binary.RHS, "b"))))),
                    field(
                        ShiftExpression.SHIFT_RIGHT_ZERO_FILL,
                        lambda("b",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string(">>>"),
                                apply(
                                    ref(Serde.shiftExpressionToExpr),
                                    proj(ShiftExpression_Binary.TYPE_, ShiftExpression_Binary.LHS, "b")),
                                apply(
                                    ref(Serde.additiveExpressionToExpr),
                                    proj(ShiftExpression_Binary.TYPE_, ShiftExpression_Binary.RHS, "b"))))))));

    public static final Def simpleTypeNameToExpr = def(
        "simpleTypeNameToExpr",
        () -> lambda("stn",
                apply(
                    ref(Serde.typeIdentifierToExpr),
                    apply(unwrap(SimpleTypeName.TYPE_), var("stn")))));

    public static final Def singleElementAnnotationToExpr = def(
        "singleElementAnnotationToExpr",
        () -> lambda("sea",
                let(
                    field("tname",
                        proj(SingleElementAnnotation.TYPE_, SingleElementAnnotation.NAME, "sea")),
                    field("mv",
                        proj(SingleElementAnnotation.TYPE_, SingleElementAnnotation.VALUE, "sea")),
                    Maybes.cases(var("mv"), apply(
                            ref(Serde.markerAnnotationToExpr),
                            wrap(MarkerAnnotation.TYPE_, var("tname"))), lambda("v",
                            apply(
                                var("hydra.serialization.prefix"),
                                string("@"),
                                apply(
                                    var("hydra.serialization.noSep"),
                                    list(
                                        apply(ref(Serde.typeNameToExpr), var("tname")),
                                        apply(
                                            var("hydra.serialization.parenList"),
                                            bool(false),
                                            list(apply(ref(Serde.elementValueToExpr), var("v"))))))))))));

    public static final Def singleLineComment = def(
        "singleLineComment",
        () -> doc(
                "Create a single-line Java comment. Empty text emits `//` (no trailing space) so blank line comments don't carry trailing whitespace.",
                lambda("c",
                    let("sanitized",
                        apply(ref(Serde.sanitizeJavaComment), var("c")),
                        apply(
                            var("hydra.serialization.cst"),
                            Logic.ifElse(
                                Equality.equal(var("sanitized"), string("")),
                                string("//"),
                                Strings.cat2(string("// "), var("sanitized"))))))));

    public static final Def statementExpressionToExpr = def(
        "statementExpressionToExpr",
        () -> lambda("e",
                cases(StatementExpression.TYPE_,
                    var("e"),
                    field(
                        StatementExpression.ASSIGNMENT,
                        lambda("a", apply(ref(Serde.assignmentToExpr), var("a")))),
                    field(
                        StatementExpression.PRE_INCREMENT,
                        lambda("pi", apply(ref(Serde.preIncrementExpressionToExpr), var("pi")))),
                    field(
                        StatementExpression.PRE_DECREMENT,
                        lambda("pd", apply(ref(Serde.preDecrementExpressionToExpr), var("pd")))),
                    field(
                        StatementExpression.POST_INCREMENT,
                        lambda("pi", apply(ref(Serde.postIncrementExpressionToExpr), var("pi")))),
                    field(
                        StatementExpression.POST_DECREMENT,
                        lambda("pd", apply(ref(Serde.postDecrementExpressionToExpr), var("pd")))),
                    field(
                        StatementExpression.METHOD_INVOCATION,
                        lambda("m", apply(ref(Serde.methodInvocationToExpr), var("m")))),
                    field(
                        StatementExpression.CLASS_INSTANCE_CREATION,
                        lambda("cic",
                            apply(ref(Serde.classInstanceCreationExpressionToExpr), var("cic")))))));

    public static final Def statementToExpr = def(
        "statementToExpr",
        () -> lambda("s",
                cases(Statement.TYPE_,
                    var("s"),
                    field(
                        Statement.WITHOUT_TRAILING,
                        lambda("s",
                            apply(ref(Serde.statementWithoutTrailingSubstatementToExpr), var("s")))),
                    field(
                        Statement.LABELED,
                        lambda("l", apply(ref(Serde.labeledStatementToExpr), var("l")))),
                    field(
                        Statement.IF_THEN,
                        lambda("it", apply(ref(Serde.ifThenStatementToExpr), var("it")))),
                    field(
                        Statement.IF_THEN_ELSE,
                        lambda("ite", apply(ref(Serde.ifThenElseStatementToExpr), var("ite")))),
                    field(
                        Statement.WHILE,
                        lambda("w", apply(ref(Serde.whileStatementToExpr), var("w")))),
                    field(
                        Statement.FOR,
                        lambda("f", apply(ref(Serde.forStatementToExpr), var("f")))))));

    public static final Def statementWithoutTrailingSubstatementToExpr = def(
        "statementWithoutTrailingSubstatementToExpr",
        () -> lambda("s",
                cases(StatementWithoutTrailingSubstatement.TYPE_,
                    var("s"),
                    field(
                        StatementWithoutTrailingSubstatement.BLOCK,
                        lambda("b", apply(ref(Serde.blockToExpr), var("b")))),
                    field(
                        StatementWithoutTrailingSubstatement.EMPTY,
                        constant(apply(var("hydra.serialization.cst"), string(";")))),
                    field(
                        StatementWithoutTrailingSubstatement.EXPRESSION,
                        lambda("e", apply(ref(Serde.expressionStatementToExpr), var("e")))),
                    field(
                        StatementWithoutTrailingSubstatement.ASSERT,
                        lambda("a", apply(ref(Serde.assertStatementToExpr), var("a")))),
                    field(
                        StatementWithoutTrailingSubstatement.SWITCH,
                        lambda("s", apply(ref(Serde.switchStatementToExpr), var("s")))),
                    field(
                        StatementWithoutTrailingSubstatement.DO,
                        lambda("d", apply(ref(Serde.doStatementToExpr), var("d")))),
                    field(
                        StatementWithoutTrailingSubstatement.BREAK,
                        lambda("b", apply(ref(Serde.breakStatementToExpr), var("b")))),
                    field(
                        StatementWithoutTrailingSubstatement.CONTINUE,
                        lambda("c", apply(ref(Serde.continueStatementToExpr), var("c")))),
                    field(
                        StatementWithoutTrailingSubstatement.RETURN,
                        lambda("r", apply(ref(Serde.returnStatementToExpr), var("r")))),
                    field(
                        StatementWithoutTrailingSubstatement.SYNCHRONIZED,
                        lambda("s", apply(ref(Serde.synchronizedStatementToExpr), var("s")))),
                    field(
                        StatementWithoutTrailingSubstatement.THROW,
                        lambda("t", apply(ref(Serde.throwStatementToExpr), var("t")))),
                    field(
                        StatementWithoutTrailingSubstatement.TRY,
                        lambda("t", apply(ref(Serde.tryStatementToExpr), var("t")))))));

    public static final Def staticInitializerToExpr = def(
        "staticInitializerToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:StaticInitializer"))));

    public static final Def stringLiteralToExpr = def(
        "stringLiteralToExpr",
        () -> doc("Serialize a Java string literal with proper Unicode escaping.",
                lambda("sl",
                    let("s",
                        apply(unwrap(StringLiteral.TYPE_), var("sl")),
                        apply(
                            var("hydra.serialization.cst"),
                            Strings.cat2(
                                string("\""),
                                Strings.cat2(
                                    apply(ref(Serde.escapeJavaString), var("s")),
                                    string("\""))))))));

    public static final Def switchStatementToExpr = def(
        "switchStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:SwitchStatement"))));

    public static final Def synchronizedStatementToExpr = def(
        "synchronizedStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:SynchronizedStatement"))));

    public static final Def throwStatementToExpr = def(
        "throwStatementToExpr",
        () -> lambda("ts",
                apply(
                    var("hydra.serialization.withSemi"),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        list(
                            apply(var("hydra.serialization.cst"), string("throw")),
                            apply(
                                ref(Serde.expressionToExpr),
                                apply(unwrap(ThrowStatement.TYPE_), var("ts"))))))));

    public static final Def throwsToExpr = def(
        "throwsToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:Throws"))));

    public static final Def tryStatementToExpr = def(
        "tryStatementToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:TryStatement"))));

    public static final Def typeArgumentToExpr = def(
        "typeArgumentToExpr",
        () -> lambda("a",
                cases(TypeArgument.TYPE_,
                    var("a"),
                    field(
                        TypeArgument.REFERENCE,
                        lambda("rt", apply(ref(Serde.referenceTypeToExpr), var("rt")))),
                    field(
                        TypeArgument.WILDCARD,
                        lambda("w", apply(ref(Serde.wildcardToExpr), var("w")))))));

    public static final Def typeArgumentsOrDiamondToExpr = def(
        "typeArgumentsOrDiamondToExpr",
        () -> lambda("targs",
                cases(TypeArgumentsOrDiamond.TYPE_,
                    var("targs"),
                    field(
                        TypeArgumentsOrDiamond.ARGUMENTS,
                        lambda("args",
                            apply(
                                var("hydra.serialization.angleBracesList"),
                                var("hydra.serialization.inlineStyle"),
                                Lists.map(ref(Serde.typeArgumentToExpr), var("args"))))),
                    field(
                        TypeArgumentsOrDiamond.DIAMOND,
                        constant(apply(var("hydra.serialization.cst"), string("<>")))))));

    public static final Def typeBoundToExpr = def(
        "typeBoundToExpr",
        () -> lambda("b",
                cases(TypeBound.TYPE_,
                    var("b"),
                    field(
                        TypeBound.VARIABLE,
                        lambda("tv", apply(ref(Serde.typeVariableToExpr), var("tv")))),
                    field(
                        TypeBound.CLASS_OR_INTERFACE,
                        lambda("ci",
                            let(
                                field("cit",
                                    proj(TypeBound_ClassOrInterface.TYPE_, TypeBound_ClassOrInterface.TYPE, "ci")),
                                field("additional",
                                    proj(TypeBound_ClassOrInterface.TYPE_, TypeBound_ClassOrInterface.ADDITIONAL, "ci")),
                                Logic.ifElse(
                                    Lists.null_(var("additional")),
                                    apply(ref(Serde.classOrInterfaceTypeToExpr), var("cit")),
                                    apply(
                                        var("hydra.serialization.spaceSep"),
                                        Lists.cons(
                                            apply(ref(Serde.classOrInterfaceTypeToExpr), var("cit")),
                                            Lists.map(
                                                ref(Serde.additionalBoundToExpr),
                                                var("additional")))))))))));

    public static final Def typeDeclarationToExpr = def(
        "typeDeclarationToExpr",
        () -> lambda("d",
                cases(TopLevelClassOrInterfaceDeclaration.TYPE_,
                    var("d"),
                    field(
                        TopLevelClassOrInterfaceDeclaration.CLASS,
                        lambda("d", apply(ref(Serde.classDeclarationToExpr), var("d")))),
                    field(
                        TopLevelClassOrInterfaceDeclaration.INTERFACE,
                        lambda("d", apply(ref(Serde.interfaceDeclarationToExpr), var("d")))),
                    field(
                        TopLevelClassOrInterfaceDeclaration.NONE,
                        constant(apply(var("hydra.serialization.cst"), string(";")))))));

    public static final Def typeDeclarationWithCommentsToExpr = def(
        "typeDeclarationWithCommentsToExpr",
        () -> lambda("tdwc",
                let(
                    field("d",
                        proj(TopLevelClassOrInterfaceDeclarationWithComments.TYPE_, TopLevelClassOrInterfaceDeclarationWithComments.VALUE, "tdwc")),
                    field("mc",
                        proj(TopLevelClassOrInterfaceDeclarationWithComments.TYPE_, TopLevelClassOrInterfaceDeclarationWithComments.COMMENTS, "tdwc")),
                    apply(
                        ref(Serde.withComments),
                        var("mc"),
                        apply(ref(Serde.typeDeclarationToExpr), var("d"))))));

    public static final Def typeIdentifierToExpr = def(
        "typeIdentifierToExpr",
        () -> lambda("tid",
                apply(
                    ref(Serde.identifierToExpr),
                    apply(unwrap(TypeIdentifier.TYPE_), var("tid")))));

    public static final Def typeNameToExpr = def(
        "typeNameToExpr",
        () -> lambda("tn",
                let(
                    field("id",
                        proj(TypeName.TYPE_, TypeName.IDENTIFIER, "tn")),
                    field("mqual",
                        proj(TypeName.TYPE_, TypeName.QUALIFIER, "tn")),
                    apply(
                        var("hydra.serialization.dotSep"),
                        Maybes.cat(
                            list(
                                Maybes.map(ref(Serde.packageOrTypeNameToExpr), var("mqual")),
                                just(apply(ref(Serde.typeIdentifierToExpr), var("id")))))))));

    public static final Def typeParameterModifierToExpr = def(
        "typeParameterModifierToExpr",
        () -> lambda("tpm",
                apply(
                    ref(Serde.annotationToExpr),
                    apply(unwrap(TypeParameterModifier.TYPE_), var("tpm")))));

    public static final Def typeParameterToExpr = def(
        "typeParameterToExpr",
        () -> lambda("tp",
                let(
                    field("mods",
                        proj(TypeParameter.TYPE_, TypeParameter.MODIFIERS, "tp")),
                    field("id",
                        proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp")),
                    field("bound",
                        proj(TypeParameter.TYPE_, TypeParameter.BOUND, "tp")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("mods")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(
                                                ref(Serde.typeParameterModifierToExpr),
                                                var("mods"))))),
                                just(apply(ref(Serde.typeIdentifierToExpr), var("id"))),
                                Maybes.map(
                                    lambda("b",
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            list(
                                                apply(
                                                    var("hydra.serialization.cst"),
                                                    string("extends")),
                                                apply(ref(Serde.typeBoundToExpr), var("b"))))),
                                    var("bound"))))))));

    public static final Def typeToExpr = def(
        "typeToExpr",
        () -> lambda("t",
                cases(hydra.java.syntax.Type.TYPE_,
                    var("t"),
                    field(
                        hydra.java.syntax.Type.PRIMITIVE,
                        lambda("pt",
                            apply(ref(Serde.primitiveTypeWithAnnotationsToExpr), var("pt")))),
                    field(
                        hydra.java.syntax.Type.REFERENCE,
                        lambda("rt", apply(ref(Serde.referenceTypeToExpr), var("rt")))))));

    public static final Def typeVariableToExpr = def(
        "typeVariableToExpr",
        () -> lambda("tv",
                let(
                    field("anns",
                        proj(TypeVariable.TYPE_, TypeVariable.ANNOTATIONS, "tv")),
                    field("id",
                        proj(TypeVariable.TYPE_, TypeVariable.IDENTIFIER, "tv")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("anns")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.spaceSep"),
                                            Lists.map(ref(Serde.annotationToExpr), var("anns"))))),
                                just(apply(ref(Serde.typeIdentifierToExpr), var("id")))))))));

    public static final Def unannTypeToExpr = def(
        "unannTypeToExpr",
        () -> lambda("ut",
                apply(
                    ref(Serde.typeToExpr),
                    apply(unwrap(UnannType.TYPE_), var("ut")))));

    public static final Def unaryExpressionNotPlusMinusToExpr = def(
        "unaryExpressionNotPlusMinusToExpr",
        () -> lambda("e",
                cases(UnaryExpressionNotPlusMinus.TYPE_,
                    var("e"),
                    field(
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        lambda("p", apply(ref(Serde.postfixExpressionToExpr), var("p")))),
                    field(
                        UnaryExpressionNotPlusMinus.TILDE,
                        lambda("u",
                            apply(
                                var("hydra.serialization.spaceSep"),
                                list(
                                    apply(var("hydra.serialization.cst"), string("~")),
                                    apply(ref(Serde.unaryExpressionToExpr), var("u")))))),
                    field(
                        UnaryExpressionNotPlusMinus.NOT,
                        lambda("u",
                            apply(
                                var("hydra.serialization.noSep"),
                                list(
                                    apply(var("hydra.serialization.cst"), string("!")),
                                    apply(ref(Serde.unaryExpressionToExpr), var("u")))))),
                    field(
                        UnaryExpressionNotPlusMinus.CAST,
                        lambda("c", apply(ref(Serde.castExpressionToExpr), var("c")))))));

    public static final Def unaryExpressionToExpr = def(
        "unaryExpressionToExpr",
        () -> lambda("e",
                cases(UnaryExpression.TYPE_,
                    var("e"),
                    field(
                        UnaryExpression.PRE_INCREMENT,
                        lambda("pi", apply(ref(Serde.preIncrementExpressionToExpr), var("pi")))),
                    field(
                        UnaryExpression.PRE_DECREMENT,
                        lambda("pd", apply(ref(Serde.preDecrementExpressionToExpr), var("pd")))),
                    field(
                        UnaryExpression.PLUS,
                        lambda("p",
                            apply(
                                var("hydra.serialization.spaceSep"),
                                list(
                                    apply(var("hydra.serialization.cst"), string("+")),
                                    apply(ref(Serde.unaryExpressionToExpr), var("p")))))),
                    field(
                        UnaryExpression.MINUS,
                        lambda("m",
                            apply(
                                var("hydra.serialization.spaceSep"),
                                list(
                                    apply(var("hydra.serialization.cst"), string("-")),
                                    apply(ref(Serde.unaryExpressionToExpr), var("m")))))),
                    field(
                        UnaryExpression.OTHER,
                        lambda("o", apply(ref(Serde.unaryExpressionNotPlusMinusToExpr), var("o")))))));

    public static final Def unqualifiedClassInstanceCreationExpressionToExpr = def(
        "unqualifiedClassInstanceCreationExpressionToExpr",
        () -> lambda("ucice",
                let(
                    field("targs",
                        proj(UnqualifiedClassInstanceCreationExpression.TYPE_, UnqualifiedClassInstanceCreationExpression.TYPE_ARGUMENTS, "ucice")),
                    field("cit",
                        proj(UnqualifiedClassInstanceCreationExpression.TYPE_, UnqualifiedClassInstanceCreationExpression.CLASS_OR_INTERFACE, "ucice")),
                    field("args",
                        proj(UnqualifiedClassInstanceCreationExpression.TYPE_, UnqualifiedClassInstanceCreationExpression.ARGUMENTS, "ucice")),
                    field("mbody",
                        proj(UnqualifiedClassInstanceCreationExpression.TYPE_, UnqualifiedClassInstanceCreationExpression.BODY, "ucice")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                just(apply(var("hydra.serialization.cst"), string("new"))),
                                Logic.ifElse(
                                    Lists.null_(var("targs")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.angleBracesList"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(ref(Serde.typeArgumentToExpr), var("targs"))))),
                                just(
                                    apply(
                                        var("hydra.serialization.noSep"),
                                        list(
                                            apply(
                                                ref(Serde.classOrInterfaceTypeToInstantiateToExpr),
                                                var("cit")),
                                            apply(
                                                var("hydra.serialization.parenList"),
                                                bool(false),
                                                Lists.map(ref(Serde.expressionToExpr), var("args")))))),
                                Maybes.map(ref(Serde.classBodyToExpr), var("mbody"))))))));

    public static final Def variableArityParameterToExpr = def(
        "variableArityParameterToExpr",
        () -> constant(apply(var("hydra.serialization.cst"), string("STUB:VariableArityParameter"))));

    public static final Def variableDeclaratorIdToExpr = def(
        "variableDeclaratorIdToExpr",
        () -> lambda("vdi",
                let(
                    field("id",
                        proj(VariableDeclaratorId.TYPE_, VariableDeclaratorId.IDENTIFIER, "vdi")),
                    field("mdims",
                        proj(VariableDeclaratorId.TYPE_, VariableDeclaratorId.DIMS, "vdi")),
                    apply(
                        var("hydra.serialization.noSep"),
                        Maybes.cat(
                            list(
                                just(apply(ref(Serde.identifierToExpr), var("id"))),
                                Maybes.map(ref(Serde.dimsToExpr), var("mdims"))))))));

    public static final Def variableDeclaratorToExpr = def(
        "variableDeclaratorToExpr",
        () -> lambda("vd",
                let(
                    field("id",
                        proj(VariableDeclarator.TYPE_, VariableDeclarator.ID, "vd")),
                    field("minit",
                        proj(VariableDeclarator.TYPE_, VariableDeclarator.INITIALIZER, "vd")),
                    field("idSec",
                        apply(ref(Serde.variableDeclaratorIdToExpr), var("id"))),
                    Maybes.cases(var("minit"), var("idSec"), lambda("init",
                            apply(
                                var("hydra.serialization.infixWs"),
                                string("="),
                                var("idSec"),
                                apply(ref(Serde.variableInitializerToExpr), var("init"))))))));

    public static final Def variableInitializerToExpr = def(
        "variableInitializerToExpr",
        () -> lambda("i",
                cases(VariableInitializer.TYPE_,
                    var("i"),
                    field(
                        VariableInitializer.EXPRESSION,
                        lambda("e", apply(ref(Serde.expressionToExpr), var("e")))),
                    field(
                        VariableInitializer.ARRAY_INITIALIZER,
                        lambda("ai", apply(ref(Serde.arrayInitializerToExpr), var("ai")))))));

    public static final Def variableModifierToExpr = def(
        "variableModifierToExpr",
        () -> lambda("m",
                cases(VariableModifier.TYPE_,
                    var("m"),
                    field(
                        VariableModifier.ANNOTATION,
                        lambda("ann", apply(ref(Serde.annotationToExpr), var("ann")))),
                    field(
                        VariableModifier.FINAL,
                        constant(apply(var("hydra.serialization.cst"), string("final")))))));

    public static final Def whileStatementToExpr = def(
        "whileStatementToExpr",
        () -> lambda("ws",
                let(
                    field("mcond",
                        proj(WhileStatement.TYPE_, WhileStatement.COND, "ws")),
                    field("body",
                        proj(WhileStatement.TYPE_, WhileStatement.BODY, "ws")),
                    field("condSer",
                        Maybes.cases(var("mcond"), apply(var("hydra.serialization.cst"), string("true")), lambda("c", apply(ref(Serde.expressionToExpr), var("c"))))),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        list(
                            apply(var("hydra.serialization.cst"), string("while")),
                            apply(
                                var("hydra.serialization.parenList"),
                                bool(false),
                                list(var("condSer"))),
                            apply(
                                var("hydra.serialization.curlyBlock"),
                                var("hydra.serialization.fullBlockStyle"),
                                apply(ref(Serde.statementToExpr), var("body"))))))));

    public static final Def wildcardBoundsToExpr = def(
        "wildcardBoundsToExpr",
        () -> lambda("b",
                cases(WildcardBounds.TYPE_,
                    var("b"),
                    field(
                        WildcardBounds.EXTENDS,
                        lambda("rt",
                            apply(
                                var("hydra.serialization.spaceSep"),
                                list(
                                    apply(var("hydra.serialization.cst"), string("extends")),
                                    apply(ref(Serde.referenceTypeToExpr), var("rt")))))),
                    field(
                        WildcardBounds.SUPER,
                        lambda("rt",
                            apply(
                                var("hydra.serialization.spaceSep"),
                                list(
                                    apply(var("hydra.serialization.cst"), string("super")),
                                    apply(ref(Serde.referenceTypeToExpr), var("rt")))))))));

    public static final Def wildcardToExpr = def(
        "wildcardToExpr",
        () -> lambda("w",
                let(
                    field("anns",
                        proj(Wildcard.TYPE_, Wildcard.ANNOTATIONS, "w")),
                    field("mbounds",
                        proj(Wildcard.TYPE_, Wildcard.WILDCARD, "w")),
                    apply(
                        var("hydra.serialization.spaceSep"),
                        Maybes.cat(
                            list(
                                Logic.ifElse(
                                    Lists.null_(var("anns")),
                                    nothing(),
                                    just(
                                        apply(
                                            var("hydra.serialization.commaSep"),
                                            var("hydra.serialization.inlineStyle"),
                                            Lists.map(ref(Serde.annotationToExpr), var("anns"))))),
                                just(apply(var("hydra.serialization.cst"), string("*"))),
                                Maybes.map(ref(Serde.wildcardBoundsToExpr), var("mbounds"))))))));

    public static final Def withComments = def(
        "withComments",
        () -> doc(
                "Wrap an expression with optional Javadoc comments. Blank lines inside the doc body emit ` *` (no trailing space) instead of ` * `.",
                lambda(
                    "mc",
                    "expr",
                    Maybes.cases(var("mc"), var("expr"), lambda("c",
                            apply(
                                var("hydra.serialization.newlineSep"),
                                list(
                                    apply(
                                        var("hydra.serialization.cst"),
                                        Strings.cat2(
                                            string("/**\n"),
                                            Strings.cat2(
                                                Strings.intercalate(
                                                    string("\n"),
                                                    Lists.map(
                                                        lambda("l",
                                                            Logic.ifElse(
                                                                Equality.equal(var("l"), string("")),
                                                                string(" *"),
                                                                Strings.cat2(
                                                                    string(" * "),
                                                                    var("l")))),
                                                        Strings.lines(
                                                            apply(
                                                                ref(Serde.sanitizeJavaComment),
                                                                var("c"))))),
                                                string("\n */")))),
                                    var("expr"))))))));









    private static final List<Definition> DEFINITIONS = definitionsOf(
            additionalBoundToExpr,
            additiveExpressionToExpr,
            ambiguousNameToExpr,
            andExpressionToExpr,
            annotatedIdentifierToExpr,
            annotationToExpr,
            annotationTypeDeclarationToExpr,
            arrayAccessToExpr,
            arrayCreationExpressionToExpr,
            arrayInitializerToExpr,
            arrayTypeToExpr,
            assertStatementToExpr,
            assignmentExpressionToExpr,
            assignmentToExpr,
            blockStatementToExpr,
            blockToExpr,
            breakStatementToExpr,
            castExpressionLambdaToExpr,
            castExpressionNotPlusMinusToExpr,
            castExpressionPrimitiveToExpr,
            castExpressionRefAndBoundsToExpr,
            castExpressionToExpr,
            classBodyDeclarationToExpr,
            classBodyDeclarationWithCommentsToExpr,
            classBodyToExpr,
            classDeclarationToExpr,
            classInstanceCreationExpressionQualifierToExpr,
            classInstanceCreationExpressionToExpr,
            classLiteralToExpr,
            classMemberDeclarationToExpr,
            classModifierToExpr,
            classOrInterfaceTypeToExpr,
            classOrInterfaceTypeToInstantiateToExpr,
            classTypeToExpr,
            compilationUnitToExpr,
            conditionalAndExpressionToExpr,
            conditionalExpressionTernaryCondToExpr,
            conditionalExpressionTernaryLambdaToExpr,
            conditionalExpressionToExpr,
            conditionalOrExpressionToExpr,
            constantDeclarationToExpr,
            constantModifierToExpr,
            constructorBodyToExpr,
            constructorDeclarationToExpr,
            constructorDeclaratorToExpr,
            constructorModifierToExpr,
            continueStatementToExpr,
            dimsToExpr,
            doStatementToExpr,
            elementValuePairToExpr,
            elementValueToExpr,
            enumDeclarationToExpr,
            equalityExpressionToExpr,
            escapeJavaChar,
            escapeJavaString,
            exclusiveOrExpressionToExpr,
            explicitConstructorInvocationToExpr,
            expressionNameToExpr,
            expressionStatementToExpr,
            expressionToExpr,
            fieldAccessToExpr,
            fieldDeclarationToExpr,
            fieldModifierToExpr,
            floatingPointLiteralToExpr,
            floatingPointTypeToExpr,
            forStatementToExpr,
            formalParameterSimpleToExpr,
            formalParameterToExpr,
            hexDigit,
            identifierToExpr,
            ifThenElseStatementToExpr,
            ifThenStatementToExpr,
            importDeclarationToExpr,
            inclusiveOrExpressionToExpr,
            instanceInitializerToExpr,
            integerLiteralToExpr,
            integralTypeToExpr,
            interfaceBodyToExpr,
            interfaceDeclarationToExpr,
            interfaceMemberDeclarationToExpr,
            interfaceMemberDeclarationWithCommentsToExpr,
            interfaceMethodDeclarationToExpr,
            interfaceMethodModifierToExpr,
            interfaceModifierToExpr,
            interfaceTypeToExpr,
            javaFloatLiteralText,
            javaUnicodeEscape,
            labeledStatementToExpr,
            lambdaBodyToExpr,
            lambdaExpressionToExpr,
            lambdaParametersToExpr,
            leftHandSideToExpr,
            literalToExpr,
            localNameToExpr,
            localVariableDeclarationStatementToExpr,
            localVariableDeclarationToExpr,
            markerAnnotationToExpr,
            methodBodyToExpr,
            methodDeclarationToExpr,
            methodDeclaratorToExpr,
            methodHeaderToExpr,
            methodInvocationToExpr,
            methodModifierToExpr,
            methodNameToExpr,
            methodReferenceToExpr,
            multiplicativeExpressionToExpr,
            normalAnnotationToExpr,
            normalClassDeclarationToExpr,
            normalInterfaceDeclarationToExpr,
            numericTypeToExpr,
            packageDeclarationToExpr,
            packageModifierToExpr,
            packageNameToExpr,
            packageOrTypeNameToExpr,
            padHex4,
            postDecrementExpressionToExpr,
            postIncrementExpressionToExpr,
            postfixExpressionToExpr,
            preDecrementExpressionToExpr,
            preIncrementExpressionToExpr,
            primaryNoNewArrayExpressionExpressionToExpr,
            primaryToExpr,
            primitiveTypeToExpr,
            primitiveTypeWithAnnotationsToExpr,
            receiverParameterToExpr,
            referenceTypeToExpr,
            relationalExpressionGreaterThanEqualToExpr,
            relationalExpressionGreaterThanToExpr,
            relationalExpressionInstanceOfToExpr,
            relationalExpressionLessThanEqualToExpr,
            relationalExpressionLessThanToExpr,
            relationalExpressionToExpr,
            resultToExpr,
            returnStatementToExpr,
            sanitizeJavaComment,
            shiftExpressionToExpr,
            simpleTypeNameToExpr,
            singleElementAnnotationToExpr,
            singleLineComment,
            statementExpressionToExpr,
            statementToExpr,
            statementWithoutTrailingSubstatementToExpr,
            staticInitializerToExpr,
            stringLiteralToExpr,
            switchStatementToExpr,
            synchronizedStatementToExpr,
            throwStatementToExpr,
            throwsToExpr,
            tryStatementToExpr,
            typeArgumentToExpr,
            typeArgumentsOrDiamondToExpr,
            typeBoundToExpr,
            typeDeclarationToExpr,
            typeDeclarationWithCommentsToExpr,
            typeIdentifierToExpr,
            typeNameToExpr,
            typeParameterModifierToExpr,
            typeParameterToExpr,
            typeToExpr,
            typeVariableToExpr,
            unannTypeToExpr,
            unaryExpressionNotPlusMinusToExpr,
            unaryExpressionToExpr,
            unqualifiedClassInstanceCreationExpressionToExpr,
            variableArityParameterToExpr,
            variableDeclaratorIdToExpr,
            variableDeclaratorToExpr,
            variableInitializerToExpr,
            variableModifierToExpr,
            whileStatementToExpr,
            wildcardBoundsToExpr,
            wildcardToExpr,
            withComments);


    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
        new ModuleName("hydra.constants"),
        new ModuleName("hydra.serialization"),
        new ModuleName("hydra.java.syntax"),
        new ModuleName("hydra.paths"),
        new ModuleName("hydra.ast"),
        new ModuleName("hydra.classes"),
        new ModuleName("hydra.coders"),
        new ModuleName("hydra.core"),
        new ModuleName("hydra.error.checking"),
        new ModuleName("hydra.error.core"),
        new ModuleName("hydra.error.packaging"),
        new ModuleName("hydra.errors"),
        new ModuleName("hydra.graph"),
        new ModuleName("hydra.json.model"),
        new ModuleName("hydra.packaging"),
        new ModuleName("hydra.parsing"),
        new ModuleName("hydra.query"),
        new ModuleName("hydra.relational"),
        new ModuleName("hydra.tabular"),
        new ModuleName("hydra.testing"),
        new ModuleName("hydra.topology"),
        new ModuleName("hydra.typed"),
        new ModuleName("hydra.typing"),
        new ModuleName("hydra.util"),
        new ModuleName("hydra.validation"),
        new ModuleName("hydra.variants"));

    public static final Module module_ = new Module(
        NS,
        Maybe.just(new EntityMetadata(
            Maybe.just("Java serializer: converts Java AST to concrete syntax"),
            java.util.List.of(),
            java.util.List.of(),
            Maybe.nothing())),
        DEPENDENCIES,
        DEFINITIONS);
}
