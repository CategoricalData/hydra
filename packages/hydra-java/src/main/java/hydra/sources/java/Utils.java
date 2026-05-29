package hydra.sources.java;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Core;
import hydra.dsl.Errors;
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
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.ModuleDependency;
import hydra.phantoms.TTerm;
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
import hydra.errors.Error_;  // AUTO-IMPORT (hydra-java DSL)
import hydra.errors.OtherError;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.environment.Aliases;  // AUTO-IMPORT (hydra-java DSL)
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
import hydra.java.syntax.CastExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression_NotPlusMinus;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression_Primitive;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CastExpression_RefAndBounds;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBodyDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassInstanceCreationExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassMemberDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassOrInterfaceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassOrInterfaceTypeToInstantiate;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassTypeQualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalAndExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalOrExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorDeclarator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstructorModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Dims;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ElementValue;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.EqualityExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.EqualityExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExclusiveOrExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Expression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExpressionName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExpressionStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldAccess;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldAccess_Qualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FormalParameter;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FormalParameter_Simple;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Identifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InclusiveOrExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InstanceofExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InstanceofExpression_Rhs;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IntegerLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IntegralType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMemberDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMethodDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaParameters;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LeftHandSide;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Literal;  // AUTO-IMPORT (hydra-java DSL)
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
import hydra.java.syntax.NormalClassDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NumericType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PostfixExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Primary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimaryNoNewArrayExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimitiveType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimitiveTypeWithAnnotations;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ReferenceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.RelationalExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Result;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ReturnStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ShiftExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.SimpleTypeName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.SingleElementAnnotation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Statement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StatementExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StatementWithoutTrailingSubstatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StringLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ThrowStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeArgument;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeIdentifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeParameter;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeVariable;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnannType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnaryExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnaryExpressionNotPlusMinus;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnqualifiedClassInstanceCreationExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableDeclarator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableDeclaratorId;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableInitializer;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Wildcard;  // AUTO-IMPORT (hydra-java DSL)
import hydra.packaging.QualifiedName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.sources.java.Names;  // AUTO-IMPORT (hydra-java DSL)

/**
 * Java utilities for constructing Java syntax trees — Java DSL port of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Utils.hs}.
 */
public class Utils {
    public static final ModuleName NS = new ModuleName("hydra.java.utils");

    private static Def def(String localName, Supplier<TTerm<?>> body) {
        return define(NS, localName, body);
    }

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def addExpressions = def(
        "addExpressions",
        () -> lambda("exprs",
                let("dummyMult",
                    inject(MultiplicativeExpression.TYPE_,
                        MultiplicativeExpression.UNARY,
                        inject(UnaryExpression.TYPE_,
                            UnaryExpression.OTHER,
                            inject(UnaryExpressionNotPlusMinus.TYPE_,
                                UnaryExpressionNotPlusMinus.POSTFIX,
                                inject(PostfixExpression.TYPE_,
                                    PostfixExpression.PRIMARY,
                                    inject(Primary.TYPE_,
                                        Primary.NO_NEW_ARRAY,
                                        inject(PrimaryNoNewArrayExpression.TYPE_,
                                            PrimaryNoNewArrayExpression.LITERAL,
                                            inject(Literal.TYPE_,
                                                Literal.INTEGER,
                                                wrap(IntegerLiteral.TYPE_,
                                                    bigint(java.math.BigInteger.valueOf(0L)))))))))),
                    Lists.foldl(
                        lambda(
                            "ae",
                            "me",
                            inject(AdditiveExpression.TYPE_,
                                AdditiveExpression.PLUS,
                                record(AdditiveExpression_Binary.TYPE_,
                                    field(
                                        AdditiveExpression_Binary.LHS,
                                        var("ae")),
                                    field(
                                        AdditiveExpression_Binary.RHS,
                                        var("me"))))),
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            Maybes.fromMaybe(var("dummyMult"), Lists.maybeHead(var("exprs")))),
                        Lists.drop(int32(1), var("exprs"))))));

    public static final Def addInScopeVar = def(
        "addInScopeVar",
        () -> lambda(
                "name",
                "aliases",
                record(Aliases.TYPE_,
                    field(
                        Aliases.CURRENT_NAMESPACE,
                        proj(Aliases.TYPE_, Aliases.CURRENT_NAMESPACE, "aliases")),
                    field(
                        Aliases.PACKAGES,
                        proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases")),
                    field(
                        Aliases.BRANCH_VARS,
                        proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases")),
                    field(
                        Aliases.RECURSIVE_VARS,
                        proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases")),
                    field(
                        Aliases.IN_SCOPE_TYPE_PARAMS,
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                    field(
                        Aliases.POLYMORPHIC_LOCALS,
                        proj(Aliases.TYPE_, Aliases.POLYMORPHIC_LOCALS, "aliases")),
                    field(
                        Aliases.IN_SCOPE_JAVA_VARS,
                        Sets.insert(
                            var("name"),
                            proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases"))),
                    field(
                        Aliases.VAR_RENAMES,
                        proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases")),
                    field(
                        Aliases.LAMBDA_VARS,
                        proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases")),
                    field(
                        Aliases.TYPE_VAR_SUBST,
                        proj(Aliases.TYPE_, Aliases.TYPE_VAR_SUBST, "aliases")),
                    field(
                        Aliases.TRUSTED_TYPE_VARS,
                        proj(Aliases.TYPE_, Aliases.TRUSTED_TYPE_VARS, "aliases")),
                    field(
                        Aliases.METHOD_CODOMAIN,
                        proj(Aliases.TYPE_, Aliases.METHOD_CODOMAIN, "aliases")),
                    field(
                        Aliases.THUNKED_VARS,
                        proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases")))));

    public static final Def addInScopeVars = def(
        "addInScopeVars",
        () -> lambda(
                "names",
                "aliases",
                Lists.foldl(
                    lambda("a", lambda("n", apply(ref(Utils.addInScopeVar), var("n"), var("a")))),
                    var("aliases"),
                    var("names"))));

    public static final Def addJavaTypeParameter = def(
        "addJavaTypeParameter",
        () -> lambda(
                "rt",
                "t",
                "cx",
                cases(hydra.java.syntax.Type.TYPE_,
                    var("t"),
                    field(
                        hydra.java.syntax.Type.REFERENCE,
                        lambda("rt1",
                            cases(ReferenceType.TYPE_,
                                var("rt1"),
                                field(
                                    ReferenceType.CLASS_OR_INTERFACE,
                                    lambda("cit",
                                        cases(ClassOrInterfaceType.TYPE_,
                                            var("cit"),
                                            field(
                                                ClassOrInterfaceType.CLASS,
                                                lambda("ct",
                                                    let(
                                                        field("anns",
                                                            proj(ClassType.TYPE_, ClassType.ANNOTATIONS, "ct")),
                                                        field("qual",
                                                            proj(ClassType.TYPE_, ClassType.QUALIFIER, "ct")),
                                                        field("id",
                                                            proj(ClassType.TYPE_, ClassType.IDENTIFIER, "ct")),
                                                        field("args",
                                                            proj(ClassType.TYPE_, ClassType.ARGUMENTS, "ct")),
                                                        right(
                                                            inject(hydra.java.syntax.Type.TYPE_,
                                                                hydra.java.syntax.Type.REFERENCE,
                                                                inject(
                                                                    ReferenceType.TYPE_,
                                                                    ReferenceType.CLASS_OR_INTERFACE,
                                                                    inject(
                                                                        ClassOrInterfaceType.TYPE_,
                                                                        ClassOrInterfaceType.CLASS,
                                                                        record(
                                                                            ClassType.TYPE_,
                                                                            field(
                                                                                ClassType.ANNOTATIONS,
                                                                                var("anns")),
                                                                            field(
                                                                                ClassType.QUALIFIER,
                                                                                var("qual")),
                                                                            field(
                                                                                ClassType.IDENTIFIER,
                                                                                var("id")),
                                                                            field(
                                                                                ClassType.ARGUMENTS,
                                                                                Lists.concat2(
                                                                                    var("args"),
                                                                                    list(
                                                                                        inject(
                                                                                            TypeArgument.TYPE_,
                                                                                            TypeArgument.REFERENCE,
                                                                                            var("rt"))))))))))))),
                                            field(
                                                ClassOrInterfaceType.INTERFACE,
                                                constant(
                                                    left(
                                                        inject(Error_.TYPE_,
                                                            Error_.OTHER,
                                                            wrap(OtherError.TYPE_,
                                                                string("expected a Java class type"))))))))),
                                field(
                                    ReferenceType.VARIABLE,
                                    lambda("tv",
                                        right(apply(ref(Utils.javaTypeVariableToType), var("tv"))))),
                                field(
                                    ReferenceType.ARRAY,
                                    constant(
                                        left(
                                            inject(Error_.TYPE_,
                                                Error_.OTHER,
                                                wrap(OtherError.TYPE_,
                                                    string("expected a Java class or interface type, or a variable"))))))))),
                    field(
                        hydra.java.syntax.Type.PRIMITIVE,
                        constant(
                            left(
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        string("expected a reference type")))))))));

    public static final Def addVarRename = def(
        "addVarRename",
        () -> lambda(
                "original",
                "renamed",
                "aliases",
                record(Aliases.TYPE_,
                    field(
                        Aliases.CURRENT_NAMESPACE,
                        proj(Aliases.TYPE_, Aliases.CURRENT_NAMESPACE, "aliases")),
                    field(
                        Aliases.PACKAGES,
                        proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases")),
                    field(
                        Aliases.BRANCH_VARS,
                        proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases")),
                    field(
                        Aliases.RECURSIVE_VARS,
                        proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases")),
                    field(
                        Aliases.IN_SCOPE_TYPE_PARAMS,
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                    field(
                        Aliases.POLYMORPHIC_LOCALS,
                        proj(Aliases.TYPE_, Aliases.POLYMORPHIC_LOCALS, "aliases")),
                    field(
                        Aliases.IN_SCOPE_JAVA_VARS,
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")),
                    field(
                        Aliases.VAR_RENAMES,
                        Maps.insert(
                            var("original"),
                            var("renamed"),
                            proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases"))),
                    field(
                        Aliases.LAMBDA_VARS,
                        proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases")),
                    field(
                        Aliases.TYPE_VAR_SUBST,
                        proj(Aliases.TYPE_, Aliases.TYPE_VAR_SUBST, "aliases")),
                    field(
                        Aliases.TRUSTED_TYPE_VARS,
                        proj(Aliases.TYPE_, Aliases.TRUSTED_TYPE_VARS, "aliases")),
                    field(
                        Aliases.METHOD_CODOMAIN,
                        proj(Aliases.TYPE_, Aliases.METHOD_CODOMAIN, "aliases")),
                    field(
                        Aliases.THUNKED_VARS,
                        proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases")))));

    public static final Def fieldExpression = def(
        "fieldExpression",
        () -> lambda(
                "varId",
                "fieldId",
                record(ExpressionName.TYPE_,
                    field(
                        ExpressionName.QUALIFIER,
                        just(wrap(AmbiguousName.TYPE_, list(var("varId"))))),
                    field(ExpressionName.IDENTIFIER, var("fieldId")))));

    public static final Def fieldNameToJavaExpression = def(
        "fieldNameToJavaExpression",
        () -> lambda("fname",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.NAME,
                                                                                                    apply(
                                                                                                        ref(Utils.javaIdentifierToJavaExpressionName),
                                                                                                        apply(
                                                                                                            ref(Utils.fieldNameToJavaIdentifier),
                                                                                                            var("fname"))))))))))))))))))))))))));

    public static final Def fieldNameToJavaIdentifier = def(
        "fieldNameToJavaIdentifier",
        () -> lambda("fname",
                apply(ref(Utils.javaIdentifier), apply(unwrap(Name.TYPE_), var("fname")))));

    public static final Def fieldNameToJavaVariableDeclarator = def(
        "fieldNameToJavaVariableDeclarator",
        () -> lambda("fname",
                apply(
                    ref(Utils.javaVariableDeclarator),
                    apply(
                        ref(Utils.javaIdentifier),
                        apply(unwrap(Name.TYPE_), var("fname"))),
                    nothing())));

    public static final Def fieldNameToJavaVariableDeclaratorId = def(
        "fieldNameToJavaVariableDeclaratorId",
        () -> lambda("fname",
                apply(
                    ref(Utils.javaVariableDeclaratorId),
                    apply(
                        ref(Utils.javaIdentifier),
                        apply(unwrap(Name.TYPE_), var("fname"))))));

    public static final Def finalVarDeclarationStatement = def(
        "finalVarDeclarationStatement",
        () -> lambda(
                "id",
                "rhs",
                inject(BlockStatement.TYPE_,
                    BlockStatement.LOCAL_VARIABLE_DECLARATION,
                    wrap(LocalVariableDeclarationStatement.TYPE_,
                        record(LocalVariableDeclaration.TYPE_,
                            field(
                                LocalVariableDeclaration.MODIFIERS,
                                list(
                                    inject(VariableModifier.TYPE_,
                                        VariableModifier.FINAL,
                                        unit()))),
                            field(
                                LocalVariableDeclaration.TYPE,
                                inject(LocalVariableType.TYPE_,
                                    LocalVariableType.VAR,
                                    unit())),
                            field(
                                LocalVariableDeclaration.DECLARATORS,
                                list(
                                    apply(
                                        ref(Utils.javaVariableDeclarator),
                                        var("id"),
                                        just(
                                            inject(VariableInitializer.TYPE_,
                                                VariableInitializer.EXPRESSION,
                                                var("rhs")))))))))));

    public static final Def importAliasesForModule = def(
        "importAliasesForModule",
        () -> lambda("mod",
                record(Aliases.TYPE_,
                    field(
                        Aliases.CURRENT_NAMESPACE,
                        proj(Module.TYPE_, Module.NAME, "mod")),
                    field(Aliases.PACKAGES, var("hydra.lib.maps.empty")),
                    field(Aliases.BRANCH_VARS, var("hydra.lib.sets.empty")),
                    field(
                        Aliases.RECURSIVE_VARS,
                        var("hydra.lib.sets.empty")),
                    field(
                        Aliases.IN_SCOPE_TYPE_PARAMS,
                        var("hydra.lib.sets.empty")),
                    field(
                        Aliases.POLYMORPHIC_LOCALS,
                        var("hydra.lib.sets.empty")),
                    field(
                        Aliases.IN_SCOPE_JAVA_VARS,
                        var("hydra.lib.sets.empty")),
                    field(Aliases.VAR_RENAMES, var("hydra.lib.maps.empty")),
                    field(Aliases.LAMBDA_VARS, var("hydra.lib.sets.empty")),
                    field(
                        Aliases.TYPE_VAR_SUBST,
                        var("hydra.lib.maps.empty")),
                    field(
                        Aliases.TRUSTED_TYPE_VARS,
                        var("hydra.lib.sets.empty")),
                    field(Aliases.METHOD_CODOMAIN, nothing()),
                    field(Aliases.THUNKED_VARS, var("hydra.lib.sets.empty")))));

    public static final Def interfaceMethodDeclaration = def(
        "interfaceMethodDeclaration",
        () -> lambda(
                "mods",
                "tparams",
                "methodName",
                "params",
                "result",
                "stmts",
                inject(InterfaceMemberDeclaration.TYPE_,
                    InterfaceMemberDeclaration.INTERFACE_METHOD,
                    record(InterfaceMethodDeclaration.TYPE_,
                        field(InterfaceMethodDeclaration.MODIFIERS, var("mods")),
                        field(
                            InterfaceMethodDeclaration.HEADER,
                            apply(
                                ref(Utils.javaMethodHeader),
                                var("tparams"),
                                var("methodName"),
                                var("params"),
                                var("result"))),
                        field(
                            InterfaceMethodDeclaration.BODY,
                            apply(ref(Utils.javaMethodBody), var("stmts")))))));

    public static final Def isEscaped = def(
        "isEscaped",
        () -> lambda("s",
                Equality.equal(
                    Maybes.fromMaybe(int32(0), Strings.maybeCharAt(int32(0), var("s"))),
                    int32(36))));

    public static final Def javaAdditiveExpressionToJavaExpression = def(
        "javaAdditiveExpressionToJavaExpression",
        () -> lambda("ae",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                var("ae")))))))))))))))))));

    public static final Def javaArrayCreation = def(
        "javaArrayCreation",
        () -> lambda(
                "primType",
                "minit",
                let("init_",
                    Maybes.cases(
                        var("minit"),
                        wrap(ArrayInitializer.TYPE_, list()),
                        lambda("i", var("i"))),
                    apply(
                        ref(Utils.javaPrimaryToJavaExpression),
                        inject(Primary.TYPE_,
                            Primary.ARRAY_CREATION,
                            inject(ArrayCreationExpression.TYPE_,
                                ArrayCreationExpression.WITH_INITIALIZER,
                                inject(
                                    ArrayCreationExpressionWithInitializer.TYPE_,
                                    ArrayCreationExpressionWithInitializer.PRIMITIVE,
                                    record(
                                        ArrayCreationExpressionWithInitializer_Primitive.TYPE_,
                                        field(
                                            ArrayCreationExpressionWithInitializer_Primitive.TYPE,
                                            var("primType")),
                                        field(
                                            ArrayCreationExpressionWithInitializer_Primitive.DIMS,
                                            list()),
                                        field(
                                            ArrayCreationExpressionWithInitializer_Primitive.ARRAY,
                                            var("init_"))))))))));

    public static final Def javaArrayInitializer = def(
        "javaArrayInitializer",
        () -> lambda("exprs",
                wrap(ArrayInitializer.TYPE_,
                    list(
                        Lists.map(
                            lambda("e",
                                inject(VariableInitializer.TYPE_,
                                    VariableInitializer.EXPRESSION,
                                    var("e"))),
                            var("exprs"))))));

    public static final Def javaAssignmentStatement = def(
        "javaAssignmentStatement",
        () -> lambda(
                "lhs",
                "rhs",
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.EXPRESSION,
                        wrap(ExpressionStatement.TYPE_,
                            inject(StatementExpression.TYPE_,
                                StatementExpression.ASSIGNMENT,
                                record(Assignment.TYPE_,
                                    field(Assignment.LHS, var("lhs")),
                                    field(
                                        Assignment.OP,
                                        inject(AssignmentOperator.TYPE_,
                                            AssignmentOperator.SIMPLE,
                                            unit())),
                                    field(Assignment.EXPRESSION, var("rhs")))))))));

    public static final Def javaBoolean = def(
        "javaBoolean",
        () -> lambda("b",
                inject(Literal.TYPE_, Literal.BOOLEAN, var("b"))));

    public static final Def javaBooleanExpression = def(
        "javaBooleanExpression",
        () -> lambda("b",
                apply(
                    ref(Utils.javaPrimaryToJavaExpression),
                    apply(
                        ref(Utils.javaLiteralToJavaPrimary),
                        apply(ref(Utils.javaBoolean), var("b"))))));

    public static final Def javaBooleanType = def(
        "javaBooleanType",
        () -> apply(
                ref(Utils.javaPrimitiveTypeToJavaType),
                inject(PrimitiveType.TYPE_,
                    PrimitiveType.BOOLEAN,
                    unit())));

    public static final Def javaBytePrimitiveType = def(
        "javaBytePrimitiveType",
        () -> record(PrimitiveTypeWithAnnotations.TYPE_,
                field(
                    PrimitiveTypeWithAnnotations.TYPE,
                    inject(PrimitiveType.TYPE_,
                        PrimitiveType.NUMERIC,
                        inject(NumericType.TYPE_,
                            NumericType.INTEGRAL,
                            inject(IntegralType.TYPE_,
                                IntegralType.BYTE,
                                unit())))),
                field(PrimitiveTypeWithAnnotations.ANNOTATIONS, list())));

    public static final Def javaCastExpression = def(
        "javaCastExpression",
        () -> lambda(
                "rt",
                "expr",
                inject(CastExpression.TYPE_,
                    CastExpression.NOT_PLUS_MINUS,
                    record(CastExpression_NotPlusMinus.TYPE_,
                        field(
                            CastExpression_NotPlusMinus.REF_AND_BOUNDS,
                            record(CastExpression_RefAndBounds.TYPE_,
                                field(CastExpression_RefAndBounds.TYPE, var("rt")),
                                field(CastExpression_RefAndBounds.BOUNDS, list()))),
                        field(CastExpression_NotPlusMinus.EXPRESSION, var("expr"))))));

    public static final Def javaCastExpressionToJavaExpression = def(
        "javaCastExpressionToJavaExpression",
        () -> lambda("ce",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.CAST,
                                                                                                var("ce")))))))))))))))))))))));

    public static final Def javaCastPrimitive = def(
        "javaCastPrimitive",
        () -> lambda(
                "pt",
                "expr",
                inject(CastExpression.TYPE_,
                    CastExpression.PRIMITIVE,
                    record(CastExpression_Primitive.TYPE_,
                        field(
                            CastExpression_Primitive.TYPE,
                            record(PrimitiveTypeWithAnnotations.TYPE_,
                                field(
                                    PrimitiveTypeWithAnnotations.TYPE,
                                    var("pt")),
                                field(
                                    PrimitiveTypeWithAnnotations.ANNOTATIONS,
                                    list()))),
                        field(CastExpression_Primitive.EXPRESSION, var("expr"))))));

    public static final Def javaClassDeclaration = def(
        "javaClassDeclaration",
        () -> lambda(
                "aliases",
                "tparams",
                "elName",
                "mods",
                "supname",
                "impls",
                "bodyDecls",
                let("extends_",
                    Maybes.map(
                        lambda("n",
                            apply(
                                ref(Utils.nameToJavaClassType),
                                var("aliases"),
                                bool(true),
                                list(),
                                var("n"),
                                nothing())),
                        var("supname")),
                    inject(ClassDeclaration.TYPE_,
                        ClassDeclaration.NORMAL,
                        record(NormalClassDeclaration.TYPE_,
                            field(NormalClassDeclaration.MODIFIERS, var("mods")),
                            field(
                                NormalClassDeclaration.IDENTIFIER,
                                apply(ref(Utils.javaDeclName), var("elName"))),
                            field(
                                NormalClassDeclaration.PARAMETERS,
                                var("tparams")),
                            field(NormalClassDeclaration.EXTENDS, var("extends_")),
                            field(NormalClassDeclaration.IMPLEMENTS, var("impls")),
                            field(NormalClassDeclaration.PERMITS, list()),
                            field(
                                NormalClassDeclaration.BODY,
                                wrap(ClassBody.TYPE_, var("bodyDecls"))))))));

    public static final Def javaClassType = def(
        "javaClassType",
        () -> lambda(
                "args",
                "pkg",
                "id",
                let(
                    field("qual",
                        Maybes.cases(
                            var("pkg"),
                            inject(ClassTypeQualifier.TYPE_,
                                ClassTypeQualifier.NONE,
                                unit()),
                            lambda("p",
                                inject(ClassTypeQualifier.TYPE_,
                                    ClassTypeQualifier.PACKAGE,
                                    var("p"))))),
                    field("targs",
                        Lists.map(
                            lambda("rt",
                                inject(TypeArgument.TYPE_,
                                    TypeArgument.REFERENCE,
                                    var("rt"))),
                            var("args"))),
                    record(ClassType.TYPE_,
                        field(ClassType.ANNOTATIONS, list()),
                        field(ClassType.QUALIFIER, var("qual")),
                        field(
                            ClassType.IDENTIFIER,
                            apply(ref(Utils.javaTypeIdentifier), var("id"))),
                        field(ClassType.ARGUMENTS, var("targs"))))));

    public static final Def javaClassTypeToJavaType = def(
        "javaClassTypeToJavaType",
        () -> lambda("ct",
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.CLASS_OR_INTERFACE,
                        inject(ClassOrInterfaceType.TYPE_,
                            ClassOrInterfaceType.CLASS,
                            var("ct"))))));

    public static final Def javaConditionalAndExpressionToJavaExpression = def(
        "javaConditionalAndExpressionToJavaExpression",
        () -> lambda("cae",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_, list(var("cae"))))))));

    public static final Def javaConstructorCall = def(
        "javaConstructorCall",
        () -> lambda(
                "ci",
                "args",
                "mbody",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.PRIMARY,
                                                                                                    inject(
                                                                                                        Primary.TYPE_,
                                                                                                        Primary.NO_NEW_ARRAY,
                                                                                                        inject(
                                                                                                            PrimaryNoNewArrayExpression.TYPE_,
                                                                                                            PrimaryNoNewArrayExpression.CLASS_INSTANCE,
                                                                                                            record(
                                                                                                                ClassInstanceCreationExpression.TYPE_,
                                                                                                                field(
                                                                                                                    ClassInstanceCreationExpression.QUALIFIER,
                                                                                                                    nothing()),
                                                                                                                field(
                                                                                                                    ClassInstanceCreationExpression.EXPRESSION,
                                                                                                                    record(
                                                                                                                        UnqualifiedClassInstanceCreationExpression.TYPE_,
                                                                                                                        field(
                                                                                                                            UnqualifiedClassInstanceCreationExpression.TYPE_ARGUMENTS,
                                                                                                                            list()),
                                                                                                                        field(
                                                                                                                            UnqualifiedClassInstanceCreationExpression.CLASS_OR_INTERFACE,
                                                                                                                            var("ci")),
                                                                                                                        field(
                                                                                                                            UnqualifiedClassInstanceCreationExpression.ARGUMENTS,
                                                                                                                            var("args")),
                                                                                                                        field(
                                                                                                                            UnqualifiedClassInstanceCreationExpression.BODY,
                                                                                                                            var("mbody"))))))))))))))))))))))))))))));

    public static final Def javaConstructorName = def(
        "javaConstructorName",
        () -> lambda(
                "id",
                "targs",
                record(ClassOrInterfaceTypeToInstantiate.TYPE_,
                    field(
                        ClassOrInterfaceTypeToInstantiate.IDENTIFIERS,
                        list(
                            record(AnnotatedIdentifier.TYPE_,
                                field(AnnotatedIdentifier.ANNOTATIONS, list()),
                                field(AnnotatedIdentifier.IDENTIFIER, var("id"))))),
                    field(
                        ClassOrInterfaceTypeToInstantiate.TYPE_ARGUMENTS,
                        var("targs")))));

    public static final Def javaDeclName = def(
        "javaDeclName",
        () -> lambda("name",
                wrap(TypeIdentifier.TYPE_,
                    apply(ref(Utils.javaVariableName), var("name")))));

    public static final Def javaDoubleCastExpression = def(
        "javaDoubleCastExpression",
        () -> lambda(
                "rawRt",
                "targetRt",
                "expr",
                let("firstCast",
                    apply(
                        ref(Utils.javaCastExpressionToJavaExpression),
                        apply(ref(Utils.javaCastExpression), var("rawRt"), var("expr"))),
                    apply(
                        ref(Utils.javaCastExpression),
                        var("targetRt"),
                        apply(ref(Utils.javaExpressionToJavaUnaryExpression), var("firstCast"))))));

    public static final Def javaDoubleCastExpressionToJavaExpression = def(
        "javaDoubleCastExpressionToJavaExpression",
        () -> lambda(
                "rawRt",
                "targetRt",
                "expr",
                apply(
                    ref(Utils.javaCastExpressionToJavaExpression),
                    apply(
                        ref(Utils.javaDoubleCastExpression),
                        var("rawRt"),
                        var("targetRt"),
                        var("expr")))));

    public static final Def javaEmptyStatement = def(
        "javaEmptyStatement",
        () -> inject(Statement.TYPE_,
                Statement.WITHOUT_TRAILING,
                inject(StatementWithoutTrailingSubstatement.TYPE_,
                    StatementWithoutTrailingSubstatement.EMPTY,
                    unit())));

    public static final Def javaEqualityExpressionToJavaExpression = def(
        "javaEqualityExpressionToJavaExpression",
        () -> lambda("ee",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(var("ee"))))))))))))))));

    public static final Def javaEqualityExpressionToJavaInclusiveOrExpression = def(
        "javaEqualityExpressionToJavaInclusiveOrExpression",
        () -> lambda("ee",
                wrap(InclusiveOrExpression.TYPE_,
                    list(
                        wrap(ExclusiveOrExpression.TYPE_,
                            list(wrap(AndExpression.TYPE_, list(var("ee")))))))));

    public static final Def javaEquals = def(
        "javaEquals",
        () -> lambda(
                "lhs",
                "rhs",
                inject(EqualityExpression.TYPE_,
                    EqualityExpression.EQUAL,
                    record(EqualityExpression_Binary.TYPE_,
                        field(EqualityExpression_Binary.LHS, var("lhs")),
                        field(EqualityExpression_Binary.RHS, var("rhs"))))));

    public static final Def javaEqualsNull = def(
        "javaEqualsNull",
        () -> lambda("lhs",
                apply(
                    ref(Utils.javaEquals),
                    var("lhs"),
                    apply(
                        ref(Utils.javaLiteralToJavaRelationalExpression),
                        inject(Literal.TYPE_,
                            Literal.NULL,
                            unit())))));

    public static final Def javaExpressionNameToJavaExpression = def(
        "javaExpressionNameToJavaExpression",
        () -> lambda("en",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.NAME,
                                                                                                    var("en"))))))))))))))))))))))));

    public static final Def javaExpressionToJavaPrimary = def(
        "javaExpressionToJavaPrimary",
        () -> doc(
                "Convert an Expression to a Primary, avoiding unnecessary parentheses when the expression is already a simple primary chain",
                lambda("e",
                    let("fallback",
                        inject(Primary.TYPE_,
                            Primary.NO_NEW_ARRAY,
                            inject(PrimaryNoNewArrayExpression.TYPE_,
                                PrimaryNoNewArrayExpression.PARENS,
                                var("e"))),
                        casesWithDefault(Expression.TYPE_,
                            var("e"),
                            var("fallback"),
                            field(
                                Expression.ASSIGNMENT,
                                lambda("ae",
                                    casesWithDefault(AssignmentExpression.TYPE_,
                                        var("ae"),
                                        var("fallback"),
                                        field(
                                            AssignmentExpression.CONDITIONAL,
                                            lambda("ce",
                                                casesWithDefault(
                                                    ConditionalExpression.TYPE_,
                                                    var("ce"),
                                                    var("fallback"),
                                                    field(
                                                        ConditionalExpression.SIMPLE,
                                                        lambda("cor",
                                                            let("cands",
                                                                apply(
                                                                    unwrap(ConditionalOrExpression.TYPE_),
                                                                    var("cor")),
                                                                Maybes.fromMaybe(
                                                                    var("fallback"),
                                                                    Maybes.bind(
                                                                        Lists.maybeHead(
                                                                            var("cands")),
                                                                        lambda("candHead",
                                                                            let("iors",
                                                                                apply(
                                                                                    unwrap(ConditionalAndExpression.TYPE_),
                                                                                    var("candHead")),
                                                                                Maybes.bind(
                                                                                    Lists.maybeHead(
                                                                                        var("iors")),
                                                                                    lambda(
                                                                                        "iorHead",
                                                                                        let("xors",
                                                                                            apply(
                                                                                                unwrap(InclusiveOrExpression.TYPE_),
                                                                                                var("iorHead")),
                                                                                            Maybes.bind(
                                                                                                Lists.maybeHead(
                                                                                                    var("xors")),
                                                                                                lambda(
                                                                                                    "xorHead",
                                                                                                    let(
                                                                                                        "ands",
                                                                                                        apply(
                                                                                                            unwrap(ExclusiveOrExpression.TYPE_),
                                                                                                            var("xorHead")),
                                                                                                        Maybes.bind(
                                                                                                            Lists.maybeHead(
                                                                                                                var("ands")),
                                                                                                            lambda(
                                                                                                                "andHead",
                                                                                                                let(
                                                                                                                    "eqs",
                                                                                                                    apply(
                                                                                                                        unwrap(AndExpression.TYPE_),
                                                                                                                        var("andHead")),
                                                                                                                    Maybes.bind(
                                                                                                                        Lists.maybeHead(
                                                                                                                            var("eqs")),
                                                                                                                        lambda(
                                                                                                                            "eqHead",
                                                                                                                            just(
                                                                                                                                casesWithDefault(
                                                                                                                                    EqualityExpression.TYPE_,
                                                                                                                                    var("eqHead"),
                                                                                                                                    var("fallback"),
                                                                                                                                    field(
                                                                                                                                        EqualityExpression.UNARY,
                                                                                                                                        lambda(
                                                                                                                                            "rel",
                                                                                                                                            casesWithDefault(
                                                                                                                                                RelationalExpression.TYPE_,
                                                                                                                                                var("rel"),
                                                                                                                                                var("fallback"),
                                                                                                                                                field(
                                                                                                                                                    RelationalExpression.SIMPLE,
                                                                                                                                                    lambda(
                                                                                                                                                        "shift",
                                                                                                                                                        casesWithDefault(
                                                                                                                                                            ShiftExpression.TYPE_,
                                                                                                                                                            var("shift"),
                                                                                                                                                            var("fallback"),
                                                                                                                                                            field(
                                                                                                                                                                ShiftExpression.UNARY,
                                                                                                                                                                lambda(
                                                                                                                                                                    "add",
                                                                                                                                                                    casesWithDefault(
                                                                                                                                                                        AdditiveExpression.TYPE_,
                                                                                                                                                                        var("add"),
                                                                                                                                                                        var("fallback"),
                                                                                                                                                                        field(
                                                                                                                                                                            AdditiveExpression.UNARY,
                                                                                                                                                                            lambda(
                                                                                                                                                                                "mul",
                                                                                                                                                                                casesWithDefault(
                                                                                                                                                                                    MultiplicativeExpression.TYPE_,
                                                                                                                                                                                    var("mul"),
                                                                                                                                                                                    var("fallback"),
                                                                                                                                                                                    field(
                                                                                                                                                                                        MultiplicativeExpression.UNARY,
                                                                                                                                                                                        lambda(
                                                                                                                                                                                            "unary",
                                                                                                                                                                                            casesWithDefault(
                                                                                                                                                                                                UnaryExpression.TYPE_,
                                                                                                                                                                                                var("unary"),
                                                                                                                                                                                                var("fallback"),
                                                                                                                                                                                                field(
                                                                                                                                                                                                    UnaryExpression.OTHER,
                                                                                                                                                                                                    lambda(
                                                                                                                                                                                                        "npm",
                                                                                                                                                                                                        casesWithDefault(
                                                                                                                                                                                                            UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                                                                                                                            var("npm"),
                                                                                                                                                                                                            var("fallback"),
                                                                                                                                                                                                            field(
                                                                                                                                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                                                                                                                                lambda(
                                                                                                                                                                                                                    "pf",
                                                                                                                                                                                                                    casesWithDefault(
                                                                                                                                                                                                                        PostfixExpression.TYPE_,
                                                                                                                                                                                                                        var("pf"),
                                                                                                                                                                                                                        var("fallback"),
                                                                                                                                                                                                                        field(
                                                                                                                                                                                                                            PostfixExpression.PRIMARY,
                                                                                                                                                                                                                            lambda(
                                                                                                                                                                                                                                "p",
                                                                                                                                                                                                                                var("p")))))))))))))))))))))))))))))))))))))))))))))))))))))));

    public static final Def javaExpressionToJavaUnaryExpression = def(
        "javaExpressionToJavaUnaryExpression",
        () -> lambda("e",
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.PRIMARY,
                            inject(Primary.TYPE_,
                                Primary.NO_NEW_ARRAY,
                                inject(PrimaryNoNewArrayExpression.TYPE_,
                                    PrimaryNoNewArrayExpression.PARENS,
                                    var("e"))))))));

    public static final Def javaFieldAccessToJavaExpression = def(
        "javaFieldAccessToJavaExpression",
        () -> lambda("fa",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.PRIMARY,
                                                                                                    inject(
                                                                                                        Primary.TYPE_,
                                                                                                        Primary.NO_NEW_ARRAY,
                                                                                                        inject(
                                                                                                            PrimaryNoNewArrayExpression.TYPE_,
                                                                                                            PrimaryNoNewArrayExpression.FIELD_ACCESS,
                                                                                                            var("fa"))))))))))))))))))))))))));

    public static final Def javaIdentifier = def(
        "javaIdentifier",
        () -> lambda("s",
                wrap(Identifier.TYPE_,
                    apply(ref(Utils.sanitizeJavaName), var("s")))));

    public static final Def javaIdentifierToJavaExpression = def(
        "javaIdentifierToJavaExpression",
        () -> lambda("id",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.NAME,
                                                                                                    record(
                                                                                                        ExpressionName.TYPE_,
                                                                                                        field(
                                                                                                            ExpressionName.QUALIFIER,
                                                                                                            nothing()),
                                                                                                        field(
                                                                                                            ExpressionName.IDENTIFIER,
                                                                                                            var("id"))))))))))))))))))))))))));

    public static final Def javaIdentifierToJavaExpressionName = def(
        "javaIdentifierToJavaExpressionName",
        () -> lambda("id",
                record(ExpressionName.TYPE_,
                    field(ExpressionName.QUALIFIER, nothing()),
                    field(ExpressionName.IDENTIFIER, var("id")))));

    public static final Def javaIdentifierToJavaRelationalExpression = def(
        "javaIdentifierToJavaRelationalExpression",
        () -> lambda("id",
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            inject(MultiplicativeExpression.TYPE_,
                                MultiplicativeExpression.UNARY,
                                inject(UnaryExpression.TYPE_,
                                    UnaryExpression.OTHER,
                                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                                        UnaryExpressionNotPlusMinus.POSTFIX,
                                        inject(PostfixExpression.TYPE_,
                                            PostfixExpression.NAME,
                                            record(ExpressionName.TYPE_,
                                                field(
                                                    ExpressionName.QUALIFIER,
                                                    nothing()),
                                                field(
                                                    ExpressionName.IDENTIFIER,
                                                    var("id"))))))))))));

    public static final Def javaIdentifierToJavaUnaryExpression = def(
        "javaIdentifierToJavaUnaryExpression",
        () -> lambda("id",
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.NAME,
                            record(ExpressionName.TYPE_,
                                field(ExpressionName.QUALIFIER, nothing()),
                                field(ExpressionName.IDENTIFIER, var("id"))))))));

    public static final Def javaInstanceOf = def(
        "javaInstanceOf",
        () -> lambda(
                "lhs",
                "rhs",
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.INSTANCEOF_EXPRESSION,
                    record(InstanceofExpression.TYPE_,
                        field(InstanceofExpression.LHS, var("lhs")),
                        field(
                            InstanceofExpression.RHS,
                            inject(InstanceofExpression_Rhs.TYPE_,
                                InstanceofExpression_Rhs.REFERENCE_TYPE,
                                var("rhs")))))));

    public static final Def javaInt = def(
        "javaInt",
        () -> lambda("i",
                inject(Literal.TYPE_,
                    Literal.INTEGER,
                    wrap(IntegerLiteral.TYPE_, var("i")))));

    public static final Def javaIntExpression = def(
        "javaIntExpression",
        () -> lambda("i",
                apply(
                    ref(Utils.javaPrimaryToJavaExpression),
                    apply(ref(Utils.javaLiteralToJavaPrimary), apply(ref(Utils.javaInt), var("i"))))));

    public static final Def javaIntType = def(
        "javaIntType",
        () -> apply(
                ref(Utils.javaPrimitiveTypeToJavaType),
                inject(PrimitiveType.TYPE_,
                    PrimitiveType.NUMERIC,
                    inject(NumericType.TYPE_,
                        NumericType.INTEGRAL,
                        inject(IntegralType.TYPE_,
                            IntegralType.INT,
                            unit())))));

    public static final Def javaInterfaceDeclarationToJavaClassBodyDeclaration = def(
        "javaInterfaceDeclarationToJavaClassBodyDeclaration",
        () -> lambda("nid",
                inject(ClassBodyDeclaration.TYPE_,
                    ClassBodyDeclaration.CLASS_MEMBER,
                    inject(ClassMemberDeclaration.TYPE_,
                        ClassMemberDeclaration.INTERFACE,
                        inject(InterfaceDeclaration.TYPE_,
                            InterfaceDeclaration.NORMAL_INTERFACE,
                            var("nid"))))));

    public static final Def javaLambda = def(
        "javaLambda",
        () -> lambda(
                "v",
                "body",
                inject(Expression.TYPE_,
                    Expression.LAMBDA,
                    record(LambdaExpression.TYPE_,
                        field(
                            LambdaExpression.PARAMETERS,
                            inject(LambdaParameters.TYPE_,
                                LambdaParameters.SINGLE,
                                apply(ref(Utils.variableToJavaIdentifier), var("v")))),
                        field(
                            LambdaExpression.BODY,
                            inject(LambdaBody.TYPE_,
                                LambdaBody.EXPRESSION,
                                var("body")))))));

    public static final Def javaLambdaFromBlock = def(
        "javaLambdaFromBlock",
        () -> lambda(
                "v",
                "block",
                inject(Expression.TYPE_,
                    Expression.LAMBDA,
                    record(LambdaExpression.TYPE_,
                        field(
                            LambdaExpression.PARAMETERS,
                            inject(LambdaParameters.TYPE_,
                                LambdaParameters.SINGLE,
                                apply(ref(Utils.variableToJavaIdentifier), var("v")))),
                        field(
                            LambdaExpression.BODY,
                            inject(LambdaBody.TYPE_,
                                LambdaBody.BLOCK,
                                var("block")))))));

    public static final Def javaLiteralToJavaExpression = def(
        "javaLiteralToJavaExpression",
        () -> lambda("lit",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.PRIMARY,
                                                                                                    inject(
                                                                                                        Primary.TYPE_,
                                                                                                        Primary.NO_NEW_ARRAY,
                                                                                                        inject(
                                                                                                            PrimaryNoNewArrayExpression.TYPE_,
                                                                                                            PrimaryNoNewArrayExpression.LITERAL,
                                                                                                            var("lit"))))))))))))))))))))))))));

    public static final Def javaLiteralToJavaMultiplicativeExpression = def(
        "javaLiteralToJavaMultiplicativeExpression",
        () -> lambda("lit",
                inject(MultiplicativeExpression.TYPE_,
                    MultiplicativeExpression.UNARY,
                    inject(UnaryExpression.TYPE_,
                        UnaryExpression.OTHER,
                        inject(UnaryExpressionNotPlusMinus.TYPE_,
                            UnaryExpressionNotPlusMinus.POSTFIX,
                            inject(PostfixExpression.TYPE_,
                                PostfixExpression.PRIMARY,
                                inject(Primary.TYPE_,
                                    Primary.NO_NEW_ARRAY,
                                    inject(PrimaryNoNewArrayExpression.TYPE_,
                                        PrimaryNoNewArrayExpression.LITERAL,
                                        var("lit")))))))));

    public static final Def javaLiteralToJavaPrimary = def(
        "javaLiteralToJavaPrimary",
        () -> lambda("lit",
                inject(Primary.TYPE_,
                    Primary.NO_NEW_ARRAY,
                    inject(PrimaryNoNewArrayExpression.TYPE_,
                        PrimaryNoNewArrayExpression.LITERAL,
                        var("lit")))));

    public static final Def javaLiteralToJavaRelationalExpression = def(
        "javaLiteralToJavaRelationalExpression",
        () -> lambda("lit",
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            inject(MultiplicativeExpression.TYPE_,
                                MultiplicativeExpression.UNARY,
                                inject(UnaryExpression.TYPE_,
                                    UnaryExpression.OTHER,
                                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                                        UnaryExpressionNotPlusMinus.POSTFIX,
                                        inject(PostfixExpression.TYPE_,
                                            PostfixExpression.PRIMARY,
                                            inject(Primary.TYPE_,
                                                Primary.NO_NEW_ARRAY,
                                                inject(
                                                    PrimaryNoNewArrayExpression.TYPE_,
                                                    PrimaryNoNewArrayExpression.LITERAL,
                                                    var("lit"))))))))))));

    public static final Def javaMemberField = def(
        "javaMemberField",
        () -> lambda(
                "mods",
                "jt",
                "v",
                inject(ClassBodyDeclaration.TYPE_,
                    ClassBodyDeclaration.CLASS_MEMBER,
                    inject(ClassMemberDeclaration.TYPE_,
                        ClassMemberDeclaration.FIELD,
                        record(FieldDeclaration.TYPE_,
                            field(FieldDeclaration.MODIFIERS, var("mods")),
                            field(
                                FieldDeclaration.UNANN_TYPE,
                                wrap(UnannType.TYPE_, var("jt"))),
                            field(
                                FieldDeclaration.VARIABLE_DECLARATORS,
                                list(var("v"))))))));

    public static final Def javaMethodBody = def(
        "javaMethodBody",
        () -> lambda("mstmts",
                Maybes.cases(
                    var("mstmts"),
                    inject(MethodBody.TYPE_,
                        MethodBody.NONE,
                        unit()),
                    lambda("stmts",
                        inject(MethodBody.TYPE_,
                            MethodBody.BLOCK,
                            wrap(Block.TYPE_, var("stmts")))))));

    public static final Def javaMethodDeclarationToJavaClassBodyDeclaration = def(
        "javaMethodDeclarationToJavaClassBodyDeclaration",
        () -> lambda("md",
                inject(ClassBodyDeclaration.TYPE_,
                    ClassBodyDeclaration.CLASS_MEMBER,
                    inject(ClassMemberDeclaration.TYPE_,
                        ClassMemberDeclaration.METHOD,
                        var("md")))));

    public static final Def javaMethodHeader = def(
        "javaMethodHeader",
        () -> lambda(
                "tparams",
                "methodName",
                "params",
                "result",
                record(MethodHeader.TYPE_,
                    field(MethodHeader.PARAMETERS, var("tparams")),
                    field(MethodHeader.RESULT, var("result")),
                    field(
                        MethodHeader.DECLARATOR,
                        record(MethodDeclarator.TYPE_,
                            field(
                                MethodDeclarator.IDENTIFIER,
                                wrap(Identifier.TYPE_, var("methodName"))),
                            field(MethodDeclarator.RECEIVER_PARAMETER, nothing()),
                            field(
                                MethodDeclarator.FORMAL_PARAMETERS,
                                var("params")))),
                    field(MethodHeader.THROWS, nothing()))));

    public static final Def javaMethodInvocationToJavaExpression = def(
        "javaMethodInvocationToJavaExpression",
        () -> lambda("mi",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.PRIMARY,
                                                                                                    inject(
                                                                                                        Primary.TYPE_,
                                                                                                        Primary.NO_NEW_ARRAY,
                                                                                                        inject(
                                                                                                            PrimaryNoNewArrayExpression.TYPE_,
                                                                                                            PrimaryNoNewArrayExpression.METHOD_INVOCATION,
                                                                                                            var("mi"))))))))))))))))))))))))));

    public static final Def javaMethodInvocationToJavaPostfixExpression = def(
        "javaMethodInvocationToJavaPostfixExpression",
        () -> lambda("mi",
                inject(PostfixExpression.TYPE_,
                    PostfixExpression.PRIMARY,
                    inject(Primary.TYPE_,
                        Primary.NO_NEW_ARRAY,
                        inject(PrimaryNoNewArrayExpression.TYPE_,
                            PrimaryNoNewArrayExpression.METHOD_INVOCATION,
                            var("mi"))))));

    public static final Def javaMethodInvocationToJavaPrimary = def(
        "javaMethodInvocationToJavaPrimary",
        () -> lambda("mi",
                inject(Primary.TYPE_,
                    Primary.NO_NEW_ARRAY,
                    inject(PrimaryNoNewArrayExpression.TYPE_,
                        PrimaryNoNewArrayExpression.METHOD_INVOCATION,
                        var("mi")))));

    public static final Def javaMethodInvocationToJavaStatement = def(
        "javaMethodInvocationToJavaStatement",
        () -> lambda("mi",
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.EXPRESSION,
                        wrap(ExpressionStatement.TYPE_,
                            inject(StatementExpression.TYPE_,
                                StatementExpression.METHOD_INVOCATION,
                                var("mi")))))));

    public static final Def javaMultiplicativeExpressionToJavaRelationalExpression = def(
        "javaMultiplicativeExpressionToJavaRelationalExpression",
        () -> lambda("me",
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            var("me"))))));

    public static final Def javaPackageDeclaration = def(
        "javaPackageDeclaration",
        () -> lambda("ns",
                record(PackageDeclaration.TYPE_,
                    field(PackageDeclaration.MODIFIERS, list()),
                    field(
                        PackageDeclaration.IDENTIFIERS,
                        Lists.map(
                            lambda("s", wrap(Identifier.TYPE_, var("s"))),
                            Strings.splitOn(
                                string("."),
                                apply(unwrap(ModuleName.TYPE_), var("ns"))))))));

    public static final Def javaPostfixExpressionToJavaEqualityExpression = def(
        "javaPostfixExpressionToJavaEqualityExpression",
        () -> lambda("pe",
                inject(EqualityExpression.TYPE_,
                    EqualityExpression.UNARY,
                    inject(RelationalExpression.TYPE_,
                        RelationalExpression.SIMPLE,
                        inject(ShiftExpression.TYPE_,
                            ShiftExpression.UNARY,
                            inject(AdditiveExpression.TYPE_,
                                AdditiveExpression.UNARY,
                                inject(MultiplicativeExpression.TYPE_,
                                    MultiplicativeExpression.UNARY,
                                    inject(UnaryExpression.TYPE_,
                                        UnaryExpression.OTHER,
                                        inject(UnaryExpressionNotPlusMinus.TYPE_,
                                            UnaryExpressionNotPlusMinus.POSTFIX,
                                            var("pe"))))))))));

    public static final Def javaPostfixExpressionToJavaExpression = def(
        "javaPostfixExpressionToJavaExpression",
        () -> lambda("pe",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                var("pe")))))))))))))))))))))));

    public static final Def javaPostfixExpressionToJavaInclusiveOrExpression = def(
        "javaPostfixExpressionToJavaInclusiveOrExpression",
        () -> lambda("pe",
                wrap(InclusiveOrExpression.TYPE_,
                    list(
                        wrap(ExclusiveOrExpression.TYPE_,
                            list(
                                wrap(AndExpression.TYPE_,
                                    list(
                                        inject(EqualityExpression.TYPE_,
                                            EqualityExpression.UNARY,
                                            inject(RelationalExpression.TYPE_,
                                                RelationalExpression.SIMPLE,
                                                inject(ShiftExpression.TYPE_,
                                                    ShiftExpression.UNARY,
                                                    inject(
                                                        AdditiveExpression.TYPE_,
                                                        AdditiveExpression.UNARY,
                                                        inject(
                                                            MultiplicativeExpression.TYPE_,
                                                            MultiplicativeExpression.UNARY,
                                                            inject(
                                                                UnaryExpression.TYPE_,
                                                                UnaryExpression.OTHER,
                                                                inject(
                                                                    UnaryExpressionNotPlusMinus.TYPE_,
                                                                    UnaryExpressionNotPlusMinus.POSTFIX,
                                                                    var("pe"))))))))))))))));

    public static final Def javaPostfixExpressionToJavaRelationalExpression = def(
        "javaPostfixExpressionToJavaRelationalExpression",
        () -> lambda("pe",
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            inject(MultiplicativeExpression.TYPE_,
                                MultiplicativeExpression.UNARY,
                                inject(UnaryExpression.TYPE_,
                                    UnaryExpression.OTHER,
                                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                                        UnaryExpressionNotPlusMinus.POSTFIX,
                                        var("pe")))))))));

    public static final Def javaPostfixExpressionToJavaUnaryExpression = def(
        "javaPostfixExpressionToJavaUnaryExpression",
        () -> lambda("pe",
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        var("pe")))));

    public static final Def javaPrimaryToJavaExpression = def(
        "javaPrimaryToJavaExpression",
        () -> lambda("p",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        inject(
                                                                                            UnaryExpression.TYPE_,
                                                                                            UnaryExpression.OTHER,
                                                                                            inject(
                                                                                                UnaryExpressionNotPlusMinus.TYPE_,
                                                                                                UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                                inject(
                                                                                                    PostfixExpression.TYPE_,
                                                                                                    PostfixExpression.PRIMARY,
                                                                                                    var("p"))))))))))))))))))))))));

    public static final Def javaPrimaryToJavaUnaryExpression = def(
        "javaPrimaryToJavaUnaryExpression",
        () -> lambda("p",
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.PRIMARY,
                            var("p"))))));

    public static final Def javaPrimitiveTypeToJavaType = def(
        "javaPrimitiveTypeToJavaType",
        () -> lambda("pt",
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.PRIMITIVE,
                    record(PrimitiveTypeWithAnnotations.TYPE_,
                        field(PrimitiveTypeWithAnnotations.TYPE, var("pt")),
                        field(PrimitiveTypeWithAnnotations.ANNOTATIONS, list())))));

    public static final Def javaRefType = def(
        "javaRefType",
        () -> lambda(
                "args",
                "pkg",
                "id",
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.CLASS_OR_INTERFACE,
                        inject(ClassOrInterfaceType.TYPE_,
                            ClassOrInterfaceType.CLASS,
                            apply(ref(Utils.javaClassType), var("args"), var("pkg"), var("id")))))));

    public static final Def javaReferenceTypeToRawType = def(
        "javaReferenceTypeToRawType",
        () -> lambda("rt",
                casesWithDefault(ReferenceType.TYPE_,
                    var("rt"),
                    var("rt"),
                    field(
                        ReferenceType.CLASS_OR_INTERFACE,
                        lambda("cit",
                            cases(ClassOrInterfaceType.TYPE_,
                                var("cit"),
                                field(
                                    ClassOrInterfaceType.CLASS,
                                    lambda("ct",
                                        let(
                                            field("anns",
                                                proj(ClassType.TYPE_, ClassType.ANNOTATIONS, "ct")),
                                            field("qual",
                                                proj(ClassType.TYPE_, ClassType.QUALIFIER, "ct")),
                                            field("id",
                                                proj(ClassType.TYPE_, ClassType.IDENTIFIER, "ct")),
                                            inject(ReferenceType.TYPE_,
                                                ReferenceType.CLASS_OR_INTERFACE,
                                                inject(ClassOrInterfaceType.TYPE_,
                                                    ClassOrInterfaceType.CLASS,
                                                    record(ClassType.TYPE_,
                                                        field(
                                                            ClassType.ANNOTATIONS,
                                                            var("anns")),
                                                        field(
                                                            ClassType.QUALIFIER,
                                                            var("qual")),
                                                        field(
                                                            ClassType.IDENTIFIER,
                                                            var("id")),
                                                        field(
                                                            ClassType.ARGUMENTS,
                                                            list()))))))),
                                field(
                                    ClassOrInterfaceType.INTERFACE,
                                    lambda("it",
                                        let(
                                            field("ct",
                                                apply(
                                                    unwrap(InterfaceType.TYPE_),
                                                    var("it"))),
                                            field("anns",
                                                proj(ClassType.TYPE_, ClassType.ANNOTATIONS, "ct")),
                                            field("qual",
                                                proj(ClassType.TYPE_, ClassType.QUALIFIER, "ct")),
                                            field("id",
                                                proj(ClassType.TYPE_, ClassType.IDENTIFIER, "ct")),
                                            inject(ReferenceType.TYPE_,
                                                ReferenceType.CLASS_OR_INTERFACE,
                                                inject(ClassOrInterfaceType.TYPE_,
                                                    ClassOrInterfaceType.INTERFACE,
                                                    wrap(InterfaceType.TYPE_,
                                                        record(ClassType.TYPE_,
                                                            field(
                                                                ClassType.ANNOTATIONS,
                                                                var("anns")),
                                                            field(
                                                                ClassType.QUALIFIER,
                                                                var("qual")),
                                                            field(
                                                                ClassType.IDENTIFIER,
                                                                var("id")),
                                                            field(
                                                                ClassType.ARGUMENTS,
                                                                list()))))))))))))));

    public static final Def javaRelationalExpressionToJavaEqualityExpression = def(
        "javaRelationalExpressionToJavaEqualityExpression",
        () -> lambda("re",
                inject(EqualityExpression.TYPE_,
                    EqualityExpression.UNARY,
                    var("re"))));

    public static final Def javaRelationalExpressionToJavaExpression = def(
        "javaRelationalExpressionToJavaExpression",
        () -> lambda("re",
                apply(
                    ref(Utils.javaEqualityExpressionToJavaExpression),
                    inject(EqualityExpression.TYPE_,
                        EqualityExpression.UNARY,
                        var("re")))));

    public static final Def javaRelationalExpressionToJavaUnaryExpression = def(
        "javaRelationalExpressionToJavaUnaryExpression",
        () -> lambda("re",
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.PRIMARY,
                            inject(Primary.TYPE_,
                                Primary.NO_NEW_ARRAY,
                                inject(PrimaryNoNewArrayExpression.TYPE_,
                                    PrimaryNoNewArrayExpression.PARENS,
                                    inject(Expression.TYPE_,
                                        Expression.ASSIGNMENT,
                                        inject(AssignmentExpression.TYPE_,
                                            AssignmentExpression.CONDITIONAL,
                                            inject(ConditionalExpression.TYPE_,
                                                ConditionalExpression.SIMPLE,
                                                wrap(
                                                    ConditionalOrExpression.TYPE_,
                                                    list(
                                                        wrap(
                                                            ConditionalAndExpression.TYPE_,
                                                            list(
                                                                wrap(
                                                                    InclusiveOrExpression.TYPE_,
                                                                    list(
                                                                        wrap(
                                                                            ExclusiveOrExpression.TYPE_,
                                                                            list(
                                                                                wrap(
                                                                                    AndExpression.TYPE_,
                                                                                    list(
                                                                                        inject(
                                                                                            EqualityExpression.TYPE_,
                                                                                            EqualityExpression.UNARY,
                                                                                            var("re"))))))))))))))))))))));

    public static final Def javaReturnStatement = def(
        "javaReturnStatement",
        () -> lambda("mex",
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.RETURN,
                        wrap(ReturnStatement.TYPE_, var("mex"))))));

    public static final Def javaStatementsToBlock = def(
        "javaStatementsToBlock",
        () -> lambda("stmts",
                wrap(Block.TYPE_,
                    Lists.map(
                        lambda("s",
                            inject(BlockStatement.TYPE_,
                                BlockStatement.STATEMENT,
                                var("s"))),
                        var("stmts")))));

    public static final Def javaString = def(
        "javaString",
        () -> lambda("s",
                inject(Literal.TYPE_,
                    Literal.STRING,
                    wrap(StringLiteral.TYPE_, var("s")))));

    public static final Def javaStringMultiplicativeExpression = def(
        "javaStringMultiplicativeExpression",
        () -> lambda("s",
                apply(
                    ref(Utils.javaLiteralToJavaMultiplicativeExpression),
                    apply(ref(Utils.javaString), var("s")))));

    public static final Def javaThis = def(
        "javaThis",
        () -> inject(Expression.TYPE_,
                Expression.ASSIGNMENT,
                inject(AssignmentExpression.TYPE_,
                    AssignmentExpression.CONDITIONAL,
                    inject(ConditionalExpression.TYPE_,
                        ConditionalExpression.SIMPLE,
                        wrap(ConditionalOrExpression.TYPE_,
                            list(
                                wrap(ConditionalAndExpression.TYPE_,
                                    list(
                                        wrap(InclusiveOrExpression.TYPE_,
                                            list(
                                                wrap(ExclusiveOrExpression.TYPE_,
                                                    list(
                                                        wrap(AndExpression.TYPE_,
                                                            list(
                                                                inject(
                                                                    EqualityExpression.TYPE_,
                                                                    EqualityExpression.UNARY,
                                                                    inject(
                                                                        RelationalExpression.TYPE_,
                                                                        RelationalExpression.SIMPLE,
                                                                        inject(
                                                                            ShiftExpression.TYPE_,
                                                                            ShiftExpression.UNARY,
                                                                            inject(
                                                                                AdditiveExpression.TYPE_,
                                                                                AdditiveExpression.UNARY,
                                                                                inject(
                                                                                    MultiplicativeExpression.TYPE_,
                                                                                    MultiplicativeExpression.UNARY,
                                                                                    inject(
                                                                                        UnaryExpression.TYPE_,
                                                                                        UnaryExpression.OTHER,
                                                                                        inject(
                                                                                            UnaryExpressionNotPlusMinus.TYPE_,
                                                                                            UnaryExpressionNotPlusMinus.POSTFIX,
                                                                                            inject(
                                                                                                PostfixExpression.TYPE_,
                                                                                                PostfixExpression.PRIMARY,
                                                                                                inject(
                                                                                                    Primary.TYPE_,
                                                                                                    Primary.NO_NEW_ARRAY,
                                                                                                    inject(
                                                                                                        PrimaryNoNewArrayExpression.TYPE_,
                                                                                                        PrimaryNoNewArrayExpression.THIS,
                                                                                                        unit()))))))))))))))))))))))));

    public static final Def javaThrowIllegalArgumentException = def(
        "javaThrowIllegalArgumentException",
        () -> lambda("args",
                apply(
                    ref(Utils.javaThrowStatement),
                    apply(
                        ref(Utils.javaConstructorCall),
                        apply(
                            ref(Utils.javaConstructorName),
                            wrap(Identifier.TYPE_,
                                string("IllegalArgumentException")),
                            nothing()),
                        var("args"),
                        nothing()))));

    public static final Def javaThrowIllegalStateException = def(
        "javaThrowIllegalStateException",
        () -> lambda("args",
                apply(
                    ref(Utils.javaThrowStatement),
                    apply(
                        ref(Utils.javaConstructorCall),
                        apply(
                            ref(Utils.javaConstructorName),
                            wrap(Identifier.TYPE_,
                                string("IllegalStateException")),
                            nothing()),
                        var("args"),
                        nothing()))));

    public static final Def javaThrowStatement = def(
        "javaThrowStatement",
        () -> lambda("e",
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.THROW,
                        wrap(ThrowStatement.TYPE_, var("e"))))));

    public static final Def javaTypeFromTypeName = def(
        "javaTypeFromTypeName",
        () -> lambda(
                "aliases",
                "elName",
                apply(
                    ref(Utils.javaTypeVariableToType),
                    record(TypeVariable.TYPE_,
                        field(TypeVariable.ANNOTATIONS, list()),
                        field(
                            TypeVariable.IDENTIFIER,
                            apply(
                                ref(Utils.nameToJavaTypeIdentifier),
                                var("aliases"),
                                bool(false),
                                var("elName")))))));

    public static final Def javaTypeIdentifier = def(
        "javaTypeIdentifier",
        () -> lambda("s",
                wrap(TypeIdentifier.TYPE_,
                    wrap(Identifier.TYPE_, var("s")))));

    public static final Def javaTypeIdentifierToJavaTypeArgument = def(
        "javaTypeIdentifierToJavaTypeArgument",
        () -> lambda("id",
                inject(TypeArgument.TYPE_,
                    TypeArgument.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.VARIABLE,
                        record(TypeVariable.TYPE_,
                            field(TypeVariable.ANNOTATIONS, list()),
                            field(TypeVariable.IDENTIFIER, var("id")))))));

    public static final Def javaTypeName = def(
        "javaTypeName",
        () -> lambda("id",
                record(TypeName.TYPE_,
                    field(
                        TypeName.IDENTIFIER,
                        wrap(TypeIdentifier.TYPE_, var("id"))),
                    field(TypeName.QUALIFIER, nothing()))));

    public static final Def javaTypeParameter = def(
        "javaTypeParameter",
        () -> lambda("v",
                record(TypeParameter.TYPE_,
                    field(TypeParameter.MODIFIERS, list()),
                    field(
                        TypeParameter.IDENTIFIER,
                        apply(ref(Utils.javaTypeIdentifier), var("v"))),
                    field(TypeParameter.BOUND, nothing()))));

    public static final Def javaTypeToJavaFormalParameter = def(
        "javaTypeToJavaFormalParameter",
        () -> lambda(
                "jt",
                "fname",
                inject(FormalParameter.TYPE_,
                    FormalParameter.SIMPLE,
                    record(FormalParameter_Simple.TYPE_,
                        field(FormalParameter_Simple.MODIFIERS, list()),
                        field(
                            FormalParameter_Simple.TYPE,
                            wrap(UnannType.TYPE_, var("jt"))),
                        field(
                            FormalParameter_Simple.ID,
                            apply(ref(Utils.fieldNameToJavaVariableDeclaratorId), var("fname")))))));

    public static final Def javaTypeToJavaReferenceType = def(
        "javaTypeToJavaReferenceType",
        () -> lambda(
                "t",
                "cx",
                cases(hydra.java.syntax.Type.TYPE_,
                    var("t"),
                    field(hydra.java.syntax.Type.REFERENCE, lambda("rt", right(var("rt")))),
                    field(
                        hydra.java.syntax.Type.PRIMITIVE,
                        constant(
                            left(
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        string("expected a Java reference type")))))))));

    public static final Def javaTypeToJavaResult = def(
        "javaTypeToJavaResult",
        () -> lambda("jt",
                inject(Result.TYPE_,
                    Result.TYPE,
                    wrap(UnannType.TYPE_, var("jt")))));

    public static final Def javaTypeToJavaTypeArgument = def(
        "javaTypeToJavaTypeArgument",
        () -> lambda("t",
                cases(hydra.java.syntax.Type.TYPE_,
                    var("t"),
                    field(
                        hydra.java.syntax.Type.REFERENCE,
                        lambda("rt",
                            inject(TypeArgument.TYPE_,
                                TypeArgument.REFERENCE,
                                var("rt")))),
                    field(
                        hydra.java.syntax.Type.PRIMITIVE,
                        constant(
                            inject(TypeArgument.TYPE_,
                                TypeArgument.WILDCARD,
                                record(Wildcard.TYPE_,
                                    field(Wildcard.ANNOTATIONS, list()),
                                    field(Wildcard.WILDCARD, nothing()))))))));

    public static final Def javaTypeVariable = def(
        "javaTypeVariable",
        () -> lambda("v",
                inject(ReferenceType.TYPE_,
                    ReferenceType.VARIABLE,
                    record(TypeVariable.TYPE_,
                        field(TypeVariable.ANNOTATIONS, list()),
                        field(
                            TypeVariable.IDENTIFIER,
                            apply(
                                ref(Utils.javaTypeIdentifier),
                                apply(var("hydra.formatting.capitalize"), var("v"))))))));

    public static final Def javaTypeVariableToType = def(
        "javaTypeVariableToType",
        () -> lambda("tv",
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.VARIABLE,
                        var("tv")))));

    public static final Def javaUnaryExpressionToJavaExpression = def(
        "javaUnaryExpressionToJavaExpression",
        () -> lambda("ue",
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_,
                                list(
                                    wrap(ConditionalAndExpression.TYPE_,
                                        list(
                                            wrap(InclusiveOrExpression.TYPE_,
                                                list(
                                                    wrap(
                                                        ExclusiveOrExpression.TYPE_,
                                                        list(
                                                            wrap(
                                                                AndExpression.TYPE_,
                                                                list(
                                                                    inject(
                                                                        EqualityExpression.TYPE_,
                                                                        EqualityExpression.UNARY,
                                                                        inject(
                                                                            RelationalExpression.TYPE_,
                                                                            RelationalExpression.SIMPLE,
                                                                            inject(
                                                                                ShiftExpression.TYPE_,
                                                                                ShiftExpression.UNARY,
                                                                                inject(
                                                                                    AdditiveExpression.TYPE_,
                                                                                    AdditiveExpression.UNARY,
                                                                                    inject(
                                                                                        MultiplicativeExpression.TYPE_,
                                                                                        MultiplicativeExpression.UNARY,
                                                                                        var("ue")))))))))))))))))))));

    public static final Def javaUnaryExpressionToJavaRelationalExpression = def(
        "javaUnaryExpressionToJavaRelationalExpression",
        () -> lambda("ue",
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            inject(MultiplicativeExpression.TYPE_,
                                MultiplicativeExpression.UNARY,
                                var("ue")))))));

    public static final Def javaVariableDeclarator = def(
        "javaVariableDeclarator",
        () -> lambda(
                "id",
                "minit",
                record(VariableDeclarator.TYPE_,
                    field(
                        VariableDeclarator.ID,
                        apply(ref(Utils.javaVariableDeclaratorId), var("id"))),
                    field(VariableDeclarator.INITIALIZER, var("minit")))));

    public static final Def javaVariableDeclaratorId = def(
        "javaVariableDeclaratorId",
        () -> lambda("id",
                record(VariableDeclaratorId.TYPE_,
                    field(VariableDeclaratorId.IDENTIFIER, var("id")),
                    field(VariableDeclaratorId.DIMS, nothing()))));

    public static final Def javaVariableName = def(
        "javaVariableName",
        () -> lambda("name",
                apply(ref(Utils.javaIdentifier), apply(var("hydra.names.localNameOf"), var("name")))));

    public static final Def lookupJavaVarName = def(
        "lookupJavaVarName",
        () -> lambda(
                "aliases",
                "name",
                Maybes.cases(
                    Maps.lookup(
                        var("name"),
                        proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases")),
                    var("name"),
                    lambda("renamed", var("renamed")))));

    public static final Def makeConstructor = def(
        "makeConstructor",
        () -> lambda(
                "aliases",
                "elName",
                "private",
                "params",
                "stmts",
                let(
                    field("nm",
                        wrap(SimpleTypeName.TYPE_,
                            apply(
                                ref(Utils.nameToJavaTypeIdentifier),
                                var("aliases"),
                                bool(false),
                                var("elName")))),
                    field("cons",
                        record(ConstructorDeclarator.TYPE_,
                            field(ConstructorDeclarator.PARAMETERS, list()),
                            field(ConstructorDeclarator.NAME, var("nm")),
                            field(
                                ConstructorDeclarator.RECEIVER_PARAMETER,
                                nothing()),
                            field(
                                ConstructorDeclarator.FORMAL_PARAMETERS,
                                var("params")))),
                    field("mods",
                        list(
                            Logic.ifElse(
                                var("private"),
                                inject(ConstructorModifier.TYPE_,
                                    ConstructorModifier.PRIVATE,
                                    unit()),
                                inject(ConstructorModifier.TYPE_,
                                    ConstructorModifier.PUBLIC,
                                    unit())))),
                    field("body",
                        record(ConstructorBody.TYPE_,
                            field(ConstructorBody.INVOCATION, nothing()),
                            field(ConstructorBody.STATEMENTS, var("stmts")))),
                    inject(ClassBodyDeclaration.TYPE_,
                        ClassBodyDeclaration.CONSTRUCTOR_DECLARATION,
                        record(ConstructorDeclaration.TYPE_,
                            field(ConstructorDeclaration.MODIFIERS, var("mods")),
                            field(ConstructorDeclaration.CONSTRUCTOR, var("cons")),
                            field(ConstructorDeclaration.THROWS, nothing()),
                            field(ConstructorDeclaration.BODY, var("body")))))));

    public static final Def methodDeclaration = def(
        "methodDeclaration",
        () -> lambda(
                "mods",
                "tparams",
                "anns",
                "methodName",
                "params",
                "result",
                "stmts",
                apply(
                    ref(Utils.javaMethodDeclarationToJavaClassBodyDeclaration),
                    record(MethodDeclaration.TYPE_,
                        field(MethodDeclaration.ANNOTATIONS, var("anns")),
                        field(MethodDeclaration.MODIFIERS, var("mods")),
                        field(
                            MethodDeclaration.HEADER,
                            apply(
                                ref(Utils.javaMethodHeader),
                                var("tparams"),
                                var("methodName"),
                                var("params"),
                                var("result"))),
                        field(
                            MethodDeclaration.BODY,
                            apply(ref(Utils.javaMethodBody), var("stmts")))))));

    public static final Def methodInvocation = def(
        "methodInvocation",
        () -> lambda(
                "lhs",
                "methodName",
                "args",
                let("header",
                    Maybes.cases(
                        var("lhs"),
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.SIMPLE,
                            wrap(MethodName.TYPE_, var("methodName"))),
                        lambda("either",
                            inject(MethodInvocation_Header.TYPE_,
                                MethodInvocation_Header.COMPLEX,
                                record(MethodInvocation_Complex.TYPE_,
                                    field(
                                        MethodInvocation_Complex.VARIANT,
                                        Eithers.either_(
                                            lambda("en",
                                                inject(
                                                    MethodInvocation_Variant.TYPE_,
                                                    MethodInvocation_Variant.EXPRESSION,
                                                    var("en"))),
                                            lambda("p",
                                                inject(
                                                    MethodInvocation_Variant.TYPE_,
                                                    MethodInvocation_Variant.PRIMARY,
                                                    var("p"))),
                                            var("either"))),
                                    field(
                                        MethodInvocation_Complex.TYPE_ARGUMENTS,
                                        list()),
                                    field(
                                        MethodInvocation_Complex.IDENTIFIER,
                                        var("methodName")))))),
                    record(MethodInvocation.TYPE_,
                        field(MethodInvocation.HEADER, var("header")),
                        field(MethodInvocation.ARGUMENTS, var("args"))))));

    public static final Def methodInvocationStatic = def(
        "methodInvocationStatic",
        () -> lambda(
                "self",
                "methodName",
                "args",
                apply(
                    ref(Utils.methodInvocation),
                    just(left(apply(ref(Utils.javaIdentifierToJavaExpressionName), var("self")))),
                    var("methodName"),
                    var("args"))));

    public static final Def methodInvocationStaticWithTypeArgs = def(
        "methodInvocationStaticWithTypeArgs",
        () -> lambda(
                "self",
                "methodName",
                "targs",
                "args",
                // An empty type-argument list would serialize to the illegal Java
                // `Foo.<>method(...)`. Fall back to a plain (header-less) static
                // invocation in that case, so callers can pass possibly-empty targs
                // uniformly. See collectionTypeArgs (#394).
                Logic.ifElse(
                    Lists.null_(var("targs")),
                    apply(
                        ref(Utils.methodInvocationStatic),
                        var("self"),
                        var("methodName"),
                        var("args")),
                    let("header",
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.COMPLEX,
                            record(MethodInvocation_Complex.TYPE_,
                                field(
                                    MethodInvocation_Complex.VARIANT,
                                    inject(MethodInvocation_Variant.TYPE_,
                                        MethodInvocation_Variant.EXPRESSION,
                                        apply(
                                            ref(Utils.javaIdentifierToJavaExpressionName),
                                            var("self")))),
                                field(
                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                    var("targs")),
                                field(
                                    MethodInvocation_Complex.IDENTIFIER,
                                    var("methodName")))),
                        record(MethodInvocation.TYPE_,
                            field(MethodInvocation.HEADER, var("header")),
                            field(MethodInvocation.ARGUMENTS, var("args")))))));

    public static final Def nameToJavaClassType = def(
        "nameToJavaClassType",
        () -> lambda(
                "aliases",
                "qualify",
                "args",
                "name",
                "mlocal",
                let(
                    field("result",
                        apply(
                            ref(Utils.nameToQualifiedJavaName),
                            var("aliases"),
                            var("qualify"),
                            var("name"),
                            var("mlocal"))),
                    field("id",
                        Pairs.first(var("result"))),
                    field("pkg",
                        Pairs.second(var("result"))),
                    record(ClassType.TYPE_,
                        field(ClassType.ANNOTATIONS, list()),
                        field(ClassType.QUALIFIER, var("pkg")),
                        field(ClassType.IDENTIFIER, var("id")),
                        field(ClassType.ARGUMENTS, var("args"))))));

    public static final Def nameToJavaName = def(
        "nameToJavaName",
        () -> lambda(
                "aliases",
                "name",
                let(
                    field("qn",
                        apply(var("hydra.names.qualifyName"), var("name"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    Logic.ifElse(
                        apply(
                            ref(Utils.isEscaped),
                            apply(unwrap(Name.TYPE_), var("name"))),
                        wrap(Identifier.TYPE_,
                            apply(ref(Utils.sanitizeJavaName), var("local"))),
                        Maybes.cases(
                            var("ns_"),
                            wrap(Identifier.TYPE_, var("local")),
                            lambda("gname",
                                let(
                                    field("parts",
                                        Maybes.cases(
                                            Maps.lookup(
                                                var("gname"),
                                                proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases")),
                                            Strings.splitOn(
                                                string("."),
                                                apply(
                                                    unwrap(ModuleName.TYPE_),
                                                    var("gname"))),
                                            lambda("pkgName",
                                                Lists.map(
                                                    lambda("i",
                                                        apply(
                                                            unwrap(Identifier.TYPE_),
                                                            var("i"))),
                                                    apply(
                                                        unwrap(PackageName.TYPE_),
                                                        var("pkgName")))))),
                                    field("allParts",
                                        Lists.concat2(
                                            var("parts"),
                                            list(apply(ref(Utils.sanitizeJavaName), var("local"))))),
                                    wrap(Identifier.TYPE_,
                                        Strings.intercalate(string("."), var("allParts"))))))))));

    public static final Def nameToJavaReferenceType = def(
        "nameToJavaReferenceType",
        () -> lambda(
                "aliases",
                "qualify",
                "args",
                "name",
                "mlocal",
                inject(ReferenceType.TYPE_,
                    ReferenceType.CLASS_OR_INTERFACE,
                    inject(ClassOrInterfaceType.TYPE_,
                        ClassOrInterfaceType.CLASS,
                        apply(
                            ref(Utils.nameToJavaClassType),
                            var("aliases"),
                            var("qualify"),
                            var("args"),
                            var("name"),
                            var("mlocal"))))));

    public static final Def nameToJavaTypeIdentifier = def(
        "nameToJavaTypeIdentifier",
        () -> lambda(
                "aliases",
                "qualify",
                "name",
                Pairs.first(
                    apply(
                        ref(Utils.nameToQualifiedJavaName),
                        var("aliases"),
                        var("qualify"),
                        var("name"),
                        nothing()))));

    public static final Def nameToQualifiedJavaName = def(
        "nameToQualifiedJavaName",
        () -> lambda(
                "aliases",
                "qualify",
                "name",
                "mlocal",
                let(
                    field("qn",
                        apply(var("hydra.names.qualifyName"), var("name"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("alias",
                        Maybes.cases(
                            var("ns_"),
                            nothing(),
                            lambda("n",
                                just(
                                    Maybes.cases(
                                        Maps.lookup(
                                            var("n"),
                                            proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases")),
                                        apply(
                                            ref(Names.javaPackageName),
                                            Strings.splitOn(
                                                string("."),
                                                apply(
                                                    unwrap(ModuleName.TYPE_),
                                                    var("n")))),
                                        lambda("id", var("id"))))))),
                    field("pkg",
                        Logic.ifElse(
                            var("qualify"),
                            Maybes.cases(
                                var("alias"),
                                inject(ClassTypeQualifier.TYPE_,
                                    ClassTypeQualifier.NONE,
                                    unit()),
                                lambda("p",
                                    inject(ClassTypeQualifier.TYPE_,
                                        ClassTypeQualifier.PACKAGE,
                                        var("p")))),
                            inject(ClassTypeQualifier.TYPE_,
                                ClassTypeQualifier.NONE,
                                unit()))),
                    field("jid",
                        apply(
                            ref(Utils.javaTypeIdentifier),
                            Maybes.cases(
                                var("mlocal"),
                                apply(ref(Utils.sanitizeJavaName), var("local")),
                                lambda("l",
                                    Strings.cat2(
                                        Strings.cat2(
                                            apply(ref(Utils.sanitizeJavaName), var("local")),
                                            string(".")),
                                        apply(ref(Utils.sanitizeJavaName), var("l"))))))),
                    pair(var("jid"), var("pkg")))));

    public static final Def overrideAnnotation = def(
        "overrideAnnotation",
        () -> inject(Annotation.TYPE_,
                Annotation.MARKER,
                wrap(MarkerAnnotation.TYPE_,
                    apply(
                        ref(Utils.javaTypeName),
                        wrap(Identifier.TYPE_, string("Override"))))));

    public static final Def referenceTypeToResult = def(
        "referenceTypeToResult",
        () -> lambda("rt",
                apply(
                    ref(Utils.javaTypeToJavaResult),
                    inject(hydra.java.syntax.Type.TYPE_,
                        hydra.java.syntax.Type.REFERENCE,
                        var("rt")))));

    public static final Def sanitizeJavaName = def(
        "sanitizeJavaName",
        () -> lambda("name",
                Logic.ifElse(
                    apply(ref(Utils.isEscaped), var("name")),
                    apply(ref(Utils.unescape), var("name")),
                    Logic.ifElse(
                        Equality.equal(var("name"), string("_")),
                        string("ignored"),
                        apply(
                            var("hydra.formatting.sanitizeWithUnderscores"),
                            var("hydra.java.language.reservedWords"),
                            var("name"))))));

    public static final Def suppressWarningsUncheckedAnnotation = def(
        "suppressWarningsUncheckedAnnotation",
        () -> inject(Annotation.TYPE_,
                Annotation.SINGLE_ELEMENT,
                record(SingleElementAnnotation.TYPE_,
                    field(
                        SingleElementAnnotation.NAME,
                        apply(
                            ref(Utils.javaTypeName),
                            wrap(Identifier.TYPE_, string("SuppressWarnings")))),
                    field(
                        SingleElementAnnotation.VALUE,
                        just(
                            inject(ElementValue.TYPE_,
                                ElementValue.CONDITIONAL_EXPRESSION,
                                inject(ConditionalExpression.TYPE_,
                                    ConditionalExpression.SIMPLE,
                                    wrap(ConditionalOrExpression.TYPE_,
                                        list(
                                            wrap(ConditionalAndExpression.TYPE_,
                                                list(
                                                    apply(
                                                        ref(Utils.javaPostfixExpressionToJavaInclusiveOrExpression),
                                                        inject(
                                                            PostfixExpression.TYPE_,
                                                            PostfixExpression.PRIMARY,
                                                            apply(
                                                                ref(Utils.javaLiteralToJavaPrimary),
                                                                apply(
                                                                    ref(Utils.javaString),
                                                                    string("unchecked"))))))))))))))));

    public static final Def toAcceptMethod = def(
        "toAcceptMethod",
        () -> lambda(
                "abstract",
                "vtparams",
                let(
                    java.util.Arrays.asList(
    field("mods",
                        Logic.ifElse(
                            var("abstract"),
                            list(
                                inject(MethodModifier.TYPE_,
                                    MethodModifier.PUBLIC,
                                    unit()),
                                inject(MethodModifier.TYPE_,
                                    MethodModifier.ABSTRACT,
                                    unit())),
                            list(
                                inject(MethodModifier.TYPE_,
                                    MethodModifier.PUBLIC,
                                    unit())))),
    field("tparams",
                        list(
                            apply(
                                ref(Utils.javaTypeParameter),
                                ref(Names.visitorReturnParameter)))),
    field("anns",
                        Logic.ifElse(var("abstract"), list(), list(ref(Utils.overrideAnnotation)))),
    field("typeArgs",
                        Lists.map(
                            lambda("tp",
                                inject(TypeArgument.TYPE_,
                                    TypeArgument.REFERENCE,
                                    apply(ref(Utils.typeParameterToReferenceType), var("tp")))),
                            var("vtparams"))),
    field("ref",
                        apply(
                            ref(Utils.javaClassTypeToJavaType),
                            record(ClassType.TYPE_,
                                field(ClassType.ANNOTATIONS, list()),
                                field(
                                    ClassType.QUALIFIER,
                                    inject(ClassTypeQualifier.TYPE_,
                                        ClassTypeQualifier.NONE,
                                        unit())),
                                field(
                                    ClassType.IDENTIFIER,
                                    apply(
                                        ref(Utils.javaTypeIdentifier),
                                        ref(Names.visitorName))),
                                field(
                                    ClassType.ARGUMENTS,
                                    Lists.concat2(
                                        var("typeArgs"),
                                        list(
                                            inject(TypeArgument.TYPE_,
                                                TypeArgument.REFERENCE,
                                                ref(Utils.visitorTypeVariable)))))))),
    field("param",
                        apply(
                            ref(Utils.javaTypeToJavaFormalParameter),
                            var("ref"),
                            wrap(Name.TYPE_, string("visitor")))),
    field("result",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            inject(hydra.java.syntax.Type.TYPE_,
                                hydra.java.syntax.Type.REFERENCE,
                                ref(Utils.visitorTypeVariable)))),
    field("returnExpr",
                        apply(
                            ref(Utils.javaMethodInvocationToJavaExpression),
                            apply(
                                ref(Utils.methodInvocationStatic),
                                wrap(Identifier.TYPE_, string("visitor")),
                                wrap(Identifier.TYPE_,
                                    ref(Names.visitMethodName)),
                                list(ref(Utils.javaThis))))),
    field("body",
                        Logic.ifElse(
                            var("abstract"),
                            nothing(),
                            just(
                                list(
                                    inject(BlockStatement.TYPE_,
                                        BlockStatement.STATEMENT,
                                        apply(
                                            ref(Utils.javaReturnStatement),
                                            just(var("returnExpr"))))))))),
                    apply(
                        ref(Utils.methodDeclaration),
                        var("mods"),
                        var("tparams"),
                        var("anns"),
                        ref(Names.acceptMethodName),
                        list(var("param")),
                        var("result"),
                        var("body")))));

    public static final Def toAssignStmt = def(
        "toAssignStmt",
        () -> lambda("fname",
                let(
                    field("id",
                        apply(ref(Utils.fieldNameToJavaIdentifier), var("fname"))),
                    field("lhs",
                        inject(LeftHandSide.TYPE_,
                            LeftHandSide.FIELD_ACCESS,
                            record(FieldAccess.TYPE_,
                                field(
                                    FieldAccess.QUALIFIER,
                                    inject(FieldAccess_Qualifier.TYPE_,
                                        FieldAccess_Qualifier.PRIMARY,
                                        inject(Primary.TYPE_,
                                            Primary.NO_NEW_ARRAY,
                                            inject(
                                                PrimaryNoNewArrayExpression.TYPE_,
                                                PrimaryNoNewArrayExpression.THIS,
                                                unit())))),
                                field(FieldAccess.IDENTIFIER, var("id"))))),
                    field("rhs",
                        apply(ref(Utils.fieldNameToJavaExpression), var("fname"))),
                    apply(ref(Utils.javaAssignmentStatement), var("lhs"), var("rhs")))));

    public static final Def toJavaArrayType = def(
        "toJavaArrayType",
        () -> lambda(
                "t",
                "cx",
                cases(hydra.java.syntax.Type.TYPE_,
                    var("t"),
                    field(
                        hydra.java.syntax.Type.REFERENCE,
                        lambda("rt",
                            cases(ReferenceType.TYPE_,
                                var("rt"),
                                field(
                                    ReferenceType.CLASS_OR_INTERFACE,
                                    lambda("cit",
                                        right(
                                            inject(hydra.java.syntax.Type.TYPE_,
                                                hydra.java.syntax.Type.REFERENCE,
                                                inject(ReferenceType.TYPE_,
                                                    ReferenceType.ARRAY,
                                                    record(ArrayType.TYPE_,
                                                        field(
                                                            ArrayType.DIMS,
                                                            wrap(Dims.TYPE_,
                                                                list(list()))),
                                                        field(
                                                            ArrayType.VARIANT,
                                                            inject(
                                                                ArrayType_Variant.TYPE_,
                                                                ArrayType_Variant.CLASS_OR_INTERFACE,
                                                                var("cit"))))))))),
                                field(
                                    ReferenceType.ARRAY,
                                    lambda("at",
                                        let(
                                            field("oldDims",
                                                apply(
                                                    unwrap(Dims.TYPE_),
                                                    proj(ArrayType.TYPE_, ArrayType.DIMS, "at"))),
                                            field("newDims",
                                                wrap(Dims.TYPE_,
                                                    Lists.concat2(var("oldDims"), list(list())))),
                                            field("variant",
                                                proj(ArrayType.TYPE_, ArrayType.VARIANT, "at")),
                                            right(
                                                inject(hydra.java.syntax.Type.TYPE_,
                                                    hydra.java.syntax.Type.REFERENCE,
                                                    inject(ReferenceType.TYPE_,
                                                        ReferenceType.ARRAY,
                                                        record(ArrayType.TYPE_,
                                                            field(
                                                                ArrayType.DIMS,
                                                                var("newDims")),
                                                            field(
                                                                ArrayType.VARIANT,
                                                                var("variant"))))))))),
                                field(
                                    ReferenceType.VARIABLE,
                                    constant(
                                        left(
                                            inject(Error_.TYPE_,
                                                Error_.OTHER,
                                                wrap(OtherError.TYPE_,
                                                    string("don't know how to make Java reference type into array type"))))))))),
                    field(
                        hydra.java.syntax.Type.PRIMITIVE,
                        constant(
                            left(
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        string("don't know how to make Java type into array type")))))))));

    public static final Def typeParameterToReferenceType = def(
        "typeParameterToReferenceType",
        () -> lambda("tp",
                apply(
                    ref(Utils.javaTypeVariable),
                    apply(
                        unwrap(Identifier.TYPE_),
                        apply(
                            unwrap(TypeIdentifier.TYPE_),
                            proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp"))))));

    public static final Def typeParameterToTypeArgument = def(
        "typeParameterToTypeArgument",
        () -> lambda("tp",
                apply(
                    ref(Utils.javaTypeIdentifierToJavaTypeArgument),
                    proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp"))));

    public static final Def unTypeParameter = def(
        "unTypeParameter",
        () -> lambda("tp",
                apply(
                    unwrap(Identifier.TYPE_),
                    apply(
                        unwrap(TypeIdentifier.TYPE_),
                        proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp")))));

    public static final Def unescape = def(
        "unescape",
        () -> lambda("s", Strings.fromList(Lists.drop(int32(1), Strings.toList(var("s"))))));

    public static final Def uniqueVarName = def(
        "uniqueVarName",
        () -> lambda(
                "aliases",
                "name",
                Logic.ifElse(
                    Sets.member(
                        var("name"),
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")),
                    apply(
                        ref(Utils.uniqueVarName_go),
                        var("aliases"),
                        apply(unwrap(Name.TYPE_), var("name")),
                        int32(2)),
                    var("name"))));

    public static final Def uniqueVarName_go = def(
        "uniqueVarName_go",
        () -> lambda(
                "aliases",
                "base",
                "n",
                let("candidate",
                    wrap(Name.TYPE_,
                        Strings.cat2(var("base"), Literals.showInt32(var("n")))),
                    Logic.ifElse(
                        Sets.member(
                            var("candidate"),
                            proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")),
                        apply(
                            ref(Utils.uniqueVarName_go),
                            var("aliases"),
                            var("base"),
                            Math_.add(var("n"), int32(1))),
                        var("candidate")))));

    public static final Def varDeclarationStatement = def(
        "varDeclarationStatement",
        () -> lambda(
                "id",
                "rhs",
                inject(BlockStatement.TYPE_,
                    BlockStatement.LOCAL_VARIABLE_DECLARATION,
                    wrap(LocalVariableDeclarationStatement.TYPE_,
                        record(LocalVariableDeclaration.TYPE_,
                            field(LocalVariableDeclaration.MODIFIERS, list()),
                            field(
                                LocalVariableDeclaration.TYPE,
                                inject(LocalVariableType.TYPE_,
                                    LocalVariableType.VAR,
                                    unit())),
                            field(
                                LocalVariableDeclaration.DECLARATORS,
                                list(
                                    apply(
                                        ref(Utils.javaVariableDeclarator),
                                        var("id"),
                                        just(
                                            inject(VariableInitializer.TYPE_,
                                                VariableInitializer.EXPRESSION,
                                                var("rhs")))))))))));

    public static final Def variableDeclarationStatement = def(
        "variableDeclarationStatement",
        () -> lambda(
                "aliases",
                "jtype",
                "id",
                "rhs",
                let(
                    field("init_",
                        inject(VariableInitializer.TYPE_,
                            VariableInitializer.EXPRESSION,
                            var("rhs"))),
                    field("vdec",
                        apply(ref(Utils.javaVariableDeclarator), var("id"), just(var("init_")))),
                    inject(BlockStatement.TYPE_,
                        BlockStatement.LOCAL_VARIABLE_DECLARATION,
                        wrap(LocalVariableDeclarationStatement.TYPE_,
                            record(LocalVariableDeclaration.TYPE_,
                                field(LocalVariableDeclaration.MODIFIERS, list()),
                                field(
                                    LocalVariableDeclaration.TYPE,
                                    inject(LocalVariableType.TYPE_,
                                        LocalVariableType.TYPE,
                                        wrap(UnannType.TYPE_, var("jtype")))),
                                field(
                                    LocalVariableDeclaration.DECLARATORS,
                                    list(var("vdec")))))))));

    public static final Def variableToJavaIdentifier = def(
        "variableToJavaIdentifier",
        () -> lambda("name",
                let("v",
                    apply(unwrap(Name.TYPE_), var("name")),
                    Logic.ifElse(
                        Equality.equal(var("v"), string("_")),
                        wrap(Identifier.TYPE_, string("ignored")),
                        wrap(Identifier.TYPE_,
                            apply(ref(Utils.sanitizeJavaName), var("v")))))));

    public static final Def variantClassName = def(
        "variantClassName",
        () -> lambda(
                "qualify",
                "elName",
                "fname",
                let(
                    field("qn",
                        apply(var("hydra.names.qualifyName"), var("elName"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("flocal",
                        apply(
                            var("hydra.formatting.capitalize"),
                            apply(unwrap(Name.TYPE_), var("fname")))),
                    field("local1",
                        Logic.ifElse(
                            var("qualify"),
                            Strings.cat2(Strings.cat2(var("local"), string(".")), var("flocal")),
                            Logic.ifElse(
                                Equality.equal(var("flocal"), var("local")),
                                Strings.cat2(var("flocal"), string("_")),
                                var("flocal")))),
                    apply(
                        var("hydra.names.unqualifyName"),
                        record(QualifiedName.TYPE_,
                            field(QualifiedName.MODULE_NAME, var("ns_")),
                            field(QualifiedName.LOCAL, var("local1")))))));

    public static final Def visitorTypeVariable = def(
        "visitorTypeVariable",
        () -> apply(ref(Utils.javaTypeVariable), string("r")));









    private static final List<Definition> DEFINITIONS = definitionsOf(
            addExpressions,
            addInScopeVar,
            addInScopeVars,
            addJavaTypeParameter,
            addVarRename,
            fieldExpression,
            fieldNameToJavaExpression,
            fieldNameToJavaIdentifier,
            fieldNameToJavaVariableDeclarator,
            fieldNameToJavaVariableDeclaratorId,
            finalVarDeclarationStatement,
            importAliasesForModule,
            interfaceMethodDeclaration,
            isEscaped,
            javaAdditiveExpressionToJavaExpression,
            javaArrayCreation,
            javaArrayInitializer,
            javaAssignmentStatement,
            javaBoolean,
            javaBooleanExpression,
            javaBooleanType,
            javaBytePrimitiveType,
            javaCastExpression,
            javaCastExpressionToJavaExpression,
            javaCastPrimitive,
            javaClassDeclaration,
            javaClassType,
            javaClassTypeToJavaType,
            javaConditionalAndExpressionToJavaExpression,
            javaConstructorCall,
            javaConstructorName,
            javaDeclName,
            javaDoubleCastExpression,
            javaDoubleCastExpressionToJavaExpression,
            javaEmptyStatement,
            javaEqualityExpressionToJavaExpression,
            javaEqualityExpressionToJavaInclusiveOrExpression,
            javaEquals,
            javaEqualsNull,
            javaExpressionNameToJavaExpression,
            javaExpressionToJavaPrimary,
            javaExpressionToJavaUnaryExpression,
            javaFieldAccessToJavaExpression,
            javaIdentifier,
            javaIdentifierToJavaExpression,
            javaIdentifierToJavaExpressionName,
            javaIdentifierToJavaRelationalExpression,
            javaIdentifierToJavaUnaryExpression,
            javaInstanceOf,
            javaInt,
            javaIntExpression,
            javaIntType,
            javaInterfaceDeclarationToJavaClassBodyDeclaration,
            javaLambda,
            javaLambdaFromBlock,
            javaLiteralToJavaExpression,
            javaLiteralToJavaMultiplicativeExpression,
            javaLiteralToJavaPrimary,
            javaLiteralToJavaRelationalExpression,
            javaMemberField,
            javaMethodBody,
            javaMethodDeclarationToJavaClassBodyDeclaration,
            javaMethodHeader,
            javaMethodInvocationToJavaExpression,
            javaMethodInvocationToJavaPostfixExpression,
            javaMethodInvocationToJavaPrimary,
            javaMethodInvocationToJavaStatement,
            javaMultiplicativeExpressionToJavaRelationalExpression,
            javaPackageDeclaration,
            javaPostfixExpressionToJavaEqualityExpression,
            javaPostfixExpressionToJavaExpression,
            javaPostfixExpressionToJavaInclusiveOrExpression,
            javaPostfixExpressionToJavaRelationalExpression,
            javaPostfixExpressionToJavaUnaryExpression,
            javaPrimaryToJavaExpression,
            javaPrimaryToJavaUnaryExpression,
            javaPrimitiveTypeToJavaType,
            javaRefType,
            javaReferenceTypeToRawType,
            javaRelationalExpressionToJavaEqualityExpression,
            javaRelationalExpressionToJavaExpression,
            javaRelationalExpressionToJavaUnaryExpression,
            javaReturnStatement,
            javaStatementsToBlock,
            javaString,
            javaStringMultiplicativeExpression,
            javaThis,
            javaThrowIllegalArgumentException,
            javaThrowIllegalStateException,
            javaThrowStatement,
            javaTypeFromTypeName,
            javaTypeIdentifier,
            javaTypeIdentifierToJavaTypeArgument,
            javaTypeName,
            javaTypeParameter,
            javaTypeToJavaFormalParameter,
            javaTypeToJavaReferenceType,
            javaTypeToJavaResult,
            javaTypeToJavaTypeArgument,
            javaTypeVariable,
            javaTypeVariableToType,
            javaUnaryExpressionToJavaExpression,
            javaUnaryExpressionToJavaRelationalExpression,
            javaVariableDeclarator,
            javaVariableDeclaratorId,
            javaVariableName,
            lookupJavaVarName,
            makeConstructor,
            methodDeclaration,
            methodInvocation,
            methodInvocationStatic,
            methodInvocationStaticWithTypeArgs,
            nameToJavaClassType,
            nameToJavaName,
            nameToJavaReferenceType,
            nameToJavaTypeIdentifier,
            nameToQualifiedJavaName,
            overrideAnnotation,
            referenceTypeToResult,
            sanitizeJavaName,
            suppressWarningsUncheckedAnnotation,
            toAcceptMethod,
            toAssignStmt,
            toJavaArrayType,
            typeParameterToReferenceType,
            typeParameterToTypeArgument,
            unTypeParameter,
            unescape,
            uniqueVarName,
            uniqueVarName_go,
            varDeclarationStatement,
            variableDeclarationStatement,
            variableToJavaIdentifier,
            variantClassName,
            visitorTypeVariable);

    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
        new ModuleName("hydra.java.language"),
        new ModuleName("hydra.java.names"),
        new ModuleName("hydra.java.serde"),
        new ModuleName("hydra.formatting"),
        new ModuleName("hydra.names"),
        new ModuleName("hydra.serialization"),
        new ModuleName("hydra.java.environment"),
        new ModuleName("hydra.java.syntax"),
        new ModuleName("hydra.paths"),
        new ModuleName("hydra.ast"),
        new ModuleName("hydra.classes"),
        new ModuleName("hydra.coders"),
        new ModuleName("hydra.context"),
        new ModuleName("hydra.core"),
        new ModuleName("hydra.error.checking"),
        new ModuleName("hydra.error.core"),
        new ModuleName("hydra.error.packaging"),
        new ModuleName("hydra.errors"),
        new ModuleName("hydra.graph"),
        new ModuleName("hydra.json.model"),
        new ModuleName("hydra.packaging"),
        new ModuleName("hydra.parsing"),
        new ModuleName("hydra.phantoms"),
        new ModuleName("hydra.query"),
        new ModuleName("hydra.relational"),
        new ModuleName("hydra.tabular"),
        new ModuleName("hydra.testing"),
        new ModuleName("hydra.topology"),
        new ModuleName("hydra.typing"),
        new ModuleName("hydra.util"),
        new ModuleName("hydra.validation"),
        new ModuleName("hydra.variants"));

    public static final Module module_ = new Module(
        Maybe.just("Java utilities for constructing Java syntax trees"),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
