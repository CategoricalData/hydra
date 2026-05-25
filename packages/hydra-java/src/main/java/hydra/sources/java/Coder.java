package hydra.sources.java;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.core.TypeScheme;
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
import hydra.core.AnnotatedTerm;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.AnnotatedType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Application;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.ApplicationType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Binding;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.CaseStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.EitherType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.FieldType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.FloatType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.FloatValue;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.ForallType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.FunctionType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Injection;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.IntegerType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.IntegerValue;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Lambda;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Let;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Literal;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.LiteralType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.MapType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.PairType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Projection;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Record;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.Term;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.TypeApplicationTerm;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.TypeLambda;  // AUTO-IMPORT (hydra-java DSL)
import hydra.core.WrappedTerm;  // AUTO-IMPORT (hydra-java DSL)
import hydra.errors.DecodingError;  // AUTO-IMPORT (hydra-java DSL)
import hydra.errors.Error_;  // AUTO-IMPORT (hydra-java DSL)
import hydra.errors.OtherError;  // AUTO-IMPORT (hydra-java DSL)
import hydra.graph.Graph;  // AUTO-IMPORT (hydra-java DSL)
import hydra.graph.Primitive;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.environment.Aliases;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.environment.JavaEnvironment;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.environment.JavaFeatures;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.environment.JavaSymbolClass;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AmbiguousName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.AnnotatedIdentifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ArrayType_Variant;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Block;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.BlockStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBodyDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassBodyDeclarationWithComments;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassMemberDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassOrInterfaceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassOrInterfaceTypeToInstantiate;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ClassTypeQualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.CompilationUnit;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConditionalAndExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ConstantDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ContinueStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Dims;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.EqualityExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.EqualityExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Expression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ExpressionName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldAccess;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldAccess_Qualifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FieldModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FloatingPointLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.FloatingPointType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Identifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IfThenStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ImportDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IntegerLiteral;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.IntegralType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMemberDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMemberDeclarationWithComments;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceMethodModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.InterfaceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaBody;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LambdaParameters;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.LeftHandSide;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation_Complex;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation_Header;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodInvocation_Variant;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodModifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MethodName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MultiplicativeExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.MultiplicativeExpression_Binary;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NormalClassDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NormalInterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.NumericType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.OrdinaryCompilationUnit;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PostfixExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimitiveType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PrimitiveTypeWithAnnotations;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.ReferenceType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Result;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.SingleTypeImportDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.Statement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.StatementWithoutTrailingSubstatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TopLevelClassOrInterfaceDeclaration;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeArgument;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeArgumentsOrDiamond;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.TypeIdentifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnannType;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnaryExpression;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.UnaryExpressionNotPlusMinus;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableDeclarator;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableDeclaratorId;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.VariableInitializer;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.WhileStatement;  // AUTO-IMPORT (hydra-java DSL)
import hydra.packaging.FileExtension;  // AUTO-IMPORT (hydra-java DSL)
import hydra.packaging.PrimitiveDefinition;  // AUTO-IMPORT (hydra-java DSL)
import hydra.packaging.QualifiedName;  // AUTO-IMPORT (hydra-java DSL)
import hydra.packaging.TermDefinition;  // AUTO-IMPORT (hydra-java DSL)
import hydra.packaging.TypeDefinition;  // AUTO-IMPORT (hydra-java DSL)
import hydra.sources.java.Names;  // AUTO-IMPORT (hydra-java DSL)
import hydra.sources.java.Serde;  // AUTO-IMPORT (hydra-java DSL)
import hydra.sources.java.Utils;  // AUTO-IMPORT (hydra-java DSL)
import hydra.typing.FunctionStructure;  // AUTO-IMPORT (hydra-java DSL)
import hydra.util.CaseConvention;  // AUTO-IMPORT (hydra-java DSL)

/**
 * Java code generator: converts Hydra modules to Java source code.
 *
 * <p>Auto-ported from the canonical JSON via /tmp/json_to_java.py
 * (mirror of {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Coder.hs}).
 * Each definition includes its pre-computed TypeScheme so the Java host
 * can skip inference for this module.</p>
 */
public class Coder {
    public static final ModuleName NS = new ModuleName("hydra.java.coder");

    private static Def def(String localName, Supplier<TTerm<?>> body) {
        return define(NS, localName, body);
    }

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def addComment = def(
        "addComment",
        () -> lambda(
                "decl",
                "field",
                "cx",
                "g",
                Eithers.map(
                    lambda("c",
                        record(ClassBodyDeclarationWithComments.TYPE_,
                            field(
                                ClassBodyDeclarationWithComments.VALUE,
                                var("decl")),
                            field(
                                ClassBodyDeclarationWithComments.COMMENTS,
                                var("c")))),
                    apply(
                        var("hydra.annotations.commentsFromFieldType"),
                        var("cx"),
                        var("g"),
                        var("field")))));

    public static final Def analyzeJavaFunction = def(
        "analyzeJavaFunction",
        () -> lambda(
                "env",
                "term",
                "cx",
                "g",
                apply(
                    var("hydra.analysis.analyzeFunctionTerm"),
                    var("cx"),
                    ref(Coder.javaEnvGetGraph),
                    ref(Coder.javaEnvSetGraph),
                    var("env"),
                    var("term"))));

    public static final Def annotateBodyWithCod = def(
        "annotateBodyWithCod",
        () -> lambda(
                "typ",
                "term",
                let("setAnn",
                    lambda("t",
                        apply(
                            var("hydra.annotations.setTermAnnotation"),
                            var("hydra.constants.keyType"),
                            just(apply(var("hydra.encode.core.type"), var("typ"))),
                            var("t"))),
                    casesWithDefault(Term.TYPE_,
                        apply(var("hydra.strip.deannotateTerm"), var("term")),
                        apply(var("setAnn"), var("term")),
                        field(
                            Term.TYPE_APPLICATION,
                            constant(apply(var("setAnn"), var("term")))),
                        field(
                            Term.APPLICATION,
                            lambda("app",
                                let(
                                    field("lhs",
                                        proj(Application.TYPE_, Application.FUNCTION, "app")),
                                    field("rhs",
                                        proj(Application.TYPE_, Application.ARGUMENT, "app")),
                                    field("annotatedRhs",
                                        casesWithDefault(Term.TYPE_,
                                            apply(var("hydra.strip.deannotateTerm"), var("rhs")),
                                            var("rhs"),
                                            field(
                                                Term.TYPE_APPLICATION,
                                                constant(
                                                    apply(
                                                        ref(Coder.annotateBodyWithCod),
                                                        apply(
                                                            ref(Coder.extractArgType),
                                                            var("lhs"),
                                                            var("typ")),
                                                        var("rhs")))))),
                                    apply(
                                        var("setAnn"),
                                        inject(Term.TYPE_,
                                            Term.APPLICATION,
                                            record(Application.TYPE_,
                                                field(Application.FUNCTION, var("lhs")),
                                                field(
                                                    Application.ARGUMENT,
                                                    var("annotatedRhs"))))))))))));

    public static final Def annotateLambdaArgs = def(
        "annotateLambdaArgs",
        () -> lambda(
                "cname",
                "tApps",
                "argTerms",
                "cx",
                "g",
                Logic.ifElse(
                    Lists.null_(var("tApps")),
                    right(var("argTerms")),
                    Eithers.bind(
                        Eithers.bind(
                            right(apply(var("hydra.lexical.lookupBinding"), var("g"), var("cname"))),
                            lambda("mel",
                                Maybes.cases(
                                    var("mel"),
                                    right(
                                        Maybes.map(
                                            lambda("prim",
                                                apply(var("hydra.scoping.termSignatureToTypeScheme"),
                                                    proj(PrimitiveDefinition.TYPE_, PrimitiveDefinition.SIGNATURE,
                                                        proj(Primitive.TYPE_, Primitive.DEFINITION, "prim")))),
                                            Maps.lookup(
                                                var("cname"),
                                                proj(Graph.TYPE_, Graph.PRIMITIVES, "g")))),
                                    lambda("el",
                                        right(
                                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el")))))),
                        lambda("mts",
                            Maybes.cases(
                                var("mts"),
                                right(var("argTerms")),
                                lambda("ts",
                                    let(
                                        field("schemeType",
                                            proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                                        field("schemeTypeVars",
                                            apply(ref(Coder.collectTypeVars), var("schemeType"))),
                                        field("schemeVars",
                                            Lists.filter(
                                                lambda("v",
                                                    Sets.member(var("v"), var("schemeTypeVars"))),
                                                proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
                                        Logic.ifElse(
                                            Logic.or_(
                                                Lists.null_(var("schemeVars")),
                                                Logic.not_(
                                                    Equality.equal(
                                                        Lists.length(var("schemeVars")),
                                                        Lists.length(var("tApps"))))),
                                            right(var("argTerms")),
                                            let(
                                                field("subst",
                                                    Maps.fromList(
                                                        Lists.zip(var("schemeVars"), var("tApps")))),
                                                field("expectedTypes",
                                                    apply(
                                                        ref(Coder.peelExpectedTypes),
                                                        var("subst"),
                                                        Lists.length(var("argTerms")),
                                                        var("schemeType"))),
                                                right(
                                                    Lists.zipWith(
                                                        lambda(
                                                            "arg",
                                                            "mExpected",
                                                            apply(
                                                                ref(Coder.propagateType),
                                                                var("mExpected"),
                                                                var("arg"))),
                                                        var("argTerms"),
                                                        Lists.concat2(
                                                            var("expectedTypes"),
                                                            Lists.replicate(
                                                                Lists.length(var("argTerms")),
                                                                inject(Type.TYPE_,
                                                                    Type.VARIABLE,
                                                                    wrap(Name.TYPE_,
                                                                        string("unused")))))))))))))))));

    public static final Def applyCastIfSafe = def(
        "applyCastIfSafe",
        () -> lambda(
                "aliases",
                "castType",
                "expr",
                "cx",
                "g",
                let(
                    field("trusted",
                        proj(Aliases.TYPE_, Aliases.TRUSTED_TYPE_VARS, "aliases")),
                    field("inScope",
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                    field("castVars",
                        apply(ref(Coder.collectTypeVars), var("castType"))),
                    field("javaTypeVars",
                        Sets.fromList(
                            Lists.filter(
                                lambda("v",
                                    Logic.or_(
                                        Sets.member(var("v"), var("inScope")),
                                        apply(ref(Coder.isLambdaBoundVariable), var("v")))),
                                Sets.toList(var("castVars"))))),
                    field("isSafe",
                        Logic.or_(
                            Sets.null_(var("trusted")),
                            Logic.or_(
                                Sets.null_(var("javaTypeVars")),
                                Sets.null_(Sets.difference(var("javaTypeVars"), var("trusted")))))),
                    Logic.ifElse(
                        var("isSafe"),
                        Eithers.bind(
                            apply(
                                ref(Coder.encodeType),
                                var("aliases"),
                                var("hydra.lib.sets.empty"),
                                var("castType"),
                                var("cx"),
                                var("g")),
                            lambda("jtype",
                                Eithers.bind(
                                    apply(
                                        ref(Utils.javaTypeToJavaReferenceType),
                                        var("jtype"),
                                        var("cx")),
                                    lambda("rt",
                                        right(
                                            apply(
                                                ref(Utils.javaCastExpressionToJavaExpression),
                                                apply(
                                                    ref(Utils.javaCastExpression),
                                                    var("rt"),
                                                    apply(
                                                        ref(Utils.javaExpressionToJavaUnaryExpression),
                                                        var("expr"))))))))),
                        right(var("expr"))))));

    public static final Def applyJavaArg = def(
        "applyJavaArg",
        () -> lambda(
                "expr",
                "jarg",
                apply(
                    ref(Utils.javaMethodInvocationToJavaExpression),
                    apply(
                        ref(Utils.methodInvocation),
                        just(
                            right(
                                apply(
                                    ref(Utils.javaExpressionToJavaPrimary),
                                    var("expr")))),
                        wrap(Identifier.TYPE_,
                            ref(Names.applyMethodName)),
                        list(var("jarg"))))));

    public static final Def applyOvergenSubstToTermAnnotations = def(
        "applyOvergenSubstToTermAnnotations",
        () -> lambda(
                "subst",
                "term0",
                "cx",
                "g",
                right(
                    apply(
                        ref(Coder.applyOvergenSubstToTermAnnotations_go),
                        var("subst"),
                        var("g"),
                        var("term0")))));

    public static final Def applyOvergenSubstToTermAnnotations_go = def(
        "applyOvergenSubstToTermAnnotations_go",
        () -> lambda(
                "subst",
                "cx",
                "term",
                casesWithDefault(Term.TYPE_,
                    var("term"),
                    var("term"),
                    field(
                        Term.ANNOTATED,
                        lambda("at",
                            let(
                                field("inner",
                                    proj(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY, "at")),
                                field("ann",
                                    proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at")),
                                field("ann'",
                                    Maybes.cases(
                                        Maps.lookup(var("hydra.constants.keyType"), var("ann")),
                                        var("ann"),
                                        lambda("typeTerm",
                                            Eithers.either_(
                                                constant(var("ann")),
                                                lambda("t",
                                                    let("t'",
                                                        apply(
                                                            ref(Coder.substituteTypeVarsWithTypes),
                                                            var("subst"),
                                                            var("t")),
                                                        Maps.insert(
                                                            var("hydra.constants.keyType"),
                                                            apply(
                                                                var("hydra.encode.core.type"),
                                                                var("t'")),
                                                            var("ann")))),
                                                apply(
                                                    var("hydra.decode.core.type"),
                                                    var("cx"),
                                                    var("typeTerm")))))),
                                inject(Term.TYPE_,
                                    Term.ANNOTATED,
                                    record(AnnotatedTerm.TYPE_,
                                        field(
                                            AnnotatedTerm.BODY,
                                            apply(
                                                ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                                var("subst"),
                                                var("cx"),
                                                var("inner"))),
                                        field(AnnotatedTerm.ANNOTATION, var("ann'"))))))),
                    field(
                        Term.APPLICATION,
                        lambda("app",
                            inject(Term.TYPE_,
                                Term.APPLICATION,
                                record(Application.TYPE_,
                                    field(
                                        Application.FUNCTION,
                                        apply(
                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                            var("subst"),
                                            var("cx"),
                                            proj(Application.TYPE_, Application.FUNCTION, "app"))),
                                    field(
                                        Application.ARGUMENT,
                                        apply(
                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                            var("subst"),
                                            var("cx"),
                                            proj(Application.TYPE_, Application.ARGUMENT, "app"))))))),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            inject(Term.TYPE_,
                                Term.LAMBDA,
                                record(Lambda.TYPE_,
                                    field(
                                        Lambda.PARAMETER,
                                        proj(Lambda.TYPE_, Lambda.PARAMETER, "lam")),
                                    field(
                                        Lambda.DOMAIN,
                                        Maybes.map(
                                            lambda("d",
                                                apply(
                                                    ref(Coder.substituteTypeVarsWithTypes),
                                                    var("subst"),
                                                    var("d"))),
                                            proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"))),
                                    field(
                                        Lambda.BODY,
                                        apply(
                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                            var("subst"),
                                            var("cx"),
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam"))))))),
                    field(
                        Term.CASES,
                        lambda("cs",
                            inject(Term.TYPE_,
                                Term.CASES,
                                record(CaseStatement.TYPE_,
                                    field(
                                        CaseStatement.TYPE_NAME,
                                        proj(CaseStatement.TYPE_, CaseStatement.TYPE_NAME, "cs")),
                                    field(
                                        CaseStatement.DEFAULT,
                                        Maybes.map(
                                            lambda("d",
                                                apply(
                                                    ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                                    var("subst"),
                                                    var("cx"),
                                                    var("d"))),
                                            proj(CaseStatement.TYPE_, CaseStatement.DEFAULT, "cs"))),
                                    field(
                                        CaseStatement.CASES,
                                        Lists.map(
                                            lambda("fld",
                                                record(Field.TYPE_,
                                                    field(
                                                        Field.NAME,
                                                        proj(Field.TYPE_, Field.NAME, "fld")),
                                                    field(
                                                        Field.TERM,
                                                        apply(
                                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                                            var("subst"),
                                                            var("cx"),
                                                            proj(Field.TYPE_, Field.TERM, "fld"))))),
                                            proj(CaseStatement.TYPE_, CaseStatement.CASES, "cs"))))))),
                    field(
                        Term.LET,
                        lambda("lt",
                            inject(Term.TYPE_,
                                Term.LET,
                                record(Let.TYPE_,
                                    field(
                                        Let.BINDINGS,
                                        Lists.map(
                                            lambda("b",
                                                record(Binding.TYPE_,
                                                    field(
                                                        Binding.NAME,
                                                        proj(Binding.TYPE_, Binding.NAME, "b")),
                                                    field(
                                                        Binding.TERM,
                                                        apply(
                                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                                            var("subst"),
                                                            var("cx"),
                                                            proj(Binding.TYPE_, Binding.TERM, "b"))),
                                                    field(
                                                        Binding.TYPE_SCHEME,
                                                        proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b")))),
                                            proj(Let.TYPE_, Let.BINDINGS, "lt"))),
                                    field(
                                        Let.BODY,
                                        apply(
                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                            var("subst"),
                                            var("cx"),
                                            proj(Let.TYPE_, Let.BODY, "lt"))))))),
                    field(
                        Term.TYPE_APPLICATION,
                        lambda("ta",
                            inject(Term.TYPE_,
                                Term.TYPE_APPLICATION,
                                record(TypeApplicationTerm.TYPE_,
                                    field(
                                        TypeApplicationTerm.BODY,
                                        apply(
                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                            var("subst"),
                                            var("cx"),
                                            proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta"))),
                                    field(
                                        TypeApplicationTerm.TYPE,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes),
                                            var("subst"),
                                            proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE, "ta"))))))),
                    field(
                        Term.TYPE_LAMBDA,
                        lambda("tl",
                            inject(Term.TYPE_,
                                Term.TYPE_LAMBDA,
                                record(TypeLambda.TYPE_,
                                    field(
                                        TypeLambda.PARAMETER,
                                        proj(TypeLambda.TYPE_, TypeLambda.PARAMETER, "tl")),
                                    field(
                                        TypeLambda.BODY,
                                        apply(
                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                            var("subst"),
                                            var("cx"),
                                            proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl"))))))))));

    public static final Def applySubstFull = def(
        "applySubstFull",
        () -> lambda(
                "s",
                "t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    var("t"),
                    field(
                        Type.VARIABLE,
                        lambda("v", Maps.findWithDefault(var("t"), var("v"), var("s")))),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            inject(Type.TYPE_,
                                Type.FUNCTION,
                                record(FunctionType.TYPE_,
                                    field(
                                        FunctionType.DOMAIN,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"))),
                                    field(
                                        FunctionType.CODOMAIN,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"))))))),
                    field(
                        Type.APPLICATION,
                        lambda("at",
                            inject(Type.TYPE_,
                                Type.APPLICATION,
                                record(ApplicationType.TYPE_,
                                    field(
                                        ApplicationType.FUNCTION,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at"))),
                                    field(
                                        ApplicationType.ARGUMENT,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at"))))))),
                    field(
                        Type.LIST,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.LIST,
                                apply(ref(Coder.applySubstFull), var("s"), var("inner"))))),
                    field(
                        Type.SET,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.SET,
                                apply(ref(Coder.applySubstFull), var("s"), var("inner"))))),
                    field(
                        Type.MAYBE,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.MAYBE,
                                apply(ref(Coder.applySubstFull), var("s"), var("inner"))))),
                    field(
                        Type.MAP,
                        lambda("mt",
                            inject(Type.TYPE_,
                                Type.MAP,
                                record(MapType.TYPE_,
                                    field(
                                        MapType.KEYS,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(MapType.TYPE_, MapType.KEYS, "mt"))),
                                    field(
                                        MapType.VALUES,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(MapType.TYPE_, MapType.VALUES, "mt"))))))),
                    field(
                        Type.PAIR,
                        lambda("pt",
                            inject(Type.TYPE_,
                                Type.PAIR,
                                record(PairType.TYPE_,
                                    field(
                                        PairType.FIRST,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(PairType.TYPE_, PairType.FIRST, "pt"))),
                                    field(
                                        PairType.SECOND,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(PairType.TYPE_, PairType.SECOND, "pt"))))))),
                    field(
                        Type.EITHER,
                        lambda("et",
                            inject(Type.TYPE_,
                                Type.EITHER,
                                record(EitherType.TYPE_,
                                    field(
                                        EitherType.LEFT,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(EitherType.TYPE_, EitherType.LEFT, "et"))),
                                    field(
                                        EitherType.RIGHT,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            var("s"),
                                            proj(EitherType.TYPE_, EitherType.RIGHT, "et"))))))),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            inject(Type.TYPE_,
                                Type.FORALL,
                                record(ForallType.TYPE_,
                                    field(
                                        ForallType.PARAMETER,
                                        proj(ForallType.TYPE_, ForallType.PARAMETER, "ft")),
                                    field(
                                        ForallType.BODY,
                                        apply(
                                            ref(Coder.applySubstFull),
                                            Maps.delete(
                                                proj(ForallType.TYPE_, ForallType.PARAMETER, "ft"),
                                                var("s")),
                                            proj(ForallType.TYPE_, ForallType.BODY, "ft"))))))))));

    public static final Def applySubstSimple = def(
        "applySubstSimple",
        () -> lambda(
                "subst",
                "t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    var("t"),
                    field(
                        Type.VARIABLE,
                        lambda("v", Maps.findWithDefault(var("t"), var("v"), var("subst")))))));

    public static final Def arraysCompareExpr = def(
        "arraysCompareExpr",
        () -> lambda(
                "otherVar",
                "fname",
                let(
                    field("header",
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.COMPLEX,
                            record(MethodInvocation_Complex.TYPE_,
                                field(
                                    MethodInvocation_Complex.VARIANT,
                                    inject(MethodInvocation_Variant.TYPE_,
                                        MethodInvocation_Variant.TYPE,
                                        apply(
                                            ref(Utils.javaTypeName),
                                            wrap(Identifier.TYPE_,
                                                string("java.util.Arrays"))))),
                                field(
                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                    list()),
                                field(
                                    MethodInvocation_Complex.IDENTIFIER,
                                    wrap(Identifier.TYPE_, string("compare")))))),
                    field("arg1",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            record(ExpressionName.TYPE_,
                                field(ExpressionName.QUALIFIER, nothing()),
                                field(
                                    ExpressionName.IDENTIFIER,
                                    wrap(Identifier.TYPE_,
                                        apply(
                                            ref(Utils.sanitizeJavaName),
                                            var("fname"))))))),
                    field("arg2",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                apply(ref(Utils.javaIdentifier), var("otherVar")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    apply(
                        ref(Utils.javaMethodInvocationToJavaExpression),
                        record(MethodInvocation.TYPE_,
                            field(MethodInvocation.HEADER, var("header")),
                            field(
                                MethodInvocation.ARGUMENTS,
                                list(var("arg1"), var("arg2"))))))));

    public static final Def arraysEqualsClause = def(
        "arraysEqualsClause",
        () -> lambda(
                "tmpName",
                "fname",
                let(
                    field("thisArg",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                wrap(Identifier.TYPE_, string("this")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    field("otherArg",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                apply(ref(Utils.javaIdentifier), var("tmpName")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    field("header",
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.COMPLEX,
                            record(MethodInvocation_Complex.TYPE_,
                                field(
                                    MethodInvocation_Complex.VARIANT,
                                    inject(MethodInvocation_Variant.TYPE_,
                                        MethodInvocation_Variant.TYPE,
                                        apply(
                                            ref(Utils.javaTypeName),
                                            wrap(Identifier.TYPE_,
                                                string("java.util.Arrays"))))),
                                field(
                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                    list()),
                                field(
                                    MethodInvocation_Complex.IDENTIFIER,
                                    wrap(Identifier.TYPE_,
                                        ref(Names.equalsMethodName)))))),
                    apply(
                        ref(Utils.javaPostfixExpressionToJavaInclusiveOrExpression),
                        apply(
                            ref(Utils.javaMethodInvocationToJavaPostfixExpression),
                            record(MethodInvocation.TYPE_,
                                field(MethodInvocation.HEADER, var("header")),
                                field(
                                    MethodInvocation.ARGUMENTS,
                                    list(var("thisArg"), var("otherArg")))))))));

    public static final Def augmentVariantClass = def(
        "augmentVariantClass",
        () -> lambda(
                "aliases",
                "tparams",
                "elName",
                "cd",
                casesWithDefault(ClassDeclaration.TYPE_,
                    var("cd"),
                    var("cd"),
                    field(
                        ClassDeclaration.NORMAL,
                        lambda("ncd",
                            let(
                                field("args",
                                    Lists.map(
                                        lambda("tp",
                                            apply(
                                                ref(Utils.typeParameterToTypeArgument),
                                                var("tp"))),
                                        var("tparams"))),
                                field("extendsPart",
                                    apply(
                                        ref(Utils.nameToJavaClassType),
                                        var("aliases"),
                                        bool(true),
                                        var("args"),
                                        var("elName"),
                                        nothing())),
                                field("newMods",
                                    list(
                                        inject(ClassModifier.TYPE_,
                                            ClassModifier.PUBLIC,
                                            unit()),
                                        inject(ClassModifier.TYPE_,
                                            ClassModifier.STATIC,
                                            unit()),
                                        inject(ClassModifier.TYPE_,
                                            ClassModifier.FINAL,
                                            unit()))),
                                field("oldBody",
                                    proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.BODY, "ncd")),
                                field("oldDecls",
                                    apply(unwrap(ClassBody.TYPE_), var("oldBody"))),
                                field("acceptDecl",
                                    apply(
                                        ref(Coder.withCommentString),
                                        string("Dispatch to {@code visitor}."),
                                        apply(
                                            ref(Utils.toAcceptMethod),
                                            bool(false),
                                            var("tparams")))),
                                field("newBody",
                                    wrap(ClassBody.TYPE_,
                                        Lists.concat2(var("oldDecls"), list(var("acceptDecl"))))),
                                inject(ClassDeclaration.TYPE_,
                                    ClassDeclaration.NORMAL,
                                    record(NormalClassDeclaration.TYPE_,
                                        field(
                                            NormalClassDeclaration.MODIFIERS,
                                            var("newMods")),
                                        field(
                                            NormalClassDeclaration.IDENTIFIER,
                                            proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.IDENTIFIER, "ncd")),
                                        field(
                                            NormalClassDeclaration.PARAMETERS,
                                            var("tparams")),
                                        field(
                                            NormalClassDeclaration.EXTENDS,
                                            just(var("extendsPart"))),
                                        field(
                                            NormalClassDeclaration.IMPLEMENTS,
                                            proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.IMPLEMENTS, "ncd")),
                                        field(
                                            NormalClassDeclaration.PERMITS,
                                            proj(NormalClassDeclaration.TYPE_, NormalClassDeclaration.PERMITS, "ncd")),
                                        field(
                                            NormalClassDeclaration.BODY,
                                            var("newBody"))))))))));

    public static final Def bindingIsFunctionType = def(
        "bindingIsFunctionType",
        () -> lambda("b",
                Maybes.maybe(
                    casesWithDefault(Term.TYPE_,
                        apply(
                            var("hydra.strip.deannotateTerm"),
                            proj(Binding.TYPE_, Binding.TERM, "b")),
                        bool(false),
                        field(Term.LAMBDA, constant(bool(true))),
                        field(Term.PROJECT, constant(bool(true))),
                        field(Term.CASES, constant(bool(true))),
                        field(Term.UNWRAP, constant(bool(true)))),
                    lambda("ts",
                        casesWithDefault(Type.TYPE_,
                            apply(
                                var("hydra.strip.deannotateType"),
                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                            bool(false),
                            field(Type.FUNCTION, constant(bool(true))),
                            field(
                                Type.FORALL,
                                lambda("fa",
                                    casesWithDefault(Type.TYPE_,
                                        apply(
                                            var("hydra.strip.deannotateType"),
                                            proj(ForallType.TYPE_, ForallType.BODY, "fa")),
                                        bool(false),
                                        field(Type.FUNCTION, constant(bool(true)))))))),
                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b"))));

    public static final Def bindingNameToFilePath = def(
        "bindingNameToFilePath",
        () -> lambda("name",
                let(
                    field("qn",
                        apply(var("hydra.names.qualifyName"), var("name"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("sanitized",
                        apply(
                            var("hydra.formatting.sanitizeWithUnderscores"),
                            var("hydra.java.language.reservedWords"),
                            var("local"))),
                    field("unq",
                        apply(
                            var("hydra.names.unqualifyName"),
                            record(QualifiedName.TYPE_,
                                field(QualifiedName.MODULE_NAME, var("ns_")),
                                field(QualifiedName.LOCAL, var("sanitized"))))),
                    apply(
                        var("hydra.names.nameToFilePath"),
                        inject(CaseConvention.TYPE_,
                            CaseConvention.CAMEL,
                            unit()),
                        inject(CaseConvention.TYPE_,
                            CaseConvention.PASCAL,
                            unit()),
                        wrap(FileExtension.TYPE_, string("java")),
                        var("unq")))));

    public static final Def bindingsToStatements = def(
        "bindingsToStatements",
        () -> lambda(
                "env",
                "bindings",
                "cx",
                "g0",
                let(
                    java.util.Arrays.asList(
    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
    field("g",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")),
    field("flatBindings",
                        apply(
                            ref(Coder.dedupBindings),
                            proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases"),
                            apply(ref(Coder.flattenBindings), var("bindings")))),
    field("gExtended",
                        apply(
                            var("hydra.scoping.extendGraphForLet"),
                            lambda(
                                "g",
                                "b",
                                Logic.ifElse(
                                    apply(
                                        var("hydra.predicates.isComplexBinding"),
                                        var("g"),
                                        var("b")),
                                    just(
                                        inject(Term.TYPE_,
                                            Term.LITERAL,
                                            inject(Literal.TYPE_,
                                                Literal.BOOLEAN,
                                                bool(true)))),
                                    nothing())),
                            var("g"),
                            record(Let.TYPE_,
                                field(Let.BINDINGS, var("flatBindings")),
                                field(
                                    Let.BODY,
                                    inject(Term.TYPE_,
                                        Term.VARIABLE,
                                        wrap(Name.TYPE_, string("dummy"))))))),
    field("bindingVars",
                        Sets.fromList(
                            Lists.map(
                                lambda("b",
                                    proj(Binding.TYPE_, Binding.NAME, "b")),
                                var("flatBindings")))),
    field("allDeps",
                        Maps.fromList(
                            Lists.map(
                                lambda("b",
                                    let(
                                        field("key",
                                            proj(Binding.TYPE_, Binding.NAME, "b")),
                                        field("deps",
                                            Sets.intersection(
                                                var("bindingVars"),
                                                apply(
                                                    var("hydra.variables.freeVariablesInTerm"),
                                                    proj(Binding.TYPE_, Binding.TERM, "b")))),
                                        pair(var("key"), var("deps")))),
                                var("flatBindings")))),
    field("sorted",
                        apply(
                            var("hydra.sorting.topologicalSortComponents"),
                            Lists.map(
                                lambda("entry",
                                    let(
                                        field("key",
                                            Pairs.first(var("entry"))),
                                        field("deps",
                                            Pairs.second(var("entry"))),
                                        pair(var("key"), Sets.toList(var("deps"))))),
                                Maps.toList(var("allDeps"))))),
    field("recursiveVars",
                        Sets.fromList(
                            Lists.concat(
                                Lists.map(
                                    lambda("names",
                                        Logic.ifElse(
                                            Equality.equal(Lists.length(var("names")), int32(1)),
                                            Maybes.maybe(
                                                list(),
                                                lambda("singleName",
                                                    Maybes.cases(
                                                        Maps.lookup(
                                                            var("singleName"),
                                                            var("allDeps")),
                                                        list(),
                                                        lambda("deps",
                                                            Logic.ifElse(
                                                                Sets.member(
                                                                    var("singleName"),
                                                                    var("deps")),
                                                                list(var("singleName")),
                                                                list())))),
                                                Lists.maybeHead(var("names"))),
                                            var("names"))),
                                    var("sorted"))))),
    field("thunkedVars",
                        Sets.fromList(
                            Lists.concat(
                                Lists.map(
                                    lambda("b",
                                        let("bname",
                                            proj(Binding.TYPE_, Binding.NAME, "b"),
                                            Logic.ifElse(
                                                Logic.and_(
                                                    Logic.not_(
                                                        Sets.member(
                                                            var("bname"),
                                                            var("recursiveVars"))),
                                                    Logic.and_(
                                                        apply(
                                                            ref(Coder.needsThunking),
                                                            proj(Binding.TYPE_, Binding.TERM, "b")),
                                                        Logic.not_(
                                                            apply(
                                                                ref(Coder.bindingIsFunctionType),
                                                                var("b"))))),
                                                list(var("bname")),
                                                list()))),
                                    var("flatBindings"))))),
    field("aliasesExtended",
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
                                Sets.union(
                                    proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases"),
                                    var("recursiveVars"))),
                            field(
                                Aliases.IN_SCOPE_TYPE_PARAMS,
                                proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                            field(
                                Aliases.POLYMORPHIC_LOCALS,
                                proj(Aliases.TYPE_, Aliases.POLYMORPHIC_LOCALS, "aliases")),
                            field(
                                Aliases.IN_SCOPE_JAVA_VARS,
                                Sets.union(
                                    proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases"),
                                    var("bindingVars"))),
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
                                Sets.union(
                                    proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases"),
                                    var("thunkedVars"))))),
    field("envExtended",
                        record(JavaEnvironment.TYPE_,
                            field(
                                JavaEnvironment.ALIASES,
                                var("aliasesExtended")),
                            field(JavaEnvironment.GRAPH, var("gExtended"))))),
                    Logic.ifElse(
                        Lists.null_(var("bindings")),
                        right(pair(list(), var("envExtended"))),
                        Eithers.bind(
                            Eithers.mapList(
                                lambda("names",
                                    Eithers.bind(
                                        Eithers.mapList(
                                            lambda("n",
                                                apply(
                                                    ref(Coder.toDeclInit),
                                                    var("aliasesExtended"),
                                                    var("gExtended"),
                                                    var("recursiveVars"),
                                                    var("flatBindings"),
                                                    var("n"),
                                                    var("cx"),
                                                    var("g"))),
                                            var("names")),
                                        lambda("inits",
                                            Eithers.bind(
                                                Eithers.mapList(
                                                    lambda("n",
                                                        apply(
                                                            ref(Coder.toDeclStatement),
                                                            var("envExtended"),
                                                            var("aliasesExtended"),
                                                            var("gExtended"),
                                                            var("recursiveVars"),
                                                            var("thunkedVars"),
                                                            var("flatBindings"),
                                                            var("n"),
                                                            var("cx"),
                                                            var("g"))),
                                                    var("names")),
                                                lambda("decls",
                                                    right(
                                                        Lists.concat2(
                                                            Maybes.cat(var("inits")),
                                                            var("decls")))))))),
                                var("sorted")),
                            lambda("groups",
                                right(pair(Lists.concat(var("groups")), var("envExtended")))))))));

    public static final Def boundTypeVariables = def(
        "boundTypeVariables",
        () -> lambda("typ",
                casesWithDefault(Type.TYPE_,
                    var("typ"),
                    list(),
                    field(
                        Type.ANNOTATED,
                        lambda("at",
                            apply(
                                ref(Coder.boundTypeVariables),
                                proj(AnnotatedType.TYPE_, AnnotatedType.BODY, "at")))),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            Lists.cons(
                                proj(ForallType.TYPE_, ForallType.PARAMETER, "ft"),
                                apply(
                                    ref(Coder.boundTypeVariables),
                                    proj(ForallType.TYPE_, ForallType.BODY, "ft"))))))));

    public static final Def buildArgSubst = def(
        "buildArgSubst",
        () -> lambda(
                "schemeVarSet",
                "schemeDoms",
                "argTypes",
                Maps.fromList(
                    Lists.bind(
                        Lists.zip(var("schemeDoms"), var("argTypes")),
                        lambda("p",
                            let(
                                field("sdom",
                                    Pairs.first(var("p"))),
                                field("argType",
                                    Pairs.second(var("p"))),
                                casesWithDefault(Type.TYPE_,
                                    apply(var("hydra.strip.deannotateType"), var("sdom")),
                                    list(),
                                    field(
                                        Type.VARIABLE,
                                        lambda("v",
                                            Logic.ifElse(
                                                Sets.member(var("v"), var("schemeVarSet")),
                                                list(pair(var("v"), var("argType"))),
                                                list()))))))))));

    public static final Def buildCurriedLambda = def(
        "buildCurriedLambda",
        () -> lambda(
                "params",
                "inner",
                Lists.foldl(
                    lambda(
                        "acc",
                        "p",
                        apply(ref(Utils.javaLambda), var("p"), var("acc"))),
                    var("inner"),
                    Lists.reverse(var("params")))));

    public static final Def buildSubstFromAnnotations = def(
        "buildSubstFromAnnotations",
        () -> lambda(
                "schemeVarSet",
                "term",
                "cx",
                "g",
                right(
                    apply(
                        ref(Coder.buildSubstFromAnnotations_go),
                        var("schemeVarSet"),
                        var("g"),
                        var("term")))));

    public static final Def buildSubstFromAnnotations_go = def(
        "buildSubstFromAnnotations_go",
        () -> lambda(
                "schemeVarSet",
                "g",
                "term",
                casesWithDefault(Term.TYPE_,
                    var("term"),
                    var("hydra.lib.maps.empty"),
                    field(
                        Term.ANNOTATED,
                        lambda("at",
                            let(
                                field("body",
                                    proj(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY, "at")),
                                field("anns",
                                    proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at")),
                                field("bodySubst",
                                    apply(
                                        ref(Coder.buildSubstFromAnnotations_go),
                                        var("schemeVarSet"),
                                        var("g"),
                                        var("body"))),
                                field("annSubst",
                                    Maybes.cases(
                                        Maps.lookup(var("hydra.constants.keyType"), var("anns")),
                                        var("hydra.lib.maps.empty"),
                                        lambda("typeTerm",
                                            Eithers.either_(
                                                constant(var("hydra.lib.maps.empty")),
                                                lambda("annType",
                                                    casesWithDefault(Term.TYPE_,
                                                        apply(
                                                            var("hydra.strip.deannotateTerm"),
                                                            var("body")),
                                                        var("hydra.lib.maps.empty"),
                                                        field(
                                                            Term.LAMBDA,
                                                            lambda("lam",
                                                                Maybes.cases(
                                                                    proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"),
                                                                    var("hydra.lib.maps.empty"),
                                                                    lambda("dom",
                                                                        casesWithDefault(
                                                                            Type.TYPE_,
                                                                            apply(
                                                                                var("hydra.strip.deannotateType"),
                                                                                var("annType")),
                                                                            var("hydra.lib.maps.empty"),
                                                                            field(
                                                                                Type.FUNCTION,
                                                                                lambda("ft",
                                                                                    apply(
                                                                                        ref(Coder.buildTypeVarSubst),
                                                                                        var("schemeVarSet"),
                                                                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                                                                        var("dom"))))))))))),
                                                apply(
                                                    var("hydra.decode.core.type"),
                                                    var("g"),
                                                    var("typeTerm")))))),
                                Maps.union(var("annSubst"), var("bodySubst"))))),
                    field(
                        Term.APPLICATION,
                        lambda("app",
                            Maps.union(
                                apply(
                                    ref(Coder.buildSubstFromAnnotations_go),
                                    var("schemeVarSet"),
                                    var("g"),
                                    proj(Application.TYPE_, Application.FUNCTION, "app")),
                                apply(
                                    ref(Coder.buildSubstFromAnnotations_go),
                                    var("schemeVarSet"),
                                    var("g"),
                                    proj(Application.TYPE_, Application.ARGUMENT, "app"))))),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            apply(
                                ref(Coder.buildSubstFromAnnotations_go),
                                var("schemeVarSet"),
                                var("g"),
                                proj(Lambda.TYPE_, Lambda.BODY, "lam")))),
                    field(
                        Term.CASES,
                        lambda("cs",
                            let(
                                field("defSubst",
                                    Maybes.cases(
                                        proj(CaseStatement.TYPE_, CaseStatement.DEFAULT, "cs"),
                                        var("hydra.lib.maps.empty"),
                                        lambda("d",
                                            apply(
                                                ref(Coder.buildSubstFromAnnotations_go),
                                                var("schemeVarSet"),
                                                var("g"),
                                                var("d"))))),
                                field("caseSubsts",
                                    Lists.foldl(
                                        lambda(
                                            "acc",
                                            "fld",
                                            Maps.union(
                                                var("acc"),
                                                apply(
                                                    ref(Coder.buildSubstFromAnnotations_go),
                                                    var("schemeVarSet"),
                                                    var("g"),
                                                    proj(Field.TYPE_, Field.TERM, "fld")))),
                                        var("hydra.lib.maps.empty"),
                                        proj(CaseStatement.TYPE_, CaseStatement.CASES, "cs"))),
                                Maps.union(var("defSubst"), var("caseSubsts"))))),
                    field(
                        Term.LET,
                        lambda("lt",
                            let("bindingSubst",
                                Lists.foldl(
                                    lambda(
                                        "acc",
                                        "b",
                                        Maps.union(
                                            var("acc"),
                                            apply(
                                                ref(Coder.buildSubstFromAnnotations_go),
                                                var("schemeVarSet"),
                                                var("g"),
                                                proj(Binding.TYPE_, Binding.TERM, "b")))),
                                    var("hydra.lib.maps.empty"),
                                    proj(Let.TYPE_, Let.BINDINGS, "lt")),
                                Maps.union(
                                    var("bindingSubst"),
                                    apply(
                                        ref(Coder.buildSubstFromAnnotations_go),
                                        var("schemeVarSet"),
                                        var("g"),
                                        proj(Let.TYPE_, Let.BODY, "lt")))))),
                    field(
                        Term.LIST,
                        lambda("terms",
                            Lists.foldl(
                                lambda(
                                    "acc",
                                    "t",
                                    Maps.union(
                                        var("acc"),
                                        apply(
                                            ref(Coder.buildSubstFromAnnotations_go),
                                            var("schemeVarSet"),
                                            var("g"),
                                            var("t")))),
                                var("hydra.lib.maps.empty"),
                                var("terms")))),
                    field(
                        Term.MAYBE,
                        lambda("mt",
                            Maybes.cases(
                                var("mt"),
                                var("hydra.lib.maps.empty"),
                                lambda("t",
                                    apply(
                                        ref(Coder.buildSubstFromAnnotations_go),
                                        var("schemeVarSet"),
                                        var("g"),
                                        var("t")))))),
                    field(
                        Term.PAIR,
                        lambda("p",
                            Maps.union(
                                apply(
                                    ref(Coder.buildSubstFromAnnotations_go),
                                    var("schemeVarSet"),
                                    var("g"),
                                    Pairs.first(var("p"))),
                                apply(
                                    ref(Coder.buildSubstFromAnnotations_go),
                                    var("schemeVarSet"),
                                    var("g"),
                                    Pairs.second(var("p")))))),
                    field(
                        Term.RECORD,
                        lambda("r",
                            Lists.foldl(
                                lambda(
                                    "acc",
                                    "fld",
                                    Maps.union(
                                        var("acc"),
                                        apply(
                                            ref(Coder.buildSubstFromAnnotations_go),
                                            var("schemeVarSet"),
                                            var("g"),
                                            proj(Field.TYPE_, Field.TERM, "fld")))),
                                var("hydra.lib.maps.empty"),
                                proj(Record.TYPE_, Record.FIELDS, "r")))),
                    field(
                        Term.SET,
                        lambda("terms",
                            Lists.foldl(
                                lambda(
                                    "acc",
                                    "t",
                                    Maps.union(
                                        var("acc"),
                                        apply(
                                            ref(Coder.buildSubstFromAnnotations_go),
                                            var("schemeVarSet"),
                                            var("g"),
                                            var("t")))),
                                var("hydra.lib.maps.empty"),
                                Sets.toList(var("terms"))))),
                    field(
                        Term.TYPE_APPLICATION,
                        lambda("ta",
                            apply(
                                ref(Coder.buildSubstFromAnnotations_go),
                                var("schemeVarSet"),
                                var("g"),
                                proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta")))),
                    field(
                        Term.TYPE_LAMBDA,
                        lambda("tl",
                            apply(
                                ref(Coder.buildSubstFromAnnotations_go),
                                var("schemeVarSet"),
                                var("g"),
                                proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl")))),
                    field(
                        Term.EITHER,
                        lambda("e",
                            Eithers.either_(
                                lambda("t",
                                    apply(
                                        ref(Coder.buildSubstFromAnnotations_go),
                                        var("schemeVarSet"),
                                        var("g"),
                                        var("t"))),
                                lambda("t",
                                    apply(
                                        ref(Coder.buildSubstFromAnnotations_go),
                                        var("schemeVarSet"),
                                        var("g"),
                                        var("t"))),
                                var("e")))))));

    public static final Def buildTypeSubst = def(
        "buildTypeSubst",
        () -> lambda(
                "schemeVarSet",
                "schemeType",
                "actualType",
                apply(
                    ref(Coder.buildTypeSubst_go),
                    var("schemeVarSet"),
                    apply(var("hydra.strip.deannotateType"), var("schemeType")),
                    apply(var("hydra.strip.deannotateType"), var("actualType")))));

    public static final Def buildTypeSubst_go = def(
        "buildTypeSubst_go",
        () -> lambda(
                "svs",
                "st",
                "at",
                let("goSub",
                    lambda(
                        "a",
                        "b",
                        apply(
                            ref(Coder.buildTypeSubst_go),
                            var("svs"),
                            apply(var("hydra.strip.deannotateType"), var("a")),
                            apply(var("hydra.strip.deannotateType"), var("b")))),
                    casesWithDefault(Type.TYPE_,
                        var("st"),
                        var("hydra.lib.maps.empty"),
                        field(
                            Type.VARIABLE,
                            lambda("v",
                                Logic.ifElse(
                                    Sets.member(var("v"), var("svs")),
                                    Maps.singleton(var("v"), var("at")),
                                    var("hydra.lib.maps.empty")))),
                        field(
                            Type.FUNCTION,
                            lambda("sft",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.FUNCTION,
                                        lambda("aft",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "sft"),
                                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "aft")),
                                                apply(
                                                    var("goSub"),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "sft"),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "aft")))))))),
                        field(
                            Type.APPLICATION,
                            lambda("sat",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.APPLICATION,
                                        lambda("aat",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "sat"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "aat")),
                                                apply(
                                                    var("goSub"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "sat"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "aat")))))))),
                        field(
                            Type.LIST,
                            lambda("sl",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.LIST,
                                        lambda("al", apply(var("goSub"), var("sl"), var("al"))))))),
                        field(
                            Type.SET,
                            lambda("ss",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.SET,
                                        lambda("as'", apply(var("goSub"), var("ss"), var("as'"))))))),
                        field(
                            Type.MAYBE,
                            lambda("sm",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.MAYBE,
                                        lambda("am", apply(var("goSub"), var("sm"), var("am"))))))),
                        field(
                            Type.MAP,
                            lambda("smt",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.MAP,
                                        lambda("amt",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(MapType.TYPE_, MapType.KEYS, "smt"),
                                                    proj(MapType.TYPE_, MapType.KEYS, "amt")),
                                                apply(
                                                    var("goSub"),
                                                    proj(MapType.TYPE_, MapType.VALUES, "smt"),
                                                    proj(MapType.TYPE_, MapType.VALUES, "amt")))))))),
                        field(
                            Type.PAIR,
                            lambda("spt",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.PAIR,
                                        lambda("apt",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(PairType.TYPE_, PairType.FIRST, "spt"),
                                                    proj(PairType.TYPE_, PairType.FIRST, "apt")),
                                                apply(
                                                    var("goSub"),
                                                    proj(PairType.TYPE_, PairType.SECOND, "spt"),
                                                    proj(PairType.TYPE_, PairType.SECOND, "apt")))))))),
                        field(
                            Type.EITHER,
                            lambda("set'",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.EITHER,
                                        lambda("aet",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(EitherType.TYPE_, EitherType.LEFT, "set'"),
                                                    proj(EitherType.TYPE_, EitherType.LEFT, "aet")),
                                                apply(
                                                    var("goSub"),
                                                    proj(EitherType.TYPE_, EitherType.RIGHT, "set'"),
                                                    proj(EitherType.TYPE_, EitherType.RIGHT, "aet")))))))),
                        field(
                            Type.FORALL,
                            lambda("sfa",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    apply(
                                        var("goSub"),
                                        proj(ForallType.TYPE_, ForallType.BODY, "sfa"),
                                        var("at")),
                                    field(
                                        Type.FORALL,
                                        lambda("afa",
                                            apply(
                                                var("goSub"),
                                                proj(ForallType.TYPE_, ForallType.BODY, "sfa"),
                                                proj(ForallType.TYPE_, ForallType.BODY, "afa")))))))))));

    public static final Def buildTypeVarSubst = def(
        "buildTypeVarSubst",
        () -> lambda(
                "schemeVarSet",
                "freshTyp",
                "canonTyp",
                apply(
                    ref(Coder.buildTypeVarSubst_go),
                    var("schemeVarSet"),
                    apply(var("hydra.strip.deannotateType"), var("freshTyp")),
                    apply(var("hydra.strip.deannotateType"), var("canonTyp")))));

    public static final Def buildTypeVarSubst_go = def(
        "buildTypeVarSubst_go",
        () -> lambda(
                "svs",
                "ft",
                "ct",
                let("goSub",
                    lambda(
                        "a",
                        "b",
                        apply(
                            ref(Coder.buildTypeVarSubst_go),
                            var("svs"),
                            apply(var("hydra.strip.deannotateType"), var("a")),
                            apply(var("hydra.strip.deannotateType"), var("b")))),
                    casesWithDefault(Type.TYPE_,
                        var("ft"),
                        casesWithDefault(Type.TYPE_,
                            var("ct"),
                            var("hydra.lib.maps.empty"),
                            field(
                                Type.FORALL,
                                lambda("cfa",
                                    apply(
                                        ref(Coder.buildTypeVarSubst_go),
                                        var("svs"),
                                        var("ft"),
                                        apply(
                                            var("hydra.strip.deannotateType"),
                                            proj(ForallType.TYPE_, ForallType.BODY, "cfa")))))),
                        field(
                            Type.VARIABLE,
                            lambda("fn",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.VARIABLE,
                                        lambda("cn",
                                            Logic.ifElse(
                                                Logic.and_(
                                                    Logic.not_(Equality.equal(var("fn"), var("cn"))),
                                                    Sets.member(var("cn"), var("svs"))),
                                                Maps.singleton(var("fn"), var("cn")),
                                                var("hydra.lib.maps.empty"))))))),
                        field(
                            Type.FUNCTION,
                            lambda("fft",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.FUNCTION,
                                        lambda("cft",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "fft"),
                                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "cft")),
                                                apply(
                                                    var("goSub"),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "fft"),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "cft")))))))),
                        field(
                            Type.APPLICATION,
                            lambda("fat",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.APPLICATION,
                                        lambda("cat",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "fat"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "cat")),
                                                apply(
                                                    var("goSub"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "fat"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "cat")))))))),
                        field(
                            Type.LIST,
                            lambda("fl",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.LIST,
                                        lambda("cl", apply(var("goSub"), var("fl"), var("cl"))))))),
                        field(
                            Type.SET,
                            lambda("fs",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.SET,
                                        lambda("cs", apply(var("goSub"), var("fs"), var("cs"))))))),
                        field(
                            Type.MAYBE,
                            lambda("fm",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.MAYBE,
                                        lambda("cm", apply(var("goSub"), var("fm"), var("cm"))))))),
                        field(
                            Type.MAP,
                            lambda("fmt",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.MAP,
                                        lambda("cmt",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(MapType.TYPE_, MapType.KEYS, "fmt"),
                                                    proj(MapType.TYPE_, MapType.KEYS, "cmt")),
                                                apply(
                                                    var("goSub"),
                                                    proj(MapType.TYPE_, MapType.VALUES, "fmt"),
                                                    proj(MapType.TYPE_, MapType.VALUES, "cmt")))))))),
                        field(
                            Type.PAIR,
                            lambda("fpt",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.PAIR,
                                        lambda("cpt",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(PairType.TYPE_, PairType.FIRST, "fpt"),
                                                    proj(PairType.TYPE_, PairType.FIRST, "cpt")),
                                                apply(
                                                    var("goSub"),
                                                    proj(PairType.TYPE_, PairType.SECOND, "fpt"),
                                                    proj(PairType.TYPE_, PairType.SECOND, "cpt")))))))),
                        field(
                            Type.EITHER,
                            lambda("fet",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    var("hydra.lib.maps.empty"),
                                    field(
                                        Type.EITHER,
                                        lambda("cet",
                                            Maps.union(
                                                apply(
                                                    var("goSub"),
                                                    proj(EitherType.TYPE_, EitherType.LEFT, "fet"),
                                                    proj(EitherType.TYPE_, EitherType.LEFT, "cet")),
                                                apply(
                                                    var("goSub"),
                                                    proj(EitherType.TYPE_, EitherType.RIGHT, "fet"),
                                                    proj(EitherType.TYPE_, EitherType.RIGHT, "cet")))))))),
                        field(
                            Type.FORALL,
                            lambda("ffa",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    apply(
                                        ref(Coder.buildTypeVarSubst_go),
                                        var("svs"),
                                        apply(
                                            var("hydra.strip.deannotateType"),
                                            proj(ForallType.TYPE_, ForallType.BODY, "ffa")),
                                        var("ct")),
                                    field(
                                        Type.FORALL,
                                        lambda("cfa",
                                            apply(
                                                var("goSub"),
                                                proj(ForallType.TYPE_, ForallType.BODY, "ffa"),
                                                proj(ForallType.TYPE_, ForallType.BODY, "cfa")))))))))));

    public static final Def classModsPublic = def(
        "classModsPublic",
        () -> list(
                inject(ClassModifier.TYPE_,
                    ClassModifier.PUBLIC,
                    unit())));

    public static final Def classifyDataReference = def(
        "classifyDataReference",
        () -> lambda(
                "name",
                "cx",
                "g",
                Eithers.bind(
                    right(apply(var("hydra.lexical.lookupBinding"), var("g"), var("name"))),
                    lambda("mel",
                        Maybes.cases(
                            var("mel"),
                            right(
                                inject(JavaSymbolClass.TYPE_,
                                    JavaSymbolClass.LOCAL_VARIABLE,
                                    unit())),
                            lambda("el",
                                Maybes.cases(
                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                    left(
                                        inject(Error_.TYPE_,
                                            Error_.OTHER,
                                            wrap(OtherError.TYPE_,
                                                Strings.cat2(
                                                    string("no type scheme for element "),
                                                    apply(
                                                        unwrap(Name.TYPE_),
                                                        proj(Binding.TYPE_, Binding.NAME, "el")))))),
                                    lambda("ts",
                                        right(
                                            apply(
                                                ref(Coder.classifyDataTerm),
                                                var("ts"),
                                                proj(Binding.TYPE_, Binding.TERM, "el")))))))))));

    public static final Def classifyDataTerm = def(
        "classifyDataTerm",
        () -> lambda(
                "ts",
                "term",
                Logic.ifElse(
                    apply(var("hydra.dependencies.isLambda"), var("term")),
                    let("n",
                        apply(ref(Coder.classifyDataTerm_countLambdaParams), var("term")),
                        Logic.ifElse(
                            Equality.gt(var("n"), int32(1)),
                            inject(JavaSymbolClass.TYPE_,
                                JavaSymbolClass.HOISTED_LAMBDA,
                                var("n")),
                            inject(JavaSymbolClass.TYPE_,
                                JavaSymbolClass.UNARY_FUNCTION,
                                unit()))),
                    let("hasTypeParams",
                        Logic.not_(
                            Lists.null_(
                                proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
                        Logic.ifElse(
                            var("hasTypeParams"),
                            let("n2",
                                apply(
                                    ref(Coder.classifyDataTerm_countLambdaParams),
                                    apply(ref(Coder.classifyDataTerm_stripTypeLambdas), var("term"))),
                                Logic.ifElse(
                                    Equality.gt(var("n2"), int32(0)),
                                    inject(JavaSymbolClass.TYPE_,
                                        JavaSymbolClass.HOISTED_LAMBDA,
                                        var("n2")),
                                    inject(JavaSymbolClass.TYPE_,
                                        JavaSymbolClass.NULLARY_FUNCTION,
                                        unit()))),
                            inject(JavaSymbolClass.TYPE_,
                                JavaSymbolClass.NULLARY_FUNCTION,
                                unit()))))));

    public static final Def classifyDataTerm_countLambdaParams = def(
        "classifyDataTerm_countLambdaParams",
        () -> lambda("t",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    int32(0),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            Math_.add(
                                int32(1),
                                apply(
                                    ref(Coder.classifyDataTerm_countLambdaParams),
                                    proj(Lambda.TYPE_, Lambda.BODY, "lam"))))),
                    field(
                        Term.LET,
                        lambda("lt",
                            apply(
                                ref(Coder.classifyDataTerm_countLambdaParams),
                                proj(Let.TYPE_, Let.BODY, "lt")))))));

    public static final Def classifyDataTerm_stripTypeLambdas = def(
        "classifyDataTerm_stripTypeLambdas",
        () -> lambda("t",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    var("t"),
                    field(
                        Term.TYPE_LAMBDA,
                        lambda("tl",
                            apply(
                                ref(Coder.classifyDataTerm_stripTypeLambdas),
                                proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl")))))));

    public static final Def cmpDeclStatement = def(
        "cmpDeclStatement",
        () -> lambda("aliases",
                apply(
                    ref(Utils.variableDeclarationStatement),
                    var("aliases"),
                    ref(Utils.javaIntType),
                    apply(ref(Utils.javaIdentifier), string("cmp")),
                    apply(
                        ref(Utils.javaIntExpression),
                        bigint(java.math.BigInteger.valueOf(0L))))));

    public static final Def cmpNotZeroExpr = def(
        "cmpNotZeroExpr",
        () -> let(
                field("lhs",
                    apply(
                        ref(Utils.javaRelationalExpressionToJavaEqualityExpression),
                        apply(
                            ref(Utils.javaPostfixExpressionToJavaRelationalExpression),
                            inject(PostfixExpression.TYPE_,
                                PostfixExpression.NAME,
                                record(ExpressionName.TYPE_,
                                    field(ExpressionName.QUALIFIER, nothing()),
                                    field(
                                        ExpressionName.IDENTIFIER,
                                        apply(
                                            ref(Utils.javaIdentifier),
                                            string("cmp")))))))),
                field("rhs",
                    apply(
                        ref(Utils.javaPostfixExpressionToJavaRelationalExpression),
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.PRIMARY,
                            apply(
                                ref(Utils.javaLiteralToJavaPrimary),
                                apply(
                                    ref(Utils.javaInt),
                                    bigint(java.math.BigInteger.valueOf(0L))))))),
                apply(
                    ref(Utils.javaEqualityExpressionToJavaExpression),
                    inject(EqualityExpression.TYPE_,
                        EqualityExpression.NOT_EQUAL,
                        record(EqualityExpression_Binary.TYPE_,
                            field(EqualityExpression_Binary.LHS, var("lhs")),
                            field(EqualityExpression_Binary.RHS, var("rhs")))))));

    public static final Def collectForallParams = def(
        "collectForallParams",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    list(),
                    field(
                        Type.FORALL,
                        lambda("fa",
                            Lists.cons(
                                proj(ForallType.TYPE_, ForallType.PARAMETER, "fa"),
                                apply(
                                    ref(Coder.collectForallParams),
                                    proj(ForallType.TYPE_, ForallType.BODY, "fa"))))))));

    public static final Def collectLambdaDomains = def(
        "collectLambdaDomains",
        () -> lambda("t",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    pair(list(), var("t")),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            Maybes.cases(
                                proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"),
                                pair(list(), var("t")),
                                lambda("dom",
                                    let("rest",
                                        apply(
                                            ref(Coder.collectLambdaDomains),
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam")),
                                        pair(
                                            Lists.cons(var("dom"), Pairs.first(var("rest"))),
                                            Pairs.second(var("rest")))))))))));

    public static final Def collectTypeApps = def(
        "collectTypeApps",
        () -> lambda(
                "t",
                "acc",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    pair(apply(var("hydra.strip.deannotateTerm"), var("t")), var("acc")),
                    field(
                        Term.TYPE_APPLICATION,
                        lambda("ta",
                            apply(
                                ref(Coder.collectTypeApps),
                                proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta"),
                                Lists.cons(
                                    proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE, "ta"),
                                    var("acc"))))))));

    public static final Def collectTypeApps0 = def(
        "collectTypeApps0",
        () -> lambda(
                "t",
                "acc",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    pair(var("t"), var("acc")),
                    field(
                        Term.TYPE_APPLICATION,
                        lambda("ta",
                            apply(
                                ref(Coder.collectTypeApps0),
                                proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta"),
                                Lists.cons(
                                    proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE, "ta"),
                                    var("acc"))))))));

    public static final Def collectTypeVars = def(
        "collectTypeVars",
        () -> lambda("typ",
                apply(
                    ref(Coder.collectTypeVars_go),
                    apply(var("hydra.strip.deannotateType"), var("typ")))));

    public static final Def collectTypeVars_go = def(
        "collectTypeVars_go",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    var("t"),
                    var("hydra.lib.sets.empty"),
                    field(Type.VARIABLE, lambda("name", Sets.singleton(var("name")))),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")))))),
                    field(
                        Type.APPLICATION,
                        lambda("at",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at")))))),
                    field(
                        Type.LIST,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                apply(var("hydra.strip.deannotateType"), var("inner"))))),
                    field(
                        Type.SET,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                apply(var("hydra.strip.deannotateType"), var("inner"))))),
                    field(
                        Type.MAYBE,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                apply(var("hydra.strip.deannotateType"), var("inner"))))),
                    field(
                        Type.MAP,
                        lambda("mt",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(MapType.TYPE_, MapType.KEYS, "mt"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(MapType.TYPE_, MapType.VALUES, "mt")))))),
                    field(
                        Type.PAIR,
                        lambda("pt",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(PairType.TYPE_, PairType.FIRST, "pt"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(PairType.TYPE_, PairType.SECOND, "pt")))))),
                    field(
                        Type.EITHER,
                        lambda("et",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(EitherType.TYPE_, EitherType.LEFT, "et"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(EitherType.TYPE_, EitherType.RIGHT, "et")))))),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                apply(
                                    var("hydra.strip.deannotateType"),
                                    proj(ForallType.TYPE_, ForallType.BODY, "ft"))))))));

    public static final Def comparableCompareExpr = def(
        "comparableCompareExpr",
        () -> lambda(
                "otherVar",
                "fname",
                let(
                    field("thisField",
                        apply(
                            ref(Utils.javaIdentifierToJavaExpression),
                            wrap(Identifier.TYPE_,
                                apply(ref(Utils.sanitizeJavaName), var("fname"))))),
                    field("otherField",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                apply(ref(Utils.javaIdentifier), var("otherVar")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    apply(
                        ref(Utils.javaMethodInvocationToJavaExpression),
                        apply(
                            ref(Utils.methodInvocationStatic),
                            wrap(Identifier.TYPE_, string("hydra.util.Comparing")),
                            wrap(Identifier.TYPE_, string("compare")),
                            list(var("thisField"), var("otherField")))))));

    public static final Def compareAndReturnStmts = def(
        "compareAndReturnStmts",
        () -> lambda(
                "otherVar",
                "f",
                list(
                    inject(BlockStatement.TYPE_,
                        BlockStatement.STATEMENT,
                        apply(
                            ref(Utils.javaAssignmentStatement),
                            inject(LeftHandSide.TYPE_,
                                LeftHandSide.EXPRESSION_NAME,
                                record(ExpressionName.TYPE_,
                                    field(ExpressionName.QUALIFIER, nothing()),
                                    field(
                                        ExpressionName.IDENTIFIER,
                                        apply(
                                            ref(Utils.javaIdentifier),
                                            string("cmp"))))),
                            apply(ref(Coder.compareFieldExpr), var("otherVar"), var("f")))),
                    inject(BlockStatement.TYPE_,
                        BlockStatement.STATEMENT,
                        inject(Statement.TYPE_,
                            Statement.IF_THEN,
                            record(IfThenStatement.TYPE_,
                                field(
                                    IfThenStatement.EXPRESSION,
                                    ref(Coder.cmpNotZeroExpr)),
                                field(
                                    IfThenStatement.STATEMENT,
                                    apply(
                                        ref(Utils.javaReturnStatement),
                                        just(
                                            apply(
                                                ref(Utils.javaExpressionNameToJavaExpression),
                                                record(ExpressionName.TYPE_,
                                                    field(
                                                        ExpressionName.QUALIFIER,
                                                        nothing()),
                                                    field(
                                                        ExpressionName.IDENTIFIER,
                                                        apply(
                                                            ref(Utils.javaIdentifier),
                                                            string("cmp"))))))))))))));

    public static final Def compareFieldExpr = def(
        "compareFieldExpr",
        () -> lambda(
                "otherVar",
                "ft",
                let(
                    field("fname",
                        apply(
                            unwrap(Name.TYPE_),
                            proj(FieldType.TYPE_, FieldType.NAME, "ft"))),
                    field("ftype",
                        proj(FieldType.TYPE_, FieldType.TYPE, "ft")),
                    Logic.ifElse(
                        apply(ref(Coder.isBinaryType), var("ftype")),
                        apply(ref(Coder.arraysCompareExpr), var("otherVar"), var("fname")),
                        Logic.ifElse(
                            apply(ref(Coder.isNonComparableType), var("ftype")),
                            apply(ref(Coder.hashCodeCompareExpr), var("otherVar"), var("fname")),
                            apply(ref(Coder.comparableCompareExpr), var("otherVar"), var("fname")))))));

    public static final Def compareToBody = def(
        "compareToBody",
        () -> lambda(
                "aliases",
                "otherVar",
                "fields",
                let("zeroStmts",
                    list(
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            apply(
                                ref(Utils.javaReturnStatement),
                                just(
                                    apply(
                                        ref(Utils.javaIntExpression),
                                        bigint(java.math.BigInteger.valueOf(0L))))))),
                    Maybes.fromMaybe(
                        var("zeroStmts"),
                        Maybes.map(
                            lambda("p",
                                let(
                                    field("firstField",
                                        Pairs.first(var("p"))),
                                    field("restFields",
                                        Pairs.second(var("p"))),
                                    Logic.ifElse(
                                        Lists.null_(var("restFields")),
                                        list(
                                            inject(BlockStatement.TYPE_,
                                                BlockStatement.STATEMENT,
                                                apply(
                                                    ref(Utils.javaReturnStatement),
                                                    just(
                                                        apply(
                                                            ref(Coder.compareFieldExpr),
                                                            var("otherVar"),
                                                            var("firstField")))))),
                                        Lists.concat2(
                                            list(apply(ref(Coder.cmpDeclStatement), var("aliases"))),
                                            Lists.concat2(
                                                Lists.concat(
                                                    Lists.map(
                                                        lambda("f",
                                                            apply(
                                                                ref(Coder.compareAndReturnStmts),
                                                                var("otherVar"),
                                                                var("f"))),
                                                        Lists.cons(
                                                            var("firstField"),
                                                            Maybes.fromMaybe(
                                                                list(),
                                                                Lists.maybeInit(var("restFields")))))),
                                                list(
                                                    inject(BlockStatement.TYPE_,
                                                        BlockStatement.STATEMENT,
                                                        apply(
                                                            ref(Utils.javaReturnStatement),
                                                            just(
                                                                apply(
                                                                    ref(Coder.compareFieldExpr),
                                                                    var("otherVar"),
                                                                    Maybes.fromMaybe(
                                                                        var("firstField"),
                                                                        Lists.maybeLast(
                                                                            var("restFields"))))))))))))),
                            Lists.uncons(var("fields")))))));

    public static final Def compareToZeroClause = def(
        "compareToZeroClause",
        () -> lambda(
                "tmpName",
                "fname",
                let(
                    field("compareToArg",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                apply(ref(Utils.javaIdentifier), var("tmpName")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    field("compareToVar",
                        inject(MethodInvocation_Variant.TYPE_,
                            MethodInvocation_Variant.EXPRESSION,
                            apply(
                                ref(Utils.fieldExpression),
                                wrap(Identifier.TYPE_, string("this")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    field("compareToHeader",
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.COMPLEX,
                            record(MethodInvocation_Complex.TYPE_,
                                field(
                                    MethodInvocation_Complex.VARIANT,
                                    var("compareToVar")),
                                field(
                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                    list()),
                                field(
                                    MethodInvocation_Complex.IDENTIFIER,
                                    wrap(Identifier.TYPE_,
                                        ref(Names.compareToMethodName)))))),
                    field("lhs",
                        apply(
                            ref(Utils.javaRelationalExpressionToJavaEqualityExpression),
                            apply(
                                ref(Utils.javaPostfixExpressionToJavaRelationalExpression),
                                apply(
                                    ref(Utils.javaMethodInvocationToJavaPostfixExpression),
                                    record(MethodInvocation.TYPE_,
                                        field(
                                            MethodInvocation.HEADER,
                                            var("compareToHeader")),
                                        field(
                                            MethodInvocation.ARGUMENTS,
                                            list(var("compareToArg")))))))),
                    field("rhs",
                        apply(
                            ref(Utils.javaPostfixExpressionToJavaRelationalExpression),
                            inject(PostfixExpression.TYPE_,
                                PostfixExpression.PRIMARY,
                                apply(
                                    ref(Utils.javaLiteralToJavaPrimary),
                                    apply(
                                        ref(Utils.javaInt),
                                        bigint(java.math.BigInteger.valueOf(0L))))))),
                    apply(
                        ref(Utils.javaEqualityExpressionToJavaInclusiveOrExpression),
                        inject(EqualityExpression.TYPE_,
                            EqualityExpression.EQUAL,
                            record(EqualityExpression_Binary.TYPE_,
                                field(EqualityExpression_Binary.LHS, var("lhs")),
                                field(EqualityExpression_Binary.RHS, var("rhs"))))))));

    public static final Def constantDecl = def(
        "constantDecl",
        () -> lambda(
                "comment",
                "javaName",
                "aliases",
                "name",
                "cx",
                "g",
                let(
                    field("mods",
                        list(
                            inject(FieldModifier.TYPE_,
                                FieldModifier.PUBLIC,
                                unit()),
                            inject(FieldModifier.TYPE_,
                                FieldModifier.STATIC,
                                unit()),
                            inject(FieldModifier.TYPE_,
                                FieldModifier.FINAL,
                                unit()))),
                    field("nameName",
                        apply(
                            ref(Utils.nameToJavaName),
                            var("aliases"),
                            wrap(Name.TYPE_, string("hydra.core.Name")))),
                    field("env",
                        record(JavaEnvironment.TYPE_,
                            field(JavaEnvironment.ALIASES, var("aliases")),
                            field(JavaEnvironment.GRAPH, var("g")))),
                    Eithers.bind(
                        apply(
                            ref(Coder.encodeType),
                            var("aliases"),
                            var("hydra.lib.sets.empty"),
                            inject(Type.TYPE_,
                                Type.VARIABLE,
                                wrap(Name.TYPE_, string("hydra.core.Name"))),
                            var("cx"),
                            var("g")),
                        lambda("jt",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeTerm),
                                    var("env"),
                                    inject(Term.TYPE_,
                                        Term.LITERAL,
                                        inject(Literal.TYPE_,
                                            Literal.STRING,
                                            apply(unwrap(Name.TYPE_), var("name")))),
                                    var("cx"),
                                    var("g")),
                                lambda("arg",
                                    let(
                                        field("init",
                                            inject(VariableInitializer.TYPE_,
                                                VariableInitializer.EXPRESSION,
                                                apply(
                                                    ref(Utils.javaConstructorCall),
                                                    apply(
                                                        ref(Utils.javaConstructorName),
                                                        var("nameName"),
                                                        nothing()),
                                                    list(var("arg")),
                                                    nothing()))),
                                        field("var",
                                            apply(
                                                ref(Utils.javaVariableDeclarator),
                                                wrap(Identifier.TYPE_,
                                                    var("javaName")),
                                                just(var("init")))),
                                        right(
                                            apply(
                                                ref(Coder.withCommentString),
                                                var("comment"),
                                                apply(
                                                    ref(Utils.javaMemberField),
                                                    var("mods"),
                                                    var("jt"),
                                                    var("var"))))))))))));

    public static final Def constantDeclForFieldType = def(
        "constantDeclForFieldType",
        () -> lambda(
                "parentName",
                "aliases",
                "ftyp",
                "cx",
                "g",
                let(
                    field("name",
                        proj(FieldType.TYPE_, FieldType.NAME, "ftyp")),
                    field("javaName",
                        apply(
                            var("hydra.formatting.nonAlnumToUnderscores"),
                            apply(
                                var("hydra.formatting.convertCase"),
                                inject(CaseConvention.TYPE_,
                                    CaseConvention.CAMEL,
                                    unit()),
                                inject(CaseConvention.TYPE_,
                                    CaseConvention.UPPER_SNAKE,
                                    unit()),
                                apply(unwrap(Name.TYPE_), var("name"))))),
                    field("comment",
                        Strings.cat(
                            list(
                                string("Name of the {@code "),
                                apply(unwrap(Name.TYPE_), var("parentName")),
                                string("."),
                                apply(unwrap(Name.TYPE_), var("name")),
                                string("} field.")))),
                    apply(
                        ref(Coder.constantDecl),
                        var("comment"),
                        var("javaName"),
                        var("aliases"),
                        var("name"),
                        var("cx"),
                        var("g")))));

    public static final Def constantDeclForTypeName = def(
        "constantDeclForTypeName",
        () -> lambda(
                "aliases",
                "name",
                "cx",
                "g",
                let("comment",
                    Strings.cat(
                        list(
                            string("Name of the {@code "),
                            apply(unwrap(Name.TYPE_), var("name")),
                            string("} type."))),
                    apply(
                        ref(Coder.constantDecl),
                        var("comment"),
                        string("TYPE_"),
                        var("aliases"),
                        var("name"),
                        var("cx"),
                        var("g")))));

    public static final Def constructElementsInterface = def(
        "constructElementsInterface",
        () -> lambda(
                "mod",
                "members",
                let(
                    java.util.Arrays.asList(
    field("ns",
                        proj(Module.TYPE_, Module.NAME, "mod")),
    field("parentNs",
                        apply(ref(Coder.namespaceParent), var("ns"))),
    field("pkg",
                        Maybes.cases(
                            var("parentNs"),
                            apply(ref(Utils.javaPackageDeclaration), var("ns")),
                            lambda("pns",
                                apply(
                                    ref(Utils.javaPackageDeclaration),
                                    var("pns"))))),
    field("mods",
                        list(
                            inject(InterfaceModifier.TYPE_,
                                InterfaceModifier.PUBLIC,
                                unit()))),
    field("className",
                        apply(ref(Coder.elementsClassName), var("ns"))),
    field("elName",
                        apply(ref(Coder.elementsQualifiedName), var("ns"))),
    field("body",
                        wrap(InterfaceBody.TYPE_, var("members"))),
    field("itf",
                        inject(TopLevelClassOrInterfaceDeclaration.TYPE_,
                            TopLevelClassOrInterfaceDeclaration.INTERFACE,
                            inject(InterfaceDeclaration.TYPE_,
                                InterfaceDeclaration.NORMAL_INTERFACE,
                                record(NormalInterfaceDeclaration.TYPE_,
                                    field(
                                        NormalInterfaceDeclaration.MODIFIERS,
                                        var("mods")),
                                    field(
                                        NormalInterfaceDeclaration.IDENTIFIER,
                                        apply(
                                            ref(Utils.javaTypeIdentifier),
                                            var("className"))),
                                    field(
                                        NormalInterfaceDeclaration.PARAMETERS,
                                        list()),
                                    field(
                                        NormalInterfaceDeclaration.EXTENDS,
                                        list()),
                                    field(
                                        NormalInterfaceDeclaration.PERMITS,
                                        list()),
                                    field(
                                        NormalInterfaceDeclaration.BODY,
                                        var("body")))))),
    field("decl",
                        record(
                            TopLevelClassOrInterfaceDeclarationWithComments.TYPE_,
                            field(
                                TopLevelClassOrInterfaceDeclarationWithComments.VALUE,
                                var("itf")),
                            field(
                                TopLevelClassOrInterfaceDeclarationWithComments.COMMENTS,
                                proj(Module.TYPE_, Module.DESCRIPTION, "mod"))))),
                    pair(
                        var("elName"),
                        inject(CompilationUnit.TYPE_,
                            CompilationUnit.ORDINARY,
                            record(OrdinaryCompilationUnit.TYPE_,
                                field(
                                    OrdinaryCompilationUnit.PACKAGE,
                                    just(var("pkg"))),
                                field(OrdinaryCompilationUnit.IMPORTS, list()),
                                field(
                                    OrdinaryCompilationUnit.TYPES,
                                    list(var("decl")))))))));

    public static final Def correctCastType = def(
        "correctCastType",
        () -> lambda(
                "innerBody",
                "typeArgs",
                "fallback",
                "cx",
                "g",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("innerBody")),
                    right(var("fallback")),
                    field(
                        Term.PAIR,
                        constant(
                            Logic.ifElse(
                                Equality.equal(Lists.length(var("typeArgs")), int32(2)),
                                right(
                                    inject(Type.TYPE_,
                                        Type.PAIR,
                                        record(PairType.TYPE_,
                                            field(
                                                PairType.FIRST,
                                                Maybes.fromMaybe(
                                                    var("fallback"),
                                                    Lists.maybeAt(int32(0), var("typeArgs")))),
                                            field(
                                                PairType.SECOND,
                                                Maybes.fromMaybe(
                                                    var("fallback"),
                                                    Lists.maybeAt(int32(1), var("typeArgs"))))))),
                                right(var("fallback"))))))));

    public static final Def correctTypeApps = def(
        "correctTypeApps",
        () -> lambda(
                "gr",
                "name",
                "args",
                "fallbackTypeApps",
                "cx",
                "g",
                Eithers.bind(
                    right(apply(var("hydra.lexical.lookupBinding"), var("g"), var("name"))),
                    lambda("mel",
                        Maybes.cases(
                            var("mel"),
                            right(var("fallbackTypeApps")),
                            lambda("el",
                                Maybes.cases(
                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                    right(var("fallbackTypeApps")),
                                    lambda("ts",
                                        let(
                                            java.util.Arrays.asList(
    field("schemeType",
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
    field("allSchemeVars",
                                                Lists.filter(
                                                    lambda("v",
                                                        apply(ref(Coder.isSimpleName), var("v"))),
                                                    proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
    field("schemeTypeVars",
                                                apply(ref(Coder.collectTypeVars), var("schemeType"))),
    field("usedFlags",
                                                Lists.map(
                                                    lambda("v",
                                                        Sets.member(var("v"), var("schemeTypeVars"))),
                                                    var("allSchemeVars"))),
    field("usedSchemeVars",
                                                apply(
                                                    ref(Coder.filterByFlags),
                                                    var("allSchemeVars"),
                                                    var("usedFlags"))),
    field("nParams",
                                                apply(
                                                    ref(Coder.countFunctionParams),
                                                    var("schemeType"))),
    field("peeled",
                                                apply(
                                                    ref(Coder.peelDomainTypes),
                                                    var("nParams"),
                                                    var("schemeType"))),
    field("calleeDoms",
                                                Pairs.first(var("peeled"))),
    field("calleeCod",
                                                Pairs.second(var("peeled"))),
    field("overgenSubst",
                                                apply(
                                                    ref(Coder.detectAccumulatorUnification),
                                                    var("calleeDoms"),
                                                    var("calleeCod"),
                                                    var("usedSchemeVars"))),
    field("keepFlags",
                                                Lists.map(
                                                    lambda("v",
                                                        Logic.and_(
                                                            Sets.member(
                                                                var("v"),
                                                                var("schemeTypeVars")),
                                                            Logic.not_(
                                                                Maps.member(
                                                                    var("v"),
                                                                    var("overgenSubst"))))),
                                                    var("allSchemeVars"))),
    field("schemeVars",
                                                apply(
                                                    ref(Coder.filterByFlags),
                                                    var("allSchemeVars"),
                                                    var("keepFlags"))),
    field("filteredFallback0",
                                                Logic.ifElse(
                                                    Equality.equal(
                                                        Lists.length(var("allSchemeVars")),
                                                        Lists.length(var("fallbackTypeApps"))),
                                                    apply(
                                                        ref(Coder.filterByFlags),
                                                        var("fallbackTypeApps"),
                                                        var("keepFlags")),
                                                    var("fallbackTypeApps"))),
    field("filteredFallback",
                                                Logic.ifElse(
                                                    Maps.null_(var("overgenSubst")),
                                                    var("filteredFallback0"),
                                                    Lists.map(
                                                        lambda("t",
                                                            apply(
                                                                ref(Coder.substituteTypeVarsWithTypes),
                                                                var("overgenSubst"),
                                                                var("t"))),
                                                        var("filteredFallback0"))))),
                                            Logic.ifElse(
                                                Logic.or_(
                                                    Lists.null_(var("schemeVars")),
                                                    Logic.not_(
                                                        Equality.equal(
                                                            Lists.length(var("schemeVars")),
                                                            Lists.length(var("filteredFallback"))))),
                                                right(var("filteredFallback")),
                                                apply(
                                                    ref(Coder.correctTypeAppsWithArgs),
                                                    var("schemeVars"),
                                                    var("filteredFallback"),
                                                    var("schemeType"),
                                                    var("args"),
                                                    var("cx"),
                                                    var("g"))))))))))));

    public static final Def correctTypeAppsWithArgs = def(
        "correctTypeAppsWithArgs",
        () -> lambda(
                "schemeVars",
                "fallbackTypeApps",
                "schemeType",
                "args",
                "cx",
                "g",
                let(
                    field("schemeVarSet",
                        Sets.fromList(var("schemeVars"))),
                    field("irSubst",
                        Maps.fromList(Lists.zip(var("schemeVars"), var("fallbackTypeApps")))),
                    field("peeled",
                        apply(
                            ref(Coder.peelDomainTypes),
                            Lists.length(var("args")),
                            var("schemeType"))),
                    field("schemeDoms",
                        Pairs.first(var("peeled"))),
                    Eithers.bind(
                        Eithers.mapList(
                            lambda("arg",
                                Eithers.bimap(
                                    lambda("__de",
                                        inject(Error_.TYPE_,
                                            Error_.OTHER,
                                            wrap(OtherError.TYPE_,
                                                apply(
                                                    unwrap(DecodingError.TYPE_),
                                                    var("__de"))))),
                                    lambda("__a", var("__a")),
                                    apply(
                                        var("hydra.annotations.getType"),
                                        var("g"),
                                        apply(
                                            var("hydra.annotations.termAnnotationInternal"),
                                            var("arg"))))),
                            var("args")),
                        lambda("mArgTypes",
                            Logic.ifElse(
                                Logic.not_(
                                    Lists.null_(
                                        Lists.filter(
                                            lambda("m", Maybes.isNothing(var("m"))),
                                            var("mArgTypes")))),
                                right(var("fallbackTypeApps")),
                                let(
                                    field("argTypes",
                                        Lists.bind(
                                            var("mArgTypes"),
                                            lambda("m",
                                                Maybes.cases(
                                                    var("m"),
                                                    list(),
                                                    lambda("x", Lists.pure(var("x"))))))),
                                    field("irDoms",
                                        Lists.map(
                                            lambda("d",
                                                apply(
                                                    ref(Coder.applySubstSimple),
                                                    var("irSubst"),
                                                    var("d"))),
                                            var("schemeDoms"))),
                                    field("domsMatch",
                                        Lists.null_(
                                            Lists.filter(
                                                lambda("p",
                                                    Logic.not_(
                                                        apply(
                                                            ref(Coder.typesMatch),
                                                            apply(
                                                                var("hydra.strip.deannotateType"),
                                                                Pairs.first(var("p"))),
                                                            apply(
                                                                var("hydra.strip.deannotateType"),
                                                                Pairs.second(var("p")))))),
                                                Lists.zip(var("irDoms"), var("argTypes"))))),
                                    Logic.ifElse(
                                        var("domsMatch"),
                                        right(var("fallbackTypeApps")),
                                        right(
                                            apply(
                                                ref(Coder.resolveTypeApps),
                                                var("schemeVars"),
                                                var("fallbackTypeApps"),
                                                apply(
                                                    ref(Coder.buildArgSubst),
                                                    var("schemeVarSet"),
                                                    var("schemeDoms"),
                                                    var("argTypes"))))))))))));

    public static final Def countFunctionParams = def(
        "countFunctionParams",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    int32(0),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            Math_.add(
                                int32(1),
                                apply(
                                    ref(Coder.countFunctionParams),
                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"))))))));

    public static final Def declarationForRecordType = def(
        "declarationForRecordType",
        () -> lambda(
                java.util.Arrays.asList("isInner", "isSer", "aliases", "tparams", "elName", "fields", "cx", "g"),
                apply(
                    ref(Coder.declarationForRecordType_prime),
                    var("isInner"),
                    var("isSer"),
                    var("aliases"),
                    var("tparams"),
                    var("elName"),
                    nothing(),
                    var("fields"),
                    var("cx"),
                    var("g"))));

    public static final Def declarationForRecordType_prime = def(
        "declarationForRecordType'",
        () -> lambda(
                java.util.Arrays.asList("isInner", "isSer", "aliases", "tparams", "elName", "parentName", "fields", "cx", "g"),
                Eithers.bind(
                    Eithers.mapList(
                        lambda("f",
                            apply(
                                ref(Coder.recordMemberVar),
                                var("aliases"),
                                var("f"),
                                var("cx"),
                                var("g"))),
                        var("fields")),
                    lambda("memberVars",
                        Eithers.bind(
                            Eithers.mapList(
                                lambda("p",
                                    apply(
                                        ref(Coder.addComment),
                                        Pairs.first(var("p")),
                                        Pairs.second(var("p")),
                                        var("cx"),
                                        var("g"))),
                                Lists.zip(var("memberVars"), var("fields"))),
                            lambda("memberVars'",
                                let("elNameStr",
                                    apply(
                                        unwrap(Identifier.TYPE_),
                                        apply(
                                            ref(Utils.nameToJavaName),
                                            var("aliases"),
                                            var("elName"))),
                                    Eithers.bind(
                                        Logic.ifElse(
                                            Equality.gt(Lists.length(var("fields")), int32(1)),
                                            Eithers.mapList(
                                                lambda("f",
                                                    Eithers.bind(
                                                        apply(
                                                            ref(Coder.recordWithMethod),
                                                            var("aliases"),
                                                            var("elName"),
                                                            var("fields"),
                                                            var("f"),
                                                            var("cx"),
                                                            var("g")),
                                                        lambda("decl",
                                                            let(
                                                                field("fname",
                                                                    apply(
                                                                        unwrap(Name.TYPE_),
                                                                        proj(FieldType.TYPE_, FieldType.NAME, "f"))),
                                                                field("comment",
                                                                    Strings.cat(
                                                                        list(
                                                                            string("Returns a copy of this {@link "),
                                                                            var("elNameStr"),
                                                                            string("} with {@code "),
                                                                            var("fname"),
                                                                            string("} replaced.")))),
                                                                right(
                                                                    apply(
                                                                        ref(Coder.withCommentString),
                                                                        var("comment"),
                                                                        var("decl"))))))),
                                                var("fields")),
                                            right(list())),
                                        lambda("withMethods",
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.recordConstructor),
                                                    var("aliases"),
                                                    var("elName"),
                                                    var("fields"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("cons",
                                                    Eithers.bind(
                                                        Eithers.mapList(
                                                            lambda("f",
                                                                let("fname",
                                                                    apply(
                                                                        unwrap(Name.TYPE_),
                                                                        proj(FieldType.TYPE_, FieldType.NAME, "f")),
                                                                    Eithers.bind(
                                                                        apply(
                                                                            var("hydra.annotations.commentsFromFieldType"),
                                                                            var("cx"),
                                                                            var("g"),
                                                                            var("f")),
                                                                        lambda("mDoc",
                                                                            right(
                                                                                Maybes.maybe(
                                                                                    string(""),
                                                                                    lambda("d",
                                                                                        Strings.cat(
                                                                                            list(
                                                                                                string("@param "),
                                                                                                var("fname"),
                                                                                                string(" "),
                                                                                                var("d")))),
                                                                                    var("mDoc"))))))),
                                                            var("fields")),
                                                        lambda("paramLines",
                                                            let(
                                                                field("nonEmptyParamLines",
                                                                    Lists.filter(
                                                                        lambda("l",
                                                                            Logic.not_(
                                                                                Equality.equal(
                                                                                    var("l"),
                                                                                    string("")))),
                                                                        var("paramLines"))),
                                                                field("consBaseComment",
                                                                    Strings.cat(
                                                                        list(
                                                                            string("Constructs an immutable {@link "),
                                                                            var("elNameStr"),
                                                                            string("}.")))),
                                                                field("consComment",
                                                                    Logic.ifElse(
                                                                        Lists.null_(
                                                                            var("nonEmptyParamLines")),
                                                                        var("consBaseComment"),
                                                                        Strings.cat(
                                                                            list(
                                                                                var("consBaseComment"),
                                                                                string("\n\n"),
                                                                                Strings.intercalate(
                                                                                    string("\n"),
                                                                                    var("nonEmptyParamLines")))))),
                                                                field("consWithComment",
                                                                    apply(
                                                                        ref(Coder.withCommentString),
                                                                        var("consComment"),
                                                                        var("cons"))),
                                                                Eithers.bind(
                                                                    Logic.ifElse(
                                                                        var("isInner"),
                                                                        right(list()),
                                                                        Eithers.bind(
                                                                            apply(
                                                                                ref(Coder.constantDeclForTypeName),
                                                                                var("aliases"),
                                                                                var("elName"),
                                                                                var("cx"),
                                                                                var("g")),
                                                                            lambda("d",
                                                                                Eithers.bind(
                                                                                    Eithers.mapList(
                                                                                        lambda("f",
                                                                                            apply(
                                                                                                ref(Coder.constantDeclForFieldType),
                                                                                                var("elName"),
                                                                                                var("aliases"),
                                                                                                var("f"),
                                                                                                var("cx"),
                                                                                                var("g"))),
                                                                                        var("fields")),
                                                                                    lambda(
                                                                                        "dfields",
                                                                                        right(
                                                                                            Lists.cons(
                                                                                                var("d"),
                                                                                                var("dfields")))))))),
                                                                    lambda("tn",
                                                                        let(
                                                                            field(
                                                                                "comparableMethods",
                                                                                Maybes.cases(
                                                                                    var("parentName"),
                                                                                    Logic.ifElse(
                                                                                        Logic.and_(
                                                                                            Logic.not_(
                                                                                                var("isInner")),
                                                                                            var("isSer")),
                                                                                        list(
                                                                                            apply(
                                                                                                ref(Coder.recordCompareToMethod),
                                                                                                var("aliases"),
                                                                                                var("tparams"),
                                                                                                var("elName"),
                                                                                                var("fields"))),
                                                                                        list()),
                                                                                    lambda("pn",
                                                                                        Logic.ifElse(
                                                                                            var("isSer"),
                                                                                            list(
                                                                                                apply(
                                                                                                    ref(Coder.variantCompareToMethod),
                                                                                                    var("aliases"),
                                                                                                    var("tparams"),
                                                                                                    var("pn"),
                                                                                                    var("elName"),
                                                                                                    var("fields"))),
                                                                                            list())))),
                                                                            field(
                                                                                "noCommentMethods",
                                                                                Lists.map(
                                                                                    lambda("x",
                                                                                        apply(
                                                                                            ref(Coder.noComment),
                                                                                            var("x"))),
                                                                                    Lists.concat2(
                                                                                        list(
                                                                                            apply(
                                                                                                ref(Coder.recordEqualsMethod),
                                                                                                var("aliases"),
                                                                                                var("elName"),
                                                                                                var("fields")),
                                                                                            apply(
                                                                                                ref(Coder.recordHashCodeMethod),
                                                                                                var("fields"))),
                                                                                        var("comparableMethods")))),
                                                                            field("bodyDecls",
                                                                                Lists.concat(
                                                                                    list(
                                                                                        var("tn"),
                                                                                        var("memberVars'"),
                                                                                        list(
                                                                                            var("consWithComment")),
                                                                                        var("noCommentMethods"),
                                                                                        var("withMethods")))),
                                                                            field("ifaces",
                                                                                Logic.ifElse(
                                                                                    var("isInner"),
                                                                                    apply(
                                                                                        ref(Coder.serializableTypes),
                                                                                        var("isSer")),
                                                                                    apply(
                                                                                        ref(Coder.interfaceTypes),
                                                                                        var("isSer"),
                                                                                        var("aliases"),
                                                                                        var("tparams"),
                                                                                        var("elName")))),
                                                                            right(
                                                                                apply(
                                                                                    ref(Utils.javaClassDeclaration),
                                                                                    var("aliases"),
                                                                                    var("tparams"),
                                                                                    var("elName"),
                                                                                    ref(Coder.classModsPublic),
                                                                                    nothing(),
                                                                                    var("ifaces"),
                                                                                    var("bodyDecls"))))))))))))))))))));

    public static final Def declarationForUnionType = def(
        "declarationForUnionType",
        () -> lambda(
                "isSer",
                "aliases",
                "tparams",
                "elName",
                "fields",
                "cx",
                "g",
                Eithers.bind(
                    Eithers.mapList(
                        lambda("ft",
                            let(
                                field("fname",
                                    proj(FieldType.TYPE_, FieldType.NAME, "ft")),
                                field("ftype",
                                    proj(FieldType.TYPE_, FieldType.TYPE, "ft")),
                                field("rfields",
                                    Logic.ifElse(
                                        apply(
                                            var("hydra.predicates.isUnitType"),
                                            apply(var("hydra.strip.deannotateType"), var("ftype"))),
                                        list(),
                                        list(
                                            record(FieldType.TYPE_,
                                                field(
                                                    FieldType.NAME,
                                                    wrap(Name.TYPE_, string("value"))),
                                                field(
                                                    FieldType.TYPE,
                                                    apply(
                                                        var("hydra.strip.deannotateType"),
                                                        var("ftype"))))))),
                                field("varName",
                                    apply(
                                        ref(Utils.variantClassName),
                                        bool(false),
                                        var("elName"),
                                        var("fname"))),
                                Eithers.bind(
                                    apply(
                                        ref(Coder.declarationForRecordType_prime),
                                        bool(true),
                                        var("isSer"),
                                        var("aliases"),
                                        list(),
                                        var("varName"),
                                        Logic.ifElse(var("isSer"), just(var("elName")), nothing()),
                                        var("rfields"),
                                        var("cx"),
                                        var("g")),
                                    lambda("innerDecl",
                                        right(
                                            apply(
                                                ref(Coder.augmentVariantClass),
                                                var("aliases"),
                                                var("tparams"),
                                                var("elName"),
                                                var("innerDecl"))))))),
                        var("fields")),
                    lambda("variantClasses",
                        let("variantDecls",
                            Lists.map(
                                lambda("vc",
                                    inject(ClassBodyDeclaration.TYPE_,
                                        ClassBodyDeclaration.CLASS_MEMBER,
                                        inject(ClassMemberDeclaration.TYPE_,
                                            ClassMemberDeclaration.CLASS,
                                            var("vc")))),
                                var("variantClasses")),
                            Eithers.bind(
                                Eithers.mapList(
                                    lambda("pair",
                                        apply(
                                            ref(Coder.addComment),
                                            Pairs.first(var("pair")),
                                            Pairs.second(var("pair")),
                                            var("cx"),
                                            var("g"))),
                                    Lists.zip(var("variantDecls"), var("fields"))),
                                lambda("variantDecls'",
                                    let(
                                        java.util.Arrays.asList(
    field("privateConst",
                                            apply(
                                                ref(Utils.makeConstructor),
                                                var("aliases"),
                                                var("elName"),
                                                bool(true),
                                                list(),
                                                list())),
    field("acceptDecl",
                                            apply(
                                                ref(Utils.toAcceptMethod),
                                                bool(true),
                                                var("tparams"))),
    field("vtparams",
                                            Lists.concat2(
                                                var("tparams"),
                                                list(
                                                    apply(
                                                        ref(Utils.javaTypeParameter),
                                                        ref(Names.visitorReturnParameter))))),
    field("elNameStr",
                                            apply(
                                                unwrap(Identifier.TYPE_),
                                                apply(
                                                    ref(Utils.nameToJavaName),
                                                    var("aliases"),
                                                    var("elName")))),
    field("visitorMethods",
                                            Lists.map(
                                                lambda("ft",
                                                    let(
                                                        java.util.Arrays.asList(
    field("fname",
                                                            proj(FieldType.TYPE_, FieldType.NAME, "ft")),
    field("fnameStr",
                                                            apply(
                                                                unwrap(Name.TYPE_),
                                                                var("fname"))),
    field("typeArgs",
                                                            Lists.map(
                                                                lambda("tp",
                                                                    apply(
                                                                        ref(Utils.typeParameterToTypeArgument),
                                                                        var("tp"))),
                                                                var("tparams"))),
    field("varName",
                                                            apply(
                                                                ref(Utils.variantClassName),
                                                                bool(false),
                                                                var("elName"),
                                                                var("fname"))),
    field("varNameStr",
                                                            apply(
                                                                unwrap(Identifier.TYPE_),
                                                                apply(
                                                                    ref(Utils.nameToJavaName),
                                                                    var("aliases"),
                                                                    var("varName")))),
    field("varRef",
                                                            apply(
                                                                ref(Utils.javaClassTypeToJavaType),
                                                                apply(
                                                                    ref(Utils.nameToJavaClassType),
                                                                    var("aliases"),
                                                                    bool(false),
                                                                    var("typeArgs"),
                                                                    var("varName"),
                                                                    nothing()))),
    field("param",
                                                            apply(
                                                                ref(Utils.javaTypeToJavaFormalParameter),
                                                                var("varRef"),
                                                                wrap(Name.TYPE_,
                                                                    string("instance")))),
    field("resultR",
                                                            apply(
                                                                ref(Utils.javaTypeToJavaResult),
                                                                inject(hydra.java.syntax.Type.TYPE_,
                                                                    hydra.java.syntax.Type.REFERENCE,
                                                                    ref(Utils.visitorTypeVariable)))),
    field("comment",
                                                            Strings.cat(
                                                                list(
                                                                    string("Visit the {@link "),
                                                                    var("varNameStr"),
                                                                    string("} case."))))),
                                                        pair(
                                                            var("comment"),
                                                            apply(
                                                                ref(Utils.interfaceMethodDeclaration),
                                                                list(),
                                                                list(),
                                                                ref(Names.visitMethodName),
                                                                list(var("param")),
                                                                var("resultR"),
                                                                nothing())))),
                                                var("fields"))),
    field("visitorBody",
                                            wrap(InterfaceBody.TYPE_,
                                                Lists.map(
                                                    lambda("p",
                                                        apply(
                                                            ref(Coder.withInterfaceCommentString),
                                                            Pairs.first(var("p")),
                                                            Pairs.second(var("p")))),
                                                    var("visitorMethods")))),
    field("visitor",
                                            apply(
                                                ref(Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration),
                                                record(
                                                    NormalInterfaceDeclaration.TYPE_,
                                                    field(
                                                        NormalInterfaceDeclaration.MODIFIERS,
                                                        list(
                                                            inject(
                                                                InterfaceModifier.TYPE_,
                                                                InterfaceModifier.PUBLIC,
                                                                unit()))),
                                                    field(
                                                        NormalInterfaceDeclaration.IDENTIFIER,
                                                        wrap(TypeIdentifier.TYPE_,
                                                            wrap(Identifier.TYPE_,
                                                                ref(Names.visitorName)))),
                                                    field(
                                                        NormalInterfaceDeclaration.PARAMETERS,
                                                        var("vtparams")),
                                                    field(
                                                        NormalInterfaceDeclaration.EXTENDS,
                                                        list()),
                                                    field(
                                                        NormalInterfaceDeclaration.PERMITS,
                                                        list()),
                                                    field(
                                                        NormalInterfaceDeclaration.BODY,
                                                        var("visitorBody"))))),
    field("typeArgs",
                                            Lists.map(
                                                lambda("tp",
                                                    apply(
                                                        ref(Utils.typeParameterToTypeArgument),
                                                        var("tp"))),
                                                var("tparams"))),
    field("visitorClassType",
                                            apply(
                                                ref(Utils.javaClassType),
                                                Lists.concat2(
                                                    Lists.map(
                                                        lambda("tp",
                                                            apply(
                                                                ref(Utils.typeParameterToReferenceType),
                                                                var("tp"))),
                                                        var("tparams")),
                                                    list(
                                                        ref(Utils.visitorTypeVariable))),
                                                nothing(),
                                                ref(Names.visitorName))),
    field("mainInstanceParam",
                                            apply(
                                                ref(Utils.javaTypeToJavaFormalParameter),
                                                apply(
                                                    ref(Utils.javaClassTypeToJavaType),
                                                    apply(
                                                        ref(Utils.nameToJavaClassType),
                                                        var("aliases"),
                                                        bool(false),
                                                        var("typeArgs"),
                                                        var("elName"),
                                                        nothing())),
                                                wrap(Name.TYPE_, string("instance")))),
    field("resultR",
                                            apply(
                                                ref(Utils.javaTypeToJavaResult),
                                                inject(hydra.java.syntax.Type.TYPE_,
                                                    hydra.java.syntax.Type.REFERENCE,
                                                    ref(Utils.visitorTypeVariable)))),
    field("throwStmt",
                                            inject(BlockStatement.TYPE_,
                                                BlockStatement.STATEMENT,
                                                apply(
                                                    ref(Utils.javaThrowIllegalStateException),
                                                    list(
                                                        apply(
                                                            ref(Utils.javaAdditiveExpressionToJavaExpression),
                                                            apply(
                                                                ref(Utils.addExpressions),
                                                                list(
                                                                    apply(
                                                                        ref(Utils.javaStringMultiplicativeExpression),
                                                                        string("Non-exhaustive patterns when matching: ")),
                                                                    inject(
                                                                        MultiplicativeExpression.TYPE_,
                                                                        MultiplicativeExpression.UNARY,
                                                                        apply(
                                                                            ref(Utils.javaIdentifierToJavaUnaryExpression),
                                                                            wrap(
                                                                                Identifier.TYPE_,
                                                                                string("instance"))))))))))),
    field("defaultMod",
                                            list(
                                                inject(
                                                    InterfaceMethodModifier.TYPE_,
                                                    InterfaceMethodModifier.DEFAULT,
                                                    unit()))),
    field("otherwiseDecl",
                                            apply(
                                                ref(Utils.interfaceMethodDeclaration),
                                                var("defaultMod"),
                                                list(),
                                                ref(Names.otherwiseMethodName),
                                                list(var("mainInstanceParam")),
                                                var("resultR"),
                                                just(list(var("throwStmt"))))),
    field("otherwiseComment",
                                            string("Default branch for unhandled cases.")),
    field("pvVisitMethods",
                                            Lists.map(
                                                lambda("ft",
                                                    let(
                                                        java.util.Arrays.asList(
    field("fname",
                                                            proj(FieldType.TYPE_, FieldType.NAME, "ft")),
    field("varName",
                                                            apply(
                                                                ref(Utils.variantClassName),
                                                                bool(false),
                                                                var("elName"),
                                                                var("fname"))),
    field("varNameStr",
                                                            apply(
                                                                unwrap(Identifier.TYPE_),
                                                                apply(
                                                                    ref(Utils.nameToJavaName),
                                                                    var("aliases"),
                                                                    var("varName")))),
    field("varRef",
                                                            apply(
                                                                ref(Utils.javaClassTypeToJavaType),
                                                                apply(
                                                                    ref(Utils.nameToJavaClassType),
                                                                    var("aliases"),
                                                                    bool(false),
                                                                    var("typeArgs"),
                                                                    var("varName"),
                                                                    nothing()))),
    field("param",
                                                            apply(
                                                                ref(Utils.javaTypeToJavaFormalParameter),
                                                                var("varRef"),
                                                                wrap(Name.TYPE_,
                                                                    string("instance")))),
    field("mi",
                                                            apply(
                                                                ref(Utils.methodInvocation),
                                                                nothing(),
                                                                wrap(
                                                                    Identifier.TYPE_,
                                                                    ref(Names.otherwiseMethodName)),
                                                                list(
                                                                    apply(
                                                                        ref(Utils.javaIdentifierToJavaExpression),
                                                                        wrap(
                                                                            Identifier.TYPE_,
                                                                            string("instance")))))),
    field("returnOtherwise",
                                                            inject(
                                                                BlockStatement.TYPE_,
                                                                BlockStatement.STATEMENT,
                                                                apply(
                                                                    ref(Utils.javaReturnStatement),
                                                                    just(
                                                                        apply(
                                                                            ref(Utils.javaPrimaryToJavaExpression),
                                                                            apply(
                                                                                ref(Utils.javaMethodInvocationToJavaPrimary),
                                                                                var("mi"))))))),
    field("comment",
                                                            Strings.cat(
                                                                list(
                                                                    string("Visit the {@link "),
                                                                    var("varNameStr"),
                                                                    string("} case."))))),
                                                        pair(
                                                            var("comment"),
                                                            apply(
                                                                ref(Utils.interfaceMethodDeclaration),
                                                                var("defaultMod"),
                                                                list(),
                                                                ref(Names.visitMethodName),
                                                                list(var("param")),
                                                                var("resultR"),
                                                                just(list(var("returnOtherwise"))))))),
                                                var("fields"))),
    field("pvBody",
                                            wrap(InterfaceBody.TYPE_,
                                                Lists.cons(
                                                    apply(
                                                        ref(Coder.withInterfaceCommentString),
                                                        var("otherwiseComment"),
                                                        var("otherwiseDecl")),
                                                    Lists.map(
                                                        lambda("p",
                                                            apply(
                                                                ref(Coder.withInterfaceCommentString),
                                                                Pairs.first(var("p")),
                                                                Pairs.second(var("p")))),
                                                        var("pvVisitMethods"))))),
    field("partialVisitor",
                                            apply(
                                                ref(Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration),
                                                record(
                                                    NormalInterfaceDeclaration.TYPE_,
                                                    field(
                                                        NormalInterfaceDeclaration.MODIFIERS,
                                                        list(
                                                            inject(
                                                                InterfaceModifier.TYPE_,
                                                                InterfaceModifier.PUBLIC,
                                                                unit()))),
                                                    field(
                                                        NormalInterfaceDeclaration.IDENTIFIER,
                                                        wrap(TypeIdentifier.TYPE_,
                                                            wrap(Identifier.TYPE_,
                                                                ref(Names.partialVisitorName)))),
                                                    field(
                                                        NormalInterfaceDeclaration.PARAMETERS,
                                                        var("vtparams")),
                                                    field(
                                                        NormalInterfaceDeclaration.EXTENDS,
                                                        list(
                                                            wrap(
                                                                InterfaceType.TYPE_,
                                                                var("visitorClassType")))),
                                                    field(
                                                        NormalInterfaceDeclaration.PERMITS,
                                                        list()),
                                                    field(
                                                        NormalInterfaceDeclaration.BODY,
                                                        var("pvBody")))))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.constantDeclForTypeName),
                                                var("aliases"),
                                                var("elName"),
                                                var("cx"),
                                                var("g")),
                                            lambda("tn0",
                                                Eithers.bind(
                                                    Eithers.mapList(
                                                        lambda("ft",
                                                            apply(
                                                                ref(Coder.constantDeclForFieldType),
                                                                var("elName"),
                                                                var("aliases"),
                                                                var("ft"),
                                                                var("cx"),
                                                                var("g"))),
                                                        var("fields")),
                                                    lambda("tn1",
                                                        let(
                                                            java.util.Arrays.asList(
    field("tn",
                                                                Lists.concat2(
                                                                    list(var("tn0")),
                                                                    var("tn1"))),
    field("privateConstComment",
                                                                Strings.cat(
                                                                    list(
                                                                        string("Constructs an immutable {@link "),
                                                                        var("elNameStr"),
                                                                        string("}.")))),
    field("acceptComment",
                                                                string("Dispatch to {@code visitor}.")),
    field("visitorIfaceComment",
                                                                Strings.cat(
                                                                    list(
                                                                        string("Visitor over {@link "),
                                                                        var("elNameStr"),
                                                                        string("}.")))),
    field("partialVisitorIfaceComment",
                                                                Strings.cat(
                                                                    list(
                                                                        string("Partial visitor over {@link "),
                                                                        var("elNameStr"),
                                                                        string("} with a default {@link #otherwise} branch.")))),
    field("otherDecls",
                                                                list(
                                                                    apply(
                                                                        ref(Coder.withCommentString),
                                                                        var("privateConstComment"),
                                                                        var("privateConst")),
                                                                    apply(
                                                                        ref(Coder.withCommentString),
                                                                        var("acceptComment"),
                                                                        var("acceptDecl")),
                                                                    apply(
                                                                        ref(Coder.withCommentString),
                                                                        var("visitorIfaceComment"),
                                                                        var("visitor")),
                                                                    apply(
                                                                        ref(Coder.withCommentString),
                                                                        var("partialVisitorIfaceComment"),
                                                                        var("partialVisitor")))),
    field("bodyDecls",
                                                                Lists.concat(
                                                                    list(
                                                                        var("tn"),
                                                                        var("otherDecls"),
                                                                        var("variantDecls'")))),
    field("mods",
                                                                Lists.concat2(
                                                                    ref(Coder.classModsPublic),
                                                                    list(
                                                                        inject(
                                                                            ClassModifier.TYPE_,
                                                                            ClassModifier.ABSTRACT,
                                                                            unit()))))),
                                                            right(
                                                                apply(
                                                                    ref(Utils.javaClassDeclaration),
                                                                    var("aliases"),
                                                                    var("tparams"),
                                                                    var("elName"),
                                                                    var("mods"),
                                                                    nothing(),
                                                                    apply(
                                                                        ref(Coder.interfaceTypes),
                                                                        var("isSer"),
                                                                        var("aliases"),
                                                                        var("tparams"),
                                                                        var("elName")),
                                                                    var("bodyDecls"))))))))))))))));

    public static final Def decodeTypeFromTerm = def(
        "decodeTypeFromTerm",
        () -> lambda("term",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("term")),
                    nothing(),
                    field(
                        Term.INJECT,
                        lambda("inj",
                            Logic.ifElse(
                                Equality.equal(
                                    proj(Injection.TYPE_, Injection.TYPE_NAME, "inj"),
                                    wrap(Name.TYPE_, string("hydra.core.Type"))),
                                let(
                                    field("fname",
                                        apply(
                                            unwrap(Name.TYPE_),
                                            apply(
                                                project(Field.TYPE_, Field.NAME),
                                                proj(Injection.TYPE_, Injection.FIELD, "inj")))),
                                    field("fterm",
                                        apply(
                                            project(Field.TYPE_, Field.TERM),
                                            proj(Injection.TYPE_, Injection.FIELD, "inj"))),
                                    Logic.ifElse(
                                        Equality.equal(var("fname"), string("variable")),
                                        casesWithDefault(Term.TYPE_,
                                            var("fterm"),
                                            nothing(),
                                            field(
                                                Term.WRAP,
                                                lambda("wt",
                                                    casesWithDefault(Term.TYPE_,
                                                        proj(WrappedTerm.TYPE_, WrappedTerm.BODY, "wt"),
                                                        nothing(),
                                                        field(
                                                            Term.LITERAL,
                                                            lambda("lit",
                                                                casesWithDefault(
                                                                    Literal.TYPE_,
                                                                    var("lit"),
                                                                    nothing(),
                                                                    field(
                                                                        Literal.STRING,
                                                                        lambda("s",
                                                                            just(
                                                                                inject(
                                                                                    Type.TYPE_,
                                                                                    Type.VARIABLE,
                                                                                    wrap(
                                                                                        Name.TYPE_,
                                                                                        var("s"))))))))))))),
                                        Logic.ifElse(
                                            Equality.equal(var("fname"), string("annotated")),
                                            casesWithDefault(Term.TYPE_,
                                                var("fterm"),
                                                nothing(),
                                                field(
                                                    Term.RECORD,
                                                    lambda("rec",
                                                        Maybes.bind(
                                                            Lists.maybeHead(
                                                                Lists.filter(
                                                                    lambda("f",
                                                                        Equality.equal(
                                                                            proj(Field.TYPE_, Field.NAME, "f"),
                                                                            wrap(
                                                                                Name.TYPE_,
                                                                                string("body")))),
                                                                    proj(Record.TYPE_, Record.FIELDS, "rec"))),
                                                            lambda("bodyField",
                                                                apply(
                                                                    ref(Coder.decodeTypeFromTerm),
                                                                    proj(Field.TYPE_, Field.TERM, "bodyField"))))))),
                                            Logic.ifElse(
                                                Equality.equal(var("fname"), string("application")),
                                                casesWithDefault(Term.TYPE_,
                                                    var("fterm"),
                                                    nothing(),
                                                    field(
                                                        Term.RECORD,
                                                        lambda("rec",
                                                            Maybes.bind(
                                                                Lists.maybeHead(
                                                                    Lists.filter(
                                                                        lambda("f",
                                                                            Equality.equal(
                                                                                proj(Field.TYPE_, Field.NAME, "f"),
                                                                                wrap(
                                                                                    Name.TYPE_,
                                                                                    string("function")))),
                                                                        proj(Record.TYPE_, Record.FIELDS, "rec"))),
                                                                lambda("funcField",
                                                                    Maybes.bind(
                                                                        apply(
                                                                            ref(Coder.decodeTypeFromTerm),
                                                                            proj(Field.TYPE_, Field.TERM, "funcField")),
                                                                        lambda("func",
                                                                            Maybes.bind(
                                                                                Lists.maybeHead(
                                                                                    Lists.filter(
                                                                                        lambda("f",
                                                                                            Equality.equal(
                                                                                                proj(Field.TYPE_, Field.NAME, "f"),
                                                                                                wrap(
                                                                                                    Name.TYPE_,
                                                                                                    string("argument")))),
                                                                                        proj(Record.TYPE_, Record.FIELDS, "rec"))),
                                                                                lambda("argField",
                                                                                    Maybes.map(
                                                                                        lambda(
                                                                                            "arg",
                                                                                            inject(
                                                                                                Type.TYPE_,
                                                                                                Type.APPLICATION,
                                                                                                record(
                                                                                                    ApplicationType.TYPE_,
                                                                                                    field(
                                                                                                        ApplicationType.FUNCTION,
                                                                                                        var("func")),
                                                                                                    field(
                                                                                                        ApplicationType.ARGUMENT,
                                                                                                        var("arg"))))),
                                                                                        apply(
                                                                                            ref(Coder.decodeTypeFromTerm),
                                                                                            proj(Field.TYPE_, Field.TERM, "argField")))))))))))),
                                                Logic.ifElse(
                                                    Equality.equal(var("fname"), string("function")),
                                                    casesWithDefault(Term.TYPE_,
                                                        var("fterm"),
                                                        nothing(),
                                                        field(
                                                            Term.RECORD,
                                                            lambda("rec",
                                                                Maybes.bind(
                                                                    Lists.maybeHead(
                                                                        Lists.filter(
                                                                            lambda("f",
                                                                                Equality.equal(
                                                                                    proj(Field.TYPE_, Field.NAME, "f"),
                                                                                    wrap(
                                                                                        Name.TYPE_,
                                                                                        string("domain")))),
                                                                            proj(Record.TYPE_, Record.FIELDS, "rec"))),
                                                                    lambda("domField",
                                                                        Maybes.bind(
                                                                            apply(
                                                                                ref(Coder.decodeTypeFromTerm),
                                                                                proj(Field.TYPE_, Field.TERM, "domField")),
                                                                            lambda("dom",
                                                                                Maybes.bind(
                                                                                    Lists.maybeHead(
                                                                                        Lists.filter(
                                                                                            lambda(
                                                                                                "f",
                                                                                                Equality.equal(
                                                                                                    proj(Field.TYPE_, Field.NAME, "f"),
                                                                                                    wrap(
                                                                                                        Name.TYPE_,
                                                                                                        string("codomain")))),
                                                                                            proj(Record.TYPE_, Record.FIELDS, "rec"))),
                                                                                    lambda(
                                                                                        "codField",
                                                                                        Maybes.map(
                                                                                            lambda(
                                                                                                "cod",
                                                                                                inject(
                                                                                                    Type.TYPE_,
                                                                                                    Type.FUNCTION,
                                                                                                    record(
                                                                                                        FunctionType.TYPE_,
                                                                                                        field(
                                                                                                            FunctionType.DOMAIN,
                                                                                                            var("dom")),
                                                                                                        field(
                                                                                                            FunctionType.CODOMAIN,
                                                                                                            var("cod"))))),
                                                                                            apply(
                                                                                                ref(Coder.decodeTypeFromTerm),
                                                                                                proj(Field.TYPE_, Field.TERM, "codField")))))))))))),
                                                    Logic.ifElse(
                                                        Equality.equal(
                                                            var("fname"),
                                                            string("literal")),
                                                        casesWithDefault(Term.TYPE_,
                                                            var("fterm"),
                                                            nothing(),
                                                            field(
                                                                Term.INJECT,
                                                                lambda("litInj",
                                                                    Logic.ifElse(
                                                                        Equality.equal(
                                                                            apply(
                                                                                unwrap(Name.TYPE_),
                                                                                apply(
                                                                                    project(Field.TYPE_, Field.NAME),
                                                                                    proj(Injection.TYPE_, Injection.FIELD, "litInj"))),
                                                                            string("string")),
                                                                        just(
                                                                            inject(
                                                                                Type.TYPE_,
                                                                                Type.LITERAL,
                                                                                inject(
                                                                                    LiteralType.TYPE_,
                                                                                    LiteralType.STRING,
                                                                                    unit()))),
                                                                        nothing())))),
                                                        nothing())))))),
                                nothing()))))));

    public static final Def dedupBindings = def(
        "dedupBindings",
        () -> lambda(
                "inScope",
                "bs",
                Maybes.fromMaybe(
                    list(),
                    Maybes.map(
                        lambda("p",
                            let(
                                field("b",
                                    Pairs.first(var("p"))),
                                field("rest",
                                    Pairs.second(var("p"))),
                                field("name",
                                    proj(Binding.TYPE_, Binding.NAME, "b")),
                                Logic.ifElse(
                                    Sets.member(var("name"), var("inScope")),
                                    let(
                                        field("newName",
                                            apply(
                                                ref(Coder.freshJavaName),
                                                var("name"),
                                                var("inScope"))),
                                        field("subst",
                                            Maps.singleton(var("name"), var("newName"))),
                                        field("rest2",
                                            Lists.map(
                                                lambda("b2",
                                                    record(Binding.TYPE_,
                                                        field(
                                                            Binding.NAME,
                                                            proj(Binding.TYPE_, Binding.NAME, "b2")),
                                                        field(
                                                            Binding.TERM,
                                                            apply(
                                                                var("hydra.variables.substituteVariables"),
                                                                var("subst"),
                                                                proj(Binding.TYPE_, Binding.TERM, "b2"))),
                                                        field(
                                                            Binding.TYPE_SCHEME,
                                                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b2")))),
                                                var("rest"))),
                                        Lists.cons(
                                            record(Binding.TYPE_,
                                                field(Binding.NAME, var("newName")),
                                                field(
                                                    Binding.TERM,
                                                    proj(Binding.TYPE_, Binding.TERM, "b")),
                                                field(
                                                    Binding.TYPE_SCHEME,
                                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b"))),
                                            apply(
                                                ref(Coder.dedupBindings),
                                                Sets.insert(var("newName"), var("inScope")),
                                                var("rest2")))),
                                    Lists.cons(
                                        var("b"),
                                        apply(
                                            ref(Coder.dedupBindings),
                                            Sets.insert(var("name"), var("inScope")),
                                            var("rest")))))),
                        Lists.uncons(var("bs"))))));

    public static final Def detectAccumulatorUnification = def(
        "detectAccumulatorUnification",
        () -> lambda(
                "doms",
                "cod",
                "tparams",
                let(
                    java.util.Arrays.asList(
    field("tparamSet",
                        Sets.fromList(var("tparams"))),
    field("allPairs",
                        Lists.bind(
                            var("doms"),
                            lambda("d", apply(ref(Coder.extractInOutPair), var("d"))))),
    field("groupedByInput",
                        apply(ref(Coder.groupPairsByFirst), var("allPairs"))),
    field("selfRefSubst",
                        apply(ref(Coder.selfRefSubstitution), var("groupedByInput"))),
    field("directPairs",
                        Lists.bind(
                            var("doms"),
                            lambda("d",
                                apply(ref(Coder.extractDirectReturn), var("tparamSet"), var("d"))))),
    field("groupedDirect",
                        apply(ref(Coder.groupPairsByFirst), var("directPairs"))),
    field("directInputVars",
                        Sets.fromList(
                            Lists.map(lambda("p", Pairs.first(var("p"))), var("directPairs")))),
    field("codVar",
                        casesWithDefault(Type.TYPE_,
                            apply(var("hydra.strip.deannotateType"), var("cod")),
                            nothing(),
                            field(Type.VARIABLE, lambda("v", just(var("v")))))),
    field("directRefSubst",
                        apply(
                            ref(Coder.directRefSubstitution),
                            var("directInputVars"),
                            var("codVar"),
                            var("groupedDirect"))),
    field("codSubst",
                        Maybes.maybe(
                            var("hydra.lib.maps.empty"),
                            lambda("cv",
                                Logic.ifElse(
                                    Maps.member(var("cv"), var("selfRefSubst")),
                                    var("hydra.lib.maps.empty"),
                                    Maybes.maybe(
                                        var("hydra.lib.maps.empty"),
                                        lambda("refVar",
                                            Logic.ifElse(
                                                Equality.equal(var("cv"), var("refVar")),
                                                var("hydra.lib.maps.empty"),
                                                Maps.singleton(var("cv"), var("refVar")))),
                                        apply(ref(Coder.findSelfRefVar), var("groupedByInput"))))),
                            apply(ref(Coder.findPairFirst), var("cod")))),
    field("domVars",
                        Sets.fromList(
                            Lists.bind(
                                var("doms"),
                                lambda("d",
                                    Sets.toList(apply(ref(Coder.collectTypeVars), var("d"))))))),
    field("danglingSubst",
                        Maybes.maybe(
                            var("hydra.lib.maps.empty"),
                            lambda("cv",
                                Logic.ifElse(
                                    Sets.member(var("cv"), var("domVars")),
                                    var("hydra.lib.maps.empty"),
                                    Maybes.maybe(
                                        var("hydra.lib.maps.empty"),
                                        lambda("refVar",
                                            Maps.singleton(
                                                var("cv"),
                                                inject(Type.TYPE_,
                                                    Type.VARIABLE,
                                                    var("refVar")))),
                                        apply(ref(Coder.findSelfRefVar), var("groupedByInput"))))),
                            apply(ref(Coder.findPairFirst), var("cod"))))),
                    Maps.union(
                        Maps.union(
                            Maps.union(
                                apply(ref(Coder.nameMapToTypeMap), var("selfRefSubst")),
                                apply(ref(Coder.nameMapToTypeMap), var("codSubst"))),
                            var("danglingSubst")),
                        apply(ref(Coder.nameMapToTypeMap), var("directRefSubst"))))));

    public static final Def directRefSubstitution = def(
        "directRefSubstitution",
        () -> lambda(
                "directInputVars",
                "codVar",
                "grouped",
                Lists.foldl(
                    lambda(
                        "subst",
                        "entry",
                        apply(
                            ref(Coder.directRefSubstitution_processGroup),
                            var("directInputVars"),
                            var("codVar"),
                            var("subst"),
                            Pairs.first(var("entry")),
                            Pairs.second(var("entry")))),
                    var("hydra.lib.maps.empty"),
                    Maps.toList(var("grouped")))));

    public static final Def directRefSubstitution_processGroup = def(
        "directRefSubstitution_processGroup",
        () -> lambda(
                "directInputVars",
                "codVar",
                "subst",
                "inVar",
                "outVars",
                let(
                    field("selfRefCount",
                        Lists.length(
                            Lists.filter(
                                lambda("v", Equality.equal(var("v"), var("inVar"))),
                                var("outVars")))),
                    field("nonSelfVars",
                        Lists.filter(
                            lambda("v", Logic.not_(Equality.equal(var("v"), var("inVar")))),
                            var("outVars"))),
                    field("safeNonSelfVars",
                        Lists.filter(
                            lambda("v",
                                Logic.and_(
                                    Logic.not_(Sets.member(var("v"), var("directInputVars"))),
                                    Logic.not_(Equality.equal(just(var("v")), var("codVar"))))),
                            var("nonSelfVars"))),
                    Logic.ifElse(
                        Logic.and_(
                            Equality.gte(var("selfRefCount"), int32(2)),
                            Logic.not_(Lists.null_(var("safeNonSelfVars")))),
                        Lists.foldl(
                            lambda("s", lambda("v", Maps.insert(var("v"), var("inVar"), var("s")))),
                            var("subst"),
                            var("safeNonSelfVars")),
                        var("subst")))));

    public static final Def domTypeArgs = def(
        "domTypeArgs",
        () -> lambda(
                "aliases",
                "d",
                "cx",
                "g",
                let("args",
                    apply(
                        ref(Coder.extractTypeApplicationArgs),
                        apply(var("hydra.strip.deannotateType"), var("d"))),
                    Logic.ifElse(
                        Logic.not_(Lists.null_(var("args"))),
                        Eithers.mapList(
                            lambda("t",
                                Eithers.bind(
                                    apply(
                                        ref(Coder.encodeType),
                                        var("aliases"),
                                        var("hydra.lib.sets.empty"),
                                        var("t"),
                                        var("cx"),
                                        var("g")),
                                    lambda("jt",
                                        Eithers.bind(
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt"),
                                                var("cx")),
                                            lambda("rt",
                                                right(
                                                    inject(TypeArgument.TYPE_,
                                                        TypeArgument.REFERENCE,
                                                        var("rt")))))))),
                            var("args")),
                        right(apply(ref(Coder.javaTypeArgumentsForType), var("d")))))));

    public static final Def elementJavaIdentifier = def(
        "elementJavaIdentifier",
        () -> lambda(
                "isPrim",
                "isMethod",
                "aliases",
                "name",
                let(
                    field("qn",
                        apply(var("hydra.names.qualifyName"), var("name"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("sep",
                        Logic.ifElse(var("isMethod"), string("::"), string("."))),
                    Logic.ifElse(
                        var("isPrim"),
                        wrap(Identifier.TYPE_,
                            Strings.cat2(
                                Strings.cat2(
                                    apply(
                                        ref(Coder.elementJavaIdentifier_qualify),
                                        var("aliases"),
                                        var("ns_"),
                                        apply(var("hydra.formatting.capitalize"), var("local"))),
                                    string(".")),
                                ref(Names.applyMethodName))),
                        Maybes.cases(
                            var("ns_"),
                            wrap(Identifier.TYPE_,
                                apply(ref(Utils.sanitizeJavaName), var("local"))),
                            lambda("n",
                                wrap(Identifier.TYPE_,
                                    Strings.cat2(
                                        Strings.cat2(
                                            apply(
                                                ref(Coder.elementJavaIdentifier_qualify),
                                                var("aliases"),
                                                apply(ref(Coder.namespaceParent), var("n")),
                                                apply(ref(Coder.elementsClassName), var("n"))),
                                            var("sep")),
                                        apply(
                                            ref(Utils.sanitizeJavaName),
                                            var("local"))))))))));

    public static final Def elementJavaIdentifier_qualify = def(
        "elementJavaIdentifier_qualify",
        () -> lambda(
                "aliases",
                "mns",
                "s",
                apply(
                    unwrap(Identifier.TYPE_),
                    apply(
                        ref(Utils.nameToJavaName),
                        var("aliases"),
                        apply(
                            var("hydra.names.unqualifyName"),
                            record(QualifiedName.TYPE_,
                                field(QualifiedName.MODULE_NAME, var("mns")),
                                field(QualifiedName.LOCAL, var("s"))))))));

    public static final Def elementsClassName = def(
        "elementsClassName",
        () -> lambda("ns",
                let(
                    field("nsStr",
                        apply(unwrap(ModuleName.TYPE_), var("ns"))),
                    field("parts",
                        Strings.splitOn(string("."), var("nsStr"))),
                    apply(
                        var("hydra.formatting.sanitizeWithUnderscores"),
                        var("hydra.java.language.reservedWords"),
                        apply(
                            var("hydra.formatting.capitalize"),
                            Maybes.fromMaybe(var("nsStr"), Lists.maybeLast(var("parts"))))))));

    public static final Def elementsQualifiedName = def(
        "elementsQualifiedName",
        () -> lambda("ns",
                apply(
                    var("hydra.names.unqualifyName"),
                    record(QualifiedName.TYPE_,
                        field(
                            QualifiedName.MODULE_NAME,
                            apply(ref(Coder.namespaceParent), var("ns"))),
                        field(
                            QualifiedName.LOCAL,
                            apply(ref(Coder.elementsClassName), var("ns")))))));

    public static final Def encodeApplication = def(
        "encodeApplication",
        () -> lambda(
                "env",
                "app",
                "cx",
                "g0",
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("g",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")),
                    field("gathered",
                        apply(
                            var("hydra.analysis.gatherArgsWithTypeApps"),
                            inject(Term.TYPE_, Term.APPLICATION, var("app")),
                            list(),
                            list())),
                    field("fun",
                        Pairs.first(var("gathered"))),
                    field("args",
                        Pairs.first(Pairs.second(var("gathered")))),
                    field("typeApps",
                        Pairs.second(Pairs.second(var("gathered")))),
                    Eithers.bind(
                        Eithers.bimap(
                            lambda("__de",
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                            lambda("__a", var("__a")),
                            apply(
                                var("hydra.annotations.getType"),
                                var("g"),
                                apply(var("hydra.annotations.termAnnotationInternal"), var("fun")))),
                        lambda("mfunTyp",
                            Eithers.bind(
                                Maybes.cases(
                                    var("mfunTyp"),
                                    apply(
                                        var("hydra.checking.typeOfTerm"),
                                        var("cx"),
                                        var("g"),
                                        var("fun")),
                                    lambda("t", right(var("t")))),
                                lambda("funTyp",
                                    let(
                                        field("arity",
                                            apply(var("hydra.arity.typeArity"), var("funTyp"))),
                                        field("deannotatedFun",
                                            apply(var("hydra.strip.deannotateTerm"), var("fun"))),
                                        field("calleeName",
                                            casesWithDefault(Term.TYPE_,
                                                var("deannotatedFun"),
                                                nothing(),
                                                field(
                                                    Term.VARIABLE,
                                                    lambda("n", just(var("n")))))),
                                        Eithers.bind(
                                            Maybes.cases(
                                                var("calleeName"),
                                                right(var("args")),
                                                lambda("cname",
                                                    apply(
                                                        ref(Coder.annotateLambdaArgs),
                                                        var("cname"),
                                                        var("typeApps"),
                                                        var("args"),
                                                        var("cx"),
                                                        var("g")))),
                                            lambda("annotatedArgs",
                                                casesWithDefault(Term.TYPE_,
                                                    var("deannotatedFun"),
                                                    apply(
                                                        ref(Coder.encodeApplication_fallback),
                                                        var("env"),
                                                        var("aliases"),
                                                        var("g"),
                                                        var("typeApps"),
                                                        proj(Application.TYPE_, Application.FUNCTION, "app"),
                                                        proj(Application.TYPE_, Application.ARGUMENT, "app"),
                                                        var("cx"),
                                                        var("g")),
                                                    field(
                                                        Term.VARIABLE,
                                                        lambda("name",
                                                            Logic.ifElse(
                                                                Maybes.isJust(
                                                                    Maps.lookup(
                                                                        var("name"),
                                                                        proj(Graph.TYPE_, Graph.PRIMITIVES, "g"))),
                                                                let(
                                                                    field("hargs",
                                                                        Lists.take(
                                                                            var("arity"),
                                                                            var("annotatedArgs"))),
                                                                    field("rargs",
                                                                        Lists.drop(
                                                                            var("arity"),
                                                                            var("annotatedArgs"))),
                                                                    Eithers.bind(
                                                                        apply(
                                                                            ref(Coder.functionCall),
                                                                            var("env"),
                                                                            bool(true),
                                                                            var("name"),
                                                                            var("hargs"),
                                                                            list(),
                                                                            var("cx"),
                                                                            var("g")),
                                                                        lambda("initialCall",
                                                                            Eithers.foldl(
                                                                                lambda(
                                                                                    "acc",
                                                                                    "h",
                                                                                    Eithers.bind(
                                                                                        apply(
                                                                                            ref(Coder.encodeTerm),
                                                                                            var("env"),
                                                                                            var("h"),
                                                                                            var("cx"),
                                                                                            var("g")),
                                                                                        lambda(
                                                                                            "jarg",
                                                                                            right(
                                                                                                apply(
                                                                                                    ref(Coder.applyJavaArg),
                                                                                                    var("acc"),
                                                                                                    var("jarg")))))),
                                                                                var("initialCall"),
                                                                                var("rargs"))))),
                                                                Logic.ifElse(
                                                                    Logic.and_(
                                                                        apply(
                                                                            ref(Coder.isRecursiveVariable),
                                                                            var("aliases"),
                                                                            var("name")),
                                                                        Logic.not_(
                                                                            apply(
                                                                                ref(Coder.isLambdaBoundIn),
                                                                                var("name"),
                                                                                proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases")))),
                                                                    apply(
                                                                        ref(Coder.encodeApplication_fallback),
                                                                        var("env"),
                                                                        var("aliases"),
                                                                        var("g"),
                                                                        var("typeApps"),
                                                                        proj(Application.TYPE_, Application.FUNCTION, "app"),
                                                                        proj(Application.TYPE_, Application.ARGUMENT, "app"),
                                                                        var("cx"),
                                                                        var("g")),
                                                                    Eithers.bind(
                                                                        apply(
                                                                            ref(Coder.classifyDataReference),
                                                                            var("name"),
                                                                            var("cx"),
                                                                            var("g")),
                                                                        lambda("symClass",
                                                                            let(
                                                                                field("methodArity",
                                                                                    casesWithDefault(
                                                                                        JavaSymbolClass.TYPE_,
                                                                                        var("symClass"),
                                                                                        var("arity"),
                                                                                        field(
                                                                                            JavaSymbolClass.HOISTED_LAMBDA,
                                                                                            lambda(
                                                                                                "n",
                                                                                                var("n"))))),
                                                                                field("hargs",
                                                                                    Lists.take(
                                                                                        var("methodArity"),
                                                                                        var("annotatedArgs"))),
                                                                                field("rargs",
                                                                                    Lists.drop(
                                                                                        var("methodArity"),
                                                                                        var("annotatedArgs"))),
                                                                                field("trusted",
                                                                                    proj(Aliases.TYPE_, Aliases.TRUSTED_TYPE_VARS, "aliases")),
                                                                                field("inScope",
                                                                                    proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                                                                                field(
                                                                                    "filteredTypeApps",
                                                                                    Logic.ifElse(
                                                                                        Logic.or_(
                                                                                            Sets.null_(
                                                                                                var("trusted")),
                                                                                            Sets.null_(
                                                                                                var("inScope"))),
                                                                                        list(),
                                                                                        let(
                                                                                            "allVars",
                                                                                            Sets.unions(
                                                                                                Lists.map(
                                                                                                    lambda(
                                                                                                        "t",
                                                                                                        apply(
                                                                                                            ref(Coder.collectTypeVars),
                                                                                                            var("t"))),
                                                                                                    var("typeApps"))),
                                                                                            Logic.ifElse(
                                                                                                Logic.not_(
                                                                                                    Sets.null_(
                                                                                                        Sets.difference(
                                                                                                            var("allVars"),
                                                                                                            var("inScope")))),
                                                                                                list(),
                                                                                                Logic.ifElse(
                                                                                                    Sets.null_(
                                                                                                        Sets.difference(
                                                                                                            var("allVars"),
                                                                                                            var("trusted"))),
                                                                                                    var("typeApps"),
                                                                                                    list()))))),
                                                                                Eithers.bind(
                                                                                    Logic.ifElse(
                                                                                        Lists.null_(
                                                                                            var("filteredTypeApps")),
                                                                                        right(
                                                                                            list()),
                                                                                        apply(
                                                                                            ref(Coder.correctTypeApps),
                                                                                            var("g"),
                                                                                            var("name"),
                                                                                            var("hargs"),
                                                                                            var("filteredTypeApps"),
                                                                                            var("cx"),
                                                                                            var("g"))),
                                                                                    lambda(
                                                                                        "safeTypeApps",
                                                                                        Eithers.bind(
                                                                                            apply(
                                                                                                ref(Coder.filterPhantomTypeArgs),
                                                                                                var("name"),
                                                                                                var("safeTypeApps"),
                                                                                                var("cx"),
                                                                                                var("g")),
                                                                                            lambda(
                                                                                                "finalTypeApps",
                                                                                                Eithers.bind(
                                                                                                    apply(
                                                                                                        ref(Coder.functionCall),
                                                                                                        var("env"),
                                                                                                        bool(false),
                                                                                                        var("name"),
                                                                                                        var("hargs"),
                                                                                                        var("finalTypeApps"),
                                                                                                        var("cx"),
                                                                                                        var("g")),
                                                                                                    lambda(
                                                                                                        "initialCall",
                                                                                                        Eithers.foldl(
                                                                                                            lambda(
                                                                                                                "acc",
                                                                                                                "h",
                                                                                                                Eithers.bind(
                                                                                                                    apply(
                                                                                                                        ref(Coder.encodeTerm),
                                                                                                                        var("env"),
                                                                                                                        var("h"),
                                                                                                                        var("cx"),
                                                                                                                        var("g")),
                                                                                                                    lambda(
                                                                                                                        "jarg",
                                                                                                                        right(
                                                                                                                            apply(
                                                                                                                                ref(Coder.applyJavaArg),
                                                                                                                                var("acc"),
                                                                                                                                var("jarg")))))),
                                                                                                            var("initialCall"),
                                                                                                            var("rargs"))))))))))))))))))))))))));

    public static final Def encodeApplication_fallback = def(
        "encodeApplication_fallback",
        () -> lambda(
                java.util.Arrays.asList("env", "aliases", "gr", "typeApps", "lhs", "rhs", "cx", "g"),
                Eithers.bind(
                    Eithers.bimap(
                        lambda("__de",
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                        lambda("__a", var("__a")),
                        apply(
                            var("hydra.annotations.getType"),
                            var("g"),
                            apply(var("hydra.annotations.termAnnotationInternal"), var("lhs")))),
                    lambda("mt",
                        Eithers.bind(
                            Maybes.cases(
                                var("mt"),
                                apply(
                                    var("hydra.checking.typeOfTerm"),
                                    var("cx"),
                                    var("g"),
                                    var("lhs")),
                                lambda("typ", right(var("typ")))),
                            lambda("t",
                                casesWithDefault(Type.TYPE_,
                                    apply(
                                        var("hydra.strip.deannotateTypeParameters"),
                                        apply(var("hydra.strip.deannotateType"), var("t"))),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeTerm),
                                            var("env"),
                                            var("lhs"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jfun",
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeTerm),
                                                    var("env"),
                                                    var("rhs"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jarg",
                                                    right(
                                                        apply(
                                                            ref(Coder.applyJavaArg),
                                                            var("jfun"),
                                                            var("jarg"))))))),
                                    field(
                                        Type.FUNCTION,
                                        lambda("ft",
                                            let(
                                                field("dom",
                                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft")),
                                                field("cod",
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                                field("defaultExpr",
                                                    Eithers.bind(
                                                        apply(
                                                            ref(Coder.encodeTerm),
                                                            var("env"),
                                                            var("lhs"),
                                                            var("cx"),
                                                            var("g")),
                                                        lambda("jfun",
                                                            Eithers.bind(
                                                                apply(
                                                                    ref(Coder.encodeTerm),
                                                                    var("env"),
                                                                    var("rhs"),
                                                                    var("cx"),
                                                                    var("g")),
                                                                lambda("jarg",
                                                                    right(
                                                                        apply(
                                                                            ref(Coder.applyJavaArg),
                                                                            var("jfun"),
                                                                            var("jarg")))))))),
                                                field("elimBranch",
                                                    Eithers.bind(
                                                        apply(
                                                            ref(Coder.encodeTerm),
                                                            var("env"),
                                                            var("rhs"),
                                                            var("cx"),
                                                            var("g")),
                                                        lambda("jarg",
                                                            Eithers.bind(
                                                                Logic.ifElse(
                                                                    Logic.not_(
                                                                        Lists.null_(
                                                                            apply(
                                                                                ref(Coder.javaTypeArgumentsForType),
                                                                                var("dom")))),
                                                                    right(var("dom")),
                                                                    Eithers.bind(
                                                                        Eithers.bimap(
                                                                            lambda("__de",
                                                                                inject(
                                                                                    Error_.TYPE_,
                                                                                    Error_.OTHER,
                                                                                    wrap(
                                                                                        OtherError.TYPE_,
                                                                                        apply(
                                                                                            unwrap(DecodingError.TYPE_),
                                                                                            var("__de"))))),
                                                                            lambda("__a",
                                                                                var("__a")),
                                                                            apply(
                                                                                var("hydra.annotations.getType"),
                                                                                var("g"),
                                                                                apply(
                                                                                    var("hydra.annotations.termAnnotationInternal"),
                                                                                    var("rhs")))),
                                                                        lambda("mrt",
                                                                            Maybes.cases(
                                                                                var("mrt"),
                                                                                Eithers.bind(
                                                                                    apply(
                                                                                        var("hydra.checking.typeOfTerm"),
                                                                                        var("cx"),
                                                                                        var("g"),
                                                                                        var("rhs")),
                                                                                    lambda("rt",
                                                                                        right(
                                                                                            Logic.ifElse(
                                                                                                Logic.not_(
                                                                                                    Lists.null_(
                                                                                                        apply(
                                                                                                            ref(Coder.javaTypeArgumentsForType),
                                                                                                            var("rt")))),
                                                                                                var("rt"),
                                                                                                var("dom"))))),
                                                                                lambda("rt",
                                                                                    right(
                                                                                        Logic.ifElse(
                                                                                            Logic.not_(
                                                                                                Lists.null_(
                                                                                                    apply(
                                                                                                        ref(Coder.javaTypeArgumentsForType),
                                                                                                        var("rt")))),
                                                                                            var("rt"),
                                                                                            var("dom")))))))),
                                                                lambda("enrichedDom",
                                                                    apply(
                                                                        ref(Coder.encodeElimination),
                                                                        var("env"),
                                                                        just(var("jarg")),
                                                                        var("enrichedDom"),
                                                                        var("cod"),
                                                                        apply(
                                                                            var("hydra.strip.deannotateTerm"),
                                                                            var("lhs")),
                                                                        var("cx"),
                                                                        var("g"))))))),
                                                casesWithDefault(Term.TYPE_,
                                                    apply(
                                                        var("hydra.strip.deannotateAndDetypeTerm"),
                                                        var("lhs")),
                                                    var("defaultExpr"),
                                                    field(
                                                        Term.PROJECT,
                                                        constant(var("elimBranch"))),
                                                    field(
                                                        Term.CASES,
                                                        constant(var("elimBranch"))),
                                                    field(
                                                        Term.UNWRAP,
                                                        constant(var("elimBranch"))))))))))))));

    public static final Def encodeDefinitions = def(
        "encodeDefinitions",
        () -> lambda(
                "mod",
                "defs",
                "cx",
                "g",
                let(
                    field("aliases",
                        apply(ref(Utils.importAliasesForModule), var("mod"))),
                    field("env",
                        record(JavaEnvironment.TYPE_,
                            field(JavaEnvironment.ALIASES, var("aliases")),
                            field(JavaEnvironment.GRAPH, var("g")))),
                    field("pkg",
                        apply(
                            ref(Utils.javaPackageDeclaration),
                            proj(Module.TYPE_, Module.NAME, "mod"))),
                    field("partitioned",
                        apply(var("hydra.environment.partitionDefinitions"), var("defs"))),
                    field("typeDefs",
                        Pairs.first(var("partitioned"))),
                    field("termDefs",
                        Pairs.second(var("partitioned"))),
                    field("nonTypedefDefs",
                        Lists.filter(
                            lambda("td",
                                let("typ",
                                    apply(
                                        project(TypeScheme.TYPE_, TypeScheme.BODY),
                                        proj(TypeDefinition.TYPE_, TypeDefinition.TYPE_SCHEME, "td")),
                                    apply(ref(Coder.isSerializableJavaType), var("typ")))),
                            var("typeDefs"))),
                    Eithers.bind(
                        Eithers.mapList(
                            lambda("td",
                                apply(
                                    ref(Coder.encodeTypeDefinition),
                                    var("pkg"),
                                    var("aliases"),
                                    var("td"),
                                    var("cx"),
                                    var("g"))),
                            var("nonTypedefDefs")),
                        lambda("typeUnits",
                            Eithers.bind(
                                Logic.ifElse(
                                    Lists.null_(var("termDefs")),
                                    right(list()),
                                    Eithers.bind(
                                        Eithers.mapList(
                                            lambda("td",
                                                apply(
                                                    ref(Coder.encodeTermDefinition),
                                                    var("env"),
                                                    var("td"),
                                                    var("cx"),
                                                    var("g"))),
                                            var("termDefs")),
                                        lambda("dataMembers",
                                            right(
                                                list(
                                                    apply(
                                                        ref(Coder.constructElementsInterface),
                                                        var("mod"),
                                                        var("dataMembers"))))))),
                                lambda("termUnits",
                                    right(
                                        Maps.fromList(
                                            Lists.concat2(var("typeUnits"), var("termUnits")))))))))));

    public static final Def encodeElimination = def(
        "encodeElimination",
        () -> lambda(
                "env",
                "marg",
                "dom",
                "cod",
                "elimTerm",
                "cx",
                "g",
                let("aliases",
                    proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env"),
                    casesWithDefault(Term.TYPE_,
                        apply(var("hydra.strip.deannotateAndDetypeTerm"), var("elimTerm")),
                        left(
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    Strings.cat2(
                                        string("unexpected "),
                                        Strings.cat2(
                                            string("elimination case"),
                                            Strings.cat2(
                                                string(" in "),
                                                string("encodeElimination"))))))),
                        field(
                            Term.PROJECT,
                            lambda("proj",
                                let("fname",
                                    proj(Projection.TYPE_, Projection.FIELD_NAME, "proj"),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("hydra.lib.sets.empty"),
                                            var("dom"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jdom0",
                                            Eithers.bind(
                                                apply(
                                                    ref(Utils.javaTypeToJavaReferenceType),
                                                    var("jdom0"),
                                                    var("cx")),
                                                constant(
                                                    Maybes.cases(
                                                        var("marg"),
                                                        let(
                                                            field("projVar",
                                                                wrap(Name.TYPE_,
                                                                    string("projected"))),
                                                            field("jbody",
                                                                apply(
                                                                    ref(Utils.javaExpressionNameToJavaExpression),
                                                                    apply(
                                                                        ref(Utils.fieldExpression),
                                                                        apply(
                                                                            ref(Utils.variableToJavaIdentifier),
                                                                            var("projVar")),
                                                                        apply(
                                                                            ref(Utils.javaIdentifier),
                                                                            apply(
                                                                                unwrap(Name.TYPE_),
                                                                                var("fname")))))),
                                                            right(
                                                                apply(
                                                                    ref(Utils.javaLambda),
                                                                    var("projVar"),
                                                                    var("jbody")))),
                                                        lambda("jarg",
                                                            let("qual",
                                                                inject(
                                                                    FieldAccess_Qualifier.TYPE_,
                                                                    FieldAccess_Qualifier.PRIMARY,
                                                                    apply(
                                                                        ref(Utils.javaExpressionToJavaPrimary),
                                                                        var("jarg"))),
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaFieldAccessToJavaExpression),
                                                                        record(
                                                                            FieldAccess.TYPE_,
                                                                            field(
                                                                                FieldAccess.QUALIFIER,
                                                                                var("qual")),
                                                                            field(
                                                                                FieldAccess.IDENTIFIER,
                                                                                apply(
                                                                                    ref(Utils.javaIdentifier),
                                                                                    apply(
                                                                                        unwrap(Name.TYPE_),
                                                                                        var("fname"))))))))))))))))),
                        field(
                            Term.CASES,
                            lambda("cs",
                                let(
                                    field("tname",
                                        proj(CaseStatement.TYPE_, CaseStatement.TYPE_NAME, "cs")),
                                    field("def_",
                                        proj(CaseStatement.TYPE_, CaseStatement.DEFAULT, "cs")),
                                    field("fields",
                                        proj(CaseStatement.TYPE_, CaseStatement.CASES, "cs")),
                                    Maybes.cases(
                                        var("marg"),
                                        let(
                                            field("uVar",
                                                wrap(Name.TYPE_, string("u"))),
                                            field("domTypeArgs0",
                                                lambda(
                                                    "ty",
                                                    "acc",
                                                    casesWithDefault(Type.TYPE_,
                                                        apply(
                                                            var("hydra.strip.deannotateType"),
                                                            var("ty")),
                                                        var("acc"),
                                                        field(
                                                            Type.APPLICATION,
                                                            lambda("atyp",
                                                                apply(
                                                                    var("domTypeArgs0"),
                                                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "atyp"),
                                                                    Lists.cons(
                                                                        proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "atyp"),
                                                                        var("acc")))))))),
                                            field("domTypeArgs",
                                                apply(var("domTypeArgs0"), var("dom"), list())),
                                            field("bareElim",
                                                apply(
                                                    var("hydra.strip.deannotateAndDetypeTerm"),
                                                    var("elimTerm"))),
                                            field("wrappedElimTerm",
                                                Lists.foldl(
                                                    lambda(
                                                        "trm",
                                                        "t",
                                                        inject(Term.TYPE_,
                                                            Term.TYPE_APPLICATION,
                                                            record(
                                                                TypeApplicationTerm.TYPE_,
                                                                field(
                                                                    TypeApplicationTerm.BODY,
                                                                    var("trm")),
                                                                field(
                                                                    TypeApplicationTerm.TYPE,
                                                                    var("t"))))),
                                                    var("bareElim"),
                                                    var("domTypeArgs"))),
                                            field("typedLambda",
                                                inject(Term.TYPE_,
                                                    Term.LAMBDA,
                                                    record(Lambda.TYPE_,
                                                        field(
                                                            Lambda.PARAMETER,
                                                            var("uVar")),
                                                        field(
                                                            Lambda.DOMAIN,
                                                            just(var("dom"))),
                                                        field(
                                                            Lambda.BODY,
                                                            inject(Term.TYPE_,
                                                                Term.APPLICATION,
                                                                record(Application.TYPE_,
                                                                    field(
                                                                        Application.FUNCTION,
                                                                        var("wrappedElimTerm")),
                                                                    field(
                                                                        Application.ARGUMENT,
                                                                        inject(
                                                                            Term.TYPE_,
                                                                            Term.VARIABLE,
                                                                            var("uVar"))))))))),
                                            apply(
                                                ref(Coder.encodeTerm),
                                                var("env"),
                                                var("typedLambda"),
                                                var("cx"),
                                                var("g"))),
                                        lambda("jarg",
                                            let(
                                                field("prim",
                                                    apply(
                                                        ref(Utils.javaExpressionToJavaPrimary),
                                                        var("jarg"))),
                                                field("consId",
                                                    apply(
                                                        ref(Coder.innerClassRef),
                                                        var("aliases"),
                                                        var("tname"),
                                                        ref(Names.partialVisitorName))),
                                                field("effectiveCod",
                                                    var("cod")),
                                                Eithers.bind(
                                                    apply(
                                                        ref(Coder.encodeType),
                                                        var("aliases"),
                                                        var("hydra.lib.sets.empty"),
                                                        var("effectiveCod"),
                                                        var("cx"),
                                                        var("g")),
                                                    lambda("jcod",
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Utils.javaTypeToJavaReferenceType),
                                                                var("jcod"),
                                                                var("cx")),
                                                            lambda("rt",
                                                                Eithers.bind(
                                                                    apply(
                                                                        ref(Coder.domTypeArgs),
                                                                        var("aliases"),
                                                                        var("dom"),
                                                                        var("cx"),
                                                                        var("g")),
                                                                    lambda("domArgs",
                                                                        let("targs",
                                                                            apply(
                                                                                ref(Coder.typeArgsOrDiamond),
                                                                                Lists.concat2(
                                                                                    var("domArgs"),
                                                                                    list(
                                                                                        inject(
                                                                                            TypeArgument.TYPE_,
                                                                                            TypeArgument.REFERENCE,
                                                                                            var("rt"))))),
                                                                            Eithers.bind(
                                                                                Maybes.cases(
                                                                                    var("def_"),
                                                                                    right(list()),
                                                                                    lambda("d",
                                                                                        Eithers.bind(
                                                                                            apply(
                                                                                                ref(Coder.otherwiseBranch),
                                                                                                var("env"),
                                                                                                var("aliases"),
                                                                                                var("dom"),
                                                                                                var("cod"),
                                                                                                var("tname"),
                                                                                                var("jcod"),
                                                                                                var("domArgs"),
                                                                                                var("d"),
                                                                                                var("cx"),
                                                                                                var("g")),
                                                                                            lambda(
                                                                                                "b",
                                                                                                right(
                                                                                                    list(
                                                                                                        var("b"))))))),
                                                                                lambda(
                                                                                    "otherwiseBranches",
                                                                                    Eithers.bind(
                                                                                        Eithers.mapList(
                                                                                            lambda(
                                                                                                "f",
                                                                                                apply(
                                                                                                    ref(Coder.visitBranch),
                                                                                                    var("env"),
                                                                                                    var("aliases"),
                                                                                                    var("dom"),
                                                                                                    var("tname"),
                                                                                                    var("jcod"),
                                                                                                    var("domArgs"),
                                                                                                    var("f"),
                                                                                                    var("cx"),
                                                                                                    var("g"))),
                                                                                            var("fields")),
                                                                                        lambda(
                                                                                            "visitBranches",
                                                                                            let(
                                                                                                field(
                                                                                                    "body",
                                                                                                    wrap(
                                                                                                        ClassBody.TYPE_,
                                                                                                        Lists.concat2(
                                                                                                            var("otherwiseBranches"),
                                                                                                            var("visitBranches")))),
                                                                                                field(
                                                                                                    "visitor",
                                                                                                    apply(
                                                                                                        ref(Utils.javaConstructorCall),
                                                                                                        apply(
                                                                                                            ref(Utils.javaConstructorName),
                                                                                                            var("consId"),
                                                                                                            just(
                                                                                                                var("targs"))),
                                                                                                        list(),
                                                                                                        just(
                                                                                                            var("body")))),
                                                                                                right(
                                                                                                    apply(
                                                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                                                        apply(
                                                                                                            ref(Utils.methodInvocation),
                                                                                                            just(
                                                                                                                right(
                                                                                                                    var("prim"))),
                                                                                                            wrap(
                                                                                                                Identifier.TYPE_,
                                                                                                                ref(Names.acceptMethodName)),
                                                                                                            list(
                                                                                                                var("visitor"))))))))))))))))))))))),
                        field(
                            Term.UNWRAP,
                            constant(
                                let("withArg",
                                    lambda("ja",
                                        apply(
                                            ref(Utils.javaFieldAccessToJavaExpression),
                                            record(FieldAccess.TYPE_,
                                                field(
                                                    FieldAccess.QUALIFIER,
                                                    inject(
                                                        FieldAccess_Qualifier.TYPE_,
                                                        FieldAccess_Qualifier.PRIMARY,
                                                        apply(
                                                            ref(Utils.javaExpressionToJavaPrimary),
                                                            var("ja")))),
                                                field(
                                                    FieldAccess.IDENTIFIER,
                                                    apply(
                                                        ref(Utils.javaIdentifier),
                                                        ref(Names.valueFieldName)))))),
                                    right(
                                        Maybes.cases(
                                            var("marg"),
                                            let(
                                                field("wVar",
                                                    wrap(Name.TYPE_, string("wrapped"))),
                                                field("wArg",
                                                    apply(
                                                        ref(Utils.javaIdentifierToJavaExpression),
                                                        apply(
                                                            ref(Utils.variableToJavaIdentifier),
                                                            var("wVar")))),
                                                apply(
                                                    ref(Utils.javaLambda),
                                                    var("wVar"),
                                                    apply(var("withArg"), var("wArg")))),
                                            lambda("jarg", apply(var("withArg"), var("jarg"))))))))))));

    public static final Def encodeFunction = def(
        "encodeFunction",
        () -> lambda(
                "env",
                "dom",
                "cod",
                "funTerm",
                "cx",
                "g",
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("encodeLambdaFallback",
                        lambda(
                            "env2",
                            "lam",
                            let(
                                field("lambdaVar",
                                    proj(Lambda.TYPE_, Lambda.PARAMETER, "lam")),
                                field("body",
                                    proj(Lambda.TYPE_, Lambda.BODY, "lam")),
                                Eithers.bind(
                                    apply(
                                        ref(Coder.analyzeJavaFunction),
                                        var("env2"),
                                        var("body"),
                                        var("cx"),
                                        var("g")),
                                    lambda("fs",
                                        let(
                                            field("bindings",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.BINDINGS, "fs")),
                                            field("innerBody",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.BODY, "fs")),
                                            field("env3",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.ENVIRONMENT, "fs")),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.bindingsToStatements),
                                                    var("env3"),
                                                    var("bindings"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("bindResult",
                                                    let(
                                                        field("bindingStmts",
                                                            Pairs.first(var("bindResult"))),
                                                        field("env4",
                                                            Pairs.second(var("bindResult"))),
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Coder.encodeTerm),
                                                                var("env4"),
                                                                var("innerBody"),
                                                                var("cx"),
                                                                var("g")),
                                                            lambda("jbody",
                                                                let("lam1",
                                                                    Logic.ifElse(
                                                                        Lists.null_(var("bindings")),
                                                                        apply(
                                                                            ref(Utils.javaLambda),
                                                                            var("lambdaVar"),
                                                                            var("jbody")),
                                                                        let("returnSt",
                                                                            inject(
                                                                                BlockStatement.TYPE_,
                                                                                BlockStatement.STATEMENT,
                                                                                apply(
                                                                                    ref(Utils.javaReturnStatement),
                                                                                    just(
                                                                                        var("jbody")))),
                                                                            apply(
                                                                                ref(Utils.javaLambdaFromBlock),
                                                                                var("lambdaVar"),
                                                                                wrap(
                                                                                    Block.TYPE_,
                                                                                    Lists.concat2(
                                                                                        var("bindingStmts"),
                                                                                        list(
                                                                                            var("returnSt"))))))),
                                                                    apply(
                                                                        ref(Coder.applyCastIfSafe),
                                                                        var("aliases"),
                                                                        inject(
                                                                            Type.TYPE_,
                                                                            Type.FUNCTION,
                                                                            record(
                                                                                FunctionType.TYPE_,
                                                                                field(
                                                                                    FunctionType.DOMAIN,
                                                                                    var("dom")),
                                                                                field(
                                                                                    FunctionType.CODOMAIN,
                                                                                    var("cod")))),
                                                                        var("lam1"),
                                                                        var("cx"),
                                                                        var("g")))))))))))))),
                    casesWithDefault(Term.TYPE_,
                        apply(var("hydra.strip.deannotateTerm"), var("funTerm")),
                        right(
                            apply(
                                ref(Coder.encodeLiteral),
                                inject(Literal.TYPE_,
                                    Literal.STRING,
                                    Strings.cat2(
                                        string("Unimplemented function variant: "),
                                        apply(var("hydra.show.core.term"), var("funTerm")))))),
                        field(
                            Term.PROJECT,
                            constant(
                                apply(
                                    ref(Coder.encodeElimination),
                                    var("env"),
                                    nothing(),
                                    var("dom"),
                                    var("cod"),
                                    apply(var("hydra.strip.deannotateTerm"), var("funTerm")),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.CASES,
                            constant(
                                apply(
                                    ref(Coder.encodeElimination),
                                    var("env"),
                                    nothing(),
                                    var("dom"),
                                    var("cod"),
                                    apply(var("hydra.strip.deannotateTerm"), var("funTerm")),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.UNWRAP,
                            constant(
                                apply(
                                    ref(Coder.encodeElimination),
                                    var("env"),
                                    nothing(),
                                    var("dom"),
                                    var("cod"),
                                    apply(var("hydra.strip.deannotateTerm"), var("funTerm")),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.LAMBDA,
                            lambda("lam",
                                apply(
                                    ref(Coder.withLambda),
                                    var("env"),
                                    var("lam"),
                                    lambda("env2",
                                        let(
                                            field("lambdaVar",
                                                proj(Lambda.TYPE_, Lambda.PARAMETER, "lam")),
                                            field("body",
                                                proj(Lambda.TYPE_, Lambda.BODY, "lam")),
                                            casesWithDefault(Term.TYPE_,
                                                apply(
                                                    var("hydra.strip.deannotateTerm"),
                                                    var("body")),
                                                apply(
                                                    var("encodeLambdaFallback"),
                                                    var("env2"),
                                                    var("lam")),
                                                field(
                                                    Term.LAMBDA,
                                                    lambda("innerLam",
                                                        casesWithDefault(Type.TYPE_,
                                                            apply(
                                                                var("hydra.strip.deannotateType"),
                                                                var("cod")),
                                                            left(
                                                                inject(Error_.TYPE_,
                                                                    Error_.OTHER,
                                                                    wrap(
                                                                        OtherError.TYPE_,
                                                                        Strings.cat2(
                                                                            string("expected function type for lambda body, but got: "),
                                                                            apply(
                                                                                var("hydra.show.core.type"),
                                                                                var("cod")))))),
                                                            field(
                                                                Type.FUNCTION,
                                                                lambda("ft",
                                                                    let(
                                                                        field("dom2",
                                                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft")),
                                                                        field("cod2",
                                                                            proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                                                        Eithers.bind(
                                                                            apply(
                                                                                ref(Coder.encodeFunction),
                                                                                var("env2"),
                                                                                var("dom2"),
                                                                                var("cod2"),
                                                                                inject(
                                                                                    Term.TYPE_,
                                                                                    Term.LAMBDA,
                                                                                    var("innerLam")),
                                                                                var("cx"),
                                                                                var("g")),
                                                                            lambda(
                                                                                "innerJavaLambda",
                                                                                let("lam1",
                                                                                    apply(
                                                                                        ref(Utils.javaLambda),
                                                                                        var("lambdaVar"),
                                                                                        var("innerJavaLambda")),
                                                                                    apply(
                                                                                        ref(Coder.applyCastIfSafe),
                                                                                        var("aliases"),
                                                                                        inject(
                                                                                            Type.TYPE_,
                                                                                            Type.FUNCTION,
                                                                                            record(
                                                                                                FunctionType.TYPE_,
                                                                                                field(
                                                                                                    FunctionType.DOMAIN,
                                                                                                    var("dom")),
                                                                                                field(
                                                                                                    FunctionType.CODOMAIN,
                                                                                                    var("cod")))),
                                                                                        var("lam1"),
                                                                                        var("cx"),
                                                                                        var("g")))))))))))))))))))));

    public static final Def encodeFunctionFormTerm = def(
        "encodeFunctionFormTerm",
        () -> lambda(
                "env",
                "anns",
                "term",
                "cx",
                "g",
                let("combinedAnns",
                    Lists.foldl(
                        lambda("acc", lambda("m", Maps.union(var("acc"), var("m")))),
                        var("hydra.lib.maps.empty"),
                        var("anns")),
                    Eithers.bind(
                        Eithers.bimap(
                            lambda("__de",
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                            lambda("__a", var("__a")),
                            apply(var("hydra.annotations.getType"), var("g"), var("combinedAnns"))),
                        lambda("mt",
                            Eithers.bind(
                                Maybes.cases(
                                    var("mt"),
                                    Maybes.cases(
                                        apply(ref(Coder.tryInferFunctionType), var("term")),
                                        apply(
                                            var("hydra.checking.typeOfTerm"),
                                            var("cx"),
                                            var("g"),
                                            var("term")),
                                        lambda("inferredType", right(var("inferredType")))),
                                    lambda("t", right(var("t")))),
                                lambda("typ",
                                    casesWithDefault(Type.TYPE_,
                                        apply(var("hydra.strip.deannotateType"), var("typ")),
                                        apply(
                                            ref(Coder.encodeNullaryConstant),
                                            var("env"),
                                            var("typ"),
                                            var("term"),
                                            var("cx"),
                                            var("g")),
                                        field(
                                            Type.FUNCTION,
                                            lambda("ft",
                                                apply(
                                                    ref(Coder.encodeFunction),
                                                    var("env"),
                                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"),
                                                    var("term"),
                                                    var("cx"),
                                                    var("g"))))))))))));

    public static final Def encodeFunctionPrimitiveByName = def(
        "encodeFunctionPrimitiveByName",
        () -> lambda(
                "env",
                "dom",
                "cod",
                "name",
                "cx",
                "g",
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("classWithApply",
                        apply(
                            unwrap(Identifier.TYPE_),
                            apply(
                                ref(Coder.elementJavaIdentifier),
                                bool(true),
                                bool(false),
                                var("aliases"),
                                var("name")))),
                    field("suffix",
                        Strings.cat2(string("."), ref(Names.applyMethodName))),
                    field("className",
                        Strings.fromList(
                            Lists.take(
                                Math_.sub(
                                    Strings.length(var("classWithApply")),
                                    Strings.length(var("suffix"))),
                                Strings.toList(var("classWithApply"))))),
                    field("arity",
                        apply(
                            var("hydra.arity.typeArity"),
                            inject(Type.TYPE_,
                                Type.FUNCTION,
                                record(FunctionType.TYPE_,
                                    field(FunctionType.DOMAIN, var("dom")),
                                    field(FunctionType.CODOMAIN, var("cod")))))),
                    Logic.ifElse(
                        Equality.lte(var("arity"), int32(1)),
                        right(
                            apply(
                                ref(Utils.javaIdentifierToJavaExpression),
                                wrap(Identifier.TYPE_,
                                    Strings.cat(
                                        list(
                                            var("className"),
                                            string("::"),
                                            ref(Names.applyMethodName)))))),
                        let(
                            field("paramNames",
                                Lists.map(
                                    lambda("i",
                                        wrap(Name.TYPE_,
                                            Strings.cat2(string("p"), Literals.showInt32(var("i"))))),
                                    Math_.range_(int32(0), Math_.sub(var("arity"), int32(1))))),
                            field("paramExprs",
                                Lists.map(
                                    lambda("p",
                                        apply(
                                            ref(Utils.javaIdentifierToJavaExpression),
                                            apply(
                                                ref(Utils.variableToJavaIdentifier),
                                                var("p")))),
                                    var("paramNames"))),
                            field("classId",
                                wrap(Identifier.TYPE_, var("className"))),
                            field("call",
                                apply(
                                    ref(Utils.javaMethodInvocationToJavaExpression),
                                    apply(
                                        ref(Utils.methodInvocationStatic),
                                        var("classId"),
                                        wrap(Identifier.TYPE_,
                                            ref(Names.applyMethodName)),
                                        var("paramExprs")))),
                            field("curried",
                                apply(ref(Coder.buildCurriedLambda), var("paramNames"), var("call"))),
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("hydra.lib.sets.empty"),
                                    inject(Type.TYPE_,
                                        Type.FUNCTION,
                                        record(FunctionType.TYPE_,
                                            field(FunctionType.DOMAIN, var("dom")),
                                            field(FunctionType.CODOMAIN, var("cod")))),
                                    var("cx"),
                                    var("g")),
                                lambda("jtype",
                                    Eithers.bind(
                                        apply(
                                            ref(Utils.javaTypeToJavaReferenceType),
                                            var("jtype"),
                                            var("cx")),
                                        lambda("rt",
                                            right(
                                                apply(
                                                    ref(Utils.javaCastExpressionToJavaExpression),
                                                    apply(
                                                        ref(Utils.javaCastExpression),
                                                        var("rt"),
                                                        apply(
                                                            ref(Utils.javaExpressionToJavaUnaryExpression),
                                                            var("curried"))))))))))))));

    public static final Def encodeLiteral = def(
        "encodeLiteral",
        () -> lambda("lit",
                cases(Literal.TYPE_,
                    var("lit"),
                    field(
                        Literal.BINARY,
                        lambda("bs",
                            let("byteValues",
                                Literals.binaryToBytes(var("bs")),
                                apply(
                                    ref(Utils.javaArrayCreation),
                                    ref(Utils.javaBytePrimitiveType),
                                    just(
                                        apply(
                                            ref(Utils.javaArrayInitializer),
                                            Lists.map(
                                                lambda("w",
                                                    apply(
                                                        ref(Utils.javaLiteralToJavaExpression),
                                                        inject(hydra.java.syntax.Literal.TYPE_,
                                                            hydra.java.syntax.Literal.INTEGER,
                                                            wrap(
                                                                IntegerLiteral.TYPE_,
                                                                Literals.int32ToBigint(var("w")))))),
                                                var("byteValues")))))))),
                    field(
                        Literal.BOOLEAN,
                        lambda("b",
                            apply(
                                ref(Coder.encodeLiteral_litExp),
                                apply(ref(Utils.javaBoolean), var("b"))))),
                    field(
                        Literal.DECIMAL,
                        lambda("v",
                            apply(
                                ref(Utils.javaConstructorCall),
                                apply(
                                    ref(Utils.javaConstructorName),
                                    wrap(Identifier.TYPE_,
                                        string("java.math.BigDecimal")),
                                    nothing()),
                                list(
                                    apply(
                                        ref(Coder.encodeLiteral),
                                        inject(Literal.TYPE_,
                                            Literal.STRING,
                                            Literals.showDecimal(var("v"))))),
                                nothing()))),
                    field(
                        Literal.FLOAT,
                        lambda("f", apply(ref(Coder.encodeLiteral_encodeFloat), var("f")))),
                    field(
                        Literal.INTEGER,
                        lambda("i", apply(ref(Coder.encodeLiteral_encodeInteger), var("i")))),
                    field(
                        Literal.STRING,
                        lambda("s",
                            apply(
                                ref(Coder.encodeLiteral_litExp),
                                apply(ref(Utils.javaString), var("s"))))))));

    public static final Def encodeLiteralType = def(
        "encodeLiteralType",
        () -> lambda(
                "lt",
                "cx",
                "g",
                cases(LiteralType.TYPE_,
                    var("lt"),
                    field(
                        LiteralType.BINARY,
                        constant(
                            right(
                                inject(hydra.java.syntax.Type.TYPE_,
                                    hydra.java.syntax.Type.REFERENCE,
                                    inject(ReferenceType.TYPE_,
                                        ReferenceType.ARRAY,
                                        record(ArrayType.TYPE_,
                                            field(
                                                ArrayType.DIMS,
                                                wrap(Dims.TYPE_, list(list()))),
                                            field(
                                                ArrayType.VARIANT,
                                                inject(ArrayType_Variant.TYPE_,
                                                    ArrayType_Variant.PRIMITIVE,
                                                    record(
                                                        PrimitiveTypeWithAnnotations.TYPE_,
                                                        field(
                                                            PrimitiveTypeWithAnnotations.TYPE,
                                                            inject(
                                                                PrimitiveType.TYPE_,
                                                                PrimitiveType.NUMERIC,
                                                                inject(
                                                                    NumericType.TYPE_,
                                                                    NumericType.INTEGRAL,
                                                                    inject(
                                                                        IntegralType.TYPE_,
                                                                        IntegralType.BYTE,
                                                                        unit())))),
                                                        field(
                                                            PrimitiveTypeWithAnnotations.ANNOTATIONS,
                                                            list())))))))))),
                    field(
                        LiteralType.BOOLEAN,
                        constant(
                            apply(
                                ref(Coder.encodeLiteralType_simple),
                                string("Boolean"),
                                var("cx"),
                                var("g")))),
                    field(
                        LiteralType.DECIMAL,
                        constant(
                            right(
                                apply(
                                    ref(Utils.javaRefType),
                                    list(),
                                    just(
                                        apply(
                                            ref(Names.javaPackageName),
                                            list(string("java"), string("math")))),
                                    string("BigDecimal"))))),
                    field(
                        LiteralType.FLOAT,
                        lambda("ft",
                            cases(FloatType.TYPE_,
                                var("ft"),
                                field(
                                    FloatType.FLOAT32,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Float"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    FloatType.FLOAT64,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Double"),
                                            var("cx"),
                                            var("g"))))))),
                    field(
                        LiteralType.INTEGER,
                        lambda("it",
                            cases(IntegerType.TYPE_,
                                var("it"),
                                field(
                                    IntegerType.BIGINT,
                                    constant(
                                        right(
                                            apply(
                                                ref(Utils.javaRefType),
                                                list(),
                                                just(
                                                    apply(
                                                        ref(Names.javaPackageName),
                                                        list(string("java"), string("math")))),
                                                string("BigInteger"))))),
                                field(
                                    IntegerType.INT8,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Byte"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.INT16,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Short"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.INT32,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Integer"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.INT64,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Long"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.UINT8,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Short"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.UINT16,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Character"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.UINT32,
                                    constant(
                                        apply(
                                            ref(Coder.encodeLiteralType_simple),
                                            string("Long"),
                                            var("cx"),
                                            var("g")))),
                                field(
                                    IntegerType.UINT64,
                                    constant(
                                        right(
                                            apply(
                                                ref(Utils.javaRefType),
                                                list(),
                                                just(
                                                    apply(
                                                        ref(Names.javaPackageName),
                                                        list(string("java"), string("math")))),
                                                string("BigInteger")))))))),
                    field(
                        LiteralType.STRING,
                        constant(
                            apply(
                                ref(Coder.encodeLiteralType_simple),
                                string("String"),
                                var("cx"),
                                var("g")))))));

    public static final Def encodeLiteralType_simple = def(
        "encodeLiteralType_simple",
        () -> lambda(
                "n",
                "cx",
                "g",
                right(apply(ref(Utils.javaRefType), list(), nothing(), var("n")))));

    public static final Def encodeLiteral_encodeFloat = def(
        "encodeLiteral_encodeFloat",
        () -> lambda("f",
                cases(FloatValue.TYPE_,
                    var("f"),
                    field(
                        FloatValue.FLOAT32,
                        lambda("v", apply(ref(Coder.encodeLiteral_encodeFloat32), var("v")))),
                    field(
                        FloatValue.FLOAT64,
                        lambda("v", apply(ref(Coder.encodeLiteral_encodeFloat64), var("v")))))));

    public static final Def encodeLiteral_encodeFloat32 = def(
        "encodeLiteral_encodeFloat32",
        () -> lambda("v",
                let("s",
                    Literals.showFloat32(var("v")),
                    Logic.ifElse(
                        Equality.equal(var("s"), string("NaN")),
                        apply(
                            ref(Coder.encodeLiteral_javaSpecialFloatExpr),
                            string("Float"),
                            string("NaN")),
                        Logic.ifElse(
                            Equality.equal(var("s"), string("Infinity")),
                            apply(
                                ref(Coder.encodeLiteral_javaSpecialFloatExpr),
                                string("Float"),
                                string("POSITIVE_INFINITY")),
                            Logic.ifElse(
                                Equality.equal(var("s"), string("-Infinity")),
                                apply(
                                    ref(Coder.encodeLiteral_javaSpecialFloatExpr),
                                    string("Float"),
                                    string("NEGATIVE_INFINITY")),
                                apply(
                                    ref(Coder.encodeLiteral_primCast),
                                    inject(PrimitiveType.TYPE_,
                                        PrimitiveType.NUMERIC,
                                        inject(NumericType.TYPE_,
                                            NumericType.FLOATING_POINT,
                                            inject(FloatingPointType.TYPE_,
                                                FloatingPointType.FLOAT,
                                                unit()))),
                                    apply(
                                        ref(Coder.encodeLiteral_litExp),
                                        inject(hydra.java.syntax.Literal.TYPE_,
                                            hydra.java.syntax.Literal.FLOATING_POINT,
                                            wrap(FloatingPointLiteral.TYPE_,
                                                Literals.float32ToFloat64(var("v"))))))))))));

    public static final Def encodeLiteral_encodeFloat64 = def(
        "encodeLiteral_encodeFloat64",
        () -> lambda("v",
                let("s",
                    Literals.showFloat64(var("v")),
                    Logic.ifElse(
                        Equality.equal(var("s"), string("NaN")),
                        apply(
                            ref(Coder.encodeLiteral_javaSpecialFloatExpr),
                            string("Double"),
                            string("NaN")),
                        Logic.ifElse(
                            Equality.equal(var("s"), string("Infinity")),
                            apply(
                                ref(Coder.encodeLiteral_javaSpecialFloatExpr),
                                string("Double"),
                                string("POSITIVE_INFINITY")),
                            Logic.ifElse(
                                Equality.equal(var("s"), string("-Infinity")),
                                apply(
                                    ref(Coder.encodeLiteral_javaSpecialFloatExpr),
                                    string("Double"),
                                    string("NEGATIVE_INFINITY")),
                                Logic.ifElse(
                                    Equality.equal(var("s"), string("-0.0")),
                                    apply(ref(Coder.encodeLiteral_javaParseDouble), string("-0.0")),
                                    apply(
                                        ref(Coder.encodeLiteral_litExp),
                                        inject(hydra.java.syntax.Literal.TYPE_,
                                            hydra.java.syntax.Literal.FLOATING_POINT,
                                            wrap(FloatingPointLiteral.TYPE_,
                                                var("v")))))))))));

    public static final Def encodeLiteral_encodeInteger = def(
        "encodeLiteral_encodeInteger",
        () -> lambda("i",
                cases(IntegerValue.TYPE_,
                    var("i"),
                    field(
                        IntegerValue.BIGINT,
                        lambda("v",
                            apply(
                                ref(Utils.javaConstructorCall),
                                apply(
                                    ref(Utils.javaConstructorName),
                                    wrap(Identifier.TYPE_,
                                        string("java.math.BigInteger")),
                                    nothing()),
                                list(
                                    apply(
                                        ref(Coder.encodeLiteral),
                                        inject(Literal.TYPE_,
                                            Literal.STRING,
                                            Literals.showBigint(var("v"))))),
                                nothing()))),
                    field(
                        IntegerValue.INT8,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_primCast),
                                inject(PrimitiveType.TYPE_,
                                    PrimitiveType.NUMERIC,
                                    inject(NumericType.TYPE_,
                                        NumericType.INTEGRAL,
                                        inject(IntegralType.TYPE_,
                                            IntegralType.BYTE,
                                            unit()))),
                                apply(
                                    ref(Coder.encodeLiteral_litExp),
                                    inject(hydra.java.syntax.Literal.TYPE_,
                                        hydra.java.syntax.Literal.INTEGER,
                                        wrap(IntegerLiteral.TYPE_,
                                            Literals.int8ToBigint(var("v")))))))),
                    field(
                        IntegerValue.INT16,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_primCast),
                                inject(PrimitiveType.TYPE_,
                                    PrimitiveType.NUMERIC,
                                    inject(NumericType.TYPE_,
                                        NumericType.INTEGRAL,
                                        inject(IntegralType.TYPE_,
                                            IntegralType.SHORT,
                                            unit()))),
                                apply(
                                    ref(Coder.encodeLiteral_litExp),
                                    inject(hydra.java.syntax.Literal.TYPE_,
                                        hydra.java.syntax.Literal.INTEGER,
                                        wrap(IntegerLiteral.TYPE_,
                                            Literals.int16ToBigint(var("v")))))))),
                    field(
                        IntegerValue.INT32,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_litExp),
                                inject(hydra.java.syntax.Literal.TYPE_,
                                    hydra.java.syntax.Literal.INTEGER,
                                    wrap(IntegerLiteral.TYPE_,
                                        Literals.int32ToBigint(var("v"))))))),
                    field(
                        IntegerValue.INT64,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_primCast),
                                inject(PrimitiveType.TYPE_,
                                    PrimitiveType.NUMERIC,
                                    inject(NumericType.TYPE_,
                                        NumericType.INTEGRAL,
                                        inject(IntegralType.TYPE_,
                                            IntegralType.LONG,
                                            unit()))),
                                apply(
                                    ref(Coder.encodeLiteral_litExp),
                                    inject(hydra.java.syntax.Literal.TYPE_,
                                        hydra.java.syntax.Literal.INTEGER,
                                        wrap(IntegerLiteral.TYPE_,
                                            Literals.int64ToBigint(var("v")))))))),
                    field(
                        IntegerValue.UINT8,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_primCast),
                                inject(PrimitiveType.TYPE_,
                                    PrimitiveType.NUMERIC,
                                    inject(NumericType.TYPE_,
                                        NumericType.INTEGRAL,
                                        inject(IntegralType.TYPE_,
                                            IntegralType.SHORT,
                                            unit()))),
                                apply(
                                    ref(Coder.encodeLiteral_litExp),
                                    inject(hydra.java.syntax.Literal.TYPE_,
                                        hydra.java.syntax.Literal.INTEGER,
                                        wrap(IntegerLiteral.TYPE_,
                                            Literals.uint8ToBigint(var("v")))))))),
                    field(
                        IntegerValue.UINT16,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_litExp),
                                inject(hydra.java.syntax.Literal.TYPE_,
                                    hydra.java.syntax.Literal.CHARACTER,
                                    var("v"))))),
                    field(
                        IntegerValue.UINT32,
                        lambda("v",
                            apply(
                                ref(Coder.encodeLiteral_primCast),
                                inject(PrimitiveType.TYPE_,
                                    PrimitiveType.NUMERIC,
                                    inject(NumericType.TYPE_,
                                        NumericType.INTEGRAL,
                                        inject(IntegralType.TYPE_,
                                            IntegralType.LONG,
                                            unit()))),
                                apply(
                                    ref(Coder.encodeLiteral_litExp),
                                    inject(hydra.java.syntax.Literal.TYPE_,
                                        hydra.java.syntax.Literal.INTEGER,
                                        wrap(IntegerLiteral.TYPE_,
                                            Literals.uint32ToBigint(var("v")))))))),
                    field(
                        IntegerValue.UINT64,
                        lambda("v",
                            apply(
                                ref(Utils.javaConstructorCall),
                                apply(
                                    ref(Utils.javaConstructorName),
                                    wrap(Identifier.TYPE_,
                                        string("java.math.BigInteger")),
                                    nothing()),
                                list(
                                    apply(
                                        ref(Coder.encodeLiteral),
                                        inject(Literal.TYPE_,
                                            Literal.STRING,
                                            Literals.showBigint(Literals.uint64ToBigint(var("v")))))),
                                nothing()))))));

    public static final Def encodeLiteral_javaParseDouble = def(
        "encodeLiteral_javaParseDouble",
        () -> lambda("value",
                apply(
                    ref(Utils.javaMethodInvocationToJavaExpression),
                    apply(
                        ref(Utils.methodInvocationStatic),
                        wrap(Identifier.TYPE_, string("Double")),
                        wrap(Identifier.TYPE_, string("parseDouble")),
                        list(
                            apply(
                                ref(Coder.encodeLiteral),
                                inject(Literal.TYPE_,
                                    Literal.STRING,
                                    var("value"))))))));

    public static final Def encodeLiteral_javaSpecialFloatExpr = def(
        "encodeLiteral_javaSpecialFloatExpr",
        () -> lambda(
                "className",
                "fieldName",
                apply(
                    ref(Utils.javaExpressionNameToJavaExpression),
                    record(ExpressionName.TYPE_,
                        field(
                            ExpressionName.QUALIFIER,
                            just(
                                wrap(AmbiguousName.TYPE_,
                                    list(wrap(Identifier.TYPE_, var("className")))))),
                        field(
                            ExpressionName.IDENTIFIER,
                            wrap(Identifier.TYPE_, var("fieldName")))))));

    public static final Def encodeLiteral_litExp = def(
        "encodeLiteral_litExp",
        () -> lambda("l", apply(ref(Utils.javaLiteralToJavaExpression), var("l"))));

    public static final Def encodeLiteral_primCast = def(
        "encodeLiteral_primCast",
        () -> lambda(
                "pt",
                "expr",
                apply(
                    ref(Utils.javaCastExpressionToJavaExpression),
                    apply(
                        ref(Utils.javaCastPrimitive),
                        var("pt"),
                        apply(
                            ref(Utils.javaExpressionToJavaUnaryExpression),
                            var("expr"))))));

    public static final Def encodeNullaryConstant = def(
        "encodeNullaryConstant",
        () -> lambda(
                "env",
                "typ",
                "funTerm",
                "cx",
                "g",
                left(
                    inject(Error_.TYPE_,
                        Error_.OTHER,
                        wrap(OtherError.TYPE_,
                            Strings.cat2(
                                string("unexpected "),
                                Strings.cat2(
                                    string("nullary function"),
                                    Strings.cat2(
                                        string(" in "),
                                        apply(var("hydra.show.core.term"), var("funTerm"))))))))));

    public static final Def encodeNullaryConstant_typeArgsFromReturnType = def(
        "encodeNullaryConstant_typeArgsFromReturnType",
        () -> lambda(
                "aliases",
                "t",
                "cx",
                "g",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    right(list()),
                    field(
                        Type.SET,
                        lambda("st",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("hydra.lib.sets.empty"),
                                    var("st"),
                                    var("cx"),
                                    var("g")),
                                lambda("jst",
                                    Eithers.bind(
                                        apply(
                                            ref(Utils.javaTypeToJavaReferenceType),
                                            var("jst"),
                                            var("cx")),
                                        lambda("rt",
                                            right(
                                                list(
                                                    inject(TypeArgument.TYPE_,
                                                        TypeArgument.REFERENCE,
                                                        var("rt")))))))))),
                    field(
                        Type.LIST,
                        lambda("lt_",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("hydra.lib.sets.empty"),
                                    var("lt_"),
                                    var("cx"),
                                    var("g")),
                                lambda("jlt",
                                    Eithers.bind(
                                        apply(
                                            ref(Utils.javaTypeToJavaReferenceType),
                                            var("jlt"),
                                            var("cx")),
                                        lambda("rt",
                                            right(
                                                list(
                                                    inject(TypeArgument.TYPE_,
                                                        TypeArgument.REFERENCE,
                                                        var("rt")))))))))),
                    field(
                        Type.MAYBE,
                        lambda("mt",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("hydra.lib.sets.empty"),
                                    var("mt"),
                                    var("cx"),
                                    var("g")),
                                lambda("jmt",
                                    Eithers.bind(
                                        apply(
                                            ref(Utils.javaTypeToJavaReferenceType),
                                            var("jmt"),
                                            var("cx")),
                                        lambda("rt",
                                            right(
                                                list(
                                                    inject(TypeArgument.TYPE_,
                                                        TypeArgument.REFERENCE,
                                                        var("rt")))))))))),
                    field(
                        Type.MAP,
                        lambda("mp",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("hydra.lib.sets.empty"),
                                    proj(MapType.TYPE_, MapType.KEYS, "mp"),
                                    var("cx"),
                                    var("g")),
                                lambda("jkt",
                                    Eithers.bind(
                                        apply(
                                            ref(Utils.javaTypeToJavaReferenceType),
                                            var("jkt"),
                                            var("cx")),
                                        lambda("rk",
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeType),
                                                    var("aliases"),
                                                    var("hydra.lib.sets.empty"),
                                                    proj(MapType.TYPE_, MapType.VALUES, "mp"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jvt",
                                                    Eithers.bind(
                                                        apply(
                                                            ref(Utils.javaTypeToJavaReferenceType),
                                                            var("jvt"),
                                                            var("cx")),
                                                        lambda("rv",
                                                            right(
                                                                list(
                                                                    inject(
                                                                        TypeArgument.TYPE_,
                                                                        TypeArgument.REFERENCE,
                                                                        var("rk")),
                                                                    inject(
                                                                        TypeArgument.TYPE_,
                                                                        TypeArgument.REFERENCE,
                                                                        var("rv")))))))))))))))));

    public static final Def encodeNullaryPrimitiveByName = def(
        "encodeNullaryPrimitiveByName",
        () -> lambda(
                "env",
                "typ",
                "name",
                "cx",
                "g",
                let("aliases",
                    proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env"),
                    Eithers.bind(
                        apply(
                            ref(Coder.encodeNullaryConstant_typeArgsFromReturnType),
                            var("aliases"),
                            var("typ"),
                            var("cx"),
                            var("g")),
                        lambda("targs",
                            Logic.ifElse(
                                Lists.null_(var("targs")),
                                let("header",
                                    inject(MethodInvocation_Header.TYPE_,
                                        MethodInvocation_Header.SIMPLE,
                                        wrap(MethodName.TYPE_,
                                            apply(
                                                ref(Coder.elementJavaIdentifier),
                                                bool(true),
                                                bool(false),
                                                var("aliases"),
                                                var("name")))),
                                    right(
                                        apply(
                                            ref(Utils.javaMethodInvocationToJavaExpression),
                                            record(MethodInvocation.TYPE_,
                                                field(
                                                    MethodInvocation.HEADER,
                                                    var("header")),
                                                field(
                                                    MethodInvocation.ARGUMENTS,
                                                    list()))))),
                                let(
                                    field("fullName",
                                        apply(
                                            unwrap(Identifier.TYPE_),
                                            apply(
                                                ref(Coder.elementJavaIdentifier),
                                                bool(true),
                                                bool(false),
                                                var("aliases"),
                                                var("name")))),
                                    field("parts",
                                        Strings.splitOn(string("."), var("fullName"))),
                                    field("className",
                                        wrap(Identifier.TYPE_,
                                            Strings.intercalate(
                                                string("."),
                                                Maybes.fromMaybe(
                                                    list(),
                                                    Lists.maybeInit(var("parts")))))),
                                    field("methodName",
                                        wrap(Identifier.TYPE_,
                                            Maybes.fromMaybe(
                                                var("fullName"),
                                                Lists.maybeLast(var("parts"))))),
                                    right(
                                        apply(
                                            ref(Utils.javaMethodInvocationToJavaExpression),
                                            apply(
                                                ref(Utils.methodInvocationStaticWithTypeArgs),
                                                var("className"),
                                                var("methodName"),
                                                var("targs"),
                                                list()))))))))));

    public static final Def encodeTerm = def(
        "encodeTerm",
        () -> lambda(
                "env",
                "term",
                "cx",
                "g",
                apply(
                    ref(Coder.encodeTermInternal),
                    var("env"),
                    list(),
                    list(),
                    var("term"),
                    var("cx"),
                    var("g"))));

    public static final Def encodeTermDefinition = def(
        "encodeTermDefinition",
        () -> lambda(
                "env",
                "tdef",
                "cx",
                "g",
                let(
                    field("name",
                        proj(TermDefinition.TYPE_, TermDefinition.NAME, "tdef")),
                    field("term0",
                        proj(TermDefinition.TYPE_, TermDefinition.TERM, "tdef")),
                    Eithers.bind(
                        apply(
                            var("hydra.annotations.getTermDescription"),
                            var("cx"),
                            var("g"),
                            var("term0")),
                        lambda("mDoc",
                            let(
                                field("ts",
                                    Maybes.maybe(
                                        record(TypeScheme.TYPE_,
                                            field(TypeScheme.VARIABLES, list()),
                                            field(
                                                TypeScheme.BODY,
                                                inject(Type.TYPE_,
                                                    Type.VARIABLE,
                                                    wrap(Name.TYPE_,
                                                        string("hydra.core.Unit")))),
                                            field(TypeScheme.CONSTRAINTS, nothing())),
                                        lambda("sig",
                                            apply(var("hydra.scoping.termSignatureToTypeScheme"),
                                                var("sig"))),
                                        proj(TermDefinition.TYPE_, TermDefinition.SIGNATURE, "tdef"))),
                                field("term",
                                    apply(var("hydra.variables.unshadowVariables"), var("term0"))),
                                Eithers.bind(
                                    apply(
                                        ref(Coder.analyzeJavaFunction),
                                        var("env"),
                                        var("term"),
                                        var("cx"),
                                        var("g")),
                                    lambda("fs",
                                        let(
                                            java.util.Arrays.asList(
    field("schemeVars",
                                                Lists.filter(
                                                    lambda("v",
                                                        apply(ref(Coder.isSimpleName), var("v"))),
                                                    proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
    field("termVars",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.TYPE_PARAMS, "fs")),
    field("schemeTypeVars",
                                                apply(
                                                    ref(Coder.collectTypeVars),
                                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"))),
    field("usedSchemeVars",
                                                Lists.filter(
                                                    lambda("v",
                                                        Sets.member(var("v"), var("schemeTypeVars"))),
                                                    var("schemeVars"))),
    field("tparams",
                                                Logic.ifElse(
                                                    Lists.null_(var("usedSchemeVars")),
                                                    var("termVars"),
                                                    var("usedSchemeVars"))),
    field("params",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.PARAMS, "fs")),
    field("bindings",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.BINDINGS, "fs")),
    field("body",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.BODY, "fs")),
    field("doms",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.DOMAINS, "fs")),
    field("env2",
                                                proj(FunctionStructure.TYPE_, FunctionStructure.ENVIRONMENT, "fs")),
    field("schemeType",
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
    field("numParams",
                                                Lists.length(var("params"))),
    field("peelResult",
                                                apply(
                                                    ref(Coder.peelDomainsAndCod),
                                                    var("numParams"),
                                                    var("schemeType"))),
    field("schemeDoms",
                                                Pairs.first(var("peelResult"))),
    field("cod",
                                                Pairs.second(var("peelResult"))),
    field("schemeVarSet",
                                                Sets.fromList(var("tparams")))),
                                            Eithers.bind(
                                                Logic.ifElse(
                                                    Lists.null_(var("tparams")),
                                                    right(var("hydra.lib.maps.empty")),
                                                    apply(
                                                        ref(Coder.buildSubstFromAnnotations),
                                                        var("schemeVarSet"),
                                                        var("term"),
                                                        var("cx"),
                                                        var("g"))),
                                                lambda("typeVarSubst",
                                                    let(
                                                        java.util.Arrays.asList(
    field("overgenSubst",
                                                            apply(
                                                                ref(Coder.detectAccumulatorUnification),
                                                                var("schemeDoms"),
                                                                var("cod"),
                                                                var("tparams"))),
    field("overgenVarSubst",
                                                            Maps.fromList(
                                                                Maybes.cat(
                                                                    Lists.map(
                                                                        lambda("entry",
                                                                            let(
                                                                                field("k",
                                                                                    Pairs.first(
                                                                                        var("entry"))),
                                                                                field("v",
                                                                                    Pairs.second(
                                                                                        var("entry"))),
                                                                                casesWithDefault(
                                                                                    Type.TYPE_,
                                                                                    var("v"),
                                                                                    nothing(),
                                                                                    field(
                                                                                        Type.VARIABLE,
                                                                                        lambda("n",
                                                                                            just(
                                                                                                pair(
                                                                                                    var("k"),
                                                                                                    var("n")))))))),
                                                                        Maps.toList(
                                                                            var("overgenSubst")))))),
    field("fixedCod",
                                                            Logic.ifElse(
                                                                Maps.null_(var("overgenSubst")),
                                                                var("cod"),
                                                                apply(
                                                                    ref(Coder.substituteTypeVarsWithTypes),
                                                                    var("overgenSubst"),
                                                                    var("cod")))),
    field("fixedDoms",
                                                            Logic.ifElse(
                                                                Maps.null_(var("overgenSubst")),
                                                                var("schemeDoms"),
                                                                Lists.map(
                                                                    lambda("d",
                                                                        apply(
                                                                            ref(Coder.substituteTypeVarsWithTypes),
                                                                            var("overgenSubst"),
                                                                            var("d"))),
                                                                    var("schemeDoms")))),
    field("fixedTparams",
                                                            Logic.ifElse(
                                                                Maps.null_(var("overgenSubst")),
                                                                var("tparams"),
                                                                Lists.filter(
                                                                    lambda("v",
                                                                        Logic.not_(
                                                                            Maps.member(
                                                                                var("v"),
                                                                                var("overgenSubst")))),
                                                                    var("tparams")))),
    field("constraints",
                                                            Maybes.fromMaybe(
                                                                var("hydra.lib.maps.empty"),
                                                                proj(TypeScheme.TYPE_, TypeScheme.CONSTRAINTS, "ts"))),
    field("jparams",
                                                            Lists.map(
                                                                lambda("v",
                                                                    apply(
                                                                        ref(Utils.javaTypeParameter),
                                                                        apply(
                                                                            var("hydra.formatting.capitalize"),
                                                                            apply(
                                                                                unwrap(Name.TYPE_),
                                                                                var("v"))))),
                                                                var("fixedTparams"))),
    field("aliases2base",
                                                            proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env2")),
    field("trustedVars",
                                                            Sets.unions(
                                                                Lists.map(
                                                                    lambda("d",
                                                                        apply(
                                                                            ref(Coder.collectTypeVars),
                                                                            var("d"))),
                                                                    Lists.concat2(
                                                                        var("fixedDoms"),
                                                                        list(var("fixedCod")))))),
    field("fixedSchemeVarSet",
                                                            Sets.fromList(var("fixedTparams"))),
    field("aliases2",
                                                            record(
                                                                Aliases.TYPE_,
                                                                field(
                                                                    Aliases.CURRENT_NAMESPACE,
                                                                    proj(Aliases.TYPE_, Aliases.CURRENT_NAMESPACE, "aliases2base")),
                                                                field(
                                                                    Aliases.PACKAGES,
                                                                    proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases2base")),
                                                                field(
                                                                    Aliases.BRANCH_VARS,
                                                                    proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases2base")),
                                                                field(
                                                                    Aliases.RECURSIVE_VARS,
                                                                    proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases2base")),
                                                                field(
                                                                    Aliases.IN_SCOPE_TYPE_PARAMS,
                                                                    var("fixedSchemeVarSet")),
                                                                field(
                                                                    Aliases.POLYMORPHIC_LOCALS,
                                                                    proj(Aliases.TYPE_, Aliases.POLYMORPHIC_LOCALS, "aliases2base")),
                                                                field(
                                                                    Aliases.IN_SCOPE_JAVA_VARS,
                                                                    proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases2base")),
                                                                field(
                                                                    Aliases.VAR_RENAMES,
                                                                    proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases2base")),
                                                                field(
                                                                    Aliases.LAMBDA_VARS,
                                                                    Sets.union(
                                                                        proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases2base"),
                                                                        Sets.fromList(var("params")))),
                                                                field(
                                                                    Aliases.TYPE_VAR_SUBST,
                                                                    Maps.union(
                                                                        var("overgenVarSubst"),
                                                                        var("typeVarSubst"))),
                                                                field(
                                                                    Aliases.TRUSTED_TYPE_VARS,
                                                                    Sets.intersection(
                                                                        var("trustedVars"),
                                                                        var("fixedSchemeVarSet"))),
                                                                field(
                                                                    Aliases.METHOD_CODOMAIN,
                                                                    just(var("fixedCod"))),
                                                                field(
                                                                    Aliases.THUNKED_VARS,
                                                                    proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases2base")))),
    field("env2WithTypeParams",
                                                            record(
                                                                JavaEnvironment.TYPE_,
                                                                field(
                                                                    JavaEnvironment.ALIASES,
                                                                    var("aliases2")),
                                                                field(
                                                                    JavaEnvironment.GRAPH,
                                                                    proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env2"))))),
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Coder.bindingsToStatements),
                                                                var("env2WithTypeParams"),
                                                                var("bindings"),
                                                                var("cx"),
                                                                var("g")),
                                                            lambda("bindResult",
                                                                let(
                                                                    field("bindingStmts",
                                                                        Pairs.first(
                                                                            var("bindResult"))),
                                                                    field("env3",
                                                                        Pairs.second(
                                                                            var("bindResult"))),
                                                                    Eithers.bind(
                                                                        Logic.ifElse(
                                                                            Maps.null_(
                                                                                var("overgenSubst")),
                                                                            right(var("body")),
                                                                            apply(
                                                                                ref(Coder.applyOvergenSubstToTermAnnotations),
                                                                                var("overgenSubst"),
                                                                                var("body"),
                                                                                var("cx"),
                                                                                var("g"))),
                                                                        lambda("body'",
                                                                            let("annotatedBody",
                                                                                apply(
                                                                                    ref(Coder.propagateTypesInAppChain),
                                                                                    var("fixedCod"),
                                                                                    var("fixedCod"),
                                                                                    var("body'")),
                                                                                Eithers.bind(
                                                                                    Eithers.mapList(
                                                                                        lambda(
                                                                                            "pair",
                                                                                            Eithers.bind(
                                                                                                apply(
                                                                                                    ref(Coder.encodeType),
                                                                                                    var("aliases2"),
                                                                                                    var("hydra.lib.sets.empty"),
                                                                                                    Pairs.first(
                                                                                                        var("pair")),
                                                                                                    var("cx"),
                                                                                                    var("g")),
                                                                                                lambda(
                                                                                                    "jdom",
                                                                                                    right(
                                                                                                        apply(
                                                                                                            ref(Utils.javaTypeToJavaFormalParameter),
                                                                                                            var("jdom"),
                                                                                                            Pairs.second(
                                                                                                                var("pair"))))))),
                                                                                        Lists.zip(
                                                                                            var("fixedDoms"),
                                                                                            var("params"))),
                                                                                    lambda(
                                                                                        "jformalParams",
                                                                                        Eithers.bind(
                                                                                            apply(
                                                                                                ref(Coder.encodeType),
                                                                                                var("aliases2"),
                                                                                                var("hydra.lib.sets.empty"),
                                                                                                var("fixedCod"),
                                                                                                var("cx"),
                                                                                                var("g")),
                                                                                            lambda(
                                                                                                "jcod",
                                                                                                let(
                                                                                                    field(
                                                                                                        "result",
                                                                                                        apply(
                                                                                                            ref(Utils.javaTypeToJavaResult),
                                                                                                            var("jcod"))),
                                                                                                    field(
                                                                                                        "mods",
                                                                                                        list(
                                                                                                            inject(
                                                                                                                InterfaceMethodModifier.TYPE_,
                                                                                                                InterfaceMethodModifier.STATIC,
                                                                                                                unit()))),
                                                                                                    field(
                                                                                                        "jname",
                                                                                                        apply(
                                                                                                            ref(Utils.sanitizeJavaName),
                                                                                                            apply(
                                                                                                                var("hydra.formatting.decapitalize"),
                                                                                                                apply(
                                                                                                                    var("hydra.names.localNameOf"),
                                                                                                                    var("name"))))),
                                                                                                    field(
                                                                                                        "isTCO",
                                                                                                        bool(false)),
                                                                                                    Eithers.bind(
                                                                                                        Logic.ifElse(
                                                                                                            var("isTCO"),
                                                                                                            let(
                                                                                                                field(
                                                                                                                    "tcoSuffix",
                                                                                                                    string("_tco")),
                                                                                                                field(
                                                                                                                    "snapshotNames",
                                                                                                                    Lists.map(
                                                                                                                        lambda(
                                                                                                                            "p",
                                                                                                                            wrap(
                                                                                                                                Name.TYPE_,
                                                                                                                                Strings.cat2(
                                                                                                                                    apply(
                                                                                                                                        unwrap(Name.TYPE_),
                                                                                                                                        var("p")),
                                                                                                                                    var("tcoSuffix")))),
                                                                                                                        var("params"))),
                                                                                                                field(
                                                                                                                    "tcoVarRenames",
                                                                                                                    Maps.fromList(
                                                                                                                        Lists.zip(
                                                                                                                            var("params"),
                                                                                                                            var("snapshotNames")))),
                                                                                                                field(
                                                                                                                    "snapshotDecls",
                                                                                                                    Lists.map(
                                                                                                                        lambda(
                                                                                                                            "pair",
                                                                                                                            apply(
                                                                                                                                ref(Utils.finalVarDeclarationStatement),
                                                                                                                                apply(
                                                                                                                                    ref(Utils.variableToJavaIdentifier),
                                                                                                                                    Pairs.second(
                                                                                                                                        var("pair"))),
                                                                                                                                apply(
                                                                                                                                    ref(Utils.javaIdentifierToJavaExpression),
                                                                                                                                    apply(
                                                                                                                                        ref(Utils.variableToJavaIdentifier),
                                                                                                                                        Pairs.first(
                                                                                                                                            var("pair")))))),
                                                                                                                        Lists.zip(
                                                                                                                            var("params"),
                                                                                                                            var("snapshotNames")))),
                                                                                                                field(
                                                                                                                    "tcoBody",
                                                                                                                    Logic.ifElse(
                                                                                                                        Lists.null_(
                                                                                                                            var("bindings")),
                                                                                                                        var("annotatedBody"),
                                                                                                                        inject(
                                                                                                                            Term.TYPE_,
                                                                                                                            Term.LET,
                                                                                                                            record(
                                                                                                                                Let.TYPE_,
                                                                                                                                field(
                                                                                                                                    Let.BINDINGS,
                                                                                                                                    var("bindings")),
                                                                                                                                field(
                                                                                                                                    Let.BODY,
                                                                                                                                    var("annotatedBody")))))),
                                                                                                                Eithers.bind(
                                                                                                                    apply(
                                                                                                                        ref(Coder.encodeTermTCO),
                                                                                                                        var("env2WithTypeParams"),
                                                                                                                        var("name"),
                                                                                                                        var("params"),
                                                                                                                        var("tcoVarRenames"),
                                                                                                                        int32(0),
                                                                                                                        var("tcoBody"),
                                                                                                                        var("cx"),
                                                                                                                        var("g")),
                                                                                                                    lambda(
                                                                                                                        "tcoStmts",
                                                                                                                        let(
                                                                                                                            field(
                                                                                                                                "whileBodyStmts",
                                                                                                                                Lists.concat2(
                                                                                                                                    var("snapshotDecls"),
                                                                                                                                    var("tcoStmts"))),
                                                                                                                            field(
                                                                                                                                "whileBodyBlock",
                                                                                                                                inject(
                                                                                                                                    Statement.TYPE_,
                                                                                                                                    Statement.WITHOUT_TRAILING,
                                                                                                                                    inject(
                                                                                                                                        StatementWithoutTrailingSubstatement.TYPE_,
                                                                                                                                        StatementWithoutTrailingSubstatement.BLOCK,
                                                                                                                                        wrap(
                                                                                                                                            Block.TYPE_,
                                                                                                                                            var("whileBodyStmts"))))),
                                                                                                                            field(
                                                                                                                                "noCond",
                                                                                                                                nothing()),
                                                                                                                            field(
                                                                                                                                "whileStmt",
                                                                                                                                inject(
                                                                                                                                    BlockStatement.TYPE_,
                                                                                                                                    BlockStatement.STATEMENT,
                                                                                                                                    inject(
                                                                                                                                        Statement.TYPE_,
                                                                                                                                        Statement.WHILE,
                                                                                                                                        record(
                                                                                                                                            WhileStatement.TYPE_,
                                                                                                                                            field(
                                                                                                                                                WhileStatement.COND,
                                                                                                                                                var("noCond")),
                                                                                                                                            field(
                                                                                                                                                WhileStatement.BODY,
                                                                                                                                                var("whileBodyBlock")))))),
                                                                                                                            right(
                                                                                                                                list(
                                                                                                                                    var("whileStmt"))))))),
                                                                                                            Eithers.bind(
                                                                                                                apply(
                                                                                                                    ref(Coder.encodeTerm),
                                                                                                                    var("env3"),
                                                                                                                    var("annotatedBody"),
                                                                                                                    var("cx"),
                                                                                                                    var("g")),
                                                                                                                lambda(
                                                                                                                    "jbody",
                                                                                                                    let(
                                                                                                                        "returnSt",
                                                                                                                        inject(
                                                                                                                            BlockStatement.TYPE_,
                                                                                                                            BlockStatement.STATEMENT,
                                                                                                                            apply(
                                                                                                                                ref(Utils.javaReturnStatement),
                                                                                                                                just(
                                                                                                                                    var("jbody")))),
                                                                                                                        right(
                                                                                                                            Lists.concat2(
                                                                                                                                var("bindingStmts"),
                                                                                                                                list(
                                                                                                                                    var("returnSt")))))))),
                                                                                                        lambda(
                                                                                                            "methodBody",
                                                                                                            let(
                                                                                                                "imdMember",
                                                                                                                apply(
                                                                                                                    ref(Utils.interfaceMethodDeclaration),
                                                                                                                    var("mods"),
                                                                                                                    var("jparams"),
                                                                                                                    var("jname"),
                                                                                                                    var("jformalParams"),
                                                                                                                    var("result"),
                                                                                                                    just(
                                                                                                                        var("methodBody"))),
                                                                                                                right(
                                                                                                                    Maybes.maybe(
                                                                                                                        apply(
                                                                                                                            ref(Coder.noInterfaceComment),
                                                                                                                            var("imdMember")),
                                                                                                                        lambda(
                                                                                                                            "doc",
                                                                                                                            apply(
                                                                                                                                ref(Coder.withInterfaceCommentString),
                                                                                                                                var("doc"),
                                                                                                                                var("imdMember"))),
                                                                                                                        var("mDoc")))))))))))))))))))))))))))));

    public static final Def encodeTermInternal = def(
        "encodeTermInternal",
        () -> lambda(
                "env",
                "anns",
                "tyapps",
                "term",
                "cx",
                "g0",
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("g",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")),
                    field("encode",
                        lambda("t",
                            apply(ref(Coder.encodeTerm), var("env"), var("t"), var("cx"), var("g")))),
                    casesWithDefault(Term.TYPE_,
                        var("term"),
                        right(
                            apply(
                                ref(Coder.encodeLiteral),
                                inject(Literal.TYPE_,
                                    Literal.STRING,
                                    string("Unimplemented term variant")))),
                        field(
                            Term.ANNOTATED,
                            lambda("at",
                                apply(
                                    ref(Coder.encodeTermInternal),
                                    var("env"),
                                    Lists.cons(
                                        proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at"),
                                        var("anns")),
                                    var("tyapps"),
                                    proj(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY, "at"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.APPLICATION,
                            lambda("app",
                                apply(
                                    ref(Coder.encodeApplication),
                                    var("env"),
                                    var("app"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.EITHER,
                            lambda("et",
                                Eithers.bind(
                                    Logic.ifElse(
                                        Lists.null_(var("tyapps")),
                                        right(nothing()),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.takeTypeArgs),
                                                string("either"),
                                                int32(2),
                                                var("tyapps"),
                                                var("cx"),
                                                var("g")),
                                            lambda("ta", right(just(var("ta")))))),
                                    lambda("mtargs",
                                        let("combinedAnns",
                                            Lists.foldl(
                                                lambda(
                                                    "acc",
                                                    "m",
                                                    Maps.union(var("acc"), var("m"))),
                                                var("hydra.lib.maps.empty"),
                                                var("anns")),
                                            Eithers.bind(
                                                Eithers.bimap(
                                                    lambda("__de",
                                                        inject(Error_.TYPE_,
                                                            Error_.OTHER,
                                                            wrap(OtherError.TYPE_,
                                                                apply(
                                                                    unwrap(DecodingError.TYPE_),
                                                                    var("__de"))))),
                                                    lambda("__a", var("__a")),
                                                    apply(
                                                        var("hydra.annotations.getType"),
                                                        var("g"),
                                                        var("combinedAnns"))),
                                                lambda("mEitherType",
                                                    let(
                                                        field("branchTypes",
                                                            Maybes.bind(
                                                                var("mEitherType"),
                                                                lambda("etyp",
                                                                    casesWithDefault(
                                                                        Type.TYPE_,
                                                                        apply(
                                                                            var("hydra.strip.deannotateType"),
                                                                            var("etyp")),
                                                                        nothing(),
                                                                        field(
                                                                            Type.EITHER,
                                                                            lambda("et2",
                                                                                just(
                                                                                    pair(
                                                                                        proj(EitherType.TYPE_, EitherType.LEFT, "et2"),
                                                                                        proj(EitherType.TYPE_, EitherType.RIGHT, "et2"))))))))),
                                                        field("encodeWithType",
                                                            lambda(
                                                                "branchType",
                                                                "t1",
                                                                let("annotated",
                                                                    apply(
                                                                        var("hydra.annotations.setTermAnnotation"),
                                                                        var("hydra.constants.keyType"),
                                                                        just(
                                                                            apply(
                                                                                var("hydra.encode.core.type"),
                                                                                var("branchType"))),
                                                                        var("t1")),
                                                                    apply(
                                                                        ref(Coder.encodeTermInternal),
                                                                        var("env"),
                                                                        var("anns"),
                                                                        list(),
                                                                        var("annotated"),
                                                                        var("cx"),
                                                                        var("g"))))),
                                                        field("eitherCall",
                                                            lambda(
                                                                "methodName",
                                                                "expr",
                                                                Maybes.cases(
                                                                    var("mtargs"),
                                                                    apply(
                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                        apply(
                                                                            ref(Utils.methodInvocationStatic),
                                                                            wrap(
                                                                                Identifier.TYPE_,
                                                                                string("hydra.util.Either")),
                                                                            wrap(
                                                                                Identifier.TYPE_,
                                                                                var("methodName")),
                                                                            list(var("expr")))),
                                                                    lambda("targs",
                                                                        apply(
                                                                            ref(Utils.javaMethodInvocationToJavaExpression),
                                                                            apply(
                                                                                ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                                wrap(
                                                                                    Identifier.TYPE_,
                                                                                    string("hydra.util.Either")),
                                                                                wrap(
                                                                                    Identifier.TYPE_,
                                                                                    var("methodName")),
                                                                                var("targs"),
                                                                                list(var("expr")))))))),
                                                        Eithers.either_(
                                                            lambda("term1",
                                                                Eithers.bind(
                                                                    Maybes.cases(
                                                                        var("branchTypes"),
                                                                        apply(
                                                                            var("encode"),
                                                                            var("term1")),
                                                                        lambda("bt",
                                                                            apply(
                                                                                var("encodeWithType"),
                                                                                Pairs.first(
                                                                                    var("bt")),
                                                                                var("term1")))),
                                                                    lambda("expr",
                                                                        right(
                                                                            apply(
                                                                                var("eitherCall"),
                                                                                string("left"),
                                                                                var("expr")))))),
                                                            lambda("term1",
                                                                Eithers.bind(
                                                                    Maybes.cases(
                                                                        var("branchTypes"),
                                                                        apply(
                                                                            var("encode"),
                                                                            var("term1")),
                                                                        lambda("bt",
                                                                            apply(
                                                                                var("encodeWithType"),
                                                                                Pairs.second(
                                                                                    var("bt")),
                                                                                var("term1")))),
                                                                    lambda("expr",
                                                                        right(
                                                                            apply(
                                                                                var("eitherCall"),
                                                                                string("right"),
                                                                                var("expr")))))),
                                                            var("et")))))))))),
                        field(
                            Term.LAMBDA,
                            constant(
                                apply(
                                    ref(Coder.encodeFunctionFormTerm),
                                    var("env"),
                                    var("anns"),
                                    var("term"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.PROJECT,
                            constant(
                                apply(
                                    ref(Coder.encodeFunctionFormTerm),
                                    var("env"),
                                    var("anns"),
                                    var("term"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.CASES,
                            constant(
                                apply(
                                    ref(Coder.encodeFunctionFormTerm),
                                    var("env"),
                                    var("anns"),
                                    var("term"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.UNWRAP,
                            constant(
                                apply(
                                    ref(Coder.encodeFunctionFormTerm),
                                    var("env"),
                                    var("anns"),
                                    var("term"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Term.LET,
                            lambda("lt",
                                let(
                                    field("bindings",
                                        proj(Let.TYPE_, Let.BINDINGS, "lt")),
                                    field("body",
                                        proj(Let.TYPE_, Let.BODY, "lt")),
                                    Logic.ifElse(
                                        Lists.null_(var("bindings")),
                                        apply(
                                            ref(Coder.encodeTermInternal),
                                            var("env"),
                                            var("anns"),
                                            list(),
                                            var("body"),
                                            var("cx"),
                                            var("g")),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.bindingsToStatements),
                                                var("env"),
                                                var("bindings"),
                                                var("cx"),
                                                var("g")),
                                            lambda("bindResult",
                                                let(
                                                    field("bindingStmts",
                                                        Pairs.first(var("bindResult"))),
                                                    field("env2",
                                                        Pairs.second(var("bindResult"))),
                                                    Eithers.bind(
                                                        apply(
                                                            ref(Coder.encodeTermInternal),
                                                            var("env2"),
                                                            var("anns"),
                                                            list(),
                                                            var("body"),
                                                            var("cx"),
                                                            var("g")),
                                                        lambda("jbody",
                                                            let(
                                                                field("returnSt",
                                                                    inject(
                                                                        BlockStatement.TYPE_,
                                                                        BlockStatement.STATEMENT,
                                                                        apply(
                                                                            ref(Utils.javaReturnStatement),
                                                                            just(var("jbody"))))),
                                                                field("block",
                                                                    wrap(
                                                                        Block.TYPE_,
                                                                        Lists.concat2(
                                                                            var("bindingStmts"),
                                                                            list(var("returnSt"))))),
                                                                field("nullaryLambda",
                                                                    inject(
                                                                        Expression.TYPE_,
                                                                        Expression.LAMBDA,
                                                                        record(
                                                                            LambdaExpression.TYPE_,
                                                                            field(
                                                                                LambdaExpression.PARAMETERS,
                                                                                inject(
                                                                                    LambdaParameters.TYPE_,
                                                                                    LambdaParameters.TUPLE,
                                                                                    list())),
                                                                            field(
                                                                                LambdaExpression.BODY,
                                                                                inject(
                                                                                    LambdaBody.TYPE_,
                                                                                    LambdaBody.BLOCK,
                                                                                    var("block")))))),
                                                                field("combinedAnns",
                                                                    Lists.foldl(
                                                                        lambda(
                                                                            "acc",
                                                                            "m",
                                                                            Maps.union(
                                                                                var("acc"),
                                                                                var("m"))),
                                                                        var("hydra.lib.maps.empty"),
                                                                        var("anns"))),
                                                                field("g2",
                                                                    proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env2")),
                                                                field("aliases2",
                                                                    proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env2")),
                                                                Eithers.bind(
                                                                    Eithers.bimap(
                                                                        lambda("__de",
                                                                            inject(
                                                                                Error_.TYPE_,
                                                                                Error_.OTHER,
                                                                                wrap(
                                                                                    OtherError.TYPE_,
                                                                                    apply(
                                                                                        unwrap(DecodingError.TYPE_),
                                                                                        var("__de"))))),
                                                                        lambda("__a", var("__a")),
                                                                        apply(
                                                                            var("hydra.annotations.getType"),
                                                                            var("g"),
                                                                            var("combinedAnns"))),
                                                                    lambda("mt",
                                                                        Eithers.bind(
                                                                            Maybes.cases(
                                                                                var("mt"),
                                                                                apply(
                                                                                    var("hydra.checking.typeOfTerm"),
                                                                                    var("cx"),
                                                                                    var("g2"),
                                                                                    var("body")),
                                                                                lambda("t",
                                                                                    right(var("t")))),
                                                                            lambda("letType",
                                                                                Eithers.bind(
                                                                                    apply(
                                                                                        ref(Coder.encodeType),
                                                                                        var("aliases2"),
                                                                                        var("hydra.lib.sets.empty"),
                                                                                        var("letType"),
                                                                                        var("cx"),
                                                                                        var("g")),
                                                                                    lambda(
                                                                                        "jLetType",
                                                                                        Eithers.bind(
                                                                                            apply(
                                                                                                ref(Utils.javaTypeToJavaReferenceType),
                                                                                                var("jLetType"),
                                                                                                var("cx")),
                                                                                            lambda(
                                                                                                "rt",
                                                                                                let(
                                                                                                    field(
                                                                                                        "supplierRt",
                                                                                                        inject(
                                                                                                            ReferenceType.TYPE_,
                                                                                                            ReferenceType.CLASS_OR_INTERFACE,
                                                                                                            inject(
                                                                                                                ClassOrInterfaceType.TYPE_,
                                                                                                                ClassOrInterfaceType.CLASS,
                                                                                                                apply(
                                                                                                                    ref(Utils.javaClassType),
                                                                                                                    list(
                                                                                                                        var("rt")),
                                                                                                                    ref(Names.javaUtilFunctionPackageName),
                                                                                                                    string("Supplier"))))),
                                                                                                    field(
                                                                                                        "castExpr",
                                                                                                        apply(
                                                                                                            ref(Utils.javaCastExpressionToJavaExpression),
                                                                                                            apply(
                                                                                                                ref(Utils.javaCastExpression),
                                                                                                                var("supplierRt"),
                                                                                                                apply(
                                                                                                                    ref(Utils.javaExpressionToJavaUnaryExpression),
                                                                                                                    var("nullaryLambda"))))),
                                                                                                    right(
                                                                                                        apply(
                                                                                                            ref(Utils.javaMethodInvocationToJavaExpression),
                                                                                                            apply(
                                                                                                                ref(Utils.methodInvocation),
                                                                                                                just(
                                                                                                                    right(
                                                                                                                        apply(
                                                                                                                            ref(Utils.javaExpressionToJavaPrimary),
                                                                                                                            var("castExpr")))),
                                                                                                                wrap(
                                                                                                                    Identifier.TYPE_,
                                                                                                                    string("get")),
                                                                                                                list())))))))))))))))))))))),
                        field(
                            Term.LIST,
                            lambda("els",
                                Logic.ifElse(
                                    Lists.null_(var("els")),
                                    Logic.ifElse(
                                        Lists.null_(var("tyapps")),
                                        right(
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                apply(
                                                    ref(Utils.methodInvocationStatic),
                                                    wrap(Identifier.TYPE_,
                                                        string("hydra.util.ConsList")),
                                                    wrap(Identifier.TYPE_,
                                                        string("empty")),
                                                    list()))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.takeTypeArgs),
                                                string("list"),
                                                int32(1),
                                                var("tyapps"),
                                                var("cx"),
                                                var("g")),
                                            lambda("targs",
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        apply(
                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                            wrap(Identifier.TYPE_,
                                                                string("hydra.util.ConsList")),
                                                            wrap(Identifier.TYPE_,
                                                                string("empty")),
                                                            var("targs"),
                                                            list())))))),
                                    Eithers.bind(
                                        Eithers.mapList(var("encode"), var("els")),
                                        lambda("jels",
                                            right(
                                                apply(
                                                    ref(Utils.javaMethodInvocationToJavaExpression),
                                                    apply(
                                                        ref(Utils.methodInvocationStatic),
                                                        wrap(Identifier.TYPE_,
                                                            string("hydra.util.ConsList")),
                                                        wrap(Identifier.TYPE_,
                                                            string("of")),
                                                        var("jels"))))))))),
                        field(
                            Term.LITERAL,
                            lambda("l", right(apply(ref(Coder.encodeLiteral), var("l"))))),
                        field(
                            Term.MAP,
                            lambda("m",
                                Logic.ifElse(
                                    Maps.null_(var("m")),
                                    Logic.ifElse(
                                        Lists.null_(var("tyapps")),
                                        right(
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                apply(
                                                    ref(Utils.methodInvocationStatic),
                                                    wrap(Identifier.TYPE_,
                                                        string("hydra.util.PersistentMap")),
                                                    wrap(Identifier.TYPE_,
                                                        string("empty")),
                                                    list()))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.takeTypeArgs),
                                                string("map"),
                                                int32(2),
                                                var("tyapps"),
                                                var("cx"),
                                                var("g")),
                                            lambda("targs",
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        apply(
                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                            wrap(Identifier.TYPE_,
                                                                string("hydra.util.PersistentMap")),
                                                            wrap(Identifier.TYPE_,
                                                                string("empty")),
                                                            var("targs"),
                                                            list())))))),
                                    Eithers.bind(
                                        Eithers.mapList(var("encode"), Maps.keys(var("m"))),
                                        lambda("jkeys",
                                            Eithers.bind(
                                                Eithers.mapList(var("encode"), Maps.elems(var("m"))),
                                                lambda("jvals",
                                                    let("pairExprs",
                                                        Lists.map(
                                                            lambda("kv",
                                                                apply(
                                                                    ref(Utils.javaMethodInvocationToJavaExpression),
                                                                    apply(
                                                                        ref(Utils.methodInvocationStatic),
                                                                        wrap(
                                                                            Identifier.TYPE_,
                                                                            string("java.util.Map")),
                                                                        wrap(
                                                                            Identifier.TYPE_,
                                                                            string("entry")),
                                                                        list(
                                                                            Pairs.first(var("kv")),
                                                                            Pairs.second(var("kv")))))),
                                                            Lists.zip(var("jkeys"), var("jvals"))),
                                                        right(
                                                            apply(
                                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                                apply(
                                                                    ref(Utils.methodInvocationStatic),
                                                                    wrap(
                                                                        Identifier.TYPE_,
                                                                        string("hydra.util.PersistentMap")),
                                                                    wrap(
                                                                        Identifier.TYPE_,
                                                                        string("ofEntries")),
                                                                    var("pairExprs")))))))))))),
                        field(
                            Term.MAYBE,
                            lambda("mt",
                                Maybes.cases(
                                    var("mt"),
                                    Logic.ifElse(
                                        Lists.null_(var("tyapps")),
                                        right(
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                apply(
                                                    ref(Utils.methodInvocationStatic),
                                                    wrap(Identifier.TYPE_,
                                                        string("hydra.util.Maybe")),
                                                    wrap(Identifier.TYPE_,
                                                        string("nothing")),
                                                    list()))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.takeTypeArgs),
                                                string("maybe"),
                                                int32(1),
                                                var("tyapps"),
                                                var("cx"),
                                                var("g")),
                                            lambda("targs",
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        apply(
                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                            wrap(Identifier.TYPE_,
                                                                string("hydra.util.Maybe")),
                                                            wrap(Identifier.TYPE_,
                                                                string("nothing")),
                                                            var("targs"),
                                                            list())))))),
                                    lambda("term1",
                                        Eithers.bind(
                                            apply(var("encode"), var("term1")),
                                            lambda("expr",
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        apply(
                                                            ref(Utils.methodInvocationStatic),
                                                            wrap(Identifier.TYPE_,
                                                                string("hydra.util.Maybe")),
                                                            wrap(Identifier.TYPE_,
                                                                string("just")),
                                                            list(var("expr"))))))))))),
                        field(
                            Term.PAIR,
                            lambda("p",
                                Eithers.bind(
                                    apply(var("encode"), Pairs.first(var("p"))),
                                    lambda("jterm1",
                                        Eithers.bind(
                                            apply(var("encode"), Pairs.second(var("p"))),
                                            lambda("jterm2",
                                                Eithers.bind(
                                                    Logic.ifElse(
                                                        Lists.null_(var("tyapps")),
                                                        right(nothing()),
                                                        Eithers.bind(
                                                            Eithers.mapList(
                                                                lambda("jt",
                                                                    apply(
                                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                                        var("jt"),
                                                                        var("cx"))),
                                                                var("tyapps")),
                                                            lambda("rts",
                                                                right(
                                                                    just(
                                                                        inject(
                                                                            TypeArgumentsOrDiamond.TYPE_,
                                                                            TypeArgumentsOrDiamond.ARGUMENTS,
                                                                            Lists.map(
                                                                                lambda("rt",
                                                                                    inject(
                                                                                        TypeArgument.TYPE_,
                                                                                        TypeArgument.REFERENCE,
                                                                                        var("rt"))),
                                                                                var("rts")))))))),
                                                    lambda("mtargs",
                                                        right(
                                                            apply(
                                                                ref(Utils.javaConstructorCall),
                                                                apply(
                                                                    ref(Utils.javaConstructorName),
                                                                    wrap(
                                                                        Identifier.TYPE_,
                                                                        string("hydra.util.Pair")),
                                                                    var("mtargs")),
                                                                list(var("jterm1"), var("jterm2")),
                                                                nothing())))))))))),
                        field(
                            Term.RECORD,
                            lambda("rec",
                                let(
                                    field("recName",
                                        proj(Record.TYPE_, Record.TYPE_NAME, "rec")),
                                    field("mRecordType",
                                        Eithers.either_(
                                            constant(nothing()),
                                            lambda("t", just(var("t"))),
                                            apply(
                                                var("hydra.resolution.requireType"),
                                                var("cx"),
                                                var("g"),
                                                var("recName")))),
                                    field("strippedRecTyp",
                                        Maybes.map(
                                            lambda("recTyp",
                                                apply(
                                                    ref(Coder.stripForalls),
                                                    apply(
                                                        var("hydra.strip.deannotateType"),
                                                        var("recTyp")))),
                                            var("mRecordType"))),
                                    field("mFieldTypeMap",
                                        Maybes.bind(
                                            var("strippedRecTyp"),
                                            lambda("bodyTyp",
                                                casesWithDefault(Type.TYPE_,
                                                    var("bodyTyp"),
                                                    nothing(),
                                                    field(
                                                        Type.RECORD,
                                                        lambda("rt",
                                                            just(
                                                                Maps.fromList(
                                                                    Lists.map(
                                                                        lambda("ft",
                                                                            pair(
                                                                                proj(FieldType.TYPE_, FieldType.NAME, "ft"),
                                                                                proj(FieldType.TYPE_, FieldType.TYPE, "ft"))),
                                                                        var("rt")))))))))),
                                    field("combinedAnnsRec",
                                        Lists.foldl(
                                            lambda(
                                                "acc",
                                                "m",
                                                Maps.union(var("acc"), var("m"))),
                                            var("hydra.lib.maps.empty"),
                                            var("anns"))),
                                    Eithers.bind(
                                        Eithers.bimap(
                                            lambda("__de",
                                                inject(Error_.TYPE_,
                                                    Error_.OTHER,
                                                    wrap(OtherError.TYPE_,
                                                        apply(
                                                            unwrap(DecodingError.TYPE_),
                                                            var("__de"))))),
                                            lambda("__a", var("__a")),
                                            apply(
                                                var("hydra.annotations.getType"),
                                                var("g"),
                                                var("combinedAnnsRec"))),
                                        lambda("mAnnotType",
                                            let(
                                                field("mTypeSubst",
                                                    Maybes.bind(
                                                        var("mAnnotType"),
                                                        lambda("annTyp",
                                                            Maybes.bind(
                                                                var("mRecordType"),
                                                                lambda("recTyp",
                                                                    let(
                                                                        field("args",
                                                                            apply(
                                                                                ref(Coder.extractTypeApplicationArgs),
                                                                                apply(
                                                                                    var("hydra.strip.deannotateType"),
                                                                                    var("annTyp")))),
                                                                        field("params",
                                                                            apply(
                                                                                ref(Coder.collectForallParams),
                                                                                apply(
                                                                                    var("hydra.strip.deannotateType"),
                                                                                    var("recTyp")))),
                                                                        Logic.ifElse(
                                                                            Logic.or_(
                                                                                Lists.null_(
                                                                                    var("args")),
                                                                                Logic.not_(
                                                                                    Equality.equal(
                                                                                        Lists.length(
                                                                                            var("args")),
                                                                                        Lists.length(
                                                                                            var("params"))))),
                                                                            nothing(),
                                                                            just(
                                                                                Maps.fromList(
                                                                                    Lists.zip(
                                                                                        var("params"),
                                                                                        var("args"))))))))))),
                                                field("encodeField",
                                                    lambda("fld",
                                                        Maybes.cases(
                                                            var("mFieldTypeMap"),
                                                            apply(
                                                                var("encode"),
                                                                proj(Field.TYPE_, Field.TERM, "fld")),
                                                            lambda("ftmap",
                                                                let("mftyp",
                                                                    Maps.lookup(
                                                                        proj(Field.TYPE_, Field.NAME, "fld"),
                                                                        var("ftmap")),
                                                                    Maybes.cases(
                                                                        var("mftyp"),
                                                                        apply(
                                                                            var("encode"),
                                                                            proj(Field.TYPE_, Field.TERM, "fld")),
                                                                        lambda("ftyp",
                                                                            let(
                                                                                field(
                                                                                    "resolvedType",
                                                                                    Maybes.cases(
                                                                                        var("mTypeSubst"),
                                                                                        var("ftyp"),
                                                                                        lambda(
                                                                                            "subst",
                                                                                            apply(
                                                                                                ref(Coder.applySubstFull),
                                                                                                var("subst"),
                                                                                                var("ftyp"))))),
                                                                                field(
                                                                                    "annotatedFieldTerm",
                                                                                    apply(
                                                                                        var("hydra.annotations.setTermAnnotation"),
                                                                                        var("hydra.constants.keyType"),
                                                                                        just(
                                                                                            apply(
                                                                                                var("hydra.encode.core.type"),
                                                                                                var("resolvedType"))),
                                                                                        proj(Field.TYPE_, Field.TERM, "fld"))),
                                                                                apply(
                                                                                    ref(Coder.encodeTermInternal),
                                                                                    var("env"),
                                                                                    var("anns"),
                                                                                    list(),
                                                                                    var("annotatedFieldTerm"),
                                                                                    var("cx"),
                                                                                    var("g")))))))))),
                                                Eithers.bind(
                                                    Eithers.mapList(
                                                        var("encodeField"),
                                                        proj(Record.TYPE_, Record.FIELDS, "rec")),
                                                    lambda("fieldExprs",
                                                        let("consId",
                                                            apply(
                                                                ref(Utils.nameToJavaName),
                                                                var("aliases"),
                                                                var("recName")),
                                                            Eithers.bind(
                                                                Logic.ifElse(
                                                                    Logic.not_(
                                                                        Lists.null_(var("tyapps"))),
                                                                    Eithers.bind(
                                                                        Eithers.mapList(
                                                                            lambda("jt",
                                                                                apply(
                                                                                    ref(Utils.javaTypeToJavaReferenceType),
                                                                                    var("jt"),
                                                                                    var("cx"))),
                                                                            var("tyapps")),
                                                                        lambda("rts",
                                                                            right(
                                                                                just(
                                                                                    inject(
                                                                                        TypeArgumentsOrDiamond.TYPE_,
                                                                                        TypeArgumentsOrDiamond.ARGUMENTS,
                                                                                        Lists.map(
                                                                                            lambda(
                                                                                                "rt",
                                                                                                inject(
                                                                                                    TypeArgument.TYPE_,
                                                                                                    TypeArgument.REFERENCE,
                                                                                                    var("rt"))),
                                                                                            var("rts"))))))),
                                                                    let("combinedAnns",
                                                                        Lists.foldl(
                                                                            lambda(
                                                                                "acc",
                                                                                "m",
                                                                                Maps.union(
                                                                                    var("acc"),
                                                                                    var("m"))),
                                                                            var("hydra.lib.maps.empty"),
                                                                            var("anns")),
                                                                        Eithers.bind(
                                                                            Eithers.bimap(
                                                                                lambda("__de",
                                                                                    inject(
                                                                                        Error_.TYPE_,
                                                                                        Error_.OTHER,
                                                                                        wrap(
                                                                                            OtherError.TYPE_,
                                                                                            apply(
                                                                                                unwrap(DecodingError.TYPE_),
                                                                                                var("__de"))))),
                                                                                lambda("__a",
                                                                                    var("__a")),
                                                                                apply(
                                                                                    var("hydra.annotations.getType"),
                                                                                    var("g"),
                                                                                    var("combinedAnns"))),
                                                                            lambda("mtyp",
                                                                                Maybes.cases(
                                                                                    var("mtyp"),
                                                                                    right(nothing()),
                                                                                    lambda("annTyp",
                                                                                        let(
                                                                                            "typeArgs",
                                                                                            apply(
                                                                                                ref(Coder.extractTypeApplicationArgs),
                                                                                                apply(
                                                                                                    var("hydra.strip.deannotateType"),
                                                                                                    var("annTyp"))),
                                                                                            Logic.ifElse(
                                                                                                Lists.null_(
                                                                                                    var("typeArgs")),
                                                                                                right(
                                                                                                    nothing()),
                                                                                                Eithers.bind(
                                                                                                    Eithers.mapList(
                                                                                                        lambda(
                                                                                                            "t",
                                                                                                            Eithers.bind(
                                                                                                                apply(
                                                                                                                    ref(Coder.encodeType),
                                                                                                                    var("aliases"),
                                                                                                                    var("hydra.lib.sets.empty"),
                                                                                                                    var("t"),
                                                                                                                    var("cx"),
                                                                                                                    var("g")),
                                                                                                                lambda(
                                                                                                                    "jt",
                                                                                                                    apply(
                                                                                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                                                                                        var("jt"),
                                                                                                                        var("cx"))))),
                                                                                                        var("typeArgs")),
                                                                                                    lambda(
                                                                                                        "jTypeArgs",
                                                                                                        right(
                                                                                                            just(
                                                                                                                inject(
                                                                                                                    TypeArgumentsOrDiamond.TYPE_,
                                                                                                                    TypeArgumentsOrDiamond.ARGUMENTS,
                                                                                                                    Lists.map(
                                                                                                                        lambda(
                                                                                                                            "rt",
                                                                                                                            inject(
                                                                                                                                TypeArgument.TYPE_,
                                                                                                                                TypeArgument.REFERENCE,
                                                                                                                                var("rt"))),
                                                                                                                        var("jTypeArgs"))))))))))))))),
                                                                lambda("mtargs",
                                                                    right(
                                                                        apply(
                                                                            ref(Utils.javaConstructorCall),
                                                                            apply(
                                                                                ref(Utils.javaConstructorName),
                                                                                var("consId"),
                                                                                var("mtargs")),
                                                                            var("fieldExprs"),
                                                                            nothing()))))))))))))),
                        field(
                            Term.SET,
                            lambda("s",
                                Logic.ifElse(
                                    Sets.null_(var("s")),
                                    Logic.ifElse(
                                        Lists.null_(var("tyapps")),
                                        right(
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                apply(
                                                    ref(Utils.methodInvocationStatic),
                                                    wrap(Identifier.TYPE_,
                                                        string("hydra.util.PersistentSet")),
                                                    wrap(Identifier.TYPE_,
                                                        string("empty")),
                                                    list()))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.takeTypeArgs),
                                                string("set"),
                                                int32(1),
                                                var("tyapps"),
                                                var("cx"),
                                                var("g")),
                                            lambda("targs",
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        apply(
                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                            wrap(Identifier.TYPE_,
                                                                string("hydra.util.PersistentSet")),
                                                            wrap(Identifier.TYPE_,
                                                                string("empty")),
                                                            var("targs"),
                                                            list())))))),
                                    let("slist",
                                        Sets.toList(var("s")),
                                        Eithers.bind(
                                            Eithers.mapList(var("encode"), var("slist")),
                                            lambda("jels",
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        apply(
                                                            ref(Utils.methodInvocationStatic),
                                                            wrap(Identifier.TYPE_,
                                                                string("hydra.util.PersistentSet")),
                                                            wrap(Identifier.TYPE_,
                                                                string("of")),
                                                            var("jels")))))))))),
                        field(
                            Term.TYPE_LAMBDA,
                            lambda("tl",
                                apply(
                                    ref(Coder.withTypeLambda),
                                    var("env"),
                                    var("tl"),
                                    lambda("env2",
                                        let("combinedAnns",
                                            Lists.foldl(
                                                lambda(
                                                    "acc",
                                                    "m",
                                                    Maps.union(var("acc"), var("m"))),
                                                var("hydra.lib.maps.empty"),
                                                var("anns")),
                                            Eithers.bind(
                                                Eithers.bimap(
                                                    lambda("__de",
                                                        inject(Error_.TYPE_,
                                                            Error_.OTHER,
                                                            wrap(OtherError.TYPE_,
                                                                apply(
                                                                    unwrap(DecodingError.TYPE_),
                                                                    var("__de"))))),
                                                    lambda("__a", var("__a")),
                                                    apply(
                                                        var("hydra.annotations.getType"),
                                                        var("g"),
                                                        var("combinedAnns"))),
                                                lambda("mtyp",
                                                    let("annotatedBody",
                                                        Maybes.cases(
                                                            var("mtyp"),
                                                            proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl"),
                                                            lambda("t",
                                                                casesWithDefault(
                                                                    Type.TYPE_,
                                                                    var("t"),
                                                                    proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl"),
                                                                    field(
                                                                        Type.FORALL,
                                                                        lambda("fa",
                                                                            apply(
                                                                                var("hydra.annotations.setTermAnnotation"),
                                                                                var("hydra.constants.keyType"),
                                                                                just(
                                                                                    apply(
                                                                                        var("hydra.encode.core.type"),
                                                                                        proj(ForallType.TYPE_, ForallType.BODY, "fa"))),
                                                                                proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl"))))))),
                                                        apply(
                                                            ref(Coder.encodeTerm),
                                                            var("env2"),
                                                            var("annotatedBody"),
                                                            var("cx"),
                                                            var("g")))))))))),
                        field(
                            Term.INJECT,
                            lambda("inj",
                                let(
                                    field("injTypeName",
                                        proj(Injection.TYPE_, Injection.TYPE_NAME, "inj")),
                                    field("injField",
                                        proj(Injection.TYPE_, Injection.FIELD, "inj")),
                                    field("injFieldName",
                                        proj(Field.TYPE_, Field.NAME, "injField")),
                                    field("injFieldTerm",
                                        proj(Field.TYPE_, Field.TERM, "injField")),
                                    field("typeId",
                                        apply(
                                            unwrap(Identifier.TYPE_),
                                            apply(
                                                ref(Utils.nameToJavaName),
                                                var("aliases"),
                                                var("injTypeName")))),
                                    field("consId",
                                        wrap(Identifier.TYPE_,
                                            Strings.cat(
                                                list(
                                                    var("typeId"),
                                                    string("."),
                                                    apply(
                                                        ref(Utils.sanitizeJavaName),
                                                        apply(
                                                            var("hydra.formatting.capitalize"),
                                                            apply(
                                                                unwrap(Name.TYPE_),
                                                                var("injFieldName")))))))),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.isFieldUnitType),
                                            var("injTypeName"),
                                            var("injFieldName"),
                                            var("cx"),
                                            var("g")),
                                        lambda("fieldIsUnit",
                                            Eithers.bind(
                                                Logic.ifElse(
                                                    Logic.or_(
                                                        apply(
                                                            var("hydra.predicates.isUnitTerm"),
                                                            apply(
                                                                var("hydra.strip.deannotateTerm"),
                                                                var("injFieldTerm"))),
                                                        var("fieldIsUnit")),
                                                    right(list()),
                                                    Eithers.bind(
                                                        apply(var("encode"), var("injFieldTerm")),
                                                        lambda("ex", right(list(var("ex")))))),
                                                lambda("args",
                                                    right(
                                                        apply(
                                                            ref(Utils.javaConstructorCall),
                                                            apply(
                                                                ref(Utils.javaConstructorName),
                                                                var("consId"),
                                                                nothing()),
                                                            var("args"),
                                                            nothing()))))))))),
                        field(
                            Term.VARIABLE,
                            lambda("name",
                                Maybes.cases(
                                    Maps.lookup(
                                        var("name"),
                                        proj(Graph.TYPE_, Graph.PRIMITIVES, "g")),
                                    apply(
                                        ref(Coder.encodeVariable),
                                        var("env"),
                                        var("name"),
                                        var("cx"),
                                        var("g")),
                                    constant(
                                        let("combinedAnns",
                                            Lists.foldl(
                                                lambda(
                                                    "acc",
                                                    "m",
                                                    Maps.union(var("acc"), var("m"))),
                                                var("hydra.lib.maps.empty"),
                                                var("anns")),
                                            Eithers.bind(
                                                Eithers.bimap(
                                                    lambda("__de",
                                                        inject(Error_.TYPE_,
                                                            Error_.OTHER,
                                                            wrap(OtherError.TYPE_,
                                                                apply(
                                                                    unwrap(DecodingError.TYPE_),
                                                                    var("__de"))))),
                                                    lambda("__a", var("__a")),
                                                    apply(
                                                        var("hydra.annotations.getType"),
                                                        var("g"),
                                                        var("combinedAnns"))),
                                                lambda("mt",
                                                    Eithers.bind(
                                                        Maybes.cases(
                                                            var("mt"),
                                                            apply(
                                                                var("hydra.checking.typeOfTerm"),
                                                                var("cx"),
                                                                var("g"),
                                                                var("term")),
                                                            lambda("t", right(var("t")))),
                                                        lambda("typ",
                                                            casesWithDefault(Type.TYPE_,
                                                                apply(
                                                                    var("hydra.strip.deannotateType"),
                                                                    var("typ")),
                                                                apply(
                                                                    ref(Coder.encodeNullaryPrimitiveByName),
                                                                    var("env"),
                                                                    var("typ"),
                                                                    var("name"),
                                                                    var("cx"),
                                                                    var("g")),
                                                                field(
                                                                    Type.FUNCTION,
                                                                    lambda("ft",
                                                                        apply(
                                                                            ref(Coder.encodeFunctionPrimitiveByName),
                                                                            var("env"),
                                                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                                                            proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"),
                                                                            var("name"),
                                                                            var("cx"),
                                                                            var("g")))))))))))))),
                        field(
                            Term.UNIT,
                            constant(
                                right(
                                    apply(
                                        ref(Utils.javaLiteralToJavaExpression),
                                        inject(hydra.java.syntax.Literal.TYPE_,
                                            hydra.java.syntax.Literal.NULL,
                                            unit()))))),
                        field(
                            Term.WRAP,
                            lambda("wt",
                                Eithers.bind(
                                    apply(
                                        var("encode"),
                                        proj(WrappedTerm.TYPE_, WrappedTerm.BODY, "wt")),
                                    lambda("jarg",
                                        right(
                                            apply(
                                                ref(Utils.javaConstructorCall),
                                                apply(
                                                    ref(Utils.javaConstructorName),
                                                    apply(
                                                        ref(Utils.nameToJavaName),
                                                        var("aliases"),
                                                        proj(WrappedTerm.TYPE_, WrappedTerm.TYPE_NAME, "wt")),
                                                    nothing()),
                                                list(var("jarg")),
                                                nothing())))))),
                        field(
                            Term.TYPE_APPLICATION,
                            lambda("ta",
                                let(
                                    field("atyp",
                                        proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE, "ta")),
                                    field("body",
                                        proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta")),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("hydra.lib.sets.empty"),
                                            var("atyp"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jatyp",
                                            let("combinedAnns",
                                                Lists.foldl(
                                                    lambda(
                                                        "acc",
                                                        "m",
                                                        Maps.union(var("acc"), var("m"))),
                                                    var("hydra.lib.maps.empty"),
                                                    var("anns")),
                                                Eithers.bind(
                                                    Eithers.bimap(
                                                        lambda("__de",
                                                            inject(Error_.TYPE_,
                                                                Error_.OTHER,
                                                                wrap(OtherError.TYPE_,
                                                                    apply(
                                                                        unwrap(DecodingError.TYPE_),
                                                                        var("__de"))))),
                                                        lambda("__a", var("__a")),
                                                        apply(
                                                            var("hydra.annotations.getType"),
                                                            var("g"),
                                                            var("combinedAnns"))),
                                                    lambda("mtyp",
                                                        Eithers.bind(
                                                            Maybes.cases(
                                                                var("mtyp"),
                                                                apply(
                                                                    var("hydra.checking.typeOfTerm"),
                                                                    var("cx"),
                                                                    var("g"),
                                                                    var("term")),
                                                                lambda("t", right(var("t")))),
                                                            lambda("typ",
                                                                let(
                                                                    field("collected0",
                                                                        apply(
                                                                            ref(Coder.collectTypeApps0),
                                                                            var("body"),
                                                                            list(var("atyp")))),
                                                                    field("innermostBody0",
                                                                        Pairs.first(
                                                                            var("collected0"))),
                                                                    field("allTypeArgs0",
                                                                        Pairs.second(
                                                                            var("collected0"))),
                                                                    Eithers.bind(
                                                                        apply(
                                                                            ref(Coder.correctCastType),
                                                                            var("innermostBody0"),
                                                                            var("allTypeArgs0"),
                                                                            var("typ"),
                                                                            var("cx"),
                                                                            var("g")),
                                                                        lambda("correctedTyp",
                                                                            let(
                                                                                field("collected",
                                                                                    apply(
                                                                                        ref(Coder.collectTypeApps),
                                                                                        var("body"),
                                                                                        list(
                                                                                            var("atyp")))),
                                                                                field(
                                                                                    "innermostBody",
                                                                                    Pairs.first(
                                                                                        var("collected"))),
                                                                                field("allTypeArgs",
                                                                                    Pairs.second(
                                                                                        var("collected"))),
                                                                                casesWithDefault(
                                                                                    Term.TYPE_,
                                                                                    var("innermostBody"),
                                                                                    apply(
                                                                                        ref(Coder.typeAppFallbackCast),
                                                                                        var("env"),
                                                                                        var("aliases"),
                                                                                        var("anns"),
                                                                                        var("tyapps"),
                                                                                        var("jatyp"),
                                                                                        var("body"),
                                                                                        var("correctedTyp"),
                                                                                        var("cx"),
                                                                                        var("g")),
                                                                                    field(
                                                                                        Term.VARIABLE,
                                                                                        lambda(
                                                                                            "varName",
                                                                                            Eithers.bind(
                                                                                                apply(
                                                                                                    ref(Coder.classifyDataReference),
                                                                                                    var("varName"),
                                                                                                    var("cx"),
                                                                                                    var("g")),
                                                                                                lambda(
                                                                                                    "cls",
                                                                                                    apply(
                                                                                                        ref(Coder.typeAppNullaryOrHoisted),
                                                                                                        var("env"),
                                                                                                        var("aliases"),
                                                                                                        var("anns"),
                                                                                                        var("tyapps"),
                                                                                                        var("jatyp"),
                                                                                                        var("body"),
                                                                                                        var("correctedTyp"),
                                                                                                        var("varName"),
                                                                                                        var("cls"),
                                                                                                        var("allTypeArgs"),
                                                                                                        var("cx"),
                                                                                                        var("g")))))),
                                                                                    field(
                                                                                        Term.EITHER,
                                                                                        lambda(
                                                                                            "eitherTerm",
                                                                                            Logic.ifElse(
                                                                                                Equality.equal(
                                                                                                    Lists.length(
                                                                                                        var("allTypeArgs")),
                                                                                                    int32(2)),
                                                                                                let(
                                                                                                    "eitherBranchTypes",
                                                                                                    pair(
                                                                                                        Maybes.fromMaybe(
                                                                                                            var("correctedTyp"),
                                                                                                            Lists.maybeAt(
                                                                                                                int32(0),
                                                                                                                var("allTypeArgs"))),
                                                                                                        Maybes.fromMaybe(
                                                                                                            var("correctedTyp"),
                                                                                                            Lists.maybeAt(
                                                                                                                int32(1),
                                                                                                                var("allTypeArgs")))),
                                                                                                    Eithers.bind(
                                                                                                        Eithers.mapList(
                                                                                                            lambda(
                                                                                                                "t",
                                                                                                                Eithers.bind(
                                                                                                                    apply(
                                                                                                                        ref(Coder.encodeType),
                                                                                                                        var("aliases"),
                                                                                                                        var("hydra.lib.sets.empty"),
                                                                                                                        var("t"),
                                                                                                                        var("cx"),
                                                                                                                        var("g")),
                                                                                                                    lambda(
                                                                                                                        "jt",
                                                                                                                        apply(
                                                                                                                            ref(Utils.javaTypeToJavaReferenceType),
                                                                                                                            var("jt"),
                                                                                                                            var("cx"))))),
                                                                                                            var("allTypeArgs")),
                                                                                                        lambda(
                                                                                                            "jTypeArgs",
                                                                                                            let(
                                                                                                                field(
                                                                                                                    "eitherTargs",
                                                                                                                    Lists.map(
                                                                                                                        lambda(
                                                                                                                            "rt",
                                                                                                                            inject(
                                                                                                                                TypeArgument.TYPE_,
                                                                                                                                TypeArgument.REFERENCE,
                                                                                                                                var("rt"))),
                                                                                                                        var("jTypeArgs"))),
                                                                                                                field(
                                                                                                                    "encodeEitherBranch",
                                                                                                                    lambda(
                                                                                                                        "branchType",
                                                                                                                        "t1",
                                                                                                                        let(
                                                                                                                            "annotated",
                                                                                                                            apply(
                                                                                                                                var("hydra.annotations.setTermAnnotation"),
                                                                                                                                var("hydra.constants.keyType"),
                                                                                                                                just(
                                                                                                                                    apply(
                                                                                                                                        var("hydra.encode.core.type"),
                                                                                                                                        var("branchType"))),
                                                                                                                                var("t1")),
                                                                                                                            apply(
                                                                                                                                ref(Coder.encodeTermInternal),
                                                                                                                                var("env"),
                                                                                                                                var("anns"),
                                                                                                                                list(),
                                                                                                                                var("annotated"),
                                                                                                                                var("cx"),
                                                                                                                                var("g"))))),
                                                                                                                Eithers.either_(
                                                                                                                    lambda(
                                                                                                                        "term1",
                                                                                                                        Eithers.bind(
                                                                                                                            apply(
                                                                                                                                var("encodeEitherBranch"),
                                                                                                                                Pairs.first(
                                                                                                                                    var("eitherBranchTypes")),
                                                                                                                                var("term1")),
                                                                                                                            lambda(
                                                                                                                                "expr",
                                                                                                                                right(
                                                                                                                                    apply(
                                                                                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                                                                                        apply(
                                                                                                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                                                                                            wrap(
                                                                                                                                                Identifier.TYPE_,
                                                                                                                                                string("hydra.util.Either")),
                                                                                                                                            wrap(
                                                                                                                                                Identifier.TYPE_,
                                                                                                                                                string("left")),
                                                                                                                                            var("eitherTargs"),
                                                                                                                                            list(
                                                                                                                                                var("expr")))))))),
                                                                                                                    lambda(
                                                                                                                        "term1",
                                                                                                                        Eithers.bind(
                                                                                                                            apply(
                                                                                                                                var("encodeEitherBranch"),
                                                                                                                                Pairs.second(
                                                                                                                                    var("eitherBranchTypes")),
                                                                                                                                var("term1")),
                                                                                                                            lambda(
                                                                                                                                "expr",
                                                                                                                                right(
                                                                                                                                    apply(
                                                                                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                                                                                        apply(
                                                                                                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                                                                                            wrap(
                                                                                                                                                Identifier.TYPE_,
                                                                                                                                                string("hydra.util.Either")),
                                                                                                                                            wrap(
                                                                                                                                                Identifier.TYPE_,
                                                                                                                                                string("right")),
                                                                                                                                            var("eitherTargs"),
                                                                                                                                            list(
                                                                                                                                                var("expr")))))))),
                                                                                                                    var("eitherTerm")))))),
                                                                                                apply(
                                                                                                    ref(Coder.typeAppFallbackCast),
                                                                                                    var("env"),
                                                                                                    var("aliases"),
                                                                                                    var("anns"),
                                                                                                    var("tyapps"),
                                                                                                    var("jatyp"),
                                                                                                    var("body"),
                                                                                                    var("correctedTyp"),
                                                                                                    var("cx"),
                                                                                                    var("g"))))))))))))))))))))))));

    public static final Def encodeTermTCO = def(
        "encodeTermTCO",
        () -> lambda(
                java.util.Arrays.asList("env0", "funcName", "paramNames", "tcoVarRenames", "tcoDepth", "term", "cx", "g"),
                let(
                    java.util.Arrays.asList(
    field("aliases0",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env0")),
    field("env",
                        record(JavaEnvironment.TYPE_,
                            field(
                                JavaEnvironment.ALIASES,
                                record(Aliases.TYPE_,
                                    field(
                                        Aliases.CURRENT_NAMESPACE,
                                        proj(Aliases.TYPE_, Aliases.CURRENT_NAMESPACE, "aliases0")),
                                    field(
                                        Aliases.PACKAGES,
                                        proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases0")),
                                    field(
                                        Aliases.BRANCH_VARS,
                                        proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases0")),
                                    field(
                                        Aliases.RECURSIVE_VARS,
                                        proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases0")),
                                    field(
                                        Aliases.IN_SCOPE_TYPE_PARAMS,
                                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases0")),
                                    field(
                                        Aliases.POLYMORPHIC_LOCALS,
                                        proj(Aliases.TYPE_, Aliases.POLYMORPHIC_LOCALS, "aliases0")),
                                    field(
                                        Aliases.IN_SCOPE_JAVA_VARS,
                                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases0")),
                                    field(
                                        Aliases.VAR_RENAMES,
                                        Maps.union(
                                            var("tcoVarRenames"),
                                            proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases0"))),
                                    field(
                                        Aliases.LAMBDA_VARS,
                                        proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases0")),
                                    field(
                                        Aliases.TYPE_VAR_SUBST,
                                        proj(Aliases.TYPE_, Aliases.TYPE_VAR_SUBST, "aliases0")),
                                    field(
                                        Aliases.TRUSTED_TYPE_VARS,
                                        proj(Aliases.TYPE_, Aliases.TRUSTED_TYPE_VARS, "aliases0")),
                                    field(
                                        Aliases.METHOD_CODOMAIN,
                                        proj(Aliases.TYPE_, Aliases.METHOD_CODOMAIN, "aliases0")),
                                    field(
                                        Aliases.THUNKED_VARS,
                                        proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases0")))),
                            field(
                                JavaEnvironment.GRAPH,
                                proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env0")))),
    field("stripped",
                        apply(var("hydra.strip.deannotateAndDetypeTerm"), var("term"))),
    field("gathered",
                        apply(var("hydra.analysis.gatherApplications"), var("stripped"))),
    field("gatherArgs",
                        Pairs.first(var("gathered"))),
    field("gatherFun",
                        Pairs.second(var("gathered"))),
    field("strippedFun",
                        apply(var("hydra.strip.deannotateAndDetypeTerm"), var("gatherFun"))),
    field("isSelfCall",
                        casesWithDefault(Term.TYPE_,
                            var("strippedFun"),
                            bool(false),
                            field(
                                Term.VARIABLE,
                                lambda("n", Equality.equal(var("n"), var("funcName"))))))),
                    Logic.ifElse(
                        Logic.and_(
                            var("isSelfCall"),
                            Equality.equal(
                                Lists.length(var("gatherArgs")),
                                Lists.length(var("paramNames")))),
                        let(
                            field("changePairs",
                                Lists.filter(
                                    lambda("pair",
                                        Logic.not_(
                                            casesWithDefault(Term.TYPE_,
                                                apply(
                                                    var("hydra.strip.deannotateAndDetypeTerm"),
                                                    Pairs.second(var("pair"))),
                                                bool(false),
                                                field(
                                                    Term.VARIABLE,
                                                    lambda("n",
                                                        Equality.equal(
                                                            var("n"),
                                                            Pairs.first(var("pair")))))))),
                                    Lists.zip(var("paramNames"), var("gatherArgs")))),
                            field("changedParams",
                                Lists.map(var("hydra.lib.pairs.first"), var("changePairs"))),
                            Eithers.bind(
                                Eithers.mapList(
                                    lambda("pair",
                                        apply(
                                            ref(Coder.encodeTerm),
                                            var("env"),
                                            Pairs.second(var("pair")),
                                            var("cx"),
                                            var("g"))),
                                    var("changePairs")),
                                lambda("jChangedArgs",
                                    let(
                                        field("assignments",
                                            Lists.map(
                                                lambda("pair",
                                                    let(
                                                        field("paramName",
                                                            Pairs.first(var("pair"))),
                                                        field("jArg",
                                                            Pairs.second(var("pair"))),
                                                        inject(
                                                            BlockStatement.TYPE_,
                                                            BlockStatement.STATEMENT,
                                                            apply(
                                                                ref(Utils.javaAssignmentStatement),
                                                                inject(
                                                                    LeftHandSide.TYPE_,
                                                                    LeftHandSide.EXPRESSION_NAME,
                                                                    apply(
                                                                        ref(Utils.javaIdentifierToJavaExpressionName),
                                                                        apply(
                                                                            ref(Utils.variableToJavaIdentifier),
                                                                            var("paramName")))),
                                                                var("jArg"))))),
                                                Lists.zip(var("changedParams"), var("jChangedArgs")))),
                                        field("continueStmt",
                                            inject(BlockStatement.TYPE_,
                                                BlockStatement.STATEMENT,
                                                inject(Statement.TYPE_,
                                                    Statement.WITHOUT_TRAILING,
                                                    inject(
                                                        StatementWithoutTrailingSubstatement.TYPE_,
                                                        StatementWithoutTrailingSubstatement.CONTINUE,
                                                        wrap(
                                                            ContinueStatement.TYPE_,
                                                            nothing()))))),
                                        right(
                                            Lists.concat2(
                                                var("assignments"),
                                                list(var("continueStmt")))))))),
                        casesWithDefault(Term.TYPE_,
                            var("stripped"),
                            let(
                                field("gathered2",
                                    apply(var("hydra.analysis.gatherApplications"), var("term"))),
                                field("args2",
                                    Pairs.first(var("gathered2"))),
                                field("body2",
                                    Pairs.second(var("gathered2"))),
                                Logic.ifElse(
                                    Equality.equal(Lists.length(var("args2")), int32(1)),
                                    let("arg",
                                        Maybes.fromMaybe(
                                            inject(Term.TYPE_,
                                                Term.UNIT,
                                                unit()),
                                            Lists.maybeHead(var("args2"))),
                                        casesWithDefault(Term.TYPE_,
                                            apply(
                                                var("hydra.strip.deannotateAndDetypeTerm"),
                                                var("body2")),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeTerm),
                                                    var("env"),
                                                    var("term"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("expr",
                                                    right(
                                                        list(
                                                            inject(
                                                                BlockStatement.TYPE_,
                                                                BlockStatement.STATEMENT,
                                                                apply(
                                                                    ref(Utils.javaReturnStatement),
                                                                    just(var("expr")))))))),
                                            field(
                                                Term.CASES,
                                                lambda("cs",
                                                    let(
                                                        field("aliases",
                                                            proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                                                        field("tname",
                                                            proj(CaseStatement.TYPE_, CaseStatement.TYPE_NAME, "cs")),
                                                        field("dflt",
                                                            proj(CaseStatement.TYPE_, CaseStatement.DEFAULT, "cs")),
                                                        field("cases_",
                                                            proj(CaseStatement.TYPE_, CaseStatement.CASES, "cs")),
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Coder.domTypeArgs),
                                                                var("aliases"),
                                                                apply(
                                                                    var("hydra.resolution.nominalApplication"),
                                                                    var("tname"),
                                                                    list()),
                                                                var("cx"),
                                                                var("g")),
                                                            lambda("domArgs",
                                                                Eithers.bind(
                                                                    apply(
                                                                        ref(Coder.encodeTerm),
                                                                        var("env"),
                                                                        var("arg"),
                                                                        var("cx"),
                                                                        var("g")),
                                                                    lambda("jArgRaw",
                                                                        let(
                                                                            field("depthSuffix",
                                                                                Logic.ifElse(
                                                                                    Equality.equal(
                                                                                        var("tcoDepth"),
                                                                                        int32(0)),
                                                                                    string(""),
                                                                                    Literals.showInt32(
                                                                                        var("tcoDepth")))),
                                                                            field("matchVarId",
                                                                                apply(
                                                                                    ref(Utils.javaIdentifier),
                                                                                    Strings.cat(
                                                                                        list(
                                                                                            string("_tco_match_"),
                                                                                            apply(
                                                                                                var("hydra.formatting.decapitalize"),
                                                                                                apply(
                                                                                                    var("hydra.names.localNameOf"),
                                                                                                    var("tname"))),
                                                                                            var("depthSuffix"))))),
                                                                            field("matchDecl",
                                                                                apply(
                                                                                    ref(Utils.varDeclarationStatement),
                                                                                    var("matchVarId"),
                                                                                    var("jArgRaw"))),
                                                                            field("jArg",
                                                                                apply(
                                                                                    ref(Utils.javaIdentifierToJavaExpression),
                                                                                    var("matchVarId"))),
                                                                            Eithers.bind(
                                                                                Eithers.mapList(
                                                                                    lambda("field",
                                                                                        let(
                                                                                            field(
                                                                                                "fieldName",
                                                                                                proj(Field.TYPE_, Field.NAME, "field")),
                                                                                            field(
                                                                                                "variantRefType",
                                                                                                apply(
                                                                                                    ref(Utils.nameToJavaReferenceType),
                                                                                                    var("aliases"),
                                                                                                    bool(true),
                                                                                                    var("domArgs"),
                                                                                                    var("tname"),
                                                                                                    just(
                                                                                                        apply(
                                                                                                            var("hydra.formatting.capitalize"),
                                                                                                            apply(
                                                                                                                unwrap(Name.TYPE_),
                                                                                                                var("fieldName")))))),
                                                                                            casesWithDefault(
                                                                                                Term.TYPE_,
                                                                                                apply(
                                                                                                    var("hydra.strip.deannotateTerm"),
                                                                                                    proj(Field.TYPE_, Field.TERM, "field")),
                                                                                                left(
                                                                                                    inject(
                                                                                                        Error_.TYPE_,
                                                                                                        Error_.OTHER,
                                                                                                        wrap(
                                                                                                            OtherError.TYPE_,
                                                                                                            string("TCO: case branch is not a lambda")))),
                                                                                                field(
                                                                                                    Term.LAMBDA,
                                                                                                    lambda(
                                                                                                        "lam",
                                                                                                        apply(
                                                                                                            ref(Coder.withLambda),
                                                                                                            var("env"),
                                                                                                            var("lam"),
                                                                                                            lambda(
                                                                                                                "env2",
                                                                                                                let(
                                                                                                                    field(
                                                                                                                        "lambdaParam",
                                                                                                                        proj(Lambda.TYPE_, Lambda.PARAMETER, "lam")),
                                                                                                                    field(
                                                                                                                        "branchBody",
                                                                                                                        proj(Lambda.TYPE_, Lambda.BODY, "lam")),
                                                                                                                    field(
                                                                                                                        "env3",
                                                                                                                        apply(
                                                                                                                            ref(Coder.insertBranchVar),
                                                                                                                            var("lambdaParam"),
                                                                                                                            var("env2"))),
                                                                                                                    field(
                                                                                                                        "varId",
                                                                                                                        apply(
                                                                                                                            ref(Utils.variableToJavaIdentifier),
                                                                                                                            var("lambdaParam"))),
                                                                                                                    field(
                                                                                                                        "castExpr",
                                                                                                                        apply(
                                                                                                                            ref(Utils.javaCastExpressionToJavaExpression),
                                                                                                                            apply(
                                                                                                                                ref(Utils.javaCastExpression),
                                                                                                                                var("variantRefType"),
                                                                                                                                apply(
                                                                                                                                    ref(Utils.javaExpressionToJavaUnaryExpression),
                                                                                                                                    var("jArg"))))),
                                                                                                                    field(
                                                                                                                        "localDecl",
                                                                                                                        apply(
                                                                                                                            ref(Utils.varDeclarationStatement),
                                                                                                                            var("varId"),
                                                                                                                            var("castExpr"))),
                                                                                                                    field(
                                                                                                                        "isBranchTailCall",
                                                                                                                        apply(
                                                                                                                            var("hydra.analysis.isTailRecursiveInTailPosition"),
                                                                                                                            var("funcName"),
                                                                                                                            var("branchBody"))),
                                                                                                                    Eithers.bind(
                                                                                                                        Logic.ifElse(
                                                                                                                            var("isBranchTailCall"),
                                                                                                                            apply(
                                                                                                                                ref(Coder.encodeTermTCO),
                                                                                                                                var("env3"),
                                                                                                                                var("funcName"),
                                                                                                                                var("paramNames"),
                                                                                                                                var("tcoVarRenames"),
                                                                                                                                Math_.add(
                                                                                                                                    var("tcoDepth"),
                                                                                                                                    int32(1)),
                                                                                                                                var("branchBody"),
                                                                                                                                var("cx"),
                                                                                                                                var("g")),
                                                                                                                            Eithers.bind(
                                                                                                                                apply(
                                                                                                                                    ref(Coder.analyzeJavaFunction),
                                                                                                                                    var("env3"),
                                                                                                                                    var("branchBody"),
                                                                                                                                    var("cx"),
                                                                                                                                    var("g")),
                                                                                                                                lambda(
                                                                                                                                    "fs",
                                                                                                                                    let(
                                                                                                                                        field(
                                                                                                                                            "bindings",
                                                                                                                                            proj(FunctionStructure.TYPE_, FunctionStructure.BINDINGS, "fs")),
                                                                                                                                        field(
                                                                                                                                            "innerBody",
                                                                                                                                            proj(FunctionStructure.TYPE_, FunctionStructure.BODY, "fs")),
                                                                                                                                        field(
                                                                                                                                            "env4",
                                                                                                                                            proj(FunctionStructure.TYPE_, FunctionStructure.ENVIRONMENT, "fs")),
                                                                                                                                        Eithers.bind(
                                                                                                                                            apply(
                                                                                                                                                ref(Coder.bindingsToStatements),
                                                                                                                                                var("env4"),
                                                                                                                                                var("bindings"),
                                                                                                                                                var("cx"),
                                                                                                                                                var("g")),
                                                                                                                                            lambda(
                                                                                                                                                "bindResult",
                                                                                                                                                let(
                                                                                                                                                    field(
                                                                                                                                                        "bindingStmts",
                                                                                                                                                        Pairs.first(
                                                                                                                                                            var("bindResult"))),
                                                                                                                                                    field(
                                                                                                                                                        "env5",
                                                                                                                                                        Pairs.second(
                                                                                                                                                            var("bindResult"))),
                                                                                                                                                    Eithers.bind(
                                                                                                                                                        apply(
                                                                                                                                                            ref(Coder.encodeTerm),
                                                                                                                                                            var("env5"),
                                                                                                                                                            var("innerBody"),
                                                                                                                                                            var("cx"),
                                                                                                                                                            var("g")),
                                                                                                                                                        lambda(
                                                                                                                                                            "jret",
                                                                                                                                                            let(
                                                                                                                                                                "returnStmt",
                                                                                                                                                                inject(
                                                                                                                                                                    BlockStatement.TYPE_,
                                                                                                                                                                    BlockStatement.STATEMENT,
                                                                                                                                                                    apply(
                                                                                                                                                                        ref(Utils.javaReturnStatement),
                                                                                                                                                                        just(
                                                                                                                                                                            var("jret")))),
                                                                                                                                                                right(
                                                                                                                                                                    Lists.concat2(
                                                                                                                                                                        var("bindingStmts"),
                                                                                                                                                                        list(
                                                                                                                                                                            var("returnStmt")))))))))))))),
                                                                                                                        lambda(
                                                                                                                            "bodyStmts",
                                                                                                                            let(
                                                                                                                                field(
                                                                                                                                    "relExpr",
                                                                                                                                    apply(
                                                                                                                                        ref(Utils.javaInstanceOf),
                                                                                                                                        apply(
                                                                                                                                            ref(Utils.javaUnaryExpressionToJavaRelationalExpression),
                                                                                                                                            apply(
                                                                                                                                                ref(Utils.javaExpressionToJavaUnaryExpression),
                                                                                                                                                var("jArg"))),
                                                                                                                                        var("variantRefType"))),
                                                                                                                                field(
                                                                                                                                    "condExpr",
                                                                                                                                    apply(
                                                                                                                                        ref(Utils.javaRelationalExpressionToJavaExpression),
                                                                                                                                        var("relExpr"))),
                                                                                                                                field(
                                                                                                                                    "blockStmts",
                                                                                                                                    Lists.cons(
                                                                                                                                        var("localDecl"),
                                                                                                                                        var("bodyStmts"))),
                                                                                                                                field(
                                                                                                                                    "ifBody",
                                                                                                                                    inject(
                                                                                                                                        Statement.TYPE_,
                                                                                                                                        Statement.WITHOUT_TRAILING,
                                                                                                                                        inject(
                                                                                                                                            StatementWithoutTrailingSubstatement.TYPE_,
                                                                                                                                            StatementWithoutTrailingSubstatement.BLOCK,
                                                                                                                                            wrap(
                                                                                                                                                Block.TYPE_,
                                                                                                                                                var("blockStmts"))))),
                                                                                                                                right(
                                                                                                                                    inject(
                                                                                                                                        BlockStatement.TYPE_,
                                                                                                                                        BlockStatement.STATEMENT,
                                                                                                                                        inject(
                                                                                                                                            Statement.TYPE_,
                                                                                                                                            Statement.IF_THEN,
                                                                                                                                            record(
                                                                                                                                                IfThenStatement.TYPE_,
                                                                                                                                                field(
                                                                                                                                                    IfThenStatement.EXPRESSION,
                                                                                                                                                    var("condExpr")),
                                                                                                                                                field(
                                                                                                                                                    IfThenStatement.STATEMENT,
                                                                                                                                                    var("ifBody"))))))))))))))))),
                                                                                    var("cases_")),
                                                                                lambda("ifBlocks",
                                                                                    Eithers.bind(
                                                                                        Maybes.cases(
                                                                                            var("dflt"),
                                                                                            right(
                                                                                                list(
                                                                                                    inject(
                                                                                                        BlockStatement.TYPE_,
                                                                                                        BlockStatement.STATEMENT,
                                                                                                        apply(
                                                                                                            ref(Utils.javaReturnStatement),
                                                                                                            just(
                                                                                                                var("jArg")))))),
                                                                                            lambda(
                                                                                                "d",
                                                                                                Eithers.bind(
                                                                                                    apply(
                                                                                                        ref(Coder.encodeTerm),
                                                                                                        var("env"),
                                                                                                        var("d"),
                                                                                                        var("cx"),
                                                                                                        var("g")),
                                                                                                    lambda(
                                                                                                        "dExpr",
                                                                                                        right(
                                                                                                            list(
                                                                                                                inject(
                                                                                                                    BlockStatement.TYPE_,
                                                                                                                    BlockStatement.STATEMENT,
                                                                                                                    apply(
                                                                                                                        ref(Utils.javaReturnStatement),
                                                                                                                        just(
                                                                                                                            var("dExpr")))))))))),
                                                                                        lambda(
                                                                                            "defaultStmt",
                                                                                            right(
                                                                                                Lists.concat(
                                                                                                    list(
                                                                                                        list(
                                                                                                            var("matchDecl")),
                                                                                                        var("ifBlocks"),
                                                                                                        var("defaultStmt")))))))))))))))))),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeTerm),
                                            var("env"),
                                            var("term"),
                                            var("cx"),
                                            var("g")),
                                        lambda("expr",
                                            right(
                                                list(
                                                    inject(BlockStatement.TYPE_,
                                                        BlockStatement.STATEMENT,
                                                        apply(
                                                            ref(Utils.javaReturnStatement),
                                                            just(var("expr")))))))))),
                            field(
                                Term.LET,
                                lambda("lt",
                                    let(
                                        field("letBindings",
                                            proj(Let.TYPE_, Let.BINDINGS, "lt")),
                                        field("letBody",
                                            proj(Let.TYPE_, Let.BODY, "lt")),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.bindingsToStatements),
                                                var("env"),
                                                var("letBindings"),
                                                var("cx"),
                                                var("g")),
                                            lambda("bindResult",
                                                let(
                                                    field("letStmts",
                                                        Pairs.first(var("bindResult"))),
                                                    field("envAfterLet",
                                                        Pairs.second(var("bindResult"))),
                                                    Eithers.bind(
                                                        apply(
                                                            ref(Coder.encodeTermTCO),
                                                            var("envAfterLet"),
                                                            var("funcName"),
                                                            var("paramNames"),
                                                            var("tcoVarRenames"),
                                                            var("tcoDepth"),
                                                            var("letBody"),
                                                            var("cx"),
                                                            var("g")),
                                                        lambda("tcoBodyStmts",
                                                            right(
                                                                Lists.concat2(
                                                                    var("letStmts"),
                                                                    var("tcoBodyStmts"))))))))))))))));

    public static final Def encodeType = def(
        "encodeType",
        () -> lambda(
                "aliases",
                "boundVars",
                "t",
                "cx",
                "g",
                let(
                    field("inScopeTypeParams",
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                    field("typeVarSubst",
                        proj(Aliases.TYPE_, Aliases.TYPE_VAR_SUBST, "aliases")),
                    casesWithDefault(Type.TYPE_,
                        apply(var("hydra.strip.deannotateType"), var("t")),
                        left(
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    Strings.cat2(
                                        string("can't encode unsupported type in Java: "),
                                        apply(var("hydra.show.core.type"), var("t")))))),
                        field(
                            Type.APPLICATION,
                            lambda("at",
                                Eithers.bind(
                                    apply(
                                        ref(Coder.encodeType),
                                        var("aliases"),
                                        var("boundVars"),
                                        proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at"),
                                        var("cx"),
                                        var("g")),
                                    lambda("jlhs",
                                        Eithers.bind(
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeType),
                                                    var("aliases"),
                                                    var("boundVars"),
                                                    proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jt_",
                                                    apply(
                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                        var("jt_"),
                                                        var("cx")))),
                                            lambda("jrhs",
                                                apply(
                                                    ref(Utils.addJavaTypeParameter),
                                                    var("jrhs"),
                                                    var("jlhs"),
                                                    var("cx")))))))),
                        field(
                            Type.FUNCTION,
                            lambda("ft",
                                Eithers.bind(
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("boundVars"),
                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt_",
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt_"),
                                                var("cx")))),
                                    lambda("jdom",
                                        Eithers.bind(
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeType),
                                                    var("aliases"),
                                                    var("boundVars"),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jt_",
                                                    apply(
                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                        var("jt_"),
                                                        var("cx")))),
                                            lambda("jcod",
                                                right(
                                                    apply(
                                                        ref(Utils.javaRefType),
                                                        list(var("jdom"), var("jcod")),
                                                        ref(Names.javaUtilFunctionPackageName),
                                                        string("Function"))))))))),
                        field(
                            Type.FORALL,
                            lambda("fa",
                                Eithers.bind(
                                    apply(
                                        ref(Coder.encodeType),
                                        var("aliases"),
                                        Sets.insert(
                                            proj(ForallType.TYPE_, ForallType.PARAMETER, "fa"),
                                            var("boundVars")),
                                        proj(ForallType.TYPE_, ForallType.BODY, "fa"),
                                        var("cx"),
                                        var("g")),
                                    lambda("jbody",
                                        apply(
                                            ref(Utils.addJavaTypeParameter),
                                            apply(
                                                ref(Utils.javaTypeVariable),
                                                apply(
                                                    unwrap(Name.TYPE_),
                                                    proj(ForallType.TYPE_, ForallType.PARAMETER, "fa"))),
                                            var("jbody"),
                                            var("cx")))))),
                        field(
                            Type.LIST,
                            lambda("et",
                                Eithers.bind(
                                    apply(
                                        ref(Coder.encodeType),
                                        var("aliases"),
                                        var("boundVars"),
                                        var("et"),
                                        var("cx"),
                                        var("g")),
                                    lambda("jet",
                                        Eithers.bind(
                                            Eithers.bind(
                                                right(var("jet")),
                                                lambda("jt_",
                                                    apply(
                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                        var("jt_"),
                                                        var("cx")))),
                                            lambda("rt",
                                                right(
                                                    apply(
                                                        ref(Utils.javaRefType),
                                                        list(var("rt")),
                                                        ref(Names.javaUtilPackageName),
                                                        string("List"))))))))),
                        field(
                            Type.LITERAL,
                            lambda("lt",
                                apply(ref(Coder.encodeLiteralType), var("lt"), var("cx"), var("g")))),
                        field(
                            Type.EITHER,
                            lambda("et",
                                Eithers.bind(
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("boundVars"),
                                            proj(EitherType.TYPE_, EitherType.LEFT, "et"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt_",
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt_"),
                                                var("cx")))),
                                    lambda("jlt",
                                        Eithers.bind(
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeType),
                                                    var("aliases"),
                                                    var("boundVars"),
                                                    proj(EitherType.TYPE_, EitherType.RIGHT, "et"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jt_",
                                                    apply(
                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                        var("jt_"),
                                                        var("cx")))),
                                            lambda("jrt",
                                                right(
                                                    apply(
                                                        ref(Utils.javaRefType),
                                                        list(var("jlt"), var("jrt")),
                                                        ref(Names.hydraUtilPackageName),
                                                        string("Either"))))))))),
                        field(
                            Type.MAP,
                            lambda("mt",
                                Eithers.bind(
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("boundVars"),
                                            proj(MapType.TYPE_, MapType.KEYS, "mt"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt_",
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt_"),
                                                var("cx")))),
                                    lambda("jkt",
                                        Eithers.bind(
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeType),
                                                    var("aliases"),
                                                    var("boundVars"),
                                                    proj(MapType.TYPE_, MapType.VALUES, "mt"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jt_",
                                                    apply(
                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                        var("jt_"),
                                                        var("cx")))),
                                            lambda("jvt",
                                                right(
                                                    apply(
                                                        ref(Utils.javaRefType),
                                                        list(var("jkt"), var("jvt")),
                                                        ref(Names.javaUtilPackageName),
                                                        string("Map"))))))))),
                        field(
                            Type.PAIR,
                            lambda("pt",
                                Eithers.bind(
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("boundVars"),
                                            proj(PairType.TYPE_, PairType.FIRST, "pt"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt_",
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt_"),
                                                var("cx")))),
                                    lambda("jfirst",
                                        Eithers.bind(
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeType),
                                                    var("aliases"),
                                                    var("boundVars"),
                                                    proj(PairType.TYPE_, PairType.SECOND, "pt"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jt_",
                                                    apply(
                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                        var("jt_"),
                                                        var("cx")))),
                                            lambda("jsecond",
                                                right(
                                                    apply(
                                                        ref(Utils.javaRefType),
                                                        list(var("jfirst"), var("jsecond")),
                                                        ref(Names.hydraUtilPackageName),
                                                        string("Pair"))))))))),
                        field(
                            Type.UNIT,
                            constant(
                                right(
                                    apply(
                                        ref(Utils.javaRefType),
                                        list(),
                                        ref(Names.javaLangPackageName),
                                        string("Void"))))),
                        field(
                            Type.RECORD,
                            lambda("rt",
                                Logic.ifElse(
                                    Lists.null_(var("rt")),
                                    right(
                                        apply(
                                            ref(Utils.javaRefType),
                                            list(),
                                            ref(Names.javaLangPackageName),
                                            string("Void"))),
                                    left(
                                        inject(Error_.TYPE_,
                                            Error_.OTHER,
                                            wrap(OtherError.TYPE_,
                                                string("unexpected anonymous record type"))))))),
                        field(
                            Type.MAYBE,
                            lambda("ot",
                                Eithers.bind(
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("boundVars"),
                                            var("ot"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt_",
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt_"),
                                                var("cx")))),
                                    lambda("jot",
                                        right(
                                            apply(
                                                ref(Utils.javaRefType),
                                                list(var("jot")),
                                                ref(Names.hydraUtilPackageName),
                                                string("Maybe"))))))),
                        field(
                            Type.SET,
                            lambda("st",
                                Eithers.bind(
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            var("boundVars"),
                                            var("st"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt_",
                                            apply(
                                                ref(Utils.javaTypeToJavaReferenceType),
                                                var("jt_"),
                                                var("cx")))),
                                    lambda("jst",
                                        right(
                                            apply(
                                                ref(Utils.javaRefType),
                                                list(var("jst")),
                                                ref(Names.javaUtilPackageName),
                                                string("Set"))))))),
                        field(
                            Type.UNION,
                            constant(
                                left(
                                    inject(Error_.TYPE_,
                                        Error_.OTHER,
                                        wrap(OtherError.TYPE_,
                                            string("unexpected anonymous union type")))))),
                        field(
                            Type.VARIABLE,
                            lambda("name0",
                                let("name",
                                    Maybes.fromMaybe(
                                        var("name0"),
                                        Maps.lookup(var("name0"), var("typeVarSubst"))),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType_resolveIfTypedef),
                                            var("aliases"),
                                            var("boundVars"),
                                            var("inScopeTypeParams"),
                                            var("name"),
                                            var("cx"),
                                            var("g")),
                                        lambda("resolved",
                                            Maybes.cases(
                                                var("resolved"),
                                                right(
                                                    Logic.ifElse(
                                                        Logic.or_(
                                                            Sets.member(
                                                                var("name"),
                                                                var("boundVars")),
                                                            Sets.member(
                                                                var("name"),
                                                                var("inScopeTypeParams"))),
                                                        inject(hydra.java.syntax.Type.TYPE_,
                                                            hydra.java.syntax.Type.REFERENCE,
                                                            apply(
                                                                ref(Utils.javaTypeVariable),
                                                                apply(
                                                                    unwrap(Name.TYPE_),
                                                                    var("name")))),
                                                        Logic.ifElse(
                                                            apply(
                                                                ref(Coder.isLambdaBoundVariable),
                                                                var("name")),
                                                            inject(hydra.java.syntax.Type.TYPE_,
                                                                hydra.java.syntax.Type.REFERENCE,
                                                                apply(
                                                                    ref(Utils.javaTypeVariable),
                                                                    apply(
                                                                        unwrap(Name.TYPE_),
                                                                        var("name")))),
                                                            Logic.ifElse(
                                                                apply(
                                                                    ref(Coder.isUnresolvedInferenceVar),
                                                                    var("name")),
                                                                inject(hydra.java.syntax.Type.TYPE_,
                                                                    hydra.java.syntax.Type.REFERENCE,
                                                                    inject(
                                                                        ReferenceType.TYPE_,
                                                                        ReferenceType.CLASS_OR_INTERFACE,
                                                                        inject(
                                                                            ClassOrInterfaceType.TYPE_,
                                                                            ClassOrInterfaceType.CLASS,
                                                                            apply(
                                                                                ref(Utils.javaClassType),
                                                                                list(),
                                                                                ref(Names.javaLangPackageName),
                                                                                string("Object"))))),
                                                                inject(hydra.java.syntax.Type.TYPE_,
                                                                    hydra.java.syntax.Type.REFERENCE,
                                                                    apply(
                                                                        ref(Utils.nameToJavaReferenceType),
                                                                        var("aliases"),
                                                                        bool(true),
                                                                        list(),
                                                                        var("name"),
                                                                        nothing())))))),
                                                lambda("resolvedType",
                                                    apply(
                                                        ref(Coder.encodeType),
                                                        var("aliases"),
                                                        var("boundVars"),
                                                        var("resolvedType"),
                                                        var("cx"),
                                                        var("g"))))))))),
                        field(
                            Type.WRAP,
                            constant(
                                left(
                                    inject(Error_.TYPE_,
                                        Error_.OTHER,
                                        wrap(OtherError.TYPE_,
                                            string("unexpected anonymous wrap type"))))))))));

    public static final Def encodeTypeDefinition = def(
        "encodeTypeDefinition",
        () -> lambda(
                "pkg",
                "aliases",
                "tdef",
                "cx",
                "g",
                let(
                    field("name",
                        proj(TypeDefinition.TYPE_, TypeDefinition.NAME, "tdef")),
                    field("typ",
                        apply(
                            project(TypeScheme.TYPE_, TypeScheme.BODY),
                            proj(TypeDefinition.TYPE_, TypeDefinition.TYPE_SCHEME, "tdef"))),
                    field("serializable",
                        apply(ref(Coder.isSerializableJavaType), var("typ"))),
                    field("imports",
                        Logic.ifElse(
                            var("serializable"),
                            list(
                                inject(ImportDeclaration.TYPE_,
                                    ImportDeclaration.SINGLE_TYPE,
                                    wrap(SingleTypeImportDeclaration.TYPE_,
                                        apply(
                                            ref(Utils.javaTypeName),
                                            wrap(Identifier.TYPE_,
                                                string("java.io.Serializable")))))),
                            list())),
                    Eithers.bind(
                        apply(
                            ref(Coder.toClassDecl),
                            bool(false),
                            var("serializable"),
                            var("aliases"),
                            list(),
                            var("name"),
                            var("typ"),
                            var("cx"),
                            var("g")),
                        lambda("decl",
                            Eithers.bind(
                                apply(
                                    var("hydra.annotations.getTypeDescription"),
                                    var("cx"),
                                    var("g"),
                                    var("typ")),
                                lambda("comment",
                                    let("tdecl",
                                        record(
                                            TopLevelClassOrInterfaceDeclarationWithComments.TYPE_,
                                            field(
                                                TopLevelClassOrInterfaceDeclarationWithComments.VALUE,
                                                inject(
                                                    TopLevelClassOrInterfaceDeclaration.TYPE_,
                                                    TopLevelClassOrInterfaceDeclaration.CLASS,
                                                    var("decl"))),
                                            field(
                                                TopLevelClassOrInterfaceDeclarationWithComments.COMMENTS,
                                                var("comment"))),
                                        right(
                                            pair(
                                                var("name"),
                                                inject(CompilationUnit.TYPE_,
                                                    CompilationUnit.ORDINARY,
                                                    record(
                                                        OrdinaryCompilationUnit.TYPE_,
                                                        field(
                                                            OrdinaryCompilationUnit.PACKAGE,
                                                            just(var("pkg"))),
                                                        field(
                                                            OrdinaryCompilationUnit.IMPORTS,
                                                            var("imports")),
                                                        field(
                                                            OrdinaryCompilationUnit.TYPES,
                                                            list(var("tdecl")))))))))))))));

    public static final Def encodeType_resolveIfTypedef = def(
        "encodeType_resolveIfTypedef",
        () -> lambda(
                "aliases",
                "boundVars",
                "inScopeTypeParams",
                "name",
                "cx",
                "g",
                Logic.ifElse(
                    Logic.or_(
                        Sets.member(var("name"), var("boundVars")),
                        Sets.member(var("name"), var("inScopeTypeParams"))),
                    right(nothing()),
                    Logic.ifElse(
                        apply(ref(Coder.isLambdaBoundVariable), var("name")),
                        right(nothing()),
                        let("schemaTypes",
                            proj(Graph.TYPE_, Graph.SCHEMA_TYPES, "g"),
                            Maybes.cases(
                                Maps.lookup(var("name"), var("schemaTypes")),
                                right(nothing()),
                                lambda("ts",
                                    Logic.ifElse(
                                        Logic.not_(
                                            Lists.null_(
                                                proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
                                        right(nothing()),
                                        casesWithDefault(Type.TYPE_,
                                            apply(
                                                var("hydra.strip.deannotateType"),
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                                            right(
                                                just(
                                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"))),
                                            field(
                                                Type.RECORD,
                                                constant(right(nothing()))),
                                            field(Type.UNION, constant(right(nothing()))),
                                            field(Type.WRAP, constant(right(nothing()))))))))))));

    public static final Def encodeVariable = def(
        "encodeVariable",
        () -> lambda(
                "env",
                "name",
                "cx",
                "g",
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("resolvedName",
                        apply(
                            ref(Utils.lookupJavaVarName),
                            var("aliases"),
                            var("name"))),
                    field("jid",
                        apply(
                            ref(Utils.javaIdentifier),
                            apply(unwrap(Name.TYPE_), var("resolvedName")))),
                    Logic.ifElse(
                        Sets.member(
                            var("name"),
                            proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases")),
                        right(
                            apply(
                                ref(Utils.javaFieldAccessToJavaExpression),
                                record(FieldAccess.TYPE_,
                                    field(
                                        FieldAccess.QUALIFIER,
                                        inject(FieldAccess_Qualifier.TYPE_,
                                            FieldAccess_Qualifier.PRIMARY,
                                            apply(
                                                ref(Utils.javaExpressionToJavaPrimary),
                                                apply(
                                                    ref(Utils.javaIdentifierToJavaExpression),
                                                    var("jid"))))),
                                    field(
                                        FieldAccess.IDENTIFIER,
                                        apply(
                                            ref(Utils.javaIdentifier),
                                            ref(Names.valueFieldName)))))),
                        Logic.ifElse(
                            Logic.and_(
                                Equality.equal(
                                    var("name"),
                                    wrap(Name.TYPE_,
                                        Strings.cat(
                                            list(
                                                ref(Names.instanceName),
                                                string("_"),
                                                ref(Names.valueFieldName))))),
                                apply(ref(Coder.isRecursiveVariable), var("aliases"), var("name"))),
                            let("instanceExpr",
                                apply(
                                    ref(Utils.javaIdentifierToJavaExpression),
                                    apply(
                                        ref(Utils.javaIdentifier),
                                        ref(Names.instanceName))),
                                right(
                                    apply(
                                        ref(Utils.javaFieldAccessToJavaExpression),
                                        record(FieldAccess.TYPE_,
                                            field(
                                                FieldAccess.QUALIFIER,
                                                inject(
                                                    FieldAccess_Qualifier.TYPE_,
                                                    FieldAccess_Qualifier.PRIMARY,
                                                    apply(
                                                        ref(Utils.javaExpressionToJavaPrimary),
                                                        var("instanceExpr")))),
                                            field(
                                                FieldAccess.IDENTIFIER,
                                                apply(
                                                    ref(Utils.javaIdentifier),
                                                    ref(Names.valueFieldName))))))),
                            Logic.ifElse(
                                Logic.and_(
                                    apply(
                                        ref(Coder.isRecursiveVariable),
                                        var("aliases"),
                                        var("name")),
                                    Logic.not_(
                                        apply(
                                            ref(Coder.isLambdaBoundIn),
                                            var("name"),
                                            proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases")))),
                                right(
                                    apply(
                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                        apply(
                                            ref(Utils.methodInvocation),
                                            just(
                                                left(
                                                    record(ExpressionName.TYPE_,
                                                        field(
                                                            ExpressionName.QUALIFIER,
                                                            nothing()),
                                                        field(
                                                            ExpressionName.IDENTIFIER,
                                                            var("jid"))))),
                                            wrap(Identifier.TYPE_,
                                                ref(Names.getMethodName)),
                                            list()))),
                                Logic.ifElse(
                                    Logic.and_(
                                        Sets.member(
                                            var("name"),
                                            proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases")),
                                        Logic.not_(
                                            apply(
                                                ref(Coder.isLambdaBoundIn),
                                                var("name"),
                                                proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases")))),
                                    right(
                                        apply(
                                            ref(Utils.javaMethodInvocationToJavaExpression),
                                            apply(
                                                ref(Utils.methodInvocation),
                                                just(
                                                    left(
                                                        record(
                                                            ExpressionName.TYPE_,
                                                            field(
                                                                ExpressionName.QUALIFIER,
                                                                nothing()),
                                                            field(
                                                                ExpressionName.IDENTIFIER,
                                                                var("jid"))))),
                                                wrap(Identifier.TYPE_,
                                                    ref(Names.getMethodName)),
                                                list()))),
                                    Logic.ifElse(
                                        apply(
                                            ref(Coder.isLambdaBoundIn),
                                            var("name"),
                                            proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases")),
                                        let(
                                            field("actualName",
                                                apply(
                                                    ref(Coder.findMatchingLambdaVar),
                                                    var("name"),
                                                    proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases"))),
                                            field("resolvedActual",
                                                apply(
                                                    ref(Utils.lookupJavaVarName),
                                                    var("aliases"),
                                                    var("actualName"))),
                                            right(
                                                apply(
                                                    ref(Utils.javaIdentifierToJavaExpression),
                                                    apply(
                                                        ref(Utils.variableToJavaIdentifier),
                                                        var("resolvedActual"))))),
                                        Logic.ifElse(
                                            Sets.member(
                                                var("name"),
                                                proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")),
                                            right(
                                                apply(
                                                    ref(Utils.javaIdentifierToJavaExpression),
                                                    apply(
                                                        ref(Coder.elementJavaIdentifier),
                                                        bool(false),
                                                        bool(false),
                                                        var("aliases"),
                                                        var("resolvedName")))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.classifyDataReference),
                                                    var("name"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("cls",
                                                    cases(
                                                        JavaSymbolClass.TYPE_,
                                                        var("cls"),
                                                        field(
                                                            JavaSymbolClass.HOISTED_LAMBDA,
                                                            lambda("arity",
                                                                apply(
                                                                    ref(Coder.encodeVariable_hoistedLambdaCase),
                                                                    var("aliases"),
                                                                    var("name"),
                                                                    var("arity"),
                                                                    var("cx"),
                                                                    var("g")))),
                                                        field(
                                                            JavaSymbolClass.LOCAL_VARIABLE,
                                                            constant(
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaIdentifierToJavaExpression),
                                                                        apply(
                                                                            ref(Coder.elementJavaIdentifier),
                                                                            bool(false),
                                                                            bool(false),
                                                                            var("aliases"),
                                                                            var("resolvedName")))))),
                                                        field(
                                                            JavaSymbolClass.CONSTANT,
                                                            constant(
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaIdentifierToJavaExpression),
                                                                        apply(
                                                                            ref(Coder.elementJavaIdentifier),
                                                                            bool(false),
                                                                            bool(false),
                                                                            var("aliases"),
                                                                            var("name")))))),
                                                        field(
                                                            JavaSymbolClass.NULLARY_FUNCTION,
                                                            constant(
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                        apply(
                                                                            ref(Utils.methodInvocation),
                                                                            nothing(),
                                                                            apply(
                                                                                ref(Coder.elementJavaIdentifier),
                                                                                bool(false),
                                                                                bool(false),
                                                                                var("aliases"),
                                                                                var("name")),
                                                                            list()))))),
                                                        field(
                                                            JavaSymbolClass.UNARY_FUNCTION,
                                                            constant(
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaIdentifierToJavaExpression),
                                                                        apply(
                                                                            ref(Coder.elementJavaIdentifier),
                                                                            bool(false),
                                                                            bool(true),
                                                                            var("aliases"),
                                                                            var("name"))))))))))))))))));

    public static final Def encodeVariable_buildCurried = def(
        "encodeVariable_buildCurried",
        () -> lambda(
                "params",
                "inner",
                Maybes.fromMaybe(
                    var("inner"),
                    Maybes.map(
                        lambda("p",
                            apply(
                                ref(Utils.javaLambda),
                                Pairs.first(var("p")),
                                apply(
                                    ref(Coder.encodeVariable_buildCurried),
                                    Pairs.second(var("p")),
                                    var("inner")))),
                        Lists.uncons(var("params"))))));

    public static final Def encodeVariable_hoistedLambdaCase = def(
        "encodeVariable_hoistedLambdaCase",
        () -> lambda(
                "aliases",
                "name",
                "arity",
                "cx",
                "g",
                let(
                    field("paramNames",
                        Lists.map(
                            lambda("i",
                                wrap(Name.TYPE_,
                                    Strings.cat2(string("p"), Literals.showInt32(var("i"))))),
                            Math_.range_(int32(0), Math_.sub(var("arity"), int32(1))))),
                    field("paramExprs",
                        Lists.map(
                            lambda("pn",
                                apply(
                                    ref(Utils.javaIdentifierToJavaExpression),
                                    apply(
                                        ref(Utils.variableToJavaIdentifier),
                                        var("pn")))),
                            var("paramNames"))),
                    field("call",
                        apply(
                            ref(Utils.javaMethodInvocationToJavaExpression),
                            apply(
                                ref(Utils.methodInvocation),
                                nothing(),
                                apply(
                                    ref(Coder.elementJavaIdentifier),
                                    bool(false),
                                    bool(false),
                                    var("aliases"),
                                    var("name")),
                                var("paramExprs")))),
                    field("lam",
                        apply(
                            ref(Coder.encodeVariable_buildCurried),
                            var("paramNames"),
                            var("call"))),
                    Eithers.bind(
                        right(apply(var("hydra.lexical.lookupBinding"), var("g"), var("name"))),
                        lambda("mel",
                            Maybes.cases(
                                var("mel"),
                                right(var("lam")),
                                lambda("el",
                                    Maybes.cases(
                                        proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                        right(var("lam")),
                                        lambda("ts",
                                            let("typ",
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"),
                                                Eithers.bind(
                                                    apply(
                                                        ref(Coder.encodeType),
                                                        var("aliases"),
                                                        var("hydra.lib.sets.empty"),
                                                        var("typ"),
                                                        var("cx"),
                                                        var("g")),
                                                    lambda("jtype",
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Utils.javaTypeToJavaReferenceType),
                                                                var("jtype"),
                                                                var("cx")),
                                                            lambda("rt",
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaCastExpressionToJavaExpression),
                                                                        apply(
                                                                            ref(Utils.javaCastExpression),
                                                                            var("rt"),
                                                                            apply(
                                                                                ref(Utils.javaExpressionToJavaUnaryExpression),
                                                                                var("lam")))))))))))))))))));

    public static final Def eqClause = def(
        "eqClause",
        () -> lambda(
                "tmpName",
                "ft",
                let(
                    field("fname",
                        apply(
                            unwrap(Name.TYPE_),
                            proj(FieldType.TYPE_, FieldType.NAME, "ft"))),
                    field("ftype",
                        proj(FieldType.TYPE_, FieldType.TYPE, "ft")),
                    Logic.ifElse(
                        apply(ref(Coder.isBinaryType), var("ftype")),
                        apply(ref(Coder.arraysEqualsClause), var("tmpName"), var("fname")),
                        Logic.ifElse(
                            apply(ref(Coder.isBigNumericType), var("ftype")),
                            apply(ref(Coder.compareToZeroClause), var("tmpName"), var("fname")),
                            apply(ref(Coder.equalsClause), var("tmpName"), var("fname")))))));

    public static final Def equalsClause = def(
        "equalsClause",
        () -> lambda(
                "tmpName",
                "fname",
                let(
                    field("thisArg",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                wrap(Identifier.TYPE_, string("this")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    field("otherArg",
                        apply(
                            ref(Utils.javaExpressionNameToJavaExpression),
                            apply(
                                ref(Utils.fieldExpression),
                                apply(ref(Utils.javaIdentifier), var("tmpName")),
                                apply(ref(Utils.javaIdentifier), var("fname"))))),
                    field("header",
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.COMPLEX,
                            record(MethodInvocation_Complex.TYPE_,
                                field(
                                    MethodInvocation_Complex.VARIANT,
                                    inject(MethodInvocation_Variant.TYPE_,
                                        MethodInvocation_Variant.TYPE,
                                        apply(
                                            ref(Utils.javaTypeName),
                                            wrap(Identifier.TYPE_,
                                                string("java.util.Objects"))))),
                                field(
                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                    list()),
                                field(
                                    MethodInvocation_Complex.IDENTIFIER,
                                    wrap(Identifier.TYPE_,
                                        ref(Names.equalsMethodName)))))),
                    apply(
                        ref(Utils.javaPostfixExpressionToJavaInclusiveOrExpression),
                        apply(
                            ref(Utils.javaMethodInvocationToJavaPostfixExpression),
                            record(MethodInvocation.TYPE_,
                                field(MethodInvocation.HEADER, var("header")),
                                field(
                                    MethodInvocation.ARGUMENTS,
                                    list(var("thisArg"), var("otherArg")))))))));

    public static final Def extractArgType = def(
        "extractArgType",
        () -> lambda(
                "_lhs",
                "typ",
                casesWithDefault(Type.TYPE_,
                    var("typ"),
                    var("typ"),
                    field(
                        Type.APPLICATION,
                        lambda("at1",
                            casesWithDefault(Type.TYPE_,
                                proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at1"),
                                var("typ"),
                                field(
                                    Type.APPLICATION,
                                    constant(
                                        proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at1")))))))));

    public static final Def extractDirectReturn = def(
        "extractDirectReturn",
        () -> lambda(
                "tparamSet",
                "t",
                apply(ref(Coder.extractDirectReturn_go), var("tparamSet"), var("t"))));

    public static final Def extractDirectReturn_go = def(
        "extractDirectReturn_go",
        () -> lambda(
                "tparamSet",
                "t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    list(),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            let(
                                field("dom",
                                    apply(
                                        var("hydra.strip.deannotateType"),
                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"))),
                                field("cod",
                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                casesWithDefault(Type.TYPE_,
                                    var("dom"),
                                    apply(
                                        ref(Coder.extractDirectReturn_go),
                                        var("tparamSet"),
                                        var("cod")),
                                    field(
                                        Type.VARIABLE,
                                        lambda("inVar",
                                            Logic.ifElse(
                                                Sets.member(var("inVar"), var("tparamSet")),
                                                casesWithDefault(Type.TYPE_,
                                                    apply(
                                                        var("hydra.strip.deannotateType"),
                                                        var("cod")),
                                                    list(),
                                                    field(
                                                        Type.FUNCTION,
                                                        lambda("ft2",
                                                            let(
                                                                field("midArg",
                                                                    apply(
                                                                        var("hydra.strip.deannotateType"),
                                                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft2"))),
                                                                field("retPart",
                                                                    apply(
                                                                        var("hydra.strip.deannotateType"),
                                                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft2"))),
                                                                casesWithDefault(
                                                                    Type.TYPE_,
                                                                    var("midArg"),
                                                                    casesWithDefault(
                                                                        Type.TYPE_,
                                                                        var("retPart"),
                                                                        list(),
                                                                        field(
                                                                            Type.VARIABLE,
                                                                            lambda("outVar",
                                                                                Logic.ifElse(
                                                                                    Sets.member(
                                                                                        var("outVar"),
                                                                                        var("tparamSet")),
                                                                                    list(
                                                                                        pair(
                                                                                            var("inVar"),
                                                                                            var("outVar"))),
                                                                                    list())))),
                                                                    field(
                                                                        Type.VARIABLE,
                                                                        lambda("midVar",
                                                                            Logic.ifElse(
                                                                                Sets.member(
                                                                                    var("midVar"),
                                                                                    var("tparamSet")),
                                                                                list(),
                                                                                casesWithDefault(
                                                                                    Type.TYPE_,
                                                                                    var("retPart"),
                                                                                    list(),
                                                                                    field(
                                                                                        Type.VARIABLE,
                                                                                        lambda(
                                                                                            "outVar",
                                                                                            Logic.ifElse(
                                                                                                Sets.member(
                                                                                                    var("outVar"),
                                                                                                    var("tparamSet")),
                                                                                                list(
                                                                                                    pair(
                                                                                                        var("inVar"),
                                                                                                        var("outVar"))),
                                                                                                list())))))))))))),
                                                apply(
                                                    ref(Coder.extractDirectReturn_go),
                                                    var("tparamSet"),
                                                    var("cod"))))))))))));

    public static final Def extractInOutPair = def(
        "extractInOutPair",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    list(),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            casesWithDefault(Type.TYPE_,
                                apply(
                                    var("hydra.strip.deannotateType"),
                                    proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft")),
                                list(),
                                field(
                                    Type.VARIABLE,
                                    lambda("inVar",
                                        let("retType",
                                            apply(
                                                ref(Coder.unwrapReturnType),
                                                proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                            casesWithDefault(Type.TYPE_,
                                                apply(
                                                    var("hydra.strip.deannotateType"),
                                                    var("retType")),
                                                list(),
                                                field(
                                                    Type.PAIR,
                                                    lambda("pt",
                                                        casesWithDefault(Type.TYPE_,
                                                            apply(
                                                                var("hydra.strip.deannotateType"),
                                                                proj(PairType.TYPE_, PairType.FIRST, "pt")),
                                                            list(),
                                                            field(
                                                                Type.VARIABLE,
                                                                lambda("outVar",
                                                                    list(
                                                                        pair(
                                                                            var("inVar"),
                                                                            var("outVar"))))))))))))))))));

    public static final Def extractTypeApplicationArgs = def(
        "extractTypeApplicationArgs",
        () -> lambda("typ",
                Lists.reverse(apply(ref(Coder.extractTypeApplicationArgs_go), var("typ")))));

    public static final Def extractTypeApplicationArgs_go = def(
        "extractTypeApplicationArgs_go",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    var("t"),
                    list(),
                    field(
                        Type.APPLICATION,
                        lambda("at",
                            Lists.cons(
                                proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at"),
                                apply(
                                    ref(Coder.extractTypeApplicationArgs_go),
                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at"))))))));

    public static final Def fieldTypeToFormalParam = def(
        "fieldTypeToFormalParam",
        () -> lambda(
                "aliases",
                "ft",
                "cx",
                "g",
                Eithers.bind(
                    apply(
                        ref(Coder.encodeType),
                        var("aliases"),
                        var("hydra.lib.sets.empty"),
                        proj(FieldType.TYPE_, FieldType.TYPE, "ft"),
                        var("cx"),
                        var("g")),
                    lambda("jt",
                        right(
                            apply(
                                ref(Utils.javaTypeToJavaFormalParameter),
                                var("jt"),
                                proj(FieldType.TYPE_, FieldType.NAME, "ft")))))));

    public static final Def filterByFlags = def(
        "filterByFlags",
        () -> lambda(
                "xs",
                "flags",
                Lists.map(
                    lambda("p", Pairs.first(var("p"))),
                    Lists.filter(
                        lambda("p", Pairs.second(var("p"))),
                        Lists.zip(var("xs"), var("flags"))))));

    public static final Def filterPhantomTypeArgs = def(
        "filterPhantomTypeArgs",
        () -> lambda(
                "calleeName",
                "allTypeArgs",
                "cx",
                "g",
                Eithers.bind(
                    right(apply(var("hydra.lexical.lookupBinding"), var("g"), var("calleeName"))),
                    lambda("mel",
                        Maybes.cases(
                            var("mel"),
                            right(var("allTypeArgs")),
                            lambda("el",
                                Maybes.cases(
                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                    right(var("allTypeArgs")),
                                    lambda("ts",
                                        let(
                                            java.util.Arrays.asList(
    field("schemeVars",
                                                Lists.filter(
                                                    lambda("v",
                                                        apply(ref(Coder.isSimpleName), var("v"))),
                                                    proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
    field("schemeTypeVars",
                                                apply(
                                                    ref(Coder.collectTypeVars),
                                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"))),
    field("schemeType",
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
    field("nParams",
                                                apply(
                                                    ref(Coder.countFunctionParams),
                                                    var("schemeType"))),
    field("peeled",
                                                apply(
                                                    ref(Coder.peelDomainTypes),
                                                    var("nParams"),
                                                    var("schemeType"))),
    field("calleeDoms",
                                                Pairs.first(var("peeled"))),
    field("calleeCod",
                                                Pairs.second(var("peeled"))),
    field("overgenSubst",
                                                apply(
                                                    ref(Coder.detectAccumulatorUnification),
                                                    var("calleeDoms"),
                                                    var("calleeCod"),
                                                    var("schemeVars"))),
    field("keepFlags",
                                                Lists.map(
                                                    lambda("v",
                                                        Logic.and_(
                                                            Sets.member(
                                                                var("v"),
                                                                var("schemeTypeVars")),
                                                            Logic.not_(
                                                                Maps.member(
                                                                    var("v"),
                                                                    var("overgenSubst"))))),
                                                    var("schemeVars")))),
                                            Logic.ifElse(
                                                Logic.not_(
                                                    Equality.equal(
                                                        Lists.length(var("schemeVars")),
                                                        Lists.length(var("allTypeArgs")))),
                                                right(var("allTypeArgs")),
                                                right(
                                                    apply(
                                                        ref(Coder.filterPhantomTypeArgs_filterAndApply),
                                                        var("allTypeArgs"),
                                                        var("keepFlags"),
                                                        var("overgenSubst")))))))))))));

    public static final Def filterPhantomTypeArgs_filterAndApply = def(
        "filterPhantomTypeArgs_filterAndApply",
        () -> lambda(
                "allTypeArgs",
                "keepFlags",
                "overgenSubst",
                let("filtered",
                    Lists.map(
                        lambda("p", Pairs.first(var("p"))),
                        Lists.filter(
                            lambda("p", Pairs.second(var("p"))),
                            Lists.zip(var("allTypeArgs"), var("keepFlags")))),
                    Logic.ifElse(
                        Logic.not_(Maps.null_(var("overgenSubst"))),
                        Lists.map(
                            lambda("t",
                                apply(
                                    ref(Coder.substituteTypeVarsWithTypes),
                                    var("overgenSubst"),
                                    var("t"))),
                            var("filtered")),
                        var("filtered")))));

    public static final Def findMatchingLambdaVar = def(
        "findMatchingLambdaVar",
        () -> lambda(
                "name",
                "lambdaVars",
                Logic.ifElse(
                    Sets.member(var("name"), var("lambdaVars")),
                    var("name"),
                    Logic.ifElse(
                        apply(ref(Coder.isLambdaBoundIn_isQualified), var("name")),
                        Maybes.fromMaybe(
                            var("name"),
                            Lists.find(
                                lambda("lv",
                                    Logic.and_(
                                        apply(ref(Coder.isLambdaBoundIn_isQualified), var("lv")),
                                        Equality.equal(
                                            apply(var("hydra.names.localNameOf"), var("lv")),
                                            apply(var("hydra.names.localNameOf"), var("name"))))),
                                Sets.toList(var("lambdaVars")))),
                        Logic.ifElse(
                            Sets.member(
                                wrap(Name.TYPE_,
                                    apply(var("hydra.names.localNameOf"), var("name"))),
                                var("lambdaVars")),
                            wrap(Name.TYPE_,
                                apply(var("hydra.names.localNameOf"), var("name"))),
                            var("name"))))));

    public static final Def findPairFirst = def(
        "findPairFirst",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    nothing(),
                    field(
                        Type.PAIR,
                        lambda("pt",
                            casesWithDefault(Type.TYPE_,
                                apply(
                                    var("hydra.strip.deannotateType"),
                                    proj(PairType.TYPE_, PairType.FIRST, "pt")),
                                nothing(),
                                field(Type.VARIABLE, lambda("v", just(var("v"))))))))));

    public static final Def findSelfRefVar = def(
        "findSelfRefVar",
        () -> lambda("grouped",
                let("selfRefs",
                    Lists.filter(
                        lambda("entry",
                            Lists.elem(Pairs.first(var("entry")), Pairs.second(var("entry")))),
                        Maps.toList(var("grouped"))),
                    Maybes.map(
                        lambda("entry", Pairs.first(var("entry"))),
                        Lists.maybeHead(var("selfRefs"))))));

    public static final Def first20Primes = def(
        "first20Primes",
        () -> list(
                bigint(java.math.BigInteger.valueOf(2L)),
                bigint(java.math.BigInteger.valueOf(3L)),
                bigint(java.math.BigInteger.valueOf(5L)),
                bigint(java.math.BigInteger.valueOf(7L)),
                bigint(java.math.BigInteger.valueOf(11L)),
                bigint(java.math.BigInteger.valueOf(13L)),
                bigint(java.math.BigInteger.valueOf(17L)),
                bigint(java.math.BigInteger.valueOf(19L)),
                bigint(java.math.BigInteger.valueOf(23L)),
                bigint(java.math.BigInteger.valueOf(29L)),
                bigint(java.math.BigInteger.valueOf(31L)),
                bigint(java.math.BigInteger.valueOf(37L)),
                bigint(java.math.BigInteger.valueOf(41L)),
                bigint(java.math.BigInteger.valueOf(43L)),
                bigint(java.math.BigInteger.valueOf(47L)),
                bigint(java.math.BigInteger.valueOf(53L)),
                bigint(java.math.BigInteger.valueOf(59L)),
                bigint(java.math.BigInteger.valueOf(61L)),
                bigint(java.math.BigInteger.valueOf(67L)),
                bigint(java.math.BigInteger.valueOf(71L))));

    public static final Def flattenApps = def(
        "flattenApps",
        () -> lambda(
                "t",
                "acc",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    pair(var("acc"), var("t")),
                    field(
                        Term.APPLICATION,
                        lambda("app",
                            apply(
                                ref(Coder.flattenApps),
                                proj(Application.TYPE_, Application.FUNCTION, "app"),
                                Lists.cons(
                                    proj(Application.TYPE_, Application.ARGUMENT, "app"),
                                    var("acc"))))))));

    public static final Def flattenBindings = def(
        "flattenBindings",
        () -> lambda("bindings",
                Lists.bind(
                    var("bindings"),
                    lambda("b",
                        casesWithDefault(Term.TYPE_,
                            apply(
                                var("hydra.strip.deannotateTerm"),
                                proj(Binding.TYPE_, Binding.TERM, "b")),
                            list(var("b")),
                            field(
                                Term.LET,
                                lambda("lt",
                                    Lists.concat2(
                                        apply(
                                            ref(Coder.flattenBindings),
                                            proj(Let.TYPE_, Let.BINDINGS, "lt")),
                                        list(
                                            record(Binding.TYPE_,
                                                field(
                                                    Binding.NAME,
                                                    proj(Binding.TYPE_, Binding.NAME, "b")),
                                                field(
                                                    Binding.TERM,
                                                    proj(Let.TYPE_, Let.BODY, "lt")),
                                                field(
                                                    Binding.TYPE_SCHEME,
                                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b"))))))))))));

    public static final Def freshJavaName = def(
        "freshJavaName",
        () -> lambda(
                "base",
                "avoid",
                apply(ref(Coder.freshJavaName_go), var("base"), var("avoid"), int32(2))));

    public static final Def freshJavaName_go = def(
        "freshJavaName_go",
        () -> lambda(
                "base",
                "avoid",
                "i",
                let("candidate",
                    wrap(Name.TYPE_,
                        Strings.cat2(
                            apply(unwrap(Name.TYPE_), var("base")),
                            Literals.showInt32(var("i")))),
                    Logic.ifElse(
                        Sets.member(var("candidate"), var("avoid")),
                        apply(
                            ref(Coder.freshJavaName_go),
                            var("base"),
                            var("avoid"),
                            Math_.add(var("i"), int32(1))),
                        var("candidate")))));

    public static final Def functionCall = def(
        "functionCall",
        () -> lambda(
                "env",
                "isPrim",
                "name",
                "args",
                "typeApps",
                "cx",
                "g",
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("isLambdaBound",
                        apply(
                            ref(Coder.isLambdaBoundIn),
                            var("name"),
                            proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases"))),
                    Eithers.bind(
                        Eithers.mapList(
                            lambda("arg",
                                apply(
                                    ref(Coder.encodeTerm),
                                    var("env"),
                                    var("arg"),
                                    var("cx"),
                                    var("g"))),
                            var("args")),
                        lambda("jargs0",
                            let(
                                field("wrapResult",
                                    apply(ref(Coder.wrapLazyArguments), var("name"), var("jargs0"))),
                                field("jargs",
                                    Pairs.first(var("wrapResult"))),
                                field("mMethodOverride",
                                    Pairs.second(var("wrapResult"))),
                                Logic.ifElse(
                                    Logic.or_(
                                        apply(ref(Coder.isLocalVariable), var("name")),
                                        var("isLambdaBound")),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeVariable),
                                            var("env"),
                                            var("name"),
                                            var("cx"),
                                            var("g")),
                                        lambda("baseExpr",
                                            right(
                                                Lists.foldl(
                                                    lambda(
                                                        "acc",
                                                        "jarg",
                                                        apply(
                                                            ref(Coder.applyJavaArg),
                                                            var("acc"),
                                                            var("jarg"))),
                                                    var("baseExpr"),
                                                    var("jargs"))))),
                                    let("overrideMethodName",
                                        lambda("jid",
                                            Maybes.cases(
                                                var("mMethodOverride"),
                                                var("jid"),
                                                lambda("m",
                                                    let("s",
                                                        apply(
                                                            unwrap(Identifier.TYPE_),
                                                            var("jid")),
                                                        wrap(Identifier.TYPE_,
                                                            Strings.cat2(
                                                                Strings.fromList(
                                                                    Lists.take(
                                                                        Math_.sub(
                                                                            Strings.length(var("s")),
                                                                            Strings.length(
                                                                                ref(Names.applyMethodName))),
                                                                        Strings.toList(var("s")))),
                                                                var("m"))))))),
                                        Logic.ifElse(
                                            Lists.null_(var("typeApps")),
                                            let("header",
                                                inject(
                                                    MethodInvocation_Header.TYPE_,
                                                    MethodInvocation_Header.SIMPLE,
                                                    wrap(MethodName.TYPE_,
                                                        apply(
                                                            var("overrideMethodName"),
                                                            apply(
                                                                ref(Coder.elementJavaIdentifier),
                                                                var("isPrim"),
                                                                bool(false),
                                                                var("aliases"),
                                                                var("name"))))),
                                                right(
                                                    apply(
                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                        record(
                                                            MethodInvocation.TYPE_,
                                                            field(
                                                                MethodInvocation.HEADER,
                                                                var("header")),
                                                            field(
                                                                MethodInvocation.ARGUMENTS,
                                                                var("jargs")))))),
                                            let(
                                                field("qn",
                                                    apply(
                                                        var("hydra.names.qualifyName"),
                                                        var("name"))),
                                                field("mns",
                                                    proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                                                field("localName",
                                                    proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                                                Maybes.cases(
                                                    var("mns"),
                                                    let("header",
                                                        inject(
                                                            MethodInvocation_Header.TYPE_,
                                                            MethodInvocation_Header.SIMPLE,
                                                            wrap(MethodName.TYPE_,
                                                                apply(
                                                                    var("overrideMethodName"),
                                                                    apply(
                                                                        ref(Coder.elementJavaIdentifier),
                                                                        var("isPrim"),
                                                                        bool(false),
                                                                        var("aliases"),
                                                                        var("name"))))),
                                                        right(
                                                            apply(
                                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                                record(
                                                                    MethodInvocation.TYPE_,
                                                                    field(
                                                                        MethodInvocation.HEADER,
                                                                        var("header")),
                                                                    field(
                                                                        MethodInvocation.ARGUMENTS,
                                                                        var("jargs")))))),
                                                    lambda("ns_",
                                                        let(
                                                            field("classId",
                                                                apply(
                                                                    ref(Utils.nameToJavaName),
                                                                    var("aliases"),
                                                                    apply(
                                                                        ref(Coder.elementsQualifiedName),
                                                                        var("ns_")))),
                                                            field("methodId",
                                                                Logic.ifElse(
                                                                    var("isPrim"),
                                                                    apply(
                                                                        var("overrideMethodName"),
                                                                        wrap(
                                                                            Identifier.TYPE_,
                                                                            Strings.cat2(
                                                                                apply(
                                                                                    unwrap(Identifier.TYPE_),
                                                                                    apply(
                                                                                        ref(Utils.nameToJavaName),
                                                                                        var("aliases"),
                                                                                        apply(
                                                                                            var("hydra.names.unqualifyName"),
                                                                                            record(
                                                                                                QualifiedName.TYPE_,
                                                                                                field(
                                                                                                    QualifiedName.MODULE_NAME,
                                                                                                    just(
                                                                                                        var("ns_"))),
                                                                                                field(
                                                                                                    QualifiedName.LOCAL,
                                                                                                    apply(
                                                                                                        var("hydra.formatting.capitalize"),
                                                                                                        var("localName"))))))),
                                                                                Strings.cat2(
                                                                                    string("."),
                                                                                    ref(Names.applyMethodName))))),
                                                                    wrap(
                                                                        Identifier.TYPE_,
                                                                        apply(
                                                                            ref(Utils.sanitizeJavaName),
                                                                            var("localName"))))),
                                                            Eithers.bind(
                                                                Eithers.mapList(
                                                                    lambda("t",
                                                                        Eithers.bind(
                                                                            apply(
                                                                                ref(Coder.encodeType),
                                                                                var("aliases"),
                                                                                var("hydra.lib.sets.empty"),
                                                                                var("t"),
                                                                                var("cx"),
                                                                                var("g")),
                                                                            lambda("jt",
                                                                                Eithers.bind(
                                                                                    apply(
                                                                                        ref(Utils.javaTypeToJavaReferenceType),
                                                                                        var("jt"),
                                                                                        var("cx")),
                                                                                    lambda("rt",
                                                                                        right(
                                                                                            inject(
                                                                                                TypeArgument.TYPE_,
                                                                                                TypeArgument.REFERENCE,
                                                                                                var("rt")))))))),
                                                                    var("typeApps")),
                                                                lambda("jTypeArgs",
                                                                    right(
                                                                        apply(
                                                                            ref(Utils.javaMethodInvocationToJavaExpression),
                                                                            apply(
                                                                                ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                                var("classId"),
                                                                                var("methodId"),
                                                                                var("jTypeArgs"),
                                                                                var("jargs")))))))))))))))))));

    public static final Def getCodomain = def(
        "getCodomain",
        () -> lambda(
                "ann",
                "cx",
                "g",
                Eithers.map(
                    lambda("ft",
                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                    apply(ref(Coder.getFunctionType), var("ann"), var("cx"), var("g")))));

    public static final Def getFunctionType = def(
        "getFunctionType",
        () -> lambda(
                "ann",
                "cx",
                "g",
                Eithers.bind(
                    Eithers.bimap(
                        lambda("__de",
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                        lambda("__a", var("__a")),
                        apply(var("hydra.annotations.getType"), var("g"), var("ann"))),
                    lambda("mt",
                        Maybes.cases(
                            var("mt"),
                            left(
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        string("type annotation is required for function and elimination terms in Java")))),
                            lambda("t",
                                casesWithDefault(Type.TYPE_,
                                    var("t"),
                                    left(
                                        inject(Error_.TYPE_,
                                            Error_.OTHER,
                                            wrap(OtherError.TYPE_,
                                                Strings.cat2(
                                                    string("expected function type, got: "),
                                                    apply(var("hydra.show.core.type"), var("t")))))),
                                    field(Type.FUNCTION, lambda("ft", right(var("ft")))))))))));

    public static final Def groupPairsByFirst = def(
        "groupPairsByFirst",
        () -> lambda("pairs",
                Lists.foldl(
                    lambda(
                        "m",
                        "p",
                        let(
                            field("k",
                                Pairs.first(var("p"))),
                            field("v",
                                Pairs.second(var("p"))),
                            Maps.alter(
                                lambda("mv",
                                    Maybes.maybe(
                                        just(list(var("v"))),
                                        lambda("vs", just(Lists.concat2(var("vs"), list(var("v"))))),
                                        var("mv"))),
                                var("k"),
                                var("m")))),
                    var("hydra.lib.maps.empty"),
                    var("pairs"))));

    public static final Def hashCodeCompareExpr = def(
        "hashCodeCompareExpr",
        () -> lambda(
                "otherVar",
                "fname",
                let(
                    field("header",
                        inject(MethodInvocation_Header.TYPE_,
                            MethodInvocation_Header.COMPLEX,
                            record(MethodInvocation_Complex.TYPE_,
                                field(
                                    MethodInvocation_Complex.VARIANT,
                                    inject(MethodInvocation_Variant.TYPE_,
                                        MethodInvocation_Variant.TYPE,
                                        apply(
                                            ref(Utils.javaTypeName),
                                            wrap(Identifier.TYPE_,
                                                string("Integer"))))),
                                field(
                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                    list()),
                                field(
                                    MethodInvocation_Complex.IDENTIFIER,
                                    wrap(Identifier.TYPE_, string("compare")))))),
                    field("thisHashCode",
                        apply(
                            ref(Utils.javaMethodInvocationToJavaExpression),
                            record(MethodInvocation.TYPE_,
                                field(
                                    MethodInvocation.HEADER,
                                    inject(MethodInvocation_Header.TYPE_,
                                        MethodInvocation_Header.COMPLEX,
                                        record(MethodInvocation_Complex.TYPE_,
                                            field(
                                                MethodInvocation_Complex.VARIANT,
                                                inject(
                                                    MethodInvocation_Variant.TYPE_,
                                                    MethodInvocation_Variant.EXPRESSION,
                                                    record(ExpressionName.TYPE_,
                                                        field(
                                                            ExpressionName.QUALIFIER,
                                                            nothing()),
                                                        field(
                                                            ExpressionName.IDENTIFIER,
                                                            wrap(Identifier.TYPE_,
                                                                apply(
                                                                    ref(Utils.sanitizeJavaName),
                                                                    var("fname"))))))),
                                            field(
                                                MethodInvocation_Complex.TYPE_ARGUMENTS,
                                                list()),
                                            field(
                                                MethodInvocation_Complex.IDENTIFIER,
                                                wrap(Identifier.TYPE_,
                                                    ref(Names.hashCodeMethodName)))))),
                                field(MethodInvocation.ARGUMENTS, list())))),
                    field("otherHashCode",
                        apply(
                            ref(Utils.javaMethodInvocationToJavaExpression),
                            record(MethodInvocation.TYPE_,
                                field(
                                    MethodInvocation.HEADER,
                                    inject(MethodInvocation_Header.TYPE_,
                                        MethodInvocation_Header.COMPLEX,
                                        record(MethodInvocation_Complex.TYPE_,
                                            field(
                                                MethodInvocation_Complex.VARIANT,
                                                inject(
                                                    MethodInvocation_Variant.TYPE_,
                                                    MethodInvocation_Variant.EXPRESSION,
                                                    apply(
                                                        ref(Utils.fieldExpression),
                                                        apply(
                                                            ref(Utils.javaIdentifier),
                                                            var("otherVar")),
                                                        apply(
                                                            ref(Utils.javaIdentifier),
                                                            var("fname"))))),
                                            field(
                                                MethodInvocation_Complex.TYPE_ARGUMENTS,
                                                list()),
                                            field(
                                                MethodInvocation_Complex.IDENTIFIER,
                                                wrap(Identifier.TYPE_,
                                                    ref(Names.hashCodeMethodName)))))),
                                field(MethodInvocation.ARGUMENTS, list())))),
                    apply(
                        ref(Utils.javaMethodInvocationToJavaExpression),
                        record(MethodInvocation.TYPE_,
                            field(MethodInvocation.HEADER, var("header")),
                            field(
                                MethodInvocation.ARGUMENTS,
                                list(var("thisHashCode"), var("otherHashCode"))))))));

    public static final Def hashCodeMultPair = def(
        "hashCodeMultPair",
        () -> lambda(
                "i",
                "fname",
                let(
                    field("fnameStr",
                        apply(unwrap(Name.TYPE_), var("fname"))),
                    field("lhs",
                        inject(MultiplicativeExpression.TYPE_,
                            MultiplicativeExpression.UNARY,
                            apply(
                                ref(Utils.javaPrimaryToJavaUnaryExpression),
                                apply(
                                    ref(Utils.javaLiteralToJavaPrimary),
                                    apply(ref(Utils.javaInt), var("i")))))),
                    field("rhs",
                        apply(
                            ref(Utils.javaPostfixExpressionToJavaUnaryExpression),
                            apply(
                                ref(Utils.javaMethodInvocationToJavaPostfixExpression),
                                record(MethodInvocation.TYPE_,
                                    field(
                                        MethodInvocation.HEADER,
                                        inject(MethodInvocation_Header.TYPE_,
                                            MethodInvocation_Header.COMPLEX,
                                            record(MethodInvocation_Complex.TYPE_,
                                                field(
                                                    MethodInvocation_Complex.VARIANT,
                                                    inject(
                                                        MethodInvocation_Variant.TYPE_,
                                                        MethodInvocation_Variant.TYPE,
                                                        apply(
                                                            ref(Utils.javaTypeName),
                                                            wrap(Identifier.TYPE_,
                                                                string("java.util.Objects"))))),
                                                field(
                                                    MethodInvocation_Complex.TYPE_ARGUMENTS,
                                                    list()),
                                                field(
                                                    MethodInvocation_Complex.IDENTIFIER,
                                                    wrap(Identifier.TYPE_,
                                                        ref(Names.hashCodeMethodName)))))),
                                    field(
                                        MethodInvocation.ARGUMENTS,
                                        list(
                                            apply(
                                                ref(Utils.javaExpressionNameToJavaExpression),
                                                record(ExpressionName.TYPE_,
                                                    field(
                                                        ExpressionName.QUALIFIER,
                                                        nothing()),
                                                    field(
                                                        ExpressionName.IDENTIFIER,
                                                        wrap(Identifier.TYPE_,
                                                            apply(
                                                                ref(Utils.sanitizeJavaName),
                                                                var("fnameStr")))))))))))),
                    inject(MultiplicativeExpression.TYPE_,
                        MultiplicativeExpression.TIMES,
                        record(MultiplicativeExpression_Binary.TYPE_,
                            field(MultiplicativeExpression_Binary.LHS, var("lhs")),
                            field(MultiplicativeExpression_Binary.RHS, var("rhs")))))));

    public static final Def innerClassRef = def(
        "innerClassRef",
        () -> lambda(
                "aliases",
                "name",
                "local",
                let("id",
                    apply(
                        unwrap(Identifier.TYPE_),
                        apply(
                            ref(Utils.nameToJavaName),
                            var("aliases"),
                            var("name"))),
                    wrap(Identifier.TYPE_,
                        Strings.cat2(Strings.cat2(var("id"), string(".")), var("local"))))));

    public static final Def insertBranchVar = def(
        "insertBranchVar",
        () -> lambda(
                "name",
                "env",
                let("aliases",
                    proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env"),
                    record(JavaEnvironment.TYPE_,
                        field(
                            JavaEnvironment.ALIASES,
                            record(Aliases.TYPE_,
                                field(
                                    Aliases.CURRENT_NAMESPACE,
                                    proj(Aliases.TYPE_, Aliases.CURRENT_NAMESPACE, "aliases")),
                                field(
                                    Aliases.PACKAGES,
                                    proj(Aliases.TYPE_, Aliases.PACKAGES, "aliases")),
                                field(
                                    Aliases.BRANCH_VARS,
                                    Sets.insert(
                                        var("name"),
                                        proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases"))),
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
                                field(Aliases.METHOD_CODOMAIN, nothing()),
                                field(
                                    Aliases.THUNKED_VARS,
                                    proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases")))),
                        field(
                            JavaEnvironment.GRAPH,
                            proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env"))))));

    public static final Def interfaceTypes = def(
        "interfaceTypes",
        () -> lambda(
                "isSer",
                "aliases",
                "tparams",
                "elName",
                let(
                    field("javaSerializableType",
                        wrap(InterfaceType.TYPE_,
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
                                        string("Serializable"))),
                                field(ClassType.ARGUMENTS, list())))),
                    field("selfTypeArg",
                        inject(TypeArgument.TYPE_,
                            TypeArgument.REFERENCE,
                            apply(
                                ref(Utils.nameToJavaReferenceType),
                                var("aliases"),
                                bool(false),
                                Lists.map(
                                    lambda("tp_",
                                        apply(
                                            ref(Utils.typeParameterToTypeArgument),
                                            var("tp_"))),
                                    var("tparams")),
                                var("elName"),
                                nothing()))),
                    field("javaComparableType",
                        wrap(InterfaceType.TYPE_,
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
                                        string("Comparable"))),
                                field(
                                    ClassType.ARGUMENTS,
                                    list(var("selfTypeArg")))))),
                    Logic.ifElse(
                        var("isSer"),
                        list(var("javaSerializableType"), var("javaComparableType")),
                        list()))));

    public static final Def isBigNumericType = def(
        "isBigNumericType",
        () -> lambda("typ",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("typ")),
                    bool(false),
                    field(
                        Type.LITERAL,
                        lambda("lt",
                            casesWithDefault(LiteralType.TYPE_,
                                var("lt"),
                                bool(false),
                                field(LiteralType.DECIMAL, constant(bool(true))),
                                field(
                                    LiteralType.INTEGER,
                                    lambda("it",
                                        casesWithDefault(IntegerType.TYPE_,
                                            var("it"),
                                            bool(false),
                                            field(
                                                IntegerType.BIGINT,
                                                constant(bool(true))))))))))));

    public static final Def isBinaryType = def(
        "isBinaryType",
        () -> lambda("typ",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("typ")),
                    bool(false),
                    field(
                        Type.LITERAL,
                        lambda("lt",
                            casesWithDefault(LiteralType.TYPE_,
                                var("lt"),
                                bool(false),
                                field(LiteralType.BINARY, constant(bool(true)))))))));

    public static final Def isFieldUnitType = def(
        "isFieldUnitType",
        () -> lambda(
                "typeName",
                "fieldName",
                "cx",
                "g",
                let("schemaTypes",
                    proj(Graph.TYPE_, Graph.SCHEMA_TYPES, "g"),
                    Maybes.cases(
                        Maps.lookup(var("typeName"), var("schemaTypes")),
                        right(bool(false)),
                        lambda("ts",
                            casesWithDefault(Type.TYPE_,
                                apply(
                                    var("hydra.strip.deannotateType"),
                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                                right(bool(false)),
                                field(
                                    Type.UNION,
                                    lambda("rt",
                                        right(
                                            Maybes.cases(
                                                Lists.find(
                                                    lambda("ft",
                                                        Equality.equal(
                                                            proj(FieldType.TYPE_, FieldType.NAME, "ft"),
                                                            var("fieldName"))),
                                                    var("rt")),
                                                bool(false),
                                                lambda("ft",
                                                    apply(
                                                        var("hydra.predicates.isUnitType"),
                                                        apply(
                                                            var("hydra.strip.deannotateType"),
                                                            proj(FieldType.TYPE_, FieldType.TYPE, "ft"))))))))))))));

    public static final Def isLambdaBoundIn = def(
        "isLambdaBoundIn",
        () -> lambda(
                "name",
                "lambdaVars",
                Logic.or_(
                    Sets.member(var("name"), var("lambdaVars")),
                    Logic.or_(
                        Logic.and_(
                            apply(ref(Coder.isLambdaBoundIn_isQualified), var("name")),
                            Maybes.isJust(
                                Lists.find(
                                    lambda("lv",
                                        Logic.and_(
                                            apply(ref(Coder.isLambdaBoundIn_isQualified), var("lv")),
                                            Equality.equal(
                                                apply(var("hydra.names.localNameOf"), var("lv")),
                                                apply(var("hydra.names.localNameOf"), var("name"))))),
                                    Sets.toList(var("lambdaVars"))))),
                        Logic.and_(
                            Logic.not_(apply(ref(Coder.isLambdaBoundIn_isQualified), var("name"))),
                            Sets.member(
                                wrap(Name.TYPE_,
                                    apply(var("hydra.names.localNameOf"), var("name"))),
                                var("lambdaVars")))))));

    public static final Def isLambdaBoundIn_isQualified = def(
        "isLambdaBoundIn_isQualified",
        () -> lambda("n",
                Maybes.isJust(
                    apply(
                        project(QualifiedName.TYPE_, QualifiedName.MODULE_NAME),
                        apply(var("hydra.names.qualifyName"), var("n"))))));

    public static final Def isLambdaBoundVariable = def(
        "isLambdaBoundVariable",
        () -> lambda("name",
                let("v",
                    apply(unwrap(Name.TYPE_), var("name")),
                    Equality.lte(Strings.length(var("v")), int32(4)))));

    public static final Def isLocalVariable = def(
        "isLocalVariable",
        () -> lambda("name",
                Maybes.isNothing(
                    apply(
                        project(QualifiedName.TYPE_, QualifiedName.MODULE_NAME),
                        apply(var("hydra.names.qualifyName"), var("name"))))));

    public static final Def isNonComparableType = def(
        "isNonComparableType",
        () -> lambda("typ",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("typ")),
                    bool(false),
                    field(Type.EITHER, constant(bool(true))),
                    field(Type.FUNCTION, constant(bool(true))),
                    field(Type.UNIT, constant(bool(true))),
                    field(
                        Type.LITERAL,
                        lambda("lt",
                            casesWithDefault(LiteralType.TYPE_,
                                var("lt"),
                                bool(false),
                                field(LiteralType.BINARY, constant(bool(true)))))),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            apply(
                                ref(Coder.isNonComparableType),
                                proj(ForallType.TYPE_, ForallType.BODY, "ft")))))));

    public static final Def isRecursiveVariable = def(
        "isRecursiveVariable",
        () -> lambda(
                "aliases",
                "name",
                Sets.member(
                    var("name"),
                    proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases"))));

    public static final Def isSerializableJavaType = def(
        "isSerializableJavaType",
        () -> lambda("typ", apply(var("hydra.predicates.isNominalType"), var("typ"))));

    public static final Def isSimpleName = def(
        "isSimpleName",
        () -> lambda("name",
                Equality.equal(
                    Lists.length(
                        Strings.splitOn(
                            string("."),
                            apply(unwrap(Name.TYPE_), var("name")))),
                    int32(1))));

    public static final Def isUnresolvedInferenceVar = def(
        "isUnresolvedInferenceVar",
        () -> lambda("name",
                let("chars",
                    Strings.toList(apply(unwrap(Name.TYPE_), var("name"))),
                    Maybes.fromMaybe(
                        bool(false),
                        Maybes.map(
                            lambda("p",
                                let(
                                    field("firstCh",
                                        Pairs.first(var("p"))),
                                    field("rest",
                                        Pairs.second(var("p"))),
                                    Logic.ifElse(
                                        Logic.not_(Equality.equal(var("firstCh"), int32(116))),
                                        bool(false),
                                        Logic.and_(
                                            Logic.not_(Lists.null_(var("rest"))),
                                            Lists.null_(
                                                Lists.filter(
                                                    lambda("c",
                                                        Logic.not_(
                                                            apply(
                                                                ref(Coder.isUnresolvedInferenceVar_isDigit),
                                                                var("c")))),
                                                    var("rest"))))))),
                            Lists.uncons(var("chars")))))));

    public static final Def isUnresolvedInferenceVar_isDigit = def(
        "isUnresolvedInferenceVar_isDigit",
        () -> lambda("c",
                Logic.and_(Equality.gte(var("c"), int32(48)), Equality.lte(var("c"), int32(57)))));

    public static final Def java11Features = def(
        "java11Features",
        () -> record(JavaFeatures.TYPE_,
                field(JavaFeatures.SUPPORTS_DIAMOND_OPERATOR, bool(true))));

    public static final Def java8Features = def(
        "java8Features",
        () -> record(JavaFeatures.TYPE_,
                field(JavaFeatures.SUPPORTS_DIAMOND_OPERATOR, bool(false))));

    public static final Def javaComparableRefType = def(
        "javaComparableRefType",
        () -> inject(ReferenceType.TYPE_,
                ReferenceType.CLASS_OR_INTERFACE,
                inject(ClassOrInterfaceType.TYPE_,
                    ClassOrInterfaceType.CLASS,
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
                                string("Comparable"))),
                        field(ClassType.ARGUMENTS, list())))));

    public static final Def javaEnvGetGraph = def(
        "javaEnvGetGraph",
        () -> lambda("env",
                proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")));

    public static final Def javaEnvSetGraph = def(
        "javaEnvSetGraph",
        () -> lambda(
                "g",
                "env",
                record(JavaEnvironment.TYPE_,
                    field(
                        JavaEnvironment.ALIASES,
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field(JavaEnvironment.GRAPH, var("g")))));

    public static final Def javaFeatures = def(
        "javaFeatures",
        () -> ref(Coder.java11Features));

    public static final Def javaIdentifierToString = def(
        "javaIdentifierToString",
        () -> lambda("id", apply(unwrap(Identifier.TYPE_), var("id"))));

    public static final Def javaTypeArgumentsForNamedType = def(
        "javaTypeArgumentsForNamedType",
        () -> lambda(
                "tname",
                "cx",
                "g",
                Eithers.bind(
                    apply(var("hydra.resolution.requireType"), var("cx"), var("g"), var("tname")),
                    lambda("typ",
                        right(
                            Lists.map(
                                lambda("tp_",
                                    apply(
                                        ref(Utils.typeParameterToTypeArgument),
                                        var("tp_"))),
                                apply(ref(Coder.javaTypeParametersForType), var("typ"))))))));

    public static final Def javaTypeArgumentsForType = def(
        "javaTypeArgumentsForType",
        () -> lambda("typ",
                Lists.reverse(
                    Lists.map(
                        ref(Utils.typeParameterToTypeArgument),
                        apply(ref(Coder.javaTypeParametersForType), var("typ"))))));

    public static final Def javaTypeParametersForType = def(
        "javaTypeParametersForType",
        () -> lambda("typ",
                let(
                    field("toParam",
                        lambda("name",
                            apply(
                                ref(Utils.javaTypeParameter),
                                apply(
                                    var("hydra.formatting.capitalize"),
                                    apply(unwrap(Name.TYPE_), var("name")))))),
                    field("boundVars",
                        apply(ref(Coder.javaTypeParametersForType_bvars), var("typ"))),
                    field("freeVars",
                        Lists.filter(
                            lambda("v", apply(ref(Coder.isLambdaBoundVariable), var("v"))),
                            Sets.toList(
                                apply(var("hydra.variables.freeVariablesInType"), var("typ"))))),
                    field("vars",
                        Lists.nub(Lists.concat2(var("boundVars"), var("freeVars")))),
                    Lists.map(var("toParam"), var("vars")))));

    public static final Def javaTypeParametersForType_bvars = def(
        "javaTypeParametersForType_bvars",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    var("t"),
                    list(),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            Lists.cons(
                                proj(ForallType.TYPE_, ForallType.PARAMETER, "ft"),
                                apply(
                                    ref(Coder.javaTypeParametersForType_bvars),
                                    proj(ForallType.TYPE_, ForallType.BODY, "ft"))))))));

    public static final Def moduleToJava = def(
        "moduleToJava",
        () -> lambda(
                "mod",
                "defs",
                "cx",
                "g",
                Eithers.bind(
                    apply(
                        ref(Coder.encodeDefinitions),
                        var("mod"),
                        var("defs"),
                        var("cx"),
                        var("g")),
                    lambda("units",
                        right(
                            Maps.fromList(
                                Lists.map(
                                    lambda("entry",
                                        let(
                                            field("name",
                                                Pairs.first(var("entry"))),
                                            field("unit",
                                                Pairs.second(var("entry"))),
                                            pair(
                                                apply(ref(Coder.bindingNameToFilePath), var("name")),
                                                apply(
                                                    var("hydra.serialization.printExpr"),
                                                    apply(
                                                        var("hydra.serialization.parenthesize"),
                                                        apply(
                                                            ref(Serde.compilationUnitToExpr),
                                                            var("unit"))))))),
                                    Maps.toList(var("units")))))))));

    public static final Def nameMapToTypeMap = def(
        "nameMapToTypeMap",
        () -> lambda("m",
                Maps.map(
                    lambda("v", inject(Type.TYPE_, Type.VARIABLE, var("v"))),
                    var("m"))));

    public static final Def namespaceParent = def(
        "namespaceParent",
        () -> lambda("ns",
                let(
                    field("parts",
                        Strings.splitOn(
                            string("."),
                            apply(unwrap(ModuleName.TYPE_), var("ns")))),
                    field("initParts",
                        Maybes.fromMaybe(list(), Lists.maybeInit(var("parts")))),
                    Logic.ifElse(
                        Lists.null_(var("initParts")),
                        nothing(),
                        just(
                            wrap(ModuleName.TYPE_,
                                Strings.intercalate(string("."), var("initParts"))))))));

    public static final Def needsThunking = def(
        "needsThunking",
        () -> lambda("t",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("t")),
                    Lists.foldl(
                        lambda(
                            "b",
                            "st",
                            Logic.or_(var("b"), apply(ref(Coder.needsThunking), var("st")))),
                        bool(false),
                        apply(var("hydra.rewriting.subterms"), var("t"))),
                    field(Term.LET, constant(bool(true))),
                    field(Term.TYPE_APPLICATION, constant(bool(true))),
                    field(Term.TYPE_LAMBDA, constant(bool(true))))));

    public static final Def noComment = def(
        "noComment",
        () -> lambda("decl",
                record(ClassBodyDeclarationWithComments.TYPE_,
                    field(ClassBodyDeclarationWithComments.VALUE, var("decl")),
                    field(ClassBodyDeclarationWithComments.COMMENTS, nothing()))));

    public static final Def noInterfaceComment = def(
        "noInterfaceComment",
        () -> lambda("decl",
                record(InterfaceMemberDeclarationWithComments.TYPE_,
                    field(
                        InterfaceMemberDeclarationWithComments.VALUE,
                        var("decl")),
                    field(
                        InterfaceMemberDeclarationWithComments.COMMENTS,
                        nothing()))));

    public static final Def otherwiseBranch = def(
        "otherwiseBranch",
        () -> lambda(
                java.util.Arrays.asList("env", "aliases", "dom", "cod", "tname", "jcod", "targs", "d", "cx", "g"),
                let(
                    field("jdom",
                        inject(hydra.java.syntax.Type.TYPE_,
                            hydra.java.syntax.Type.REFERENCE,
                            apply(
                                ref(Utils.nameToJavaReferenceType),
                                var("aliases"),
                                bool(true),
                                var("targs"),
                                var("tname"),
                                nothing()))),
                    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
                    field("anns",
                        list(ref(Utils.overrideAnnotation))),
                    field("param",
                        apply(
                            ref(Utils.javaTypeToJavaFormalParameter),
                            var("jdom"),
                            wrap(Name.TYPE_, string("instance")))),
                    field("result",
                        inject(Result.TYPE_,
                            Result.TYPE,
                            wrap(UnannType.TYPE_, var("jcod")))),
                    Eithers.bind(
                        apply(
                            ref(Coder.analyzeJavaFunction),
                            var("env"),
                            var("d"),
                            var("cx"),
                            var("g")),
                        lambda("fs",
                            let(
                                field("bindings",
                                    proj(FunctionStructure.TYPE_, FunctionStructure.BINDINGS, "fs")),
                                field("rawBody",
                                    proj(FunctionStructure.TYPE_, FunctionStructure.BODY, "fs")),
                                field("innerBody",
                                    apply(
                                        ref(Coder.annotateBodyWithCod),
                                        var("cod"),
                                        var("rawBody"))),
                                field("env2",
                                    proj(FunctionStructure.TYPE_, FunctionStructure.ENVIRONMENT, "fs")),
                                Eithers.bind(
                                    apply(
                                        ref(Coder.bindingsToStatements),
                                        var("env2"),
                                        var("bindings"),
                                        var("cx"),
                                        var("g")),
                                    lambda("bindResult",
                                        let(
                                            field("bindingStmts",
                                                Pairs.first(var("bindResult"))),
                                            field("env3",
                                                Pairs.second(var("bindResult"))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.encodeTerm),
                                                    var("env3"),
                                                    var("innerBody"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("jret",
                                                    let(
                                                        field("returnStmt",
                                                            inject(
                                                                BlockStatement.TYPE_,
                                                                BlockStatement.STATEMENT,
                                                                apply(
                                                                    ref(Utils.javaReturnStatement),
                                                                    just(var("jret"))))),
                                                        field("allStmts",
                                                            Lists.concat2(
                                                                var("bindingStmts"),
                                                                list(var("returnStmt")))),
                                                        right(
                                                            apply(
                                                                ref(Coder.noComment),
                                                                apply(
                                                                    ref(Utils.methodDeclaration),
                                                                    var("mods"),
                                                                    list(),
                                                                    var("anns"),
                                                                    ref(Names.otherwiseMethodName),
                                                                    list(var("param")),
                                                                    var("result"),
                                                                    just(var("allStmts")))))))))))))))));

    public static final Def peelDomainTypes = def(
        "peelDomainTypes",
        () -> lambda(
                "n",
                "t",
                Logic.ifElse(
                    Equality.lte(var("n"), int32(0)),
                    pair(list(), var("t")),
                    casesWithDefault(Type.TYPE_,
                        apply(var("hydra.strip.deannotateType"), var("t")),
                        pair(list(), var("t")),
                        field(
                            Type.FUNCTION,
                            lambda("ft",
                                let("rest",
                                    apply(
                                        ref(Coder.peelDomainTypes),
                                        Math_.sub(var("n"), int32(1)),
                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                    pair(
                                        Lists.cons(
                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                            Pairs.first(var("rest"))),
                                        Pairs.second(var("rest"))))))))));

    public static final Def peelDomainsAndCod = def(
        "peelDomainsAndCod",
        () -> lambda(
                "n",
                "t",
                Logic.ifElse(
                    Equality.lte(var("n"), int32(0)),
                    pair(list(), var("t")),
                    casesWithDefault(Type.TYPE_,
                        apply(var("hydra.strip.deannotateType"), var("t")),
                        pair(list(), var("t")),
                        field(
                            Type.FUNCTION,
                            lambda("ft",
                                let("rest",
                                    apply(
                                        ref(Coder.peelDomainsAndCod),
                                        Math_.sub(var("n"), int32(1)),
                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                    pair(
                                        Lists.cons(
                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                            Pairs.first(var("rest"))),
                                        Pairs.second(var("rest"))))))))));

    public static final Def peelExpectedTypes = def(
        "peelExpectedTypes",
        () -> lambda(
                "subst",
                "n",
                "t",
                Logic.ifElse(
                    Equality.equal(var("n"), int32(0)),
                    list(),
                    casesWithDefault(Type.TYPE_,
                        apply(var("hydra.strip.deannotateType"), var("t")),
                        list(),
                        field(
                            Type.FUNCTION,
                            lambda("ft",
                                Lists.cons(
                                    apply(
                                        ref(Coder.applySubstFull),
                                        var("subst"),
                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft")),
                                    apply(
                                        ref(Coder.peelExpectedTypes),
                                        var("subst"),
                                        Math_.sub(var("n"), int32(1)),
                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")))))))));

    public static final Def propagateType = def(
        "propagateType",
        () -> lambda(
                "typ",
                "term",
                let("setTypeAnn",
                    lambda("t",
                        apply(
                            var("hydra.annotations.setTermAnnotation"),
                            var("hydra.constants.keyType"),
                            just(apply(var("hydra.encode.core.type"), var("typ"))),
                            var("t"))),
                    casesWithDefault(Term.TYPE_,
                        apply(var("hydra.strip.deannotateTerm"), var("term")),
                        apply(var("setTypeAnn"), var("term")),
                        field(
                            Term.LAMBDA,
                            constant(
                                let("annotated",
                                    apply(var("setTypeAnn"), var("term")),
                                    casesWithDefault(Type.TYPE_,
                                        apply(var("hydra.strip.deannotateType"), var("typ")),
                                        var("annotated"),
                                        field(
                                            Type.FUNCTION,
                                            lambda("ft",
                                                apply(
                                                    ref(Coder.propagateType_propagateIntoLambda),
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"),
                                                    var("annotated")))))))),
                        field(
                            Term.LET,
                            lambda("lt",
                                let("propagatedBindings",
                                    Lists.map(
                                        lambda("b",
                                            Maybes.maybe(
                                                var("b"),
                                                lambda("ts",
                                                    record(Binding.TYPE_,
                                                        field(
                                                            Binding.NAME,
                                                            proj(Binding.TYPE_, Binding.NAME, "b")),
                                                        field(
                                                            Binding.TERM,
                                                            apply(
                                                                ref(Coder.propagateType),
                                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"),
                                                                proj(Binding.TYPE_, Binding.TERM, "b"))),
                                                        field(
                                                            Binding.TYPE_SCHEME,
                                                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b")))),
                                                proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b"))),
                                        proj(Let.TYPE_, Let.BINDINGS, "lt")),
                                    apply(
                                        var("setTypeAnn"),
                                        apply(
                                            ref(Coder.propagateType_rebuildLet),
                                            var("term"),
                                            var("propagatedBindings"),
                                            apply(
                                                ref(Coder.propagateType),
                                                var("typ"),
                                                proj(Let.TYPE_, Let.BODY, "lt"))))))),
                        field(
                            Term.APPLICATION,
                            lambda("app",
                                let(
                                    field("fun",
                                        proj(Application.TYPE_, Application.FUNCTION, "app")),
                                    field("arg",
                                        proj(Application.TYPE_, Application.ARGUMENT, "app")),
                                    field("annotatedFun",
                                        casesWithDefault(Term.TYPE_,
                                            apply(var("hydra.strip.deannotateTerm"), var("fun")),
                                            var("fun"),
                                            field(
                                                Term.CASES,
                                                lambda("cs",
                                                    let(
                                                        field("dom",
                                                            apply(
                                                                var("hydra.resolution.nominalApplication"),
                                                                proj(CaseStatement.TYPE_, CaseStatement.TYPE_NAME, "cs"),
                                                                list())),
                                                        field("ft",
                                                            inject(Type.TYPE_,
                                                                Type.FUNCTION,
                                                                record(
                                                                    FunctionType.TYPE_,
                                                                    field(
                                                                        FunctionType.DOMAIN,
                                                                        var("dom")),
                                                                    field(
                                                                        FunctionType.CODOMAIN,
                                                                        var("typ"))))),
                                                        apply(
                                                            var("hydra.annotations.setTermAnnotation"),
                                                            var("hydra.constants.keyType"),
                                                            just(
                                                                apply(
                                                                    var("hydra.encode.core.type"),
                                                                    var("ft"))),
                                                            var("fun"))))))),
                                    apply(
                                        var("setTypeAnn"),
                                        inject(Term.TYPE_,
                                            Term.APPLICATION,
                                            record(Application.TYPE_,
                                                field(
                                                    Application.FUNCTION,
                                                    var("annotatedFun")),
                                                field(Application.ARGUMENT, var("arg"))))))))))));

    public static final Def propagateType_propagateIntoLambda = def(
        "propagateType_propagateIntoLambda",
        () -> lambda(
                "cod",
                "t",
                casesWithDefault(Term.TYPE_,
                    var("t"),
                    var("t"),
                    field(
                        Term.ANNOTATED,
                        lambda("at",
                            inject(Term.TYPE_,
                                Term.ANNOTATED,
                                record(AnnotatedTerm.TYPE_,
                                    field(
                                        AnnotatedTerm.BODY,
                                        apply(
                                            ref(Coder.propagateType_propagateIntoLambda),
                                            var("cod"),
                                            proj(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY, "at"))),
                                    field(
                                        AnnotatedTerm.ANNOTATION,
                                        proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at")))))),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            inject(Term.TYPE_,
                                Term.LAMBDA,
                                record(Lambda.TYPE_,
                                    field(
                                        Lambda.PARAMETER,
                                        proj(Lambda.TYPE_, Lambda.PARAMETER, "lam")),
                                    field(
                                        Lambda.DOMAIN,
                                        proj(Lambda.TYPE_, Lambda.DOMAIN, "lam")),
                                    field(
                                        Lambda.BODY,
                                        apply(
                                            ref(Coder.propagateType),
                                            var("cod"),
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam"))))))))));

    public static final Def propagateType_rebuildLet = def(
        "propagateType_rebuildLet",
        () -> lambda(
                "t",
                "bindings",
                "newBody",
                casesWithDefault(Term.TYPE_,
                    var("t"),
                    var("t"),
                    field(
                        Term.ANNOTATED,
                        lambda("at",
                            inject(Term.TYPE_,
                                Term.ANNOTATED,
                                record(AnnotatedTerm.TYPE_,
                                    field(
                                        AnnotatedTerm.BODY,
                                        apply(
                                            ref(Coder.propagateType_rebuildLet),
                                            proj(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY, "at"),
                                            var("bindings"),
                                            var("newBody"))),
                                    field(
                                        AnnotatedTerm.ANNOTATION,
                                        proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at")))))),
                    field(
                        Term.LET,
                        constant(
                            inject(Term.TYPE_,
                                Term.LET,
                                record(Let.TYPE_,
                                    field(Let.BINDINGS, var("bindings")),
                                    field(Let.BODY, var("newBody")))))))));

    public static final Def propagateTypesInAppChain = def(
        "propagateTypesInAppChain",
        () -> lambda(
                "fixedCod",
                "resultType",
                "t",
                let(
                    field("flattened",
                        apply(ref(Coder.flattenApps), var("t"), list())),
                    field("args",
                        Pairs.first(var("flattened"))),
                    field("fun",
                        Pairs.second(var("flattened"))),
                    field("lambdaDomsResult",
                        apply(ref(Coder.collectLambdaDomains), var("fun"))),
                    field("lambdaDoms",
                        Pairs.first(var("lambdaDomsResult"))),
                    field("nArgs",
                        Lists.length(var("args"))),
                    field("nLambdaDoms",
                        Lists.length(var("lambdaDoms"))),
                    Logic.ifElse(
                        Logic.and_(
                            Equality.gt(var("nLambdaDoms"), int32(0)),
                            Equality.gt(var("nArgs"), int32(0))),
                        let(
                            field("bodyRetType",
                                Pairs.second(
                                    apply(
                                        ref(Coder.peelDomainsAndCod),
                                        Math_.sub(var("nLambdaDoms"), var("nArgs")),
                                        var("resultType")))),
                            field("funType",
                                Lists.foldl(
                                    lambda(
                                        "c",
                                        "d",
                                        inject(Type.TYPE_,
                                            Type.FUNCTION,
                                            record(FunctionType.TYPE_,
                                                field(FunctionType.DOMAIN, var("d")),
                                                field(FunctionType.CODOMAIN, var("c"))))),
                                    var("bodyRetType"),
                                    Lists.reverse(var("lambdaDoms")))),
                            field("annotatedFun",
                                apply(
                                    var("hydra.annotations.setTermAnnotation"),
                                    var("hydra.constants.keyType"),
                                    just(apply(var("hydra.encode.core.type"), var("funType"))),
                                    var("fun"))),
                            apply(
                                ref(Coder.rebuildApps),
                                var("annotatedFun"),
                                var("args"),
                                var("funType"))),
                        casesWithDefault(Term.TYPE_,
                            apply(var("hydra.strip.deannotateTerm"), var("t")),
                            apply(
                                var("hydra.annotations.setTermAnnotation"),
                                var("hydra.constants.keyType"),
                                just(apply(var("hydra.encode.core.type"), var("resultType"))),
                                var("t")),
                            field(
                                Term.APPLICATION,
                                lambda("app",
                                    let(
                                        field("lhs",
                                            proj(Application.TYPE_, Application.FUNCTION, "app")),
                                        field("rhs",
                                            proj(Application.TYPE_, Application.ARGUMENT, "app")),
                                        field("annotatedLhs",
                                            casesWithDefault(Term.TYPE_,
                                                apply(var("hydra.strip.deannotateTerm"), var("lhs")),
                                                var("lhs"),
                                                field(
                                                    Term.CASES,
                                                    lambda("cs",
                                                        let(
                                                            field("dom",
                                                                apply(
                                                                    var("hydra.resolution.nominalApplication"),
                                                                    proj(CaseStatement.TYPE_, CaseStatement.TYPE_NAME, "cs"),
                                                                    list())),
                                                            field("ft",
                                                                inject(Type.TYPE_,
                                                                    Type.FUNCTION,
                                                                    record(
                                                                        FunctionType.TYPE_,
                                                                        field(
                                                                            FunctionType.DOMAIN,
                                                                            var("dom")),
                                                                        field(
                                                                            FunctionType.CODOMAIN,
                                                                            var("fixedCod"))))),
                                                            apply(
                                                                var("hydra.annotations.setTermAnnotation"),
                                                                var("hydra.constants.keyType"),
                                                                just(
                                                                    apply(
                                                                        var("hydra.encode.core.type"),
                                                                        var("ft"))),
                                                                var("lhs"))))))),
                                        apply(
                                            var("hydra.annotations.setTermAnnotation"),
                                            var("hydra.constants.keyType"),
                                            just(
                                                apply(
                                                    var("hydra.encode.core.type"),
                                                    var("resultType"))),
                                            inject(Term.TYPE_,
                                                Term.APPLICATION,
                                                record(Application.TYPE_,
                                                    field(
                                                        Application.FUNCTION,
                                                        var("annotatedLhs")),
                                                    field(
                                                        Application.ARGUMENT,
                                                        var("rhs")))))))))))));

    public static final Def rebuildApps = def(
        "rebuildApps",
        () -> lambda(
                "f",
                "args",
                "fType",
                Logic.ifElse(
                    Lists.null_(var("args")),
                    var("f"),
                    casesWithDefault(Type.TYPE_,
                        apply(var("hydra.strip.deannotateType"), var("fType")),
                        Lists.foldl(
                            lambda(
                                "acc",
                                "a",
                                inject(Term.TYPE_,
                                    Term.APPLICATION,
                                    record(Application.TYPE_,
                                        field(Application.FUNCTION, var("acc")),
                                        field(Application.ARGUMENT, var("a"))))),
                            var("f"),
                            var("args")),
                        field(
                            Type.FUNCTION,
                            lambda("ft",
                                Maybes.fromMaybe(
                                    var("f"),
                                    Maybes.map(
                                        lambda("p",
                                            let(
                                                field("arg",
                                                    Pairs.first(var("p"))),
                                                field("rest",
                                                    Pairs.second(var("p"))),
                                                field("remainingType",
                                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                                                field("app",
                                                    inject(Term.TYPE_,
                                                        Term.APPLICATION,
                                                        record(Application.TYPE_,
                                                            field(
                                                                Application.FUNCTION,
                                                                var("f")),
                                                            field(
                                                                Application.ARGUMENT,
                                                                var("arg"))))),
                                                field("annotatedApp",
                                                    apply(
                                                        var("hydra.annotations.setTermAnnotation"),
                                                        var("hydra.constants.keyType"),
                                                        just(
                                                            apply(
                                                                var("hydra.encode.core.type"),
                                                                var("remainingType"))),
                                                        var("app"))),
                                                apply(
                                                    ref(Coder.rebuildApps),
                                                    var("annotatedApp"),
                                                    var("rest"),
                                                    var("remainingType")))),
                                        Lists.uncons(var("args"))))))))));

    public static final Def recordCompareToMethod = def(
        "recordCompareToMethod",
        () -> lambda(
                "aliases",
                "tparams",
                "elName",
                "fields",
                let(
                    field("anns",
                        list(
                            ref(Utils.overrideAnnotation),
                            ref(Utils.suppressWarningsUncheckedAnnotation))),
                    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
                    field("param",
                        apply(
                            ref(Utils.javaTypeToJavaFormalParameter),
                            apply(
                                ref(Utils.javaTypeFromTypeName),
                                var("aliases"),
                                var("elName")),
                            wrap(Name.TYPE_,
                                ref(Names.otherInstanceName)))),
                    field("result",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            ref(Utils.javaIntType))),
                    apply(
                        ref(Utils.methodDeclaration),
                        var("mods"),
                        list(),
                        var("anns"),
                        ref(Names.compareToMethodName),
                        list(var("param")),
                        var("result"),
                        just(
                            apply(
                                ref(Coder.compareToBody),
                                var("aliases"),
                                ref(Names.otherInstanceName),
                                var("fields")))))));

    public static final Def recordConstructor = def(
        "recordConstructor",
        () -> lambda(
                "aliases",
                "elName",
                "fields",
                "cx",
                "g",
                let("assignStmts",
                    Lists.map(
                        lambda("f",
                            inject(BlockStatement.TYPE_,
                                BlockStatement.STATEMENT,
                                apply(
                                    ref(Utils.toAssignStmt),
                                    proj(FieldType.TYPE_, FieldType.NAME, "f")))),
                        var("fields")),
                    Eithers.bind(
                        Eithers.mapList(
                            lambda("f",
                                apply(
                                    ref(Coder.fieldTypeToFormalParam),
                                    var("aliases"),
                                    var("f"),
                                    var("cx"),
                                    var("g"))),
                            var("fields")),
                        lambda("params",
                            right(
                                apply(
                                    ref(Utils.makeConstructor),
                                    var("aliases"),
                                    var("elName"),
                                    bool(false),
                                    var("params"),
                                    var("assignStmts"))))))));

    public static final Def recordEqualsMethod = def(
        "recordEqualsMethod",
        () -> lambda(
                "aliases",
                "elName",
                "fields",
                let(
                    java.util.Arrays.asList(
    field("anns",
                        list(ref(Utils.overrideAnnotation))),
    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
    field("param",
                        apply(
                            ref(Utils.javaTypeToJavaFormalParameter),
                            apply(
                                ref(Utils.javaRefType),
                                list(),
                                nothing(),
                                string("Object")),
                            wrap(Name.TYPE_,
                                ref(Names.otherInstanceName)))),
    field("result",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            ref(Utils.javaBooleanType))),
    field("tmpName",
                        string("o")),
    field("instanceOfStmt",
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            inject(Statement.TYPE_,
                                Statement.IF_THEN,
                                record(IfThenStatement.TYPE_,
                                    field(
                                        IfThenStatement.EXPRESSION,
                                        apply(
                                            ref(Utils.javaUnaryExpressionToJavaExpression),
                                            inject(UnaryExpression.TYPE_,
                                                UnaryExpression.OTHER,
                                                inject(
                                                    UnaryExpressionNotPlusMinus.TYPE_,
                                                    UnaryExpressionNotPlusMinus.NOT,
                                                    apply(
                                                        ref(Utils.javaRelationalExpressionToJavaUnaryExpression),
                                                        apply(
                                                            ref(Utils.javaInstanceOf),
                                                            apply(
                                                                ref(Utils.javaIdentifierToJavaRelationalExpression),
                                                                apply(
                                                                    ref(Utils.javaIdentifier),
                                                                    ref(Names.otherInstanceName))),
                                                            apply(
                                                                ref(Utils.nameToJavaReferenceType),
                                                                var("aliases"),
                                                                bool(false),
                                                                list(),
                                                                var("elName"),
                                                                nothing()))))))),
                                    field(
                                        IfThenStatement.STATEMENT,
                                        apply(
                                            ref(Utils.javaReturnStatement),
                                            just(
                                                apply(
                                                    ref(Utils.javaBooleanExpression),
                                                    bool(false))))))))),
    field("castStmt",
                        apply(
                            ref(Utils.variableDeclarationStatement),
                            var("aliases"),
                            apply(
                                ref(Utils.javaTypeFromTypeName),
                                var("aliases"),
                                var("elName")),
                            apply(ref(Utils.javaIdentifier), var("tmpName")),
                            apply(
                                ref(Utils.javaCastExpressionToJavaExpression),
                                apply(
                                    ref(Utils.javaCastExpression),
                                    apply(
                                        ref(Utils.nameToJavaReferenceType),
                                        var("aliases"),
                                        bool(false),
                                        list(),
                                        var("elName"),
                                        nothing()),
                                    apply(
                                        ref(Utils.javaIdentifierToJavaUnaryExpression),
                                        wrap(Identifier.TYPE_,
                                            apply(
                                                ref(Utils.sanitizeJavaName),
                                                ref(Names.otherInstanceName)))))))),
    field("returnAllFieldsEqual",
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            apply(
                                ref(Utils.javaReturnStatement),
                                just(
                                    Logic.ifElse(
                                        Lists.null_(var("fields")),
                                        apply(
                                            ref(Utils.javaBooleanExpression),
                                            bool(true)),
                                        apply(
                                            ref(Utils.javaConditionalAndExpressionToJavaExpression),
                                            wrap(ConditionalAndExpression.TYPE_,
                                                Lists.map(
                                                    lambda("f",
                                                        apply(
                                                            ref(Coder.eqClause),
                                                            var("tmpName"),
                                                            var("f"))),
                                                    var("fields")))))))))),
                    apply(
                        ref(Utils.methodDeclaration),
                        var("mods"),
                        list(),
                        var("anns"),
                        ref(Names.equalsMethodName),
                        list(var("param")),
                        var("result"),
                        just(
                            list(
                                var("instanceOfStmt"),
                                var("castStmt"),
                                var("returnAllFieldsEqual")))))));

    public static final Def recordHashCodeMethod = def(
        "recordHashCodeMethod",
        () -> lambda("fields",
                let(
                    field("anns",
                        list(ref(Utils.overrideAnnotation))),
                    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
                    field("result",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            ref(Utils.javaIntType))),
                    field("returnSum",
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            Logic.ifElse(
                                Lists.null_(var("fields")),
                                apply(
                                    ref(Utils.javaReturnStatement),
                                    just(
                                        apply(
                                            ref(Utils.javaIntExpression),
                                            bigint(java.math.BigInteger.valueOf(0L))))),
                                apply(
                                    ref(Utils.javaReturnStatement),
                                    just(
                                        apply(
                                            ref(Utils.javaAdditiveExpressionToJavaExpression),
                                            apply(
                                                ref(Utils.addExpressions),
                                                Lists.zipWith(
                                                    ref(Coder.hashCodeMultPair),
                                                    ref(Coder.first20Primes),
                                                    Lists.map(
                                                        lambda("f",
                                                            proj(FieldType.TYPE_, FieldType.NAME, "f")),
                                                        var("fields")))))))))),
                    apply(
                        ref(Utils.methodDeclaration),
                        var("mods"),
                        list(),
                        var("anns"),
                        ref(Names.hashCodeMethodName),
                        list(),
                        var("result"),
                        just(list(var("returnSum")))))));

    public static final Def recordMemberVar = def(
        "recordMemberVar",
        () -> lambda(
                "aliases",
                "ft",
                "cx",
                "g",
                let(
                    field("mods",
                        list(
                            inject(FieldModifier.TYPE_,
                                FieldModifier.PUBLIC,
                                unit()),
                            inject(FieldModifier.TYPE_,
                                FieldModifier.FINAL,
                                unit()))),
                    field("fname",
                        proj(FieldType.TYPE_, FieldType.NAME, "ft")),
                    field("ftype",
                        proj(FieldType.TYPE_, FieldType.TYPE, "ft")),
                    Eithers.bind(
                        apply(
                            ref(Coder.encodeType),
                            var("aliases"),
                            var("hydra.lib.sets.empty"),
                            var("ftype"),
                            var("cx"),
                            var("g")),
                        lambda("jt",
                            right(
                                apply(
                                    ref(Utils.javaMemberField),
                                    var("mods"),
                                    var("jt"),
                                    apply(
                                        ref(Utils.fieldNameToJavaVariableDeclarator),
                                        var("fname")))))))));

    public static final Def recordWithMethod = def(
        "recordWithMethod",
        () -> lambda(
                "aliases",
                "elName",
                "fields",
                "field",
                "cx",
                "g",
                let(
                    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
                    field("anns",
                        list()),
                    field("methodName",
                        Strings.cat2(
                            string("with"),
                            apply(
                                var("hydra.formatting.nonAlnumToUnderscores"),
                                apply(
                                    var("hydra.formatting.capitalize"),
                                    apply(
                                        unwrap(Name.TYPE_),
                                        proj(FieldType.TYPE_, FieldType.NAME, "field")))))),
                    field("result",
                        apply(
                            ref(Utils.referenceTypeToResult),
                            apply(
                                ref(Utils.nameToJavaReferenceType),
                                var("aliases"),
                                bool(false),
                                list(),
                                var("elName"),
                                nothing()))),
                    field("consId",
                        wrap(Identifier.TYPE_,
                            apply(
                                ref(Utils.sanitizeJavaName),
                                apply(var("hydra.names.localNameOf"), var("elName"))))),
                    field("fieldArgs",
                        Lists.map(
                            lambda("f",
                                apply(
                                    ref(Utils.fieldNameToJavaExpression),
                                    proj(FieldType.TYPE_, FieldType.NAME, "f"))),
                            var("fields"))),
                    field("returnStmt",
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            apply(
                                ref(Utils.javaReturnStatement),
                                just(
                                    apply(
                                        ref(Utils.javaConstructorCall),
                                        apply(
                                            ref(Utils.javaConstructorName),
                                            var("consId"),
                                            nothing()),
                                        var("fieldArgs"),
                                        nothing()))))),
                    Eithers.bind(
                        apply(
                            ref(Coder.fieldTypeToFormalParam),
                            var("aliases"),
                            var("field"),
                            var("cx"),
                            var("g")),
                        lambda("param",
                            right(
                                apply(
                                    ref(Utils.methodDeclaration),
                                    var("mods"),
                                    list(),
                                    var("anns"),
                                    var("methodName"),
                                    list(var("param")),
                                    var("result"),
                                    just(list(var("returnStmt"))))))))));

    public static final Def resolveTypeApps = def(
        "resolveTypeApps",
        () -> lambda(
                "schemeVars",
                "fallbackTypeApps",
                "argSubst",
                let(
                    field("resolvedVars",
                        Sets.fromList(Maps.keys(var("argSubst")))),
                    field("unresolvedVars",
                        Lists.filter(
                            lambda("v", Logic.not_(Sets.member(var("v"), var("resolvedVars")))),
                            var("schemeVars"))),
                    field("usedTypes",
                        Sets.fromList(Maps.elems(var("argSubst")))),
                    field("unusedIrTypes",
                        Lists.filter(
                            lambda("t", Logic.not_(Sets.member(var("t"), var("usedTypes")))),
                            var("fallbackTypeApps"))),
                    field("remainingSubst",
                        Maps.fromList(Lists.zip(var("unresolvedVars"), var("unusedIrTypes")))),
                    field("fullSubst",
                        Maps.union(var("argSubst"), var("remainingSubst"))),
                    Lists.map(
                        lambda("v",
                            Maps.findWithDefault(
                                inject(Type.TYPE_, Type.VARIABLE, var("v")),
                                var("v"),
                                var("fullSubst"))),
                        var("schemeVars")))));

    public static final Def selfRefSubstitution = def(
        "selfRefSubstitution",
        () -> lambda("grouped",
                Lists.foldl(
                    lambda(
                        "subst",
                        "entry",
                        apply(
                            ref(Coder.selfRefSubstitution_processGroup),
                            var("subst"),
                            Pairs.first(var("entry")),
                            Pairs.second(var("entry")))),
                    var("hydra.lib.maps.empty"),
                    Maps.toList(var("grouped")))));

    public static final Def selfRefSubstitution_processGroup = def(
        "selfRefSubstitution_processGroup",
        () -> lambda(
                "subst",
                "inVar",
                "outVars",
                Logic.ifElse(
                    Lists.elem(var("inVar"), var("outVars")),
                    Lists.foldl(
                        lambda(
                            "s",
                            "v",
                            Logic.ifElse(
                                Equality.equal(var("v"), var("inVar")),
                                var("s"),
                                Maps.insert(var("v"), var("inVar"), var("s")))),
                        var("subst"),
                        var("outVars")),
                    var("subst"))));

    public static final Def serializableTypes = def(
        "serializableTypes",
        () -> lambda("isSer",
                let("javaSerializableType",
                    wrap(InterfaceType.TYPE_,
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
                                    string("Serializable"))),
                            field(ClassType.ARGUMENTS, list()))),
                    Logic.ifElse(var("isSer"), list(var("javaSerializableType")), list()))));

    public static final Def splitConstantInitializer = def(
        "splitConstantInitializer",
        () -> lambda("member",
                casesWithDefault(InterfaceMemberDeclaration.TYPE_,
                    var("member"),
                    list(var("member")),
                    field(
                        InterfaceMemberDeclaration.CONSTANT,
                        lambda("cd",
                            Lists.bind(
                                proj(ConstantDeclaration.TYPE_, ConstantDeclaration.VARIABLES, "cd"),
                                apply(
                                    ref(Coder.splitConstantInitializer_splitVar),
                                    proj(ConstantDeclaration.TYPE_, ConstantDeclaration.MODIFIERS, "cd"),
                                    proj(ConstantDeclaration.TYPE_, ConstantDeclaration.TYPE, "cd"))))))));

    public static final Def splitConstantInitializer_splitVar = def(
        "splitConstantInitializer_splitVar",
        () -> lambda(
                "mods",
                "utype",
                "vd",
                let(
                    field("vid",
                        proj(VariableDeclarator.TYPE_, VariableDeclarator.ID, "vd")),
                    field("mInit",
                        proj(VariableDeclarator.TYPE_, VariableDeclarator.INITIALIZER, "vd")),
                    Maybes.cases(
                        var("mInit"),
                        list(
                            inject(InterfaceMemberDeclaration.TYPE_,
                                InterfaceMemberDeclaration.CONSTANT,
                                record(ConstantDeclaration.TYPE_,
                                    field(
                                        ConstantDeclaration.MODIFIERS,
                                        var("mods")),
                                    field(ConstantDeclaration.TYPE, var("utype")),
                                    field(
                                        ConstantDeclaration.VARIABLES,
                                        list(var("vd")))))),
                        lambda("init_",
                            casesWithDefault(VariableInitializer.TYPE_,
                                var("init_"),
                                list(
                                    inject(InterfaceMemberDeclaration.TYPE_,
                                        InterfaceMemberDeclaration.CONSTANT,
                                        record(ConstantDeclaration.TYPE_,
                                            field(
                                                ConstantDeclaration.MODIFIERS,
                                                var("mods")),
                                            field(
                                                ConstantDeclaration.TYPE,
                                                var("utype")),
                                            field(
                                                ConstantDeclaration.VARIABLES,
                                                list(var("vd")))))),
                                field(
                                    VariableInitializer.EXPRESSION,
                                    lambda("expr",
                                        let(
                                            field("varName",
                                                apply(
                                                    ref(Coder.javaIdentifierToString),
                                                    proj(VariableDeclaratorId.TYPE_, VariableDeclaratorId.IDENTIFIER, "vid"))),
                                            field("helperName",
                                                Strings.cat2(string("_init_"), var("varName"))),
                                            field("callExpr",
                                                apply(
                                                    ref(Utils.javaMethodInvocationToJavaExpression),
                                                    apply(
                                                        ref(Utils.methodInvocation),
                                                        nothing(),
                                                        wrap(Identifier.TYPE_,
                                                            var("helperName")),
                                                        list()))),
                                            field("field",
                                                inject(
                                                    InterfaceMemberDeclaration.TYPE_,
                                                    InterfaceMemberDeclaration.CONSTANT,
                                                    record(
                                                        ConstantDeclaration.TYPE_,
                                                        field(
                                                            ConstantDeclaration.MODIFIERS,
                                                            var("mods")),
                                                        field(
                                                            ConstantDeclaration.TYPE,
                                                            var("utype")),
                                                        field(
                                                            ConstantDeclaration.VARIABLES,
                                                            list(
                                                                record(
                                                                    VariableDeclarator.TYPE_,
                                                                    field(
                                                                        VariableDeclarator.ID,
                                                                        var("vid")),
                                                                    field(
                                                                        VariableDeclarator.INITIALIZER,
                                                                        just(
                                                                            inject(
                                                                                VariableInitializer.TYPE_,
                                                                                VariableInitializer.EXPRESSION,
                                                                                var("callExpr")))))))))),
                                            field("returnSt",
                                                inject(BlockStatement.TYPE_,
                                                    BlockStatement.STATEMENT,
                                                    apply(
                                                        ref(Utils.javaReturnStatement),
                                                        just(var("expr"))))),
                                            field("resultType",
                                                inject(Result.TYPE_,
                                                    Result.TYPE,
                                                    var("utype"))),
                                            field("helper",
                                                apply(
                                                    ref(Utils.interfaceMethodDeclaration),
                                                    list(
                                                        inject(
                                                            InterfaceMethodModifier.TYPE_,
                                                            InterfaceMethodModifier.STATIC,
                                                            unit()),
                                                        inject(
                                                            InterfaceMethodModifier.TYPE_,
                                                            InterfaceMethodModifier.PRIVATE,
                                                            unit())),
                                                    list(),
                                                    var("helperName"),
                                                    list(),
                                                    var("resultType"),
                                                    just(list(var("returnSt"))))),
                                            list(var("field"), var("helper")))))))))));

    public static final Def stripForalls = def(
        "stripForalls",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    var("t"),
                    field(
                        Type.FORALL,
                        lambda("fa",
                            apply(
                                ref(Coder.stripForalls),
                                proj(ForallType.TYPE_, ForallType.BODY, "fa")))))));

    public static final Def substituteTypeVarsWithTypes = def(
        "substituteTypeVarsWithTypes",
        () -> lambda(
                "subst",
                "t",
                apply(
                    ref(Coder.substituteTypeVarsWithTypes_go),
                    var("subst"),
                    apply(var("hydra.strip.deannotateType"), var("t")))));

    public static final Def substituteTypeVarsWithTypes_go = def(
        "substituteTypeVarsWithTypes_go",
        () -> lambda(
                "subst",
                "t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    var("t"),
                    field(
                        Type.VARIABLE,
                        lambda("v",
                            Maybes.cases(
                                Maps.lookup(var("v"), var("subst")),
                                var("t"),
                                lambda("rep", var("rep"))))),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            inject(Type.TYPE_,
                                Type.FUNCTION,
                                record(FunctionType.TYPE_,
                                    field(
                                        FunctionType.DOMAIN,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"))),
                                    field(
                                        FunctionType.CODOMAIN,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"))))))),
                    field(
                        Type.APPLICATION,
                        lambda("at",
                            inject(Type.TYPE_,
                                Type.APPLICATION,
                                record(ApplicationType.TYPE_,
                                    field(
                                        ApplicationType.FUNCTION,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at"))),
                                    field(
                                        ApplicationType.ARGUMENT,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at"))))))),
                    field(
                        Type.LIST,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.LIST,
                                apply(
                                    ref(Coder.substituteTypeVarsWithTypes_go),
                                    var("subst"),
                                    var("inner"))))),
                    field(
                        Type.SET,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.SET,
                                apply(
                                    ref(Coder.substituteTypeVarsWithTypes_go),
                                    var("subst"),
                                    var("inner"))))),
                    field(
                        Type.MAYBE,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.MAYBE,
                                apply(
                                    ref(Coder.substituteTypeVarsWithTypes_go),
                                    var("subst"),
                                    var("inner"))))),
                    field(
                        Type.MAP,
                        lambda("mt",
                            inject(Type.TYPE_,
                                Type.MAP,
                                record(MapType.TYPE_,
                                    field(
                                        MapType.KEYS,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(MapType.TYPE_, MapType.KEYS, "mt"))),
                                    field(
                                        MapType.VALUES,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(MapType.TYPE_, MapType.VALUES, "mt"))))))),
                    field(
                        Type.PAIR,
                        lambda("pt",
                            inject(Type.TYPE_,
                                Type.PAIR,
                                record(PairType.TYPE_,
                                    field(
                                        PairType.FIRST,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(PairType.TYPE_, PairType.FIRST, "pt"))),
                                    field(
                                        PairType.SECOND,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(PairType.TYPE_, PairType.SECOND, "pt"))))))),
                    field(
                        Type.EITHER,
                        lambda("et",
                            inject(Type.TYPE_,
                                Type.EITHER,
                                record(EitherType.TYPE_,
                                    field(
                                        EitherType.LEFT,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(EitherType.TYPE_, EitherType.LEFT, "et"))),
                                    field(
                                        EitherType.RIGHT,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(EitherType.TYPE_, EitherType.RIGHT, "et"))))))),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            inject(Type.TYPE_,
                                Type.FORALL,
                                record(ForallType.TYPE_,
                                    field(
                                        ForallType.PARAMETER,
                                        proj(ForallType.TYPE_, ForallType.PARAMETER, "ft")),
                                    field(
                                        ForallType.BODY,
                                        apply(
                                            ref(Coder.substituteTypeVarsWithTypes_go),
                                            var("subst"),
                                            proj(ForallType.TYPE_, ForallType.BODY, "ft"))))))))));

    public static final Def tagCmpNotZeroExpr = def(
        "tagCmpNotZeroExpr",
        () -> let(
                field("lhs",
                    apply(
                        ref(Utils.javaRelationalExpressionToJavaEqualityExpression),
                        apply(
                            ref(Utils.javaPostfixExpressionToJavaRelationalExpression),
                            inject(PostfixExpression.TYPE_,
                                PostfixExpression.NAME,
                                record(ExpressionName.TYPE_,
                                    field(ExpressionName.QUALIFIER, nothing()),
                                    field(
                                        ExpressionName.IDENTIFIER,
                                        apply(
                                            ref(Utils.javaIdentifier),
                                            string("tagCmp")))))))),
                field("rhs",
                    apply(
                        ref(Utils.javaPostfixExpressionToJavaRelationalExpression),
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.PRIMARY,
                            apply(
                                ref(Utils.javaLiteralToJavaPrimary),
                                apply(
                                    ref(Utils.javaInt),
                                    bigint(java.math.BigInteger.valueOf(0L))))))),
                apply(
                    ref(Utils.javaEqualityExpressionToJavaExpression),
                    inject(EqualityExpression.TYPE_,
                        EqualityExpression.NOT_EQUAL,
                        record(EqualityExpression_Binary.TYPE_,
                            field(EqualityExpression_Binary.LHS, var("lhs")),
                            field(EqualityExpression_Binary.RHS, var("rhs")))))));

    public static final Def tagCompareExpr = def(
        "tagCompareExpr",
        () -> let(
                field("thisGetClass",
                    record(MethodInvocation.TYPE_,
                        field(
                            MethodInvocation.HEADER,
                            inject(MethodInvocation_Header.TYPE_,
                                MethodInvocation_Header.COMPLEX,
                                record(MethodInvocation_Complex.TYPE_,
                                    field(
                                        MethodInvocation_Complex.VARIANT,
                                        inject(MethodInvocation_Variant.TYPE_,
                                            MethodInvocation_Variant.PRIMARY,
                                            apply(
                                                ref(Utils.javaExpressionToJavaPrimary),
                                                ref(Utils.javaThis)))),
                                    field(
                                        MethodInvocation_Complex.TYPE_ARGUMENTS,
                                        list()),
                                    field(
                                        MethodInvocation_Complex.IDENTIFIER,
                                        wrap(Identifier.TYPE_, string("getClass")))))),
                        field(MethodInvocation.ARGUMENTS, list()))),
                field("thisGetName",
                    record(MethodInvocation.TYPE_,
                        field(
                            MethodInvocation.HEADER,
                            inject(MethodInvocation_Header.TYPE_,
                                MethodInvocation_Header.COMPLEX,
                                record(MethodInvocation_Complex.TYPE_,
                                    field(
                                        MethodInvocation_Complex.VARIANT,
                                        inject(MethodInvocation_Variant.TYPE_,
                                            MethodInvocation_Variant.PRIMARY,
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaPrimary),
                                                var("thisGetClass")))),
                                    field(
                                        MethodInvocation_Complex.TYPE_ARGUMENTS,
                                        list()),
                                    field(
                                        MethodInvocation_Complex.IDENTIFIER,
                                        wrap(Identifier.TYPE_, string("getName")))))),
                        field(MethodInvocation.ARGUMENTS, list()))),
                field("otherGetClass",
                    record(MethodInvocation.TYPE_,
                        field(
                            MethodInvocation.HEADER,
                            inject(MethodInvocation_Header.TYPE_,
                                MethodInvocation_Header.COMPLEX,
                                record(MethodInvocation_Complex.TYPE_,
                                    field(
                                        MethodInvocation_Complex.VARIANT,
                                        inject(MethodInvocation_Variant.TYPE_,
                                            MethodInvocation_Variant.EXPRESSION,
                                            record(ExpressionName.TYPE_,
                                                field(
                                                    ExpressionName.QUALIFIER,
                                                    nothing()),
                                                field(
                                                    ExpressionName.IDENTIFIER,
                                                    wrap(Identifier.TYPE_,
                                                        ref(Names.otherInstanceName)))))),
                                    field(
                                        MethodInvocation_Complex.TYPE_ARGUMENTS,
                                        list()),
                                    field(
                                        MethodInvocation_Complex.IDENTIFIER,
                                        wrap(Identifier.TYPE_, string("getClass")))))),
                        field(MethodInvocation.ARGUMENTS, list()))),
                field("otherGetName",
                    record(MethodInvocation.TYPE_,
                        field(
                            MethodInvocation.HEADER,
                            inject(MethodInvocation_Header.TYPE_,
                                MethodInvocation_Header.COMPLEX,
                                record(MethodInvocation_Complex.TYPE_,
                                    field(
                                        MethodInvocation_Complex.VARIANT,
                                        inject(MethodInvocation_Variant.TYPE_,
                                            MethodInvocation_Variant.PRIMARY,
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaPrimary),
                                                var("otherGetClass")))),
                                    field(
                                        MethodInvocation_Complex.TYPE_ARGUMENTS,
                                        list()),
                                    field(
                                        MethodInvocation_Complex.IDENTIFIER,
                                        wrap(Identifier.TYPE_, string("getName")))))),
                        field(MethodInvocation.ARGUMENTS, list()))),
                apply(
                    ref(Utils.javaMethodInvocationToJavaExpression),
                    record(MethodInvocation.TYPE_,
                        field(
                            MethodInvocation.HEADER,
                            inject(MethodInvocation_Header.TYPE_,
                                MethodInvocation_Header.COMPLEX,
                                record(MethodInvocation_Complex.TYPE_,
                                    field(
                                        MethodInvocation_Complex.VARIANT,
                                        inject(MethodInvocation_Variant.TYPE_,
                                            MethodInvocation_Variant.PRIMARY,
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaPrimary),
                                                var("thisGetName")))),
                                    field(
                                        MethodInvocation_Complex.TYPE_ARGUMENTS,
                                        list()),
                                    field(
                                        MethodInvocation_Complex.IDENTIFIER,
                                        wrap(Identifier.TYPE_,
                                            ref(Names.compareToMethodName)))))),
                        field(
                            MethodInvocation.ARGUMENTS,
                            list(
                                apply(
                                    ref(Utils.javaMethodInvocationToJavaExpression),
                                    var("otherGetName"))))))));

    public static final Def takeTypeArgs = def(
        "takeTypeArgs",
        () -> lambda(
                "label",
                "n",
                "tyapps",
                "cx",
                "g",
                Logic.ifElse(
                    Equality.lt(Lists.length(var("tyapps")), var("n")),
                    left(
                        inject(Error_.TYPE_,
                            Error_.OTHER,
                            wrap(OtherError.TYPE_,
                                Strings.cat(
                                    list(
                                        string("needed type arguments for "),
                                        var("label"),
                                        string(", found too few")))))),
                    Eithers.mapList(
                        lambda("jt",
                            Eithers.bind(
                                apply(
                                    ref(Utils.javaTypeToJavaReferenceType),
                                    var("jt"),
                                    var("cx")),
                                lambda("rt",
                                    right(
                                        inject(TypeArgument.TYPE_,
                                            TypeArgument.REFERENCE,
                                            var("rt")))))),
                        Lists.take(var("n"), var("tyapps"))))));

    public static final Def toClassDecl = def(
        "toClassDecl",
        () -> lambda(
                java.util.Arrays.asList("isInner", "isSer", "aliases", "tparams", "elName", "t", "cx", "g"),
                let("wrap",
                    lambda("t'",
                        apply(
                            ref(Coder.declarationForRecordType),
                            var("isInner"),
                            var("isSer"),
                            var("aliases"),
                            var("tparams"),
                            var("elName"),
                            list(
                                record(FieldType.TYPE_,
                                    field(
                                        FieldType.NAME,
                                        wrap(Name.TYPE_, string("value"))),
                                    field(
                                        FieldType.TYPE,
                                        apply(var("hydra.strip.deannotateType"), var("t'"))))),
                            var("cx"),
                            var("g"))),
                    casesWithDefault(Type.TYPE_,
                        apply(var("hydra.strip.deannotateType"), var("t")),
                        apply(var("wrap"), var("t")),
                        field(
                            Type.RECORD,
                            lambda("rt",
                                apply(
                                    ref(Coder.declarationForRecordType),
                                    var("isInner"),
                                    var("isSer"),
                                    var("aliases"),
                                    var("tparams"),
                                    var("elName"),
                                    var("rt"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Type.UNION,
                            lambda("rt",
                                apply(
                                    ref(Coder.declarationForUnionType),
                                    var("isSer"),
                                    var("aliases"),
                                    var("tparams"),
                                    var("elName"),
                                    var("rt"),
                                    var("cx"),
                                    var("g")))),
                        field(
                            Type.FORALL,
                            lambda("fa",
                                let(
                                    field("v",
                                        proj(ForallType.TYPE_, ForallType.PARAMETER, "fa")),
                                    field("body",
                                        proj(ForallType.TYPE_, ForallType.BODY, "fa")),
                                    field("param",
                                        apply(
                                            ref(Utils.javaTypeParameter),
                                            apply(
                                                var("hydra.formatting.capitalize"),
                                                apply(unwrap(Name.TYPE_), var("v"))))),
                                    apply(
                                        ref(Coder.toClassDecl),
                                        bool(false),
                                        var("isSer"),
                                        var("aliases"),
                                        Lists.concat2(var("tparams"), list(var("param"))),
                                        var("elName"),
                                        var("body"),
                                        var("cx"),
                                        var("g"))))),
                        field(
                            Type.WRAP,
                            lambda("wt",
                                apply(
                                    ref(Coder.declarationForRecordType),
                                    var("isInner"),
                                    var("isSer"),
                                    var("aliases"),
                                    var("tparams"),
                                    var("elName"),
                                    list(
                                        record(FieldType.TYPE_,
                                            field(
                                                FieldType.NAME,
                                                wrap(Name.TYPE_, string("value"))),
                                            field(FieldType.TYPE, var("wt")))),
                                    var("cx"),
                                    var("g"))))))));

    public static final Def toDeclInit = def(
        "toDeclInit",
        () -> lambda(
                "aliasesExt",
                "gExt",
                "recursiveVars",
                "flatBindings",
                "name",
                "cx",
                "g",
                Logic.ifElse(
                    Sets.member(var("name"), var("recursiveVars")),
                    let(
                        field("binding",
                            Maybes.fromMaybe(
                                record(Binding.TYPE_,
                                    field(Binding.NAME, var("name")),
                                    field(
                                        Binding.TERM,
                                        inject(Term.TYPE_, Term.UNIT, unit())),
                                    field(Binding.TYPE_SCHEME, nothing())),
                                Lists.maybeHead(
                                    Lists.filter(
                                        lambda("b",
                                            Equality.equal(
                                                proj(Binding.TYPE_, Binding.NAME, "b"),
                                                var("name"))),
                                        var("flatBindings"))))),
                        field("value",
                            proj(Binding.TYPE_, Binding.TERM, "binding")),
                        Eithers.bind(
                            Maybes.cases(
                                proj(Binding.TYPE_, Binding.TYPE_SCHEME, "binding"),
                                apply(
                                    var("hydra.checking.typeOfTerm"),
                                    var("cx"),
                                    var("gExt"),
                                    var("value")),
                                lambda("ts",
                                    right(
                                        proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")))),
                            lambda("typ",
                                Eithers.bind(
                                    apply(
                                        ref(Coder.encodeType),
                                        var("aliasesExt"),
                                        var("hydra.lib.sets.empty"),
                                        var("typ"),
                                        var("cx"),
                                        var("g")),
                                    lambda("jtype",
                                        let(
                                            field("id",
                                                apply(
                                                    ref(Utils.variableToJavaIdentifier),
                                                    var("name"))),
                                            field("arid",
                                                wrap(Identifier.TYPE_,
                                                    string("java.util.concurrent.atomic.AtomicReference"))),
                                            field("aid",
                                                record(AnnotatedIdentifier.TYPE_,
                                                    field(
                                                        AnnotatedIdentifier.ANNOTATIONS,
                                                        list()),
                                                    field(
                                                        AnnotatedIdentifier.IDENTIFIER,
                                                        var("arid")))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Utils.javaTypeToJavaReferenceType),
                                                    var("jtype"),
                                                    var("cx")),
                                                lambda("rt",
                                                    let(
                                                        field("targs",
                                                            apply(
                                                                ref(Coder.typeArgsOrDiamond),
                                                                list(
                                                                    inject(
                                                                        TypeArgument.TYPE_,
                                                                        TypeArgument.REFERENCE,
                                                                        var("rt"))))),
                                                        field("ci",
                                                            record(
                                                                ClassOrInterfaceTypeToInstantiate.TYPE_,
                                                                field(
                                                                    ClassOrInterfaceTypeToInstantiate.IDENTIFIERS,
                                                                    list(var("aid"))),
                                                                field(
                                                                    ClassOrInterfaceTypeToInstantiate.TYPE_ARGUMENTS,
                                                                    just(var("targs"))))),
                                                        field("body",
                                                            apply(
                                                                ref(Utils.javaConstructorCall),
                                                                var("ci"),
                                                                list(),
                                                                nothing())),
                                                        field("pkg",
                                                            apply(
                                                                ref(Names.javaPackageName),
                                                                list(
                                                                    string("java"),
                                                                    string("util"),
                                                                    string("concurrent"),
                                                                    string("atomic")))),
                                                        field("artype",
                                                            apply(
                                                                ref(Utils.javaRefType),
                                                                list(var("rt")),
                                                                just(var("pkg")),
                                                                string("AtomicReference"))),
                                                        right(
                                                            just(
                                                                apply(
                                                                    ref(Utils.variableDeclarationStatement),
                                                                    var("aliasesExt"),
                                                                    var("artype"),
                                                                    var("id"),
                                                                    var("body"))))))))))))),
                    right(nothing()))));

    public static final Def toDeclStatement = def(
        "toDeclStatement",
        () -> lambda(
                java.util.Arrays.asList("envExt", "aliasesExt", "gExt", "recursiveVars", "thunkedVars", "flatBindings", "name", "cx", "g"),
                let(
                    field("binding",
                        Maybes.fromMaybe(
                            record(Binding.TYPE_,
                                field(Binding.NAME, var("name")),
                                field(
                                    Binding.TERM,
                                    inject(Term.TYPE_, Term.UNIT, unit())),
                                field(Binding.TYPE_SCHEME, nothing())),
                            Lists.maybeHead(
                                Lists.filter(
                                    lambda("b",
                                        Equality.equal(
                                            proj(Binding.TYPE_, Binding.NAME, "b"),
                                            var("name"))),
                                    var("flatBindings"))))),
                    field("value",
                        proj(Binding.TYPE_, Binding.TERM, "binding")),
                    Eithers.bind(
                        Maybes.cases(
                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "binding"),
                            apply(
                                var("hydra.checking.typeOfTerm"),
                                var("cx"),
                                var("gExt"),
                                var("value")),
                            lambda("ts",
                                right(
                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")))),
                        lambda("typ",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliasesExt"),
                                    var("hydra.lib.sets.empty"),
                                    var("typ"),
                                    var("cx"),
                                    var("g")),
                                lambda("jtype",
                                    let(
                                        field("id",
                                            apply(
                                                ref(Utils.variableToJavaIdentifier),
                                                var("name"))),
                                        field("annotatedValue",
                                            apply(
                                                var("hydra.annotations.setTermAnnotation"),
                                                var("hydra.constants.keyType"),
                                                just(
                                                    apply(var("hydra.encode.core.type"), var("typ"))),
                                                var("value"))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.encodeTerm),
                                                var("envExt"),
                                                var("annotatedValue"),
                                                var("cx"),
                                                var("g")),
                                            lambda("rhs",
                                                Logic.ifElse(
                                                    Sets.member(var("name"), var("recursiveVars")),
                                                    right(
                                                        inject(
                                                            BlockStatement.TYPE_,
                                                            BlockStatement.STATEMENT,
                                                            apply(
                                                                ref(Utils.javaMethodInvocationToJavaStatement),
                                                                apply(
                                                                    ref(Utils.methodInvocation),
                                                                    just(
                                                                        left(
                                                                            record(
                                                                                ExpressionName.TYPE_,
                                                                                field(
                                                                                    ExpressionName.QUALIFIER,
                                                                                    nothing()),
                                                                                field(
                                                                                    ExpressionName.IDENTIFIER,
                                                                                    var("id"))))),
                                                                    wrap(
                                                                        Identifier.TYPE_,
                                                                        ref(Names.setMethodName)),
                                                                    list(var("rhs")))))),
                                                    Logic.ifElse(
                                                        Sets.member(var("name"), var("thunkedVars")),
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Utils.javaTypeToJavaReferenceType),
                                                                var("jtype"),
                                                                var("cx")),
                                                            lambda("rt",
                                                                let(
                                                                    field("lazyType",
                                                                        apply(
                                                                            ref(Utils.javaRefType),
                                                                            list(var("rt")),
                                                                            ref(Names.hydraUtilPackageName),
                                                                            string("Lazy"))),
                                                                    field("lambdaBody",
                                                                        inject(
                                                                            LambdaBody.TYPE_,
                                                                            LambdaBody.EXPRESSION,
                                                                            var("rhs"))),
                                                                    field("supplierLambda",
                                                                        inject(
                                                                            Expression.TYPE_,
                                                                            Expression.LAMBDA,
                                                                            record(
                                                                                LambdaExpression.TYPE_,
                                                                                field(
                                                                                    LambdaExpression.PARAMETERS,
                                                                                    inject(
                                                                                        LambdaParameters.TYPE_,
                                                                                        LambdaParameters.TUPLE,
                                                                                        list())),
                                                                                field(
                                                                                    LambdaExpression.BODY,
                                                                                    var("lambdaBody"))))),
                                                                    field("targs",
                                                                        apply(
                                                                            ref(Coder.typeArgsOrDiamond),
                                                                            list(
                                                                                inject(
                                                                                    TypeArgument.TYPE_,
                                                                                    TypeArgument.REFERENCE,
                                                                                    var("rt"))))),
                                                                    field("lazyExpr",
                                                                        apply(
                                                                            ref(Utils.javaConstructorCall),
                                                                            apply(
                                                                                ref(Utils.javaConstructorName),
                                                                                wrap(
                                                                                    Identifier.TYPE_,
                                                                                    string("hydra.util.Lazy")),
                                                                                just(var("targs"))),
                                                                            list(
                                                                                var("supplierLambda")),
                                                                            nothing())),
                                                                    right(
                                                                        apply(
                                                                            ref(Utils.variableDeclarationStatement),
                                                                            var("aliasesExt"),
                                                                            var("lazyType"),
                                                                            var("id"),
                                                                            var("lazyExpr")))))),
                                                        right(
                                                            apply(
                                                                ref(Utils.variableDeclarationStatement),
                                                                var("aliasesExt"),
                                                                var("jtype"),
                                                                var("id"),
                                                                var("rhs")))))))))))))));

    public static final Def tryInferFunctionType = def(
        "tryInferFunctionType",
        () -> lambda("funTerm",
                casesWithDefault(Term.TYPE_,
                    apply(var("hydra.strip.deannotateTerm"), var("funTerm")),
                    nothing(),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            Maybes.bind(
                                proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"),
                                lambda("dom",
                                    let("mCod",
                                        casesWithDefault(Term.TYPE_,
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam"),
                                            nothing(),
                                            field(
                                                Term.ANNOTATED,
                                                lambda("at",
                                                    Maybes.bind(
                                                        Maps.lookup(
                                                            var("hydra.constants.keyType"),
                                                            proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at")),
                                                        lambda("typeTerm",
                                                            apply(
                                                                ref(Coder.decodeTypeFromTerm),
                                                                var("typeTerm")))))),
                                            field(
                                                Term.LAMBDA,
                                                constant(
                                                    apply(
                                                        ref(Coder.tryInferFunctionType),
                                                        proj(Lambda.TYPE_, Lambda.BODY, "lam"))))),
                                        Maybes.map(
                                            lambda("cod",
                                                inject(Type.TYPE_,
                                                    Type.FUNCTION,
                                                    record(FunctionType.TYPE_,
                                                        field(
                                                            FunctionType.DOMAIN,
                                                            var("dom")),
                                                        field(
                                                            FunctionType.CODOMAIN,
                                                            var("cod"))))),
                                            var("mCod"))))))))));

    public static final Def typeAppFallbackCast = def(
        "typeAppFallbackCast",
        () -> lambda(
                java.util.Arrays.asList("env", "aliases", "anns", "tyapps", "jatyp", "body", "typ", "cx", "g"),
                let("annotatedBody",
                    apply(
                        var("hydra.annotations.setTermAnnotation"),
                        var("hydra.constants.keyType"),
                        just(apply(var("hydra.encode.core.type"), var("typ"))),
                        var("body")),
                    Eithers.bind(
                        apply(
                            ref(Coder.encodeTermInternal),
                            var("env"),
                            var("anns"),
                            Lists.cons(var("jatyp"), var("tyapps")),
                            var("annotatedBody"),
                            var("cx"),
                            var("g")),
                        lambda("jbody",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("hydra.lib.sets.empty"),
                                    var("typ"),
                                    var("cx"),
                                    var("g")),
                                lambda("jtype",
                                    Eithers.bind(
                                        apply(
                                            ref(Utils.javaTypeToJavaReferenceType),
                                            var("jtype"),
                                            var("cx")),
                                        lambda("rt",
                                            right(
                                                apply(
                                                    ref(Utils.javaCastExpressionToJavaExpression),
                                                    apply(
                                                        ref(Utils.javaCastExpression),
                                                        var("rt"),
                                                        apply(
                                                            ref(Utils.javaExpressionToJavaUnaryExpression),
                                                            var("jbody"))))))))))))));

    public static final Def typeAppNullaryOrHoisted = def(
        "typeAppNullaryOrHoisted",
        () -> lambda(
                java.util.Arrays.asList("env", "aliases", "anns", "tyapps", "jatyp", "body", "correctedTyp", "varName", "cls", "allTypeArgs", "cx", "g"),
                let(
                    field("qn",
                        apply(var("hydra.names.qualifyName"), var("varName"))),
                    field("mns",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("localName",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    casesWithDefault(JavaSymbolClass.TYPE_,
                        var("cls"),
                        apply(
                            ref(Coder.typeAppFallbackCast),
                            var("env"),
                            var("aliases"),
                            var("anns"),
                            var("tyapps"),
                            var("jatyp"),
                            var("body"),
                            var("correctedTyp"),
                            var("cx"),
                            var("g")),
                        field(
                            JavaSymbolClass.NULLARY_FUNCTION,
                            constant(
                                Maybes.cases(
                                    var("mns"),
                                    apply(
                                        ref(Coder.typeAppFallbackCast),
                                        var("env"),
                                        var("aliases"),
                                        var("anns"),
                                        var("tyapps"),
                                        var("jatyp"),
                                        var("body"),
                                        var("correctedTyp"),
                                        var("cx"),
                                        var("g")),
                                    lambda("ns_",
                                        let(
                                            field("classId",
                                                apply(
                                                    ref(Utils.nameToJavaName),
                                                    var("aliases"),
                                                    apply(
                                                        ref(Coder.elementsQualifiedName),
                                                        var("ns_")))),
                                            field("methodId",
                                                wrap(Identifier.TYPE_,
                                                    apply(
                                                        ref(Utils.sanitizeJavaName),
                                                        var("localName")))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.filterPhantomTypeArgs),
                                                    var("varName"),
                                                    var("allTypeArgs"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("filteredTypeArgs",
                                                    Eithers.bind(
                                                        Eithers.mapList(
                                                            lambda("t",
                                                                Eithers.bind(
                                                                    apply(
                                                                        ref(Coder.encodeType),
                                                                        var("aliases"),
                                                                        var("hydra.lib.sets.empty"),
                                                                        var("t"),
                                                                        var("cx"),
                                                                        var("g")),
                                                                    lambda("jt",
                                                                        Eithers.bind(
                                                                            apply(
                                                                                ref(Utils.javaTypeToJavaReferenceType),
                                                                                var("jt"),
                                                                                var("cx")),
                                                                            lambda("rt",
                                                                                right(
                                                                                    inject(
                                                                                        TypeArgument.TYPE_,
                                                                                        TypeArgument.REFERENCE,
                                                                                        var("rt")))))))),
                                                            var("filteredTypeArgs")),
                                                        lambda("jTypeArgs",
                                                            right(
                                                                apply(
                                                                    ref(Utils.javaMethodInvocationToJavaExpression),
                                                                    apply(
                                                                        ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                        var("classId"),
                                                                        var("methodId"),
                                                                        var("jTypeArgs"),
                                                                        list())))))))))))),
                        field(
                            JavaSymbolClass.HOISTED_LAMBDA,
                            lambda("arity",
                                Maybes.cases(
                                    var("mns"),
                                    apply(
                                        ref(Coder.typeAppFallbackCast),
                                        var("env"),
                                        var("aliases"),
                                        var("anns"),
                                        var("tyapps"),
                                        var("jatyp"),
                                        var("body"),
                                        var("correctedTyp"),
                                        var("cx"),
                                        var("g")),
                                    lambda("ns_",
                                        let(
                                            field("classId",
                                                apply(
                                                    ref(Utils.nameToJavaName),
                                                    var("aliases"),
                                                    apply(
                                                        ref(Coder.elementsQualifiedName),
                                                        var("ns_")))),
                                            field("methodId",
                                                wrap(Identifier.TYPE_,
                                                    apply(
                                                        ref(Utils.sanitizeJavaName),
                                                        var("localName")))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.filterPhantomTypeArgs),
                                                    var("varName"),
                                                    var("allTypeArgs"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("filteredTypeArgs",
                                                    Eithers.bind(
                                                        Eithers.mapList(
                                                            lambda("t",
                                                                Eithers.bind(
                                                                    apply(
                                                                        ref(Coder.encodeType),
                                                                        var("aliases"),
                                                                        var("hydra.lib.sets.empty"),
                                                                        var("t"),
                                                                        var("cx"),
                                                                        var("g")),
                                                                    lambda("jt",
                                                                        Eithers.bind(
                                                                            apply(
                                                                                ref(Utils.javaTypeToJavaReferenceType),
                                                                                var("jt"),
                                                                                var("cx")),
                                                                            lambda("rt",
                                                                                right(
                                                                                    inject(
                                                                                        TypeArgument.TYPE_,
                                                                                        TypeArgument.REFERENCE,
                                                                                        var("rt")))))))),
                                                            var("filteredTypeArgs")),
                                                        lambda("jTypeArgs",
                                                            let(
                                                                field("paramNames",
                                                                    Lists.map(
                                                                        lambda("i",
                                                                            wrap(
                                                                                Name.TYPE_,
                                                                                Strings.cat2(
                                                                                    string("p"),
                                                                                    Literals.showInt32(
                                                                                        var("i"))))),
                                                                        Math_.range_(
                                                                            int32(0),
                                                                            Math_.sub(
                                                                                var("arity"),
                                                                                int32(1))))),
                                                                field("paramExprs",
                                                                    Lists.map(
                                                                        lambda("p",
                                                                            apply(
                                                                                ref(Utils.javaIdentifierToJavaExpression),
                                                                                apply(
                                                                                    ref(Utils.variableToJavaIdentifier),
                                                                                    var("p")))),
                                                                        var("paramNames"))),
                                                                field("call",
                                                                    apply(
                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                        apply(
                                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                            var("classId"),
                                                                            var("methodId"),
                                                                            var("jTypeArgs"),
                                                                            var("paramExprs")))),
                                                                right(
                                                                    apply(
                                                                        ref(Coder.buildCurriedLambda),
                                                                        var("paramNames"),
                                                                        var("call")))))))))))))))));

    public static final Def typeArgsOrDiamond = def(
        "typeArgsOrDiamond",
        () -> lambda("args",
                Logic.ifElse(
                    apply(
                        project(JavaFeatures.TYPE_, JavaFeatures.SUPPORTS_DIAMOND_OPERATOR),
                        ref(Coder.javaFeatures)),
                    inject(TypeArgumentsOrDiamond.TYPE_,
                        TypeArgumentsOrDiamond.DIAMOND,
                        unit()),
                    inject(TypeArgumentsOrDiamond.TYPE_,
                        TypeArgumentsOrDiamond.ARGUMENTS,
                        var("args")))));

    public static final Def typesMatch = def(
        "typesMatch",
        () -> lambda(
                "a",
                "b",
                casesWithDefault(Type.TYPE_,
                    var("a"),
                    bool(true),
                    field(
                        Type.VARIABLE,
                        lambda("va",
                            casesWithDefault(Type.TYPE_,
                                var("b"),
                                bool(true),
                                field(
                                    Type.VARIABLE,
                                    lambda("vb", Equality.equal(var("va"), var("vb"))))))),
                    field(
                        Type.WRAP,
                        lambda("wa",
                            casesWithDefault(Type.TYPE_,
                                var("b"),
                                bool(true),
                                field(
                                    Type.WRAP,
                                    lambda("wb", Equality.equal(var("wa"), var("wb"))))))))));

    public static final Def unwrapReturnType = def(
        "unwrapReturnType",
        () -> lambda("t",
                casesWithDefault(Type.TYPE_,
                    apply(var("hydra.strip.deannotateType"), var("t")),
                    var("t"),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            apply(
                                ref(Coder.unwrapReturnType),
                                proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")))),
                    field(
                        Type.APPLICATION,
                        lambda("at",
                            apply(
                                ref(Coder.unwrapReturnType),
                                proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at")))))));

    public static final Def variantCompareToMethod = def(
        "variantCompareToMethod",
        () -> lambda(
                "aliases",
                "tparams",
                "parentName",
                "variantName",
                "fields",
                let(
                    java.util.Arrays.asList(
    field("anns",
                        list(
                            ref(Utils.overrideAnnotation),
                            ref(Utils.suppressWarningsUncheckedAnnotation))),
    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
    field("param",
                        apply(
                            ref(Utils.javaTypeToJavaFormalParameter),
                            apply(
                                ref(Utils.javaTypeFromTypeName),
                                var("aliases"),
                                var("parentName")),
                            wrap(Name.TYPE_,
                                ref(Names.otherInstanceName)))),
    field("result",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            ref(Utils.javaIntType))),
    field("varTmpName",
                        string("o")),
    field("tagDeclStmt",
                        apply(
                            ref(Utils.variableDeclarationStatement),
                            var("aliases"),
                            ref(Utils.javaIntType),
                            apply(ref(Utils.javaIdentifier), string("tagCmp")),
                            ref(Coder.tagCompareExpr))),
    field("tagReturnStmt",
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            inject(Statement.TYPE_,
                                Statement.IF_THEN,
                                record(IfThenStatement.TYPE_,
                                    field(
                                        IfThenStatement.EXPRESSION,
                                        ref(Coder.tagCmpNotZeroExpr)),
                                    field(
                                        IfThenStatement.STATEMENT,
                                        apply(
                                            ref(Utils.javaReturnStatement),
                                            just(
                                                apply(
                                                    ref(Utils.javaExpressionNameToJavaExpression),
                                                    record(ExpressionName.TYPE_,
                                                        field(
                                                            ExpressionName.QUALIFIER,
                                                            nothing()),
                                                        field(
                                                            ExpressionName.IDENTIFIER,
                                                            apply(
                                                                ref(Utils.javaIdentifier),
                                                                string("tagCmp")))))))))))),
    field("variantJavaType",
                        apply(
                            ref(Utils.javaTypeFromTypeName),
                            var("aliases"),
                            var("variantName"))),
    field("castOtherExpr",
                        apply(
                            ref(Utils.javaCastExpressionToJavaExpression),
                            apply(
                                ref(Utils.javaCastExpression),
                                apply(
                                    ref(Utils.nameToJavaReferenceType),
                                    var("aliases"),
                                    bool(false),
                                    list(),
                                    var("variantName"),
                                    nothing()),
                                apply(
                                    ref(Utils.javaIdentifierToJavaUnaryExpression),
                                    wrap(Identifier.TYPE_,
                                        ref(Names.otherInstanceName)))))),
    field("castDeclStmt",
                        apply(
                            ref(Utils.variableDeclarationStatement),
                            var("aliases"),
                            var("variantJavaType"),
                            apply(ref(Utils.javaIdentifier), var("varTmpName")),
                            var("castOtherExpr"))),
    field("emptyReturn",
                        list(
                            inject(BlockStatement.TYPE_,
                                BlockStatement.STATEMENT,
                                apply(
                                    ref(Utils.javaReturnStatement),
                                    just(
                                        apply(
                                            ref(Utils.javaIntExpression),
                                            bigint(java.math.BigInteger.valueOf(0L)))))))),
    field("valueCompareStmt",
                        Logic.ifElse(
                            Lists.null_(var("fields")),
                            var("emptyReturn"),
                            Lists.concat2(
                                list(var("castDeclStmt")),
                                apply(
                                    ref(Coder.compareToBody),
                                    var("aliases"),
                                    var("varTmpName"),
                                    var("fields"))))),
    field("body",
                        Lists.concat2(
                            list(var("tagDeclStmt"), var("tagReturnStmt")),
                            var("valueCompareStmt")))),
                    apply(
                        ref(Utils.methodDeclaration),
                        var("mods"),
                        list(),
                        var("anns"),
                        ref(Names.compareToMethodName),
                        list(var("param")),
                        var("result"),
                        just(var("body"))))));

    public static final Def visitBranch = def(
        "visitBranch",
        () -> lambda(
                java.util.Arrays.asList("env", "aliases", "dom", "tname", "jcod", "targs", "field", "cx", "g"),
                let(
                    field("jdom",
                        inject(hydra.java.syntax.Type.TYPE_,
                            hydra.java.syntax.Type.REFERENCE,
                            apply(
                                ref(Utils.nameToJavaReferenceType),
                                var("aliases"),
                                bool(true),
                                var("targs"),
                                var("tname"),
                                just(
                                    apply(
                                        var("hydra.formatting.capitalize"),
                                        apply(
                                            unwrap(Name.TYPE_),
                                            proj(Field.TYPE_, Field.NAME, "field"))))))),
                    field("mods",
                        list(
                            inject(MethodModifier.TYPE_,
                                MethodModifier.PUBLIC,
                                unit()))),
                    field("anns",
                        list(ref(Utils.overrideAnnotation))),
                    field("result",
                        inject(Result.TYPE_,
                            Result.TYPE,
                            wrap(UnannType.TYPE_, var("jcod")))),
                    casesWithDefault(Term.TYPE_,
                        apply(
                            var("hydra.strip.deannotateTerm"),
                            proj(Field.TYPE_, Field.TERM, "field")),
                        left(
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    Strings.cat2(
                                        string("visitBranch: field term is not a lambda: "),
                                        apply(
                                            var("hydra.show.core.term"),
                                            proj(Field.TYPE_, Field.TERM, "field")))))),
                        field(
                            Term.LAMBDA,
                            lambda("lam",
                                apply(
                                    ref(Coder.withLambda),
                                    var("env"),
                                    var("lam"),
                                    lambda("env2",
                                        let(
                                            field("lambdaParam",
                                                proj(Lambda.TYPE_, Lambda.PARAMETER, "lam")),
                                            field("body",
                                                proj(Lambda.TYPE_, Lambda.BODY, "lam")),
                                            field("env3",
                                                apply(
                                                    ref(Coder.insertBranchVar),
                                                    var("lambdaParam"),
                                                    var("env2"))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.analyzeJavaFunction),
                                                    var("env3"),
                                                    var("body"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("fs",
                                                    let(
                                                        field("bindings",
                                                            proj(FunctionStructure.TYPE_, FunctionStructure.BINDINGS, "fs")),
                                                        field("innerBody",
                                                            proj(FunctionStructure.TYPE_, FunctionStructure.BODY, "fs")),
                                                        field("env4",
                                                            proj(FunctionStructure.TYPE_, FunctionStructure.ENVIRONMENT, "fs")),
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Coder.bindingsToStatements),
                                                                var("env4"),
                                                                var("bindings"),
                                                                var("cx"),
                                                                var("g")),
                                                            lambda("bindResult",
                                                                let(
                                                                    field("bindingStmts",
                                                                        Pairs.first(
                                                                            var("bindResult"))),
                                                                    field("env5",
                                                                        Pairs.second(
                                                                            var("bindResult"))),
                                                                    Eithers.bind(
                                                                        apply(
                                                                            ref(Coder.encodeTerm),
                                                                            var("env5"),
                                                                            var("innerBody"),
                                                                            var("cx"),
                                                                            var("g")),
                                                                        lambda("jret",
                                                                            let(
                                                                                field("param",
                                                                                    apply(
                                                                                        ref(Utils.javaTypeToJavaFormalParameter),
                                                                                        var("jdom"),
                                                                                        var("lambdaParam"))),
                                                                                field("returnStmt",
                                                                                    inject(
                                                                                        BlockStatement.TYPE_,
                                                                                        BlockStatement.STATEMENT,
                                                                                        apply(
                                                                                            ref(Utils.javaReturnStatement),
                                                                                            just(
                                                                                                var("jret"))))),
                                                                                field("allStmts",
                                                                                    Lists.concat2(
                                                                                        var("bindingStmts"),
                                                                                        list(
                                                                                            var("returnStmt")))),
                                                                                right(
                                                                                    apply(
                                                                                        ref(Coder.noComment),
                                                                                        apply(
                                                                                            ref(Utils.methodDeclaration),
                                                                                            var("mods"),
                                                                                            list(),
                                                                                            var("anns"),
                                                                                            ref(Names.visitMethodName),
                                                                                            list(
                                                                                                var("param")),
                                                                                            var("result"),
                                                                                            just(
                                                                                                var("allStmts")))))))))))))))))))))));

    public static final Def withCommentString = def(
        "withCommentString",
        () -> lambda(
                "comment",
                "decl",
                record(ClassBodyDeclarationWithComments.TYPE_,
                    field(ClassBodyDeclarationWithComments.VALUE, var("decl")),
                    field(
                        ClassBodyDeclarationWithComments.COMMENTS,
                        just(var("comment"))))));

    public static final Def withInterfaceCommentString = def(
        "withInterfaceCommentString",
        () -> lambda(
                "comment",
                "decl",
                record(InterfaceMemberDeclarationWithComments.TYPE_,
                    field(
                        InterfaceMemberDeclarationWithComments.VALUE,
                        var("decl")),
                    field(
                        InterfaceMemberDeclarationWithComments.COMMENTS,
                        just(var("comment"))))));

    public static final Def withLambda = def(
        "withLambda",
        () -> lambda(
                "env",
                "lam",
                "k",
                apply(
                    var("hydra.environment.withLambdaContext"),
                    ref(Coder.javaEnvGetGraph),
                    ref(Coder.javaEnvSetGraph),
                    var("env"),
                    var("lam"),
                    lambda("env1",
                        let(
                            field("aliases",
                                proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env1")),
                            field("aliases2",
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
                                        proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases")),
                                    field(
                                        Aliases.LAMBDA_VARS,
                                        Sets.insert(
                                            proj(Lambda.TYPE_, Lambda.PARAMETER, "lam"),
                                            proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases"))),
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
                                        proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases")))),
                            field("env2",
                                record(JavaEnvironment.TYPE_,
                                    field(
                                        JavaEnvironment.ALIASES,
                                        var("aliases2")),
                                    field(
                                        JavaEnvironment.GRAPH,
                                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env1")))),
                            apply(var("k"), var("env2")))))));

    public static final Def withTypeLambda = def(
        "withTypeLambda",
        () -> apply(
                var("hydra.environment.withTypeLambdaContext"),
                ref(Coder.javaEnvGetGraph),
                ref(Coder.javaEnvSetGraph)));

    public static final Def wrapInSupplierLambda = def(
        "wrapInSupplierLambda",
        () -> lambda("expr",
                inject(Expression.TYPE_,
                    Expression.LAMBDA,
                    record(LambdaExpression.TYPE_,
                        field(
                            LambdaExpression.PARAMETERS,
                            inject(LambdaParameters.TYPE_,
                                LambdaParameters.TUPLE,
                                list())),
                        field(
                            LambdaExpression.BODY,
                            inject(LambdaBody.TYPE_,
                                LambdaBody.EXPRESSION,
                                var("expr")))))));

    public static final Def wrapLazyArguments = def(
        "wrapLazyArguments",
        () -> lambda(
                "name",
                "args",
                let(
                    field("dummyExpr",
                        apply(
                            ref(Utils.javaIntExpression),
                            bigint(java.math.BigInteger.valueOf(0L)))),
                    field("argAt",
                        lambda("i",
                            Maybes.fromMaybe(var("dummyExpr"), Lists.maybeAt(var("i"), var("args"))))),
                    Logic.ifElse(
                        Logic.and_(
                            Equality.equal(
                                var("name"),
                                wrap(Name.TYPE_, string("hydra.lib.logic.ifElse"))),
                            Equality.equal(Lists.length(var("args")), int32(3))),
                        pair(
                            list(
                                apply(var("argAt"), int32(0)),
                                apply(
                                    ref(Coder.wrapInSupplierLambda),
                                    apply(var("argAt"), int32(1))),
                                apply(
                                    ref(Coder.wrapInSupplierLambda),
                                    apply(var("argAt"), int32(2)))),
                            just(string("lazy"))),
                        Logic.ifElse(
                            Logic.and_(
                                Equality.equal(
                                    var("name"),
                                    wrap(Name.TYPE_, string("hydra.lib.maybes.maybe"))),
                                Equality.equal(Lists.length(var("args")), int32(3))),
                            pair(
                                list(
                                    apply(
                                        ref(Coder.wrapInSupplierLambda),
                                        apply(var("argAt"), int32(0))),
                                    apply(var("argAt"), int32(1)),
                                    apply(var("argAt"), int32(2))),
                                just(string("applyLazy"))),
                            Logic.ifElse(
                                Logic.and_(
                                    Equality.equal(
                                        var("name"),
                                        wrap(Name.TYPE_,
                                            string("hydra.lib.maybes.cases"))),
                                    Equality.equal(Lists.length(var("args")), int32(3))),
                                pair(
                                    list(
                                        apply(var("argAt"), int32(0)),
                                        apply(
                                            ref(Coder.wrapInSupplierLambda),
                                            apply(var("argAt"), int32(1))),
                                        apply(var("argAt"), int32(2))),
                                    just(string("applyLazy"))),
                                Logic.ifElse(
                                    Logic.and_(
                                        Equality.equal(
                                            var("name"),
                                            wrap(Name.TYPE_,
                                                string("hydra.lib.maps.findWithDefault"))),
                                        Equality.equal(Lists.length(var("args")), int32(3))),
                                    pair(
                                        list(
                                            apply(
                                                ref(Coder.wrapInSupplierLambda),
                                                apply(var("argAt"), int32(0))),
                                            apply(var("argAt"), int32(1)),
                                            apply(var("argAt"), int32(2))),
                                        just(string("applyLazy"))),
                                    Logic.ifElse(
                                        Logic.and_(
                                            Logic.or_(
                                                Equality.equal(
                                                    var("name"),
                                                    wrap(Name.TYPE_,
                                                        string("hydra.lib.maybes.fromMaybe"))),
                                                Logic.or_(
                                                    Equality.equal(
                                                        var("name"),
                                                        wrap(Name.TYPE_,
                                                            string("hydra.lib.eithers.fromLeft"))),
                                                    Equality.equal(
                                                        var("name"),
                                                        wrap(Name.TYPE_,
                                                            string("hydra.lib.eithers.fromRight"))))),
                                            Equality.equal(Lists.length(var("args")), int32(2))),
                                        pair(
                                            list(
                                                apply(
                                                    ref(Coder.wrapInSupplierLambda),
                                                    apply(var("argAt"), int32(0))),
                                                apply(var("argAt"), int32(1))),
                                            just(string("applyLazy"))),
                                        pair(var("args"), nothing())))))))));









    private static final List<Definition> DEFINITIONS = definitionsOf(
            addComment,
            analyzeJavaFunction,
            annotateBodyWithCod,
            annotateLambdaArgs,
            applyCastIfSafe,
            applyJavaArg,
            applyOvergenSubstToTermAnnotations,
            applyOvergenSubstToTermAnnotations_go,
            applySubstFull,
            applySubstSimple,
            arraysCompareExpr,
            arraysEqualsClause,
            augmentVariantClass,
            bindingIsFunctionType,
            bindingNameToFilePath,
            bindingsToStatements,
            boundTypeVariables,
            buildArgSubst,
            buildCurriedLambda,
            buildSubstFromAnnotations,
            buildSubstFromAnnotations_go,
            buildTypeSubst,
            buildTypeSubst_go,
            buildTypeVarSubst,
            buildTypeVarSubst_go,
            classModsPublic,
            classifyDataReference,
            classifyDataTerm,
            classifyDataTerm_countLambdaParams,
            classifyDataTerm_stripTypeLambdas,
            cmpDeclStatement,
            cmpNotZeroExpr,
            collectForallParams,
            collectLambdaDomains,
            collectTypeApps,
            collectTypeApps0,
            collectTypeVars,
            collectTypeVars_go,
            comparableCompareExpr,
            compareAndReturnStmts,
            compareFieldExpr,
            compareToBody,
            compareToZeroClause,
            constantDecl,
            constantDeclForFieldType,
            constantDeclForTypeName,
            constructElementsInterface,
            correctCastType,
            correctTypeApps,
            correctTypeAppsWithArgs,
            countFunctionParams,
            declarationForRecordType,
            declarationForRecordType_prime,
            declarationForUnionType,
            decodeTypeFromTerm,
            dedupBindings,
            detectAccumulatorUnification,
            directRefSubstitution,
            directRefSubstitution_processGroup,
            domTypeArgs,
            elementJavaIdentifier,
            elementJavaIdentifier_qualify,
            elementsClassName,
            elementsQualifiedName,
            encodeApplication,
            encodeApplication_fallback,
            encodeDefinitions,
            encodeElimination,
            encodeFunction,
            encodeFunctionFormTerm,
            encodeFunctionPrimitiveByName,
            encodeLiteral,
            encodeLiteralType,
            encodeLiteralType_simple,
            encodeLiteral_encodeFloat,
            encodeLiteral_encodeFloat32,
            encodeLiteral_encodeFloat64,
            encodeLiteral_encodeInteger,
            encodeLiteral_javaParseDouble,
            encodeLiteral_javaSpecialFloatExpr,
            encodeLiteral_litExp,
            encodeLiteral_primCast,
            encodeNullaryConstant,
            encodeNullaryConstant_typeArgsFromReturnType,
            encodeNullaryPrimitiveByName,
            encodeTerm,
            encodeTermDefinition,
            encodeTermInternal,
            encodeTermTCO,
            encodeType,
            encodeTypeDefinition,
            encodeType_resolveIfTypedef,
            encodeVariable,
            encodeVariable_buildCurried,
            encodeVariable_hoistedLambdaCase,
            eqClause,
            equalsClause,
            extractArgType,
            extractDirectReturn,
            extractDirectReturn_go,
            extractInOutPair,
            extractTypeApplicationArgs,
            extractTypeApplicationArgs_go,
            fieldTypeToFormalParam,
            filterByFlags,
            filterPhantomTypeArgs,
            filterPhantomTypeArgs_filterAndApply,
            findMatchingLambdaVar,
            findPairFirst,
            findSelfRefVar,
            first20Primes,
            flattenApps,
            flattenBindings,
            freshJavaName,
            freshJavaName_go,
            functionCall,
            getCodomain,
            getFunctionType,
            groupPairsByFirst,
            hashCodeCompareExpr,
            hashCodeMultPair,
            innerClassRef,
            insertBranchVar,
            interfaceTypes,
            isBigNumericType,
            isBinaryType,
            isFieldUnitType,
            isLambdaBoundIn,
            isLambdaBoundIn_isQualified,
            isLambdaBoundVariable,
            isLocalVariable,
            isNonComparableType,
            isRecursiveVariable,
            isSerializableJavaType,
            isSimpleName,
            isUnresolvedInferenceVar,
            isUnresolvedInferenceVar_isDigit,
            java11Features,
            java8Features,
            javaComparableRefType,
            javaEnvGetGraph,
            javaEnvSetGraph,
            javaFeatures,
            javaIdentifierToString,
            javaTypeArgumentsForNamedType,
            javaTypeArgumentsForType,
            javaTypeParametersForType,
            javaTypeParametersForType_bvars,
            moduleToJava,
            nameMapToTypeMap,
            namespaceParent,
            needsThunking,
            noComment,
            noInterfaceComment,
            otherwiseBranch,
            peelDomainTypes,
            peelDomainsAndCod,
            peelExpectedTypes,
            propagateType,
            propagateType_propagateIntoLambda,
            propagateType_rebuildLet,
            propagateTypesInAppChain,
            rebuildApps,
            recordCompareToMethod,
            recordConstructor,
            recordEqualsMethod,
            recordHashCodeMethod,
            recordMemberVar,
            recordWithMethod,
            resolveTypeApps,
            selfRefSubstitution,
            selfRefSubstitution_processGroup,
            serializableTypes,
            splitConstantInitializer,
            splitConstantInitializer_splitVar,
            stripForalls,
            substituteTypeVarsWithTypes,
            substituteTypeVarsWithTypes_go,
            tagCmpNotZeroExpr,
            tagCompareExpr,
            takeTypeArgs,
            toClassDecl,
            toDeclInit,
            toDeclStatement,
            tryInferFunctionType,
            typeAppFallbackCast,
            typeAppNullaryOrHoisted,
            typeArgsOrDiamond,
            typesMatch,
            unwrapReturnType,
            variantCompareToMethod,
            visitBranch,
            withCommentString,
            withInterfaceCommentString,
            withLambda,
            withTypeLambda,
            wrapInSupplierLambda,
            wrapLazyArguments);

    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
        new ModuleName("hydra.java.utils"),
        new ModuleName("hydra.java.names"),
        new ModuleName("hydra.java.serde"),
        new ModuleName("hydra.java.language"),
        new ModuleName("hydra.analysis"),
        new ModuleName("hydra.checking"),
        new ModuleName("hydra.formatting"),
        new ModuleName("hydra.names"),
        new ModuleName("hydra.rewriting"),
        new ModuleName("hydra.dependencies"),
        new ModuleName("hydra.scoping"),
        new ModuleName("hydra.strip"),
        new ModuleName("hydra.variables"),
        new ModuleName("hydra.lexical"),
        new ModuleName("hydra.environment"),
        new ModuleName("hydra.predicates"),
        new ModuleName("hydra.resolution"),
        new ModuleName("hydra.show.core"),
        new ModuleName("hydra.annotations"),
        new ModuleName("hydra.constants"),
        new ModuleName("hydra.inference"),
        new ModuleName("hydra.sorting"),
        new ModuleName("hydra.arity"),
        new ModuleName("hydra.decode.core"),
        new ModuleName("hydra.encode.core"),
        new ModuleName("hydra.serialization"),
        new ModuleName("hydra.java.environment"),
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
        Maybe.just("Java code generator: converts Hydra modules to Java source code"),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
