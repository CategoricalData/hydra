package hydra.sources.java;
import hydra.Refs;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.dsl.Core;
import hydra.dsl.Packaging;
import hydra.overlay.java.dsl.Types;
import hydra.dsl.java.Environment;
import hydra.dsl.java.Syntax;
import hydra.dsl.lib.Eithers;
import hydra.dsl.lib.Equality;
import hydra.dsl.lib.Lists;
import hydra.dsl.lib.Literals;
import hydra.dsl.lib.Logic;
import hydra.dsl.lib.Maps;
import hydra.dsl.lib.Math_;
import hydra.dsl.lib.Optionals;
import hydra.dsl.lib.Pairs;
import hydra.dsl.lib.Sets;
import hydra.dsl.lib.Strings;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.ModuleDependency;
import hydra.packaging.ModuleName;
import hydra.typed.TypedTerm;
import hydra.overlay.java.util.Optional;

import java.util.Arrays;
import java.util.List;

import static hydra.overlay.java.dsl.meta.Phantoms.*;
import hydra.overlay.java.dsl.meta.Defs;
import hydra.overlay.java.dsl.meta.Defs.Def;
import static hydra.overlay.java.dsl.meta.Defs.define;
import static hydra.overlay.java.dsl.meta.Defs.unqualifiedDeps;
import static hydra.overlay.java.dsl.meta.Defs.ref;
import static hydra.overlay.java.dsl.meta.Defs.definitionsOf;
import java.util.function.Supplier;
import hydra.core.AnnotatedTerm;
import hydra.core.AnnotatedType;
import hydra.core.Application;
import hydra.core.ApplicationType;
import hydra.core.Binding;
import hydra.core.CaseAlternative;
import hydra.core.CaseStatement;
import hydra.core.EitherType;
import hydra.core.FieldType;
import hydra.core.FloatType;
import hydra.core.FloatValue;
import hydra.core.ForallType;
import hydra.core.FunctionType;
import hydra.core.Injection;
import hydra.core.IntegerType;
import hydra.core.IntegerValue;
import hydra.core.Lambda;
import hydra.core.Let;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.core.MapType;
import hydra.core.PairType;
import hydra.core.Projection;
import hydra.core.Record;
import hydra.core.Term;
import hydra.core.TypeApplicationTerm;
import hydra.core.TypeLambda;
import hydra.core.WrappedTerm;
import hydra.errors.DecodingError;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.java.environment.Aliases;
import hydra.java.environment.JavaEnvironment;
import hydra.java.environment.JavaFeatures;
import hydra.java.environment.JavaSymbolClass;
import hydra.java.syntax.AmbiguousName;
import hydra.java.syntax.AnnotatedIdentifier;
import hydra.java.syntax.ArrayType;
import hydra.java.syntax.ArrayType_Variant;
import hydra.java.syntax.Block;
import hydra.java.syntax.BlockStatement;
import hydra.java.syntax.ClassBody;
import hydra.java.syntax.ClassBodyDeclaration;
import hydra.java.syntax.ClassBodyDeclarationWithComments;
import hydra.java.syntax.ClassDeclaration;
import hydra.java.syntax.ClassMemberDeclaration;
import hydra.java.syntax.ClassModifier;
import hydra.java.syntax.ClassOrInterfaceType;
import hydra.java.syntax.ClassOrInterfaceTypeToInstantiate;
import hydra.java.syntax.ClassType;
import hydra.java.syntax.ClassTypeQualifier;
import hydra.java.syntax.CompilationUnit;
import hydra.java.syntax.ConditionalAndExpression;
import hydra.java.syntax.ConstantDeclaration;
import hydra.java.syntax.ContinueStatement;
import hydra.java.syntax.Dims;
import hydra.java.syntax.EqualityExpression;
import hydra.java.syntax.EqualityExpression_Binary;
import hydra.java.syntax.Expression;
import hydra.java.syntax.ExpressionName;
import hydra.java.syntax.FieldAccess;
import hydra.java.syntax.FieldAccess_Qualifier;
import hydra.java.syntax.FieldModifier;
import hydra.java.syntax.FloatingPointLiteral;
import hydra.java.syntax.FloatingPointType;
import hydra.java.syntax.Identifier;
import hydra.java.syntax.IfThenStatement;
import hydra.java.syntax.ImportDeclaration;
import hydra.java.syntax.IntegerLiteral;
import hydra.java.syntax.IntegralType;
import hydra.java.syntax.InterfaceBody;
import hydra.java.syntax.InterfaceDeclaration;
import hydra.java.syntax.InterfaceMemberDeclaration;
import hydra.java.syntax.InterfaceMemberDeclarationWithComments;
import hydra.java.syntax.InterfaceMethodModifier;
import hydra.java.syntax.InterfaceModifier;
import hydra.java.syntax.InterfaceType;
import hydra.java.syntax.LambdaBody;
import hydra.java.syntax.LambdaExpression;
import hydra.java.syntax.LambdaParameters;
import hydra.java.syntax.LeftHandSide;
import hydra.java.syntax.MethodInvocation;
import hydra.java.syntax.MethodInvocation_Complex;
import hydra.java.syntax.MethodInvocation_Header;
import hydra.java.syntax.MethodInvocation_Variant;
import hydra.java.syntax.MethodModifier;
import hydra.java.syntax.MethodName;
import hydra.java.syntax.MultiplicativeExpression;
import hydra.java.syntax.MultiplicativeExpression_Binary;
import hydra.java.syntax.NormalClassDeclaration;
import hydra.java.syntax.NormalInterfaceDeclaration;
import hydra.java.syntax.NumericType;
import hydra.java.syntax.OrdinaryCompilationUnit;
import hydra.java.syntax.PostfixExpression;
import hydra.java.syntax.PrimitiveType;
import hydra.java.syntax.PrimitiveTypeWithAnnotations;
import hydra.java.syntax.ReferenceType;
import hydra.java.syntax.Result;
import hydra.java.syntax.SingleTypeImportDeclaration;
import hydra.java.syntax.Statement;
import hydra.java.syntax.StatementWithoutTrailingSubstatement;
import hydra.java.syntax.TopLevelClassOrInterfaceDeclaration;
import hydra.java.syntax.TopLevelClassOrInterfaceDeclarationWithComments;
import hydra.java.syntax.TypeArgument;
import hydra.java.syntax.TypeArgumentsOrDiamond;
import hydra.java.syntax.TypeIdentifier;
import hydra.java.syntax.UnannType;
import hydra.java.syntax.UnaryExpression;
import hydra.java.syntax.UnaryExpressionNotPlusMinus;
import hydra.java.syntax.VariableDeclarator;
import hydra.java.syntax.VariableDeclaratorId;
import hydra.java.syntax.VariableInitializer;
import hydra.java.syntax.WhileStatement;
import hydra.packaging.EntityMetadata;
import hydra.file.FileExtension;
import hydra.packaging.PrimitiveDefinition;
import hydra.util.QualifiedName;
import hydra.packaging.TermDefinition;
import hydra.packaging.TypeDefinition;
import hydra.sources.java.Names;
import hydra.sources.java.Serde;
import hydra.sources.java.Utils;
import hydra.typing.FunctionStructure;
import hydra.typing.Parameter;
import hydra.typing.TermSignature;
import hydra.util.CaseConvention;

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

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").lam("x").to(() -> body)}. See Defs.DefBuilder. */
    private static Defs.DefBuilder def(String localName) {
        return define(NS, localName);
    }

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def addComment = def("addComment")
        .lam("decl").lam("field").lam("cx").lam("g")
        .to(() ->
                Eithers.map(
                    lambda("c",
                        record(ClassBodyDeclarationWithComments.TYPE_,
                            field(
                                ClassBodyDeclarationWithComments.VALUE,
                                var("decl")),
                            field(
                                ClassBodyDeclarationWithComments.COMMENTS,
                                var("c")))),
                    hydra.dsl.Annotations.commentsFromFieldType(
                        var("cx"),
                        var("g"),
                        var("field"))));

    public static final Def analyzeJavaFunction = def("analyzeJavaFunction")
        .lam("env").lam("term").lam("cx").lam("g")
        .to(() ->
                hydra.dsl.Analysis.analyzeFunctionTerm(
                    var("cx"),
                    ref(Coder.javaEnvGetGraph),
                    ref(Coder.javaEnvSetGraph),
                    var("env"),
                    var("term")));

    public static final Def annotateBodyWithCod = def("annotateBodyWithCod")
        .lam("typ").lam("term")
        .to(() ->
                let("setAnn",
                    lambda("t",
                        hydra.dsl.Annotations.setTermAnnotation(
                            hydra.dsl.Constants.keyType(),
                            just(apply(tterm(Refs.encodeRef(Core.typeType())), var("typ"))),
                            var("t"))),
                    casesWithDefault(Term.TYPE_,
                        hydra.dsl.Strip.deannotateTerm( var("term")),
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
                                            hydra.dsl.Strip.deannotateTerm( var("rhs")),
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
                                                    var("annotatedRhs")))))))))));

    public static final Def annotateLambdaArgs = def("annotateLambdaArgs")
        .lam("cname").lam("tApps").lam("argTerms").lam("cx").lam("g")
        .to(() ->
                Logic.ifElse(
                    Lists.null_(var("tApps")),
                    right(var("argTerms")),
                    Eithers.bind(
                        Eithers.bind(
                            right(hydra.dsl.Lexical.lookupBinding( var("g"), var("cname"))),
                            lambda("mel",
                                Optionals.cases(
                                    var("mel"),
                                    right(
                                        Optionals.map(
                                            lambda("prim",
                                                hydra.dsl.Scoping.termSignatureToTypeScheme(
                                                    proj(PrimitiveDefinition.TYPE_, PrimitiveDefinition.SIGNATURE,
                                                        proj(Primitive.TYPE_, Primitive.DEFINITION, "prim")))),
                                            Maps.lookup(
                                                var("cname"),
                                                proj(Graph.TYPE_, Graph.PRIMITIVES, "g")))),
                                    lambda("el",
                                        right(
                                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el")))))),
                        lambda("mts",
                            Optionals.cases(
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
                                            Logic.or(
                                                Lists.null_(var("schemeVars")),
                                                Logic.not(
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
                                                                        string("unused"))))))))))))))));

    public static final Def applyCastIfSafe = def("applyCastIfSafe")
        .lam("aliases").lam("castType").lam("expr").lam("cx").lam("g")
        .to(() ->
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
                                    Logic.or(
                                        Sets.member(var("v"), var("inScope")),
                                        apply(ref(Coder.isLambdaBoundVariable), var("v")))),
                                Sets.toList(var("castVars"))))),
                    field("isSafe",
                        Logic.or(
                            Sets.null_(var("trusted")),
                            Logic.or(
                                Sets.null_(var("javaTypeVars")),
                                Sets.null_(Sets.difference(var("javaTypeVars"), var("trusted")))))),
                    Logic.ifElse(
                        var("isSafe"),
                        Eithers.bind(
                            apply(
                                ref(Coder.encodeType),
                                var("aliases"),
                                hydra.dsl.lib.Sets.empty(),
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
                        right(var("expr")))));

    public static final Def applyJavaArg = def("applyJavaArg")
        .lam("expr").lam("jarg")
        .to(() ->
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
                        list(var("jarg")))));

    public static final Def applyOvergenSubstToTermAnnotations = def("applyOvergenSubstToTermAnnotations")
        .lam("subst").lam("term0").lam("cx").lam("g")
        .to(() ->
                right(
                    apply(
                        ref(Coder.applyOvergenSubstToTermAnnotations_go),
                        var("subst"),
                        var("g"),
                        var("term0"))));

    public static final Def applyOvergenSubstToTermAnnotations_go = def("applyOvergenSubstToTermAnnotations_go")
        .lam("subst").lam("cx").lam("term")
        .to(() ->
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
                                    hydra.dsl.Annotations.getAnnotationMap(
                                        proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at"))),
                                field("ann'",
                                    Optionals.cases(
                                        Maps.lookup(hydra.dsl.Constants.keyType(), var("ann")),
                                        var("ann"),
                                        lambda("typeTerm",
                                            Eithers.either(
                                                constant(var("ann")),
                                                lambda("t",
                                                    let("t'",
                                                        apply(
                                                            ref(Coder.substituteTypeVarsWithTypes),
                                                            var("subst"),
                                                            var("t")),
                                                        Maps.insert(
                                                            hydra.dsl.Constants.keyType(),
                                                            apply(
                                                                tterm(Refs.encodeRef(Core.typeType())),
                                                                var("t'")),
                                                            var("ann")))),
                                                apply(
                                                    tterm(Refs.decodeRef(Core.typeType())),
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
                                        field(AnnotatedTerm.ANNOTATION,
                                            hydra.dsl.Annotations.wrapAnnotationMap(
                                                var("ann'")))))))),
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
                                        Optionals.map(
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
                                        Optionals.map(
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
                                                record(CaseAlternative.TYPE_,
                                                    field(
                                                        CaseAlternative.NAME,
                                                        proj(CaseAlternative.TYPE_, CaseAlternative.NAME, "fld")),
                                                    field(
                                                        CaseAlternative.HANDLER,
                                                        apply(
                                                            ref(Coder.applyOvergenSubstToTermAnnotations_go),
                                                            var("subst"),
                                                            var("cx"),
                                                            proj(CaseAlternative.TYPE_, CaseAlternative.HANDLER, "fld"))))),
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
                                            proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl")))))))));

    public static final Def applySubstFull = def("applySubstFull")
        .lam("s").lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
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
                        Type.OPTIONAL,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.OPTIONAL,
                                apply(ref(Coder.applySubstFull), var("s"), var("inner"))))),
                    field(
                        Type.EFFECT,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.EFFECT,
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
                                            proj(ForallType.TYPE_, ForallType.BODY, "ft")))))))));

    public static final Def applySubstSimple = def("applySubstSimple")
        .lam("subst").lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    var("t"),
                    field(
                        Type.VARIABLE,
                        lambda("v", Maps.findWithDefault(var("t"), var("v"), var("subst"))))));

    public static final Def arraysCompareExpr = def("arraysCompareExpr")
        .lam("otherVar").lam("fname")
        .to(() ->
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
                                list(var("arg1"), var("arg2")))))));

    public static final Def arraysEqualsClause = def("arraysEqualsClause")
        .lam("tmpName").lam("fname")
        .to(() ->
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
                                    list(var("thisArg"), var("otherArg"))))))));

    public static final Def augmentVariantClass = def("augmentVariantClass")
        .lam("aliases").lam("tparams").lam("elName").lam("cd")
        .to(() ->
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
                                            var("newBody")))))))));

    public static final Def bindingIsFunctionType = def("bindingIsFunctionType")
        .lam("b")
        .to(() ->
                Optionals.cases(
                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b"),
                    casesWithDefault(Term.TYPE_,
                        hydra.dsl.Strip.deannotateTerm(
                            proj(Binding.TYPE_, Binding.TERM, "b")),
                        bool(false),
                        field(Term.LAMBDA, constant(bool(true))),
                        field(Term.PROJECT, constant(bool(true))),
                        field(Term.CASES, constant(bool(true))),
                        field(Term.UNWRAP, constant(bool(true)))),
                    lambda("ts",
                        casesWithDefault(Type.TYPE_,
                            hydra.dsl.Strip.deannotateType(
                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                            bool(false),
                            field(Type.FUNCTION, constant(bool(true))),
                            field(
                                Type.FORALL,
                                lambda("fa",
                                    casesWithDefault(Type.TYPE_,
                                        hydra.dsl.Strip.deannotateType(
                                            proj(ForallType.TYPE_, ForallType.BODY, "fa")),
                                        bool(false),
                                        field(Type.FUNCTION, constant(bool(true))))))))));

    public static final Def bindingNameToFilePath = def("bindingNameToFilePath")
        .lam("name")
        .to(() ->
                let(
                    field("qn",
                        hydra.dsl.Names.qualifyName( var("name"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("sanitized",
                        hydra.dsl.Formatting.sanitizeWithUnderscores(
                            var("hydra.java.language.reservedWords"),
                            var("local"))),
                    field("unq",
                        hydra.dsl.Names.unqualifyName(
                            record(QualifiedName.TYPE_,
                                field(QualifiedName.MODULE_NAME, var("ns_")),
                                field(QualifiedName.LOCAL, var("sanitized"))))),
                    hydra.dsl.Names.nameToFilePath(
                        inject(CaseConvention.TYPE_,
                            CaseConvention.CAMEL,
                            unit()),
                        inject(CaseConvention.TYPE_,
                            CaseConvention.PASCAL,
                            unit()),
                        wrap(FileExtension.TYPE_, string("java")),
                        var("unq"))));

    public static final Def bindingsToStatements = def("bindingsToStatements")
        .lam("env").lam("bindings").lam("cx").lam("g0")
        .to(() ->
                let(
                    binds(    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
    field("g",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")),
    field("flatBindings",
                        apply(
                            ref(Coder.dedupBindings),
                            proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases"),
                            apply(ref(Coder.flattenBindings), var("bindings")))),
    field("gExtended",
                        hydra.dsl.Scoping.extendGraphForLet(
                            lambda(
                                "g",
                                "b",
                                Logic.ifElse(
                                    hydra.dsl.Predicates.isComplexBinding(
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
                                                hydra.dsl.Variables.freeVariablesInTerm(
                                                    proj(Binding.TYPE_, Binding.TERM, "b")))),
                                        pair(var("key"), var("deps")))),
                                var("flatBindings")))),
    field("sorted",
                        hydra.dsl.Sorting.topologicalSortComponents(
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
                                            Optionals.cases(
                                                Lists.maybeHead(var("names")),
                                                list(),
                                                lambda("singleName",
                                                    Optionals.cases(
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
                                                                list()))))),
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
                                                Logic.and(
                                                    Logic.not(
                                                        Sets.member(
                                                            var("bname"),
                                                            var("recursiveVars"))),
                                                    Logic.and(
                                                        Logic.and(
                                                            hydra.dsl.Predicates.isComplexBinding(
                                                                var("g"),
                                                                var("b")),
                                                            Logic.not(
                                                                hydra.dsl.Predicates.isTrivialTerm(
                                                                    proj(Binding.TYPE_, Binding.TERM, "b")))),
                                                        Logic.not(
                                                            apply(
                                                                ref(Coder.bindingIsFunctionType),
                                                                var("b"))))),
                                                list(var("bname")),
                                                list()))),
                                    var("flatBindings"))))),
    field("aliasesExtended",
                        recordWith(Aliases.TYPE_, "aliases", Utils.ALIASES_FIELDS,
                            field(
                                Aliases.RECURSIVE_VARS,
                                Sets.union(
                                    proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases"),
                                    var("recursiveVars"))),
                            field(
                                Aliases.IN_SCOPE_JAVA_VARS,
                                Sets.union(
                                    proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases"),
                                    var("bindingVars"))),
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
                                                            Optionals.cat(var("inits")),
                                                            var("decls")))))))),
                                var("sorted")),
                            lambda("groups",
                                right(pair(Lists.concat(var("groups")), var("envExtended"))))))));

    public static final Def boundTypeVariables = def("boundTypeVariables")
        .lam("typ")
        .to(() ->
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
                                    proj(ForallType.TYPE_, ForallType.BODY, "ft")))))));

    public static final Def buildArgSubst = def("buildArgSubst")
        .lam("schemeVarSet").lam("schemeDoms").lam("argTypes")
        .to(() ->
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
                                    hydra.dsl.Strip.deannotateType( var("sdom")),
                                    list(),
                                    field(
                                        Type.VARIABLE,
                                        lambda("v",
                                            Logic.ifElse(
                                                Sets.member(var("v"), var("schemeVarSet")),
                                                list(pair(var("v"), var("argType"))),
                                                list())))))))));

    public static final Def buildCurriedLambda = def("buildCurriedLambda")
        .lam("params").lam("inner")
        .to(() ->
                Lists.foldl(
                    lambda(
                        "acc",
                        "p",
                        apply(ref(Utils.javaLambda), var("p"), var("acc"))),
                    var("inner"),
                    Lists.reverse(var("params"))));

    public static final Def buildSubstFromAnnotations = def("buildSubstFromAnnotations")
        .lam("schemeVarSet").lam("term").lam("cx").lam("g")
        .to(() ->
                right(
                    apply(
                        ref(Coder.buildSubstFromAnnotations_go),
                        var("schemeVarSet"),
                        var("g"),
                        var("term"))));

    public static final Def buildSubstFromAnnotations_go = def("buildSubstFromAnnotations_go")
        .lam("schemeVarSet").lam("g").lam("term")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    var("term"),
                    hydra.dsl.lib.Maps.empty(),
                    field(
                        Term.ANNOTATED,
                        lambda("at",
                            let(
                                field("body",
                                    proj(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY, "at")),
                                field("anns",
                                    hydra.dsl.Annotations.getAnnotationMap(
                                        proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at"))),
                                field("bodySubst",
                                    apply(
                                        ref(Coder.buildSubstFromAnnotations_go),
                                        var("schemeVarSet"),
                                        var("g"),
                                        var("body"))),
                                field("annSubst",
                                    Optionals.cases(
                                        Maps.lookup(hydra.dsl.Constants.keyType(), var("anns")),
                                        hydra.dsl.lib.Maps.empty(),
                                        lambda("typeTerm",
                                            Eithers.either(
                                                constant(hydra.dsl.lib.Maps.empty()),
                                                lambda("annType",
                                                    casesWithDefault(Term.TYPE_,
                                                        hydra.dsl.Strip.deannotateTerm(
                                                            var("body")),
                                                        hydra.dsl.lib.Maps.empty(),
                                                        field(
                                                            Term.LAMBDA,
                                                            lambda("lam",
                                                                Optionals.cases(
                                                                    proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"),
                                                                    hydra.dsl.lib.Maps.empty(),
                                                                    lambda("dom",
                                                                        casesWithDefault(
                                                                            Type.TYPE_,
                                                                            hydra.dsl.Strip.deannotateType(
                                                                                var("annType")),
                                                                            hydra.dsl.lib.Maps.empty(),
                                                                            field(
                                                                                Type.FUNCTION,
                                                                                lambda("ft",
                                                                                    apply(
                                                                                        ref(Coder.buildTypeVarSubst),
                                                                                        var("schemeVarSet"),
                                                                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"),
                                                                                        var("dom"))))))))))),
                                                apply(
                                                    tterm(Refs.decodeRef(Core.typeType())),
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
                                    Optionals.cases(
                                        proj(CaseStatement.TYPE_, CaseStatement.DEFAULT, "cs"),
                                        hydra.dsl.lib.Maps.empty(),
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
                                                    proj(CaseAlternative.TYPE_, CaseAlternative.HANDLER, "fld")))),
                                        hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                hydra.dsl.lib.Maps.empty(),
                                var("terms")))),
                    field(
                        Term.OPTIONAL,
                        lambda("mt",
                            Optionals.cases(
                                var("mt"),
                                hydra.dsl.lib.Maps.empty(),
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
                                hydra.dsl.lib.Maps.empty(),
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
                                hydra.dsl.lib.Maps.empty(),
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
                            Eithers.either(
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
                                var("e"))))));

    public static final Def buildTypeSubst = def("buildTypeSubst")
        .lam("schemeVarSet").lam("schemeType").lam("actualType")
        .to(() ->
                apply(
                    ref(Coder.buildTypeSubst_go),
                    var("schemeVarSet"),
                    hydra.dsl.Strip.deannotateType( var("schemeType")),
                    hydra.dsl.Strip.deannotateType( var("actualType"))));

    public static final Def buildTypeSubst_go = def("buildTypeSubst_go")
        .lam("svs").lam("st").lam("at")
        .to(() ->
                let("goSub",
                    lambda(
                        "a",
                        "b",
                        apply(
                            ref(Coder.buildTypeSubst_go),
                            var("svs"),
                            hydra.dsl.Strip.deannotateType( var("a")),
                            hydra.dsl.Strip.deannotateType( var("b")))),
                    casesWithDefault(Type.TYPE_,
                        var("st"),
                        hydra.dsl.lib.Maps.empty(),
                        field(
                            Type.VARIABLE,
                            lambda("v",
                                Logic.ifElse(
                                    Sets.member(var("v"), var("svs")),
                                    Maps.singleton(var("v"), var("at")),
                                    hydra.dsl.lib.Maps.empty()))),
                        field(
                            Type.FUNCTION,
                            lambda("sft",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.LIST,
                                        lambda("al", apply(var("goSub"), var("sl"), var("al"))))))),
                        field(
                            Type.SET,
                            lambda("ss",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.SET,
                                        lambda("as'", apply(var("goSub"), var("ss"), var("as'"))))))),
                        field(
                            Type.OPTIONAL,
                            lambda("sm",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.OPTIONAL,
                                        lambda("am", apply(var("goSub"), var("sm"), var("am"))))))),
                        field(
                            Type.MAP,
                            lambda("smt",
                                casesWithDefault(Type.TYPE_,
                                    var("at"),
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                                proj(ForallType.TYPE_, ForallType.BODY, "afa"))))))))));

    public static final Def buildTypeVarSubst = def("buildTypeVarSubst")
        .lam("schemeVarSet").lam("freshTyp").lam("canonTyp")
        .to(() ->
                apply(
                    ref(Coder.buildTypeVarSubst_go),
                    var("schemeVarSet"),
                    hydra.dsl.Strip.deannotateType( var("freshTyp")),
                    hydra.dsl.Strip.deannotateType( var("canonTyp"))));

    public static final Def buildTypeVarSubst_go = def("buildTypeVarSubst_go")
        .lam("svs").lam("ft").lam("ct")
        .to(() ->
                let("goSub",
                    lambda(
                        "a",
                        "b",
                        apply(
                            ref(Coder.buildTypeVarSubst_go),
                            var("svs"),
                            hydra.dsl.Strip.deannotateType( var("a")),
                            hydra.dsl.Strip.deannotateType( var("b")))),
                    casesWithDefault(Type.TYPE_,
                        var("ft"),
                        casesWithDefault(Type.TYPE_,
                            var("ct"),
                            hydra.dsl.lib.Maps.empty(),
                            field(
                                Type.FORALL,
                                lambda("cfa",
                                    apply(
                                        ref(Coder.buildTypeVarSubst_go),
                                        var("svs"),
                                        var("ft"),
                                        hydra.dsl.Strip.deannotateType(
                                            proj(ForallType.TYPE_, ForallType.BODY, "cfa")))))),
                        field(
                            Type.VARIABLE,
                            lambda("fn",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.VARIABLE,
                                        lambda("cn",
                                            Logic.ifElse(
                                                Logic.and(
                                                    Logic.not(Equality.equal(var("fn"), var("cn"))),
                                                    Sets.member(var("cn"), var("svs"))),
                                                Maps.singleton(var("fn"), var("cn")),
                                                hydra.dsl.lib.Maps.empty())))))),
                        field(
                            Type.FUNCTION,
                            lambda("fft",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.LIST,
                                        lambda("cl", apply(var("goSub"), var("fl"), var("cl"))))))),
                        field(
                            Type.SET,
                            lambda("fs",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.SET,
                                        lambda("cs", apply(var("goSub"), var("fs"), var("cs"))))))),
                        field(
                            Type.OPTIONAL,
                            lambda("fm",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    hydra.dsl.lib.Maps.empty(),
                                    field(
                                        Type.OPTIONAL,
                                        lambda("cm", apply(var("goSub"), var("fm"), var("cm"))))))),
                        field(
                            Type.MAP,
                            lambda("fmt",
                                casesWithDefault(Type.TYPE_,
                                    var("ct"),
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                    hydra.dsl.lib.Maps.empty(),
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
                                        hydra.dsl.Strip.deannotateType(
                                            proj(ForallType.TYPE_, ForallType.BODY, "ffa")),
                                        var("ct")),
                                    field(
                                        Type.FORALL,
                                        lambda("cfa",
                                            apply(
                                                var("goSub"),
                                                proj(ForallType.TYPE_, ForallType.BODY, "ffa"),
                                                proj(ForallType.TYPE_, ForallType.BODY, "cfa"))))))))));

    public static final Def classModsPublic = def("classModsPublic")
        .to(() ->
                list(
                inject(ClassModifier.TYPE_,
                    ClassModifier.PUBLIC,
                    unit())));

    public static final Def classifyDataReference = def("classifyDataReference")
        .lam("name").lam("cx").lam("g")
        .to(() ->
                Eithers.bind(
                    right(hydra.dsl.Lexical.lookupBinding( var("g"), var("name"))),
                    lambda("mel",
                        Optionals.cases(
                            var("mel"),
                            right(
                                inject(JavaSymbolClass.TYPE_,
                                    JavaSymbolClass.LOCAL_VARIABLE,
                                    unit())),
                            lambda("el",
                                Optionals.cases(
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
                                                proj(Binding.TYPE_, Binding.TERM, "el"))))))))));

    public static final Def classifyDataTerm = def("classifyDataTerm")
        .lam("ts").lam("term")
        .to(() ->
                Logic.ifElse(
                    hydra.dsl.Dependencies.isLambda( var("term")),
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
                        Logic.not(
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
                                unit())))));

    public static final Def classifyDataTerm_countLambdaParams = def("classifyDataTerm_countLambdaParams")
        .lam("t")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("t")),
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
                                proj(Let.TYPE_, Let.BODY, "lt"))))));

    public static final Def classifyDataTerm_stripTypeLambdas = def("classifyDataTerm_stripTypeLambdas")
        .lam("t")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("t")),
                    var("t"),
                    field(
                        Term.TYPE_LAMBDA,
                        lambda("tl",
                            apply(
                                ref(Coder.classifyDataTerm_stripTypeLambdas),
                                proj(TypeLambda.TYPE_, TypeLambda.BODY, "tl"))))));

    public static final Def cmpDeclStatement = def("cmpDeclStatement")
        .lam("aliases")
        .to(() ->
                apply(
                    ref(Utils.variableDeclarationStatement),
                    var("aliases"),
                    ref(Utils.javaIntType),
                    apply(ref(Utils.javaIdentifier), string("cmp")),
                    apply(
                        ref(Utils.javaIntExpression),
                        bigint(java.math.BigInteger.valueOf(0L)))));

    public static final Def cmpNotZeroExpr = def("cmpNotZeroExpr")
        .to(() ->
                let(
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

    public static final Def collectForallParams = def("collectForallParams")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    list(),
                    field(
                        Type.FORALL,
                        lambda("fa",
                            Lists.cons(
                                proj(ForallType.TYPE_, ForallType.PARAMETER, "fa"),
                                apply(
                                    ref(Coder.collectForallParams),
                                    proj(ForallType.TYPE_, ForallType.BODY, "fa")))))));

    public static final Def collectLambdaDomains = def("collectLambdaDomains")
        .lam("t")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("t")),
                    pair(list(), var("t")),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            Optionals.cases(
                                proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"),
                                pair(list(), var("t")),
                                lambda("dom",
                                    let("rest",
                                        apply(
                                            ref(Coder.collectLambdaDomains),
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam")),
                                        pair(
                                            Lists.cons(var("dom"), Pairs.first(var("rest"))),
                                            Pairs.second(var("rest"))))))))));

    public static final Def collectTypeApps = def("collectTypeApps")
        .lam("t").lam("acc")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("t")),
                    pair(hydra.dsl.Strip.deannotateTerm( var("t")), var("acc")),
                    field(
                        Term.TYPE_APPLICATION,
                        lambda("ta",
                            apply(
                                ref(Coder.collectTypeApps),
                                proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta"),
                                Lists.cons(
                                    proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE, "ta"),
                                    var("acc")))))));

    public static final Def collectTypeApps0 = def("collectTypeApps0")
        .lam("t").lam("acc")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("t")),
                    pair(var("t"), var("acc")),
                    field(
                        Term.TYPE_APPLICATION,
                        lambda("ta",
                            apply(
                                ref(Coder.collectTypeApps0),
                                proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY, "ta"),
                                Lists.cons(
                                    proj(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE, "ta"),
                                    var("acc")))))));

    public static final Def collectTypeVars = def("collectTypeVars")
        .lam("typ")
        .to(() ->
                apply(
                    ref(Coder.collectTypeVars_go),
                    hydra.dsl.Strip.deannotateType( var("typ"))));

    public static final Def collectTypeVars_go = def("collectTypeVars_go")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    var("t"),
                    hydra.dsl.lib.Sets.empty(),
                    field(Type.VARIABLE, lambda("name", Sets.singleton(var("name")))),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")))))),
                    field(
                        Type.APPLICATION,
                        lambda("at",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at")))))),
                    field(
                        Type.LIST,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                hydra.dsl.Strip.deannotateType( var("inner"))))),
                    field(
                        Type.SET,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                hydra.dsl.Strip.deannotateType( var("inner"))))),
                    field(
                        Type.OPTIONAL,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                hydra.dsl.Strip.deannotateType( var("inner"))))),
                    field(
                        Type.EFFECT,
                        lambda("inner",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                hydra.dsl.Strip.deannotateType( var("inner"))))),
                    field(
                        Type.MAP,
                        lambda("mt",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(MapType.TYPE_, MapType.KEYS, "mt"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(MapType.TYPE_, MapType.VALUES, "mt")))))),
                    field(
                        Type.PAIR,
                        lambda("pt",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(PairType.TYPE_, PairType.FIRST, "pt"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(PairType.TYPE_, PairType.SECOND, "pt")))))),
                    field(
                        Type.EITHER,
                        lambda("et",
                            Sets.union(
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(EitherType.TYPE_, EitherType.LEFT, "et"))),
                                apply(
                                    ref(Coder.collectTypeVars_go),
                                    hydra.dsl.Strip.deannotateType(
                                        proj(EitherType.TYPE_, EitherType.RIGHT, "et")))))),
                    field(
                        Type.FORALL,
                        lambda("ft",
                            apply(
                                ref(Coder.collectTypeVars_go),
                                hydra.dsl.Strip.deannotateType(
                                    proj(ForallType.TYPE_, ForallType.BODY, "ft")))))));

    public static final Def comparableCompareExpr = def("comparableCompareExpr")
        .lam("otherVar").lam("fname")
        .to(() ->
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
                            wrap(Identifier.TYPE_, string("hydra.overlay.java.util.Comparing")),
                            wrap(Identifier.TYPE_, string("compare")),
                            list(var("thisField"), var("otherField"))))));

    public static final Def compareAndReturnStmts = def("compareAndReturnStmts")
        .lam("otherVar").lam("f")
        .to(() ->
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
                                                            string("cmp")))))))))))));

    public static final Def compareFieldExpr = def("compareFieldExpr")
        .lam("otherVar").lam("ft")
        .to(() ->
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
                            apply(ref(Coder.comparableCompareExpr), var("otherVar"), var("fname"))))));

    public static final Def compareToBody = def("compareToBody")
        .lam("aliases").lam("otherVar").lam("fields")
        .to(() ->
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
                    Optionals.fromOptional(
                        var("zeroStmts"),
                        Optionals.map(
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
                                                            Optionals.fromOptional(
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
                                                                    Optionals.fromOptional(
                                                                        var("firstField"),
                                                                        Lists.maybeLast(
                                                                            var("restFields"))))))))))))),
                            Lists.uncons(var("fields"))))));

    public static final Def compareToZeroClause = def("compareToZeroClause")
        .lam("tmpName").lam("fname")
        .to(() ->
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
                                field(EqualityExpression_Binary.RHS, var("rhs")))))));

    public static final Def constantDecl = def("constantDecl")
        .lam("comment").lam("javaName").lam("aliases").lam("name").lam("cx").lam("g")
        .to(() ->
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
                            hydra.dsl.lib.Sets.empty(),
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
                                                    var("var")))))))))));

    public static final Def constantDeclForFieldType = def("constantDeclForFieldType")
        .lam("parentName").lam("aliases").lam("ftyp").lam("cx").lam("g")
        .to(() ->
                let(
                    field("name",
                        proj(FieldType.TYPE_, FieldType.NAME, "ftyp")),
                    field("javaName",
                        hydra.dsl.Formatting.nonAlnumToUnderscores(
                            hydra.dsl.Formatting.convertCase(
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
                        var("g"))));

    public static final Def constantDeclForTypeName = def("constantDeclForTypeName")
        .lam("aliases").lam("name").lam("cx").lam("g")
        .to(() ->
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
                        var("g"))));

    public static final Def constructElementsInterface = def("constructElementsInterface")
        .lam("mod").lam("members")
        .to(() ->
                let(
                    binds(    field("ns",
                        proj(Module.TYPE_, Module.NAME, "mod")),
    field("parentNs",
                        apply(ref(Coder.namespaceParent), var("ns"))),
    field("pkg",
                        Optionals.cases(
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
                                Optionals.bind(
                                    proj(Module.TYPE_, Module.METADATA, "mod"),
                                    lambda("em",
                                        proj(EntityMetadata.TYPE_, EntityMetadata.DESCRIPTION, "em"))))))),
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
                                    list(var("decl"))))))));

    public static final Def correctCastType = def("correctCastType")
        .lam("innerBody").lam("typeArgs").lam("fallback").lam("cx").lam("g")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("innerBody")),
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
                                                Optionals.fromOptional(
                                                    var("fallback"),
                                                    Lists.maybeAt(int32(0), var("typeArgs")))),
                                            field(
                                                PairType.SECOND,
                                                Optionals.fromOptional(
                                                    var("fallback"),
                                                    Lists.maybeAt(int32(1), var("typeArgs"))))))),
                                right(var("fallback")))))));

    public static final Def correctTypeApps = def("correctTypeApps")
        .lam("gr").lam("name").lam("args").lam("fallbackTypeApps").lam("cx").lam("g")
        .to(() ->
                Eithers.bind(
                    right(hydra.dsl.Lexical.lookupBinding( var("g"), var("name"))),
                    lambda("mel",
                        Optionals.cases(
                            var("mel"),
                            right(var("fallbackTypeApps")),
                            lambda("el",
                                Optionals.cases(
                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                    right(var("fallbackTypeApps")),
                                    lambda("ts",
                                        let(
                                            binds(    field("schemeType",
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
                                                        Logic.and(
                                                            Sets.member(
                                                                var("v"),
                                                                var("schemeTypeVars")),
                                                            Logic.not(
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
                                                Logic.or(
                                                    Lists.null_(var("schemeVars")),
                                                    Logic.not(
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
                                                    var("g")))))))))));

    public static final Def correctTypeAppsWithArgs = def("correctTypeAppsWithArgs")
        .lam("schemeVars").lam("fallbackTypeApps").lam("schemeType").lam("args").lam("cx").lam("g")
        .to(() ->
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
                                    hydra.dsl.Annotations.getType(
                                        var("g"),
                                        hydra.dsl.Annotations.termAnnotationInternal(
                                            var("arg"))))),
                            var("args")),
                        lambda("mArgTypes",
                            Logic.ifElse(
                                Logic.not(
                                    Lists.null_(
                                        Lists.filter(
                                            lambda("m", Optionals.isNone(var("m"))),
                                            var("mArgTypes")))),
                                right(var("fallbackTypeApps")),
                                let(
                                    field("argTypes",
                                        Lists.bind(
                                            var("mArgTypes"),
                                            lambda("m",
                                                Optionals.cases(
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
                                                    Logic.not(
                                                        apply(
                                                            ref(Coder.typesMatch),
                                                            hydra.dsl.Strip.deannotateType(
                                                                Pairs.first(var("p"))),
                                                            hydra.dsl.Strip.deannotateType(
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
                                                    var("argTypes")))))))))));

    public static final Def countFunctionParams = def("countFunctionParams")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    int32(0),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            Math_.add(
                                int32(1),
                                apply(
                                    ref(Coder.countFunctionParams),
                                    proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")))))));

    public static final Def declarationForRecordType = def("declarationForRecordType")
        .to(() ->
                lambda(
                params("isInner", "isSer", "aliases", "tparams", "elName", "fields", "cx", "g"),
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

    public static final Def declarationForRecordType_prime = def("declarationForRecordType'")
        .to(() ->
                lambda(
                params("isInner", "isSer", "aliases", "tparams", "elName", "parentName", "fields", "cx", "g"),
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
                                let(binds(                                    field("elNameStr",
                                        apply(
                                            unwrap(Identifier.TYPE_),
                                            apply(
                                                ref(Utils.nameToJavaName),
                                                var("aliases"),
                                                var("elName")))),
                                    field("linkTargetStr",
                                        Optionals.cases(
                                            var("parentName"),
                                            var("elNameStr"),
                                            lambda("pn",
                                                Strings.cat2(
                                                    Strings.cat2(
                                                        apply(
                                                            unwrap(Identifier.TYPE_),
                                                            apply(
                                                                ref(Utils.nameToJavaName),
                                                                var("aliases"),
                                                                var("pn"))),
                                                        string(".")),
                                                    apply(
                                                        ref(Utils.sanitizeJavaName),
                                                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL,
                                                            hydra.dsl.Names.qualifyName( var("elName"))))))))),
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
                                                                            var("linkTargetStr"),
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
                                                                        ref(Utils.sanitizeJavaName),
                                                                        apply(
                                                                            unwrap(Name.TYPE_),
                                                                            proj(FieldType.TYPE_, FieldType.NAME, "f"))),
                                                                    Eithers.bind(
                                                                        hydra.dsl.Annotations.commentsFromFieldType(
                                                                            var("cx"),
                                                                            var("g"),
                                                                            var("f")),
                                                                        lambda("mDoc",
                                                                            right(
                                                                                Optionals.cases(var("mDoc"), string(""), lambda("d",
                                                                                        Strings.cat(
                                                                                            list(
                                                                                                string("@param "),
                                                                                                var("fname"),
                                                                                                string(" "),
                                                                                                var("d")))))))))),
                                                            var("fields")),
                                                        lambda("paramLines",
                                                            let(
                                                                field("nonEmptyParamLines",
                                                                    Lists.filter(
                                                                        lambda("l",
                                                                            Logic.not(
                                                                                Equality.equal(
                                                                                    var("l"),
                                                                                    string("")))),
                                                                        var("paramLines"))),
                                                                field("consBaseComment",
                                                                    Strings.cat(
                                                                        list(
                                                                            string("Constructs an immutable {@link "),
                                                                            var("linkTargetStr"),
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
                                                                        Eithers.bind(
                                                                            // Fluent builder (static builder() + nested Builder class), top-level records only.
                                                                            Logic.ifElse(
                                                                                var("isInner"),
                                                                                right(list()),
                                                                                apply(
                                                                                    ref(Coder.recordBuilderClass),
                                                                                    var("aliases"),
                                                                                    var("tparams"),
                                                                                    var("elName"),
                                                                                    var("fields"),
                                                                                    var("cx"),
                                                                                    var("g"))),
                                                                        lambda("builderDecls",
                                                                        let(
                                                                            field(
                                                                                "comparableMethods",
                                                                                Optionals.cases(
                                                                                    var("parentName"),
                                                                                    Logic.ifElse(
                                                                                        Logic.and(
                                                                                            Logic.not(
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
                                                                                        var("withMethods"),
                                                                                        var("builderDecls")))),
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
                                                                                    var("bodyDecls"))))))))))))))))))))));

    public static final Def declarationForUnionType = def("declarationForUnionType")
        .lam("isSer").lam("aliases").lam("tparams").lam("elName").lam("fields").lam("cx").lam("g")
        .to(() ->
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
                                        hydra.dsl.Predicates.isUnitType(
                                            hydra.dsl.Strip.deannotateType( var("ftype"))),
                                        list(),
                                        list(
                                            record(FieldType.TYPE_,
                                                field(
                                                    FieldType.NAME,
                                                    wrap(Name.TYPE_, string("value"))),
                                                field(
                                                    FieldType.TYPE,
                                                    hydra.dsl.Strip.deannotateType(
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
                                        binds(    field("privateConst",
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
                                                        binds(    field("fname",
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
    field("varLocalStr",
                                                            apply(
                                                                ref(Utils.sanitizeJavaName),
                                                                proj(QualifiedName.TYPE_, QualifiedName.LOCAL,
                                                                    hydra.dsl.Names.qualifyName( var("varName"))))),
    field("linkVarNameStr",
                                                            Strings.cat2(
                                                                Strings.cat2(
                                                                    var("elNameStr"),
                                                                    string(".")),
                                                                var("varLocalStr"))),
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
                                                                    var("linkVarNameStr"),
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
                                                        binds(    field("fname",
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
    field("varLocalStr",
                                                            apply(
                                                                ref(Utils.sanitizeJavaName),
                                                                proj(QualifiedName.TYPE_, QualifiedName.LOCAL,
                                                                    hydra.dsl.Names.qualifyName( var("varName"))))),
    field("linkVarNameStr",
                                                            Strings.cat2(
                                                                Strings.cat2(
                                                                    var("elNameStr"),
                                                                    string(".")),
                                                                var("varLocalStr"))),
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
                                                                    var("linkVarNameStr"),
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
                                                            binds(    field("tn",
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
                                                                    var("bodyDecls")))))))))))))));

    public static final Def decodeTypeFromTerm = def("decodeTypeFromTerm")
        .lam("term")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("term")),
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
                                                        Optionals.bind(
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
                                                            Optionals.bind(
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
                                                                    Optionals.bind(
                                                                        apply(
                                                                            ref(Coder.decodeTypeFromTerm),
                                                                            proj(Field.TYPE_, Field.TERM, "funcField")),
                                                                        lambda("func",
                                                                            Optionals.bind(
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
                                                                                    Optionals.map(
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
                                                                Optionals.bind(
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
                                                                        Optionals.bind(
                                                                            apply(
                                                                                ref(Coder.decodeTypeFromTerm),
                                                                                proj(Field.TYPE_, Field.TERM, "domField")),
                                                                            lambda("dom",
                                                                                Optionals.bind(
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
                                                                                        Optionals.map(
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
                                nothing())))));

    public static final Def dedupBindings = def("dedupBindings")
        .lam("inScope").lam("bs")
        .to(() ->
                Optionals.fromOptional(
                    list(),
                    Optionals.map(
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
                                                            hydra.dsl.Variables.substituteVariables(
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
                        Lists.uncons(var("bs")))));

    public static final Def detectAccumulatorUnification = def("detectAccumulatorUnification")
        .lam("doms").lam("cod").lam("tparams")
        .to(() ->
                let(
                    binds(    field("tparamSet",
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
                            hydra.dsl.Strip.deannotateType( var("cod")),
                            nothing(),
                            field(Type.VARIABLE, lambda("v", just(var("v")))))),
    field("directRefSubst",
                        apply(
                            ref(Coder.directRefSubstitution),
                            var("directInputVars"),
                            var("codVar"),
                            var("groupedDirect"))),
    field("codSubst",
                        Optionals.cases(
                                apply(ref(Coder.findPairFirst), var("cod")),
                                hydra.dsl.lib.Maps.empty(),
                                lambda("cv",
                                Logic.ifElse(
                                    Maps.member(var("cv"), var("selfRefSubst")),
                                    hydra.dsl.lib.Maps.empty(),
                                    Optionals.cases(
                                            apply(ref(Coder.findSelfRefVar), var("groupedByInput")),
                                            hydra.dsl.lib.Maps.empty(),
                                            lambda("refVar",
                                            Logic.ifElse(
                                                Equality.equal(var("cv"), var("refVar")),
                                                hydra.dsl.lib.Maps.empty(),
                                                Maps.singleton(var("cv"), var("refVar"))))))))),
    field("domVars",
                        Sets.fromList(
                            Lists.bind(
                                var("doms"),
                                lambda("d",
                                    Sets.toList(apply(ref(Coder.collectTypeVars), var("d"))))))),
    field("danglingSubst",
                        Optionals.cases(apply(ref(Coder.findPairFirst), var("cod")), hydra.dsl.lib.Maps.empty(), lambda("cv",
                                Logic.ifElse(
                                    Sets.member(var("cv"), var("domVars")),
                                    hydra.dsl.lib.Maps.empty(),
                                    Optionals.cases(apply(ref(Coder.findSelfRefVar), var("groupedByInput")), hydra.dsl.lib.Maps.empty(), lambda("refVar",
                                            Maps.singleton(
                                                var("cv"),
                                                inject(Type.TYPE_,
                                                    Type.VARIABLE,
                                                    var("refVar")))))))))),
                    Maps.union(
                        Maps.union(
                            Maps.union(
                                apply(ref(Coder.nameMapToTypeMap), var("selfRefSubst")),
                                apply(ref(Coder.nameMapToTypeMap), var("codSubst"))),
                            var("danglingSubst")),
                        apply(ref(Coder.nameMapToTypeMap), var("directRefSubst")))));

    public static final Def directRefSubstitution = def("directRefSubstitution")
        .lam("directInputVars").lam("codVar").lam("grouped")
        .to(() ->
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
                    hydra.dsl.lib.Maps.empty(),
                    Maps.toList(var("grouped"))));

    public static final Def directRefSubstitution_processGroup = def("directRefSubstitution_processGroup")
        .lam("directInputVars").lam("codVar").lam("subst").lam("inVar").lam("outVars")
        .to(() ->
                let(
                    field("selfRefCount",
                        Lists.length(
                            Lists.filter(
                                lambda("v", Equality.equal(var("v"), var("inVar"))),
                                var("outVars")))),
                    field("nonSelfVars",
                        Lists.filter(
                            lambda("v", Logic.not(Equality.equal(var("v"), var("inVar")))),
                            var("outVars"))),
                    field("safeNonSelfVars",
                        Lists.filter(
                            lambda("v",
                                Logic.and(
                                    Logic.not(Sets.member(var("v"), var("directInputVars"))),
                                    Logic.not(Equality.equal(just(var("v")), var("codVar"))))),
                            var("nonSelfVars"))),
                    Logic.ifElse(
                        Logic.and(
                            Equality.gte(var("selfRefCount"), int32(2)),
                            Logic.not(Lists.null_(var("safeNonSelfVars")))),
                        Lists.foldl(
                            lambda("s", lambda("v", Maps.insert(var("v"), var("inVar"), var("s")))),
                            var("subst"),
                            var("safeNonSelfVars")),
                        var("subst"))));

    public static final Def domTypeArgs = def("domTypeArgs")
        .lam("aliases").lam("d").lam("cx").lam("g")
        .to(() ->
                let("args",
                    apply(
                        ref(Coder.extractTypeApplicationArgs),
                        hydra.dsl.Strip.deannotateType( var("d"))),
                    Logic.ifElse(
                        Logic.not(Lists.null_(var("args"))),
                        Eithers.mapList(
                            lambda("t",
                                Eithers.bind(
                                    apply(
                                        ref(Coder.encodeType),
                                        var("aliases"),
                                        hydra.dsl.lib.Sets.empty(),
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
                        right(apply(ref(Coder.javaTypeArgumentsForType), var("d"))))));

    public static final Def elementJavaIdentifier = def("elementJavaIdentifier")
        .lam("isPrim").lam("isMethod").lam("aliases").lam("name")
        .to(() ->
                let(
                    field("qn",
                        hydra.dsl.Names.qualifyName( var("name"))),
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
                                        hydra.dsl.Formatting.capitalize( var("local"))),
                                    string(".")),
                                ref(Names.applyMethodName))),
                        Optionals.cases(
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
                                            var("local")))))))));

    public static final Def elementJavaIdentifier_qualify = def("elementJavaIdentifier_qualify")
        .lam("aliases").lam("mns").lam("s")
        .to(() ->
                apply(
                    unwrap(Identifier.TYPE_),
                    apply(
                        ref(Utils.nameToJavaName),
                        var("aliases"),
                        hydra.dsl.Names.unqualifyName(
                            record(QualifiedName.TYPE_,
                                field(QualifiedName.MODULE_NAME, var("mns")),
                                field(QualifiedName.LOCAL, var("s")))))));

    public static final Def elementsClassName = def("elementsClassName")
        .lam("ns")
        .to(() ->
                let(
                    field("nsStr",
                        apply(unwrap(ModuleName.TYPE_), var("ns"))),
                    field("parts",
                        Strings.splitOn(string("."), var("nsStr"))),
                    hydra.dsl.Formatting.sanitizeWithUnderscores(
                        var("hydra.java.language.reservedWords"),
                        hydra.dsl.Formatting.capitalize(
                            Optionals.fromOptional(var("nsStr"), Lists.maybeLast(var("parts")))))));

    public static final Def elementsQualifiedName = def("elementsQualifiedName")
        .lam("ns")
        .to(() ->
                hydra.dsl.Names.unqualifyName(
                    record(QualifiedName.TYPE_,
                        field(
                            QualifiedName.MODULE_NAME,
                            apply(ref(Coder.namespaceParent), var("ns"))),
                        field(
                            QualifiedName.LOCAL,
                            apply(ref(Coder.elementsClassName), var("ns"))))));

    public static final Def encodeApplication = def("encodeApplication")
        .lam("env").lam("app").lam("cx").lam("g0")
        .to(() ->
                let(
                    field("aliases",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field("g",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")),
                    field("gathered",
                        hydra.dsl.Analysis.gatherArgsWithTypeApps(
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
                            hydra.dsl.Annotations.getType(
                                var("g"),
                                hydra.dsl.Annotations.termAnnotationInternal( var("fun")))),
                        lambda("mfunTyp",
                            Eithers.bind(
                                Optionals.cases(
                                    var("mfunTyp"),
                                    hydra.dsl.Checking.typeOfTerm(
                                        var("cx"),
                                        var("g"),
                                        var("fun")),
                                    lambda("t", right(var("t")))),
                                lambda("funTyp",
                                    let(
                                        field("arity",
                                            hydra.dsl.Arity.typeArity( var("funTyp"))),
                                        field("deannotatedFun",
                                            hydra.dsl.Strip.deannotateTerm( var("fun"))),
                                        field("calleeName",
                                            casesWithDefault(Term.TYPE_,
                                                var("deannotatedFun"),
                                                nothing(),
                                                field(
                                                    Term.VARIABLE,
                                                    lambda("n", just(var("n")))))),
                                        Eithers.bind(
                                            Optionals.cases(
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
                                                                Optionals.isGiven(
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
                                                                    Logic.and(
                                                                        apply(
                                                                            ref(Coder.isRecursiveVariable),
                                                                            var("aliases"),
                                                                            var("name")),
                                                                        Logic.not(
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
                                                                                        Logic.or(
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
                                                                                                Logic.not(
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
                                                                                                            var("rargs")))))))))))))))))))))))));

    public static final Def encodeApplication_fallback = def("encodeApplication_fallback")
        .lams("env", "aliases", "gr", "typeApps", "lhs", "rhs", "cx", "g")
        .to(() ->
                Eithers.bind(
                    Eithers.bimap(
                        lambda("__de",
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                        lambda("__a", var("__a")),
                        hydra.dsl.Annotations.getType(
                            var("g"),
                            hydra.dsl.Annotations.termAnnotationInternal( var("lhs")))),
                    lambda("mt",
                        Eithers.bind(
                            Optionals.cases(
                                var("mt"),
                                hydra.dsl.Checking.typeOfTerm(
                                    var("cx"),
                                    var("g"),
                                    var("lhs")),
                                lambda("typ", right(var("typ")))),
                            lambda("t",
                                casesWithDefault(Type.TYPE_,
                                    hydra.dsl.Strip.deannotateTypeParameters(
                                        hydra.dsl.Strip.deannotateType( var("t"))),
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
                                                // BUG #438: defaultExpr and elimBranch used to be let-bound
                                                // as plain values here. That works in lazy Haskell (let
                                                // bindings aren't forced unless used) but BREAKS in eager
                                                // Clojure: both branches evaluate even though only one is
                                                // selected. When lhs is a CASES/PROJECT/UNWRAP, the unused
                                                // defaultExpr still evaluates `encodeTerm env lhs`, which
                                                // routes back through encodeFunction →
                                                // encodeElimination(:nothing, …) → typedLambda →
                                                // encodeApplication_fallback again on the same lhs —
                                                // unbounded recursion.
                                                //
                                                // Fix: thunk each binding behind `lambda("_", …)` so it
                                                // is only evaluated when explicitly applied in the
                                                // matching casesWithDefault branch. This preserves the
                                                // original Haskell-lazy semantics under Clojure-strict
                                                // evaluation without restructuring the call site.
                                                field("defaultExpr",
                                                    lambda("__bug438_dflt",
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
                                                                            var("jarg"))))))))),
                                                field("elimBranch",
                                                    lambda("__bug438_elim",
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
                                                                    Logic.not(
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
                                                                            hydra.dsl.Annotations.getType(
                                                                                var("g"),
                                                                                hydra.dsl.Annotations.termAnnotationInternal(
                                                                                    var("rhs")))),
                                                                        lambda("mrt",
                                                                            Optionals.cases(
                                                                                var("mrt"),
                                                                                Eithers.bind(
                                                                                    hydra.dsl.Checking.typeOfTerm(
                                                                                        var("cx"),
                                                                                        var("g"),
                                                                                        var("rhs")),
                                                                                    lambda("rt",
                                                                                        right(
                                                                                            Logic.ifElse(
                                                                                                Logic.not(
                                                                                                    Lists.null_(
                                                                                                        apply(
                                                                                                            ref(Coder.javaTypeArgumentsForType),
                                                                                                            var("rt")))),
                                                                                                var("rt"),
                                                                                                var("dom"))))),
                                                                                lambda("rt",
                                                                                    right(
                                                                                        Logic.ifElse(
                                                                                            Logic.not(
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
                                                                        hydra.dsl.Strip.deannotateTerm(
                                                                            var("lhs")),
                                                                        var("cx"),
                                                                        var("g")))))))),
                                                casesWithDefault(Term.TYPE_,
                                                    hydra.dsl.Strip.deannotateAndDetypeTerm(
                                                        var("lhs")),
                                                    apply(var("defaultExpr"), unit()),
                                                    field(
                                                        Term.PROJECT,
                                                        constant(apply(var("elimBranch"), unit()))),
                                                    field(
                                                        Term.CASES,
                                                        constant(apply(var("elimBranch"), unit()))),
                                                    field(
                                                        Term.UNWRAP,
                                                        constant(apply(var("elimBranch"), unit())))))))))))));

    public static final Def encodeDefinitions = def("encodeDefinitions")
        .lam("mod").lam("defs").lam("cx").lam("g")
        .to(() ->
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
                        hydra.dsl.Environment.partitionDefinitions( var("defs"))),
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
                                        proj(TypeDefinition.TYPE_, TypeDefinition.BODY, "td")),
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
                                            Lists.concat2(var("typeUnits"), var("termUnits"))))))))));

    public static final Def encodeElimination = def("encodeElimination")
        .lam("env").lam("marg").lam("dom").lam("cod").lam("elimTerm").lam("cx").lam("g")
        .to(() ->
                let("aliases",
                    proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env"),
                    casesWithDefault(Term.TYPE_,
                        hydra.dsl.Strip.deannotateAndDetypeTerm( var("elimTerm")),
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
                                            hydra.dsl.lib.Sets.empty(),
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
                                                    Optionals.cases(
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
                                    Optionals.cases(
                                        var("marg"),
                                        let(
                                            field("uVar",
                                                wrap(Name.TYPE_, string("u"))),
                                            field("domTypeArgs0",
                                                lambda(
                                                    "ty",
                                                    "acc",
                                                    casesWithDefault(Type.TYPE_,
                                                        hydra.dsl.Strip.deannotateType(
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
                                                hydra.dsl.Strip.deannotateAndDetypeTerm(
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
                                                        hydra.dsl.lib.Sets.empty(),
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
                                                                                Optionals.cases(
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
                                        Optionals.cases(
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
                                            lambda("jarg", apply(var("withArg"), var("jarg")))))))))));

    public static final Def encodeFunction = def("encodeFunction")
        .lam("env").lam("dom").lam("cod").lam("funTerm").lam("cx").lam("g")
        .to(() ->
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
                        hydra.dsl.Strip.deannotateTerm( var("funTerm")),
                        right(
                            apply(
                                ref(Coder.encodeLiteral),
                                inject(Literal.TYPE_,
                                    Literal.STRING,
                                    Strings.cat2(
                                        string("Unimplemented function variant: "),
                                        apply(tterm(Refs.showRef(Core.termTerm())), var("funTerm")))))),
                        field(
                            Term.PROJECT,
                            constant(
                                apply(
                                    ref(Coder.encodeElimination),
                                    var("env"),
                                    nothing(),
                                    var("dom"),
                                    var("cod"),
                                    hydra.dsl.Strip.deannotateTerm( var("funTerm")),
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
                                    hydra.dsl.Strip.deannotateTerm( var("funTerm")),
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
                                    hydra.dsl.Strip.deannotateTerm( var("funTerm")),
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
                                                hydra.dsl.Strip.deannotateTerm(
                                                    var("body")),
                                                apply(
                                                    var("encodeLambdaFallback"),
                                                    var("env2"),
                                                    var("lam")),
                                                field(
                                                    Term.LAMBDA,
                                                    lambda("innerLam",
                                                        casesWithDefault(Type.TYPE_,
                                                            hydra.dsl.Strip.deannotateType(
                                                                var("cod")),
                                                            left(
                                                                inject(Error_.TYPE_,
                                                                    Error_.OTHER,
                                                                    wrap(
                                                                        OtherError.TYPE_,
                                                                        Strings.cat2(
                                                                            string("expected function type for lambda body, but got: "),
                                                                            apply(
                                                                                tterm(Refs.showRef(Core.typeType())),
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
                                                                                        var("g"))))))))))))))))))));

    public static final Def encodeFunctionFormTerm = def("encodeFunctionFormTerm")
        .lam("env").lam("anns").lam("term").lam("cx").lam("g")
        .to(() ->
                let("combinedAnns",
                    Lists.foldl(
                        lambda("acc", lambda("m", Maps.union(var("acc"), var("m")))),
                        hydra.dsl.lib.Maps.empty(),
                        var("anns")),
                    Eithers.bind(
                        Eithers.bimap(
                            lambda("__de",
                                inject(Error_.TYPE_,
                                    Error_.OTHER,
                                    wrap(OtherError.TYPE_,
                                        apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                            lambda("__a", var("__a")),
                            hydra.dsl.Annotations.getType( var("g"), var("combinedAnns"))),
                        lambda("mt",
                            Eithers.bind(
                                Optionals.cases(
                                    var("mt"),
                                    Optionals.cases(
                                        apply(ref(Coder.tryInferFunctionType), var("term")),
                                        hydra.dsl.Checking.typeOfTerm(
                                            var("cx"),
                                            var("g"),
                                            var("term")),
                                        lambda("inferredType", right(var("inferredType")))),
                                    lambda("t", right(var("t")))),
                                lambda("typ",
                                    casesWithDefault(Type.TYPE_,
                                        hydra.dsl.Strip.deannotateType( var("typ")),
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
                                                    var("g")))))))))));

    public static final Def encodeFunctionPrimitiveByName = def("encodeFunctionPrimitiveByName")
        .lam("env").lam("dom").lam("cod").lam("name").lam("cx").lam("g")
        .to(() ->
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
                        hydra.dsl.Arity.typeArity(
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
                                    Math_.range(int32(0), Math_.sub(var("arity"), int32(1))))),
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
                                    hydra.dsl.lib.Sets.empty(),
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
                                                            var("curried")))))))))))));

    public static final Def encodeLiteral = def("encodeLiteral")
        .lam("lit")
        .to(() ->
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
                                                // binaryToBytes yields unsigned [0, 255]; Java's signed byte
                                                // needs values in [-128, 127], so fold the upper half down by 256.
                                                lambda("w",
                                                    apply(
                                                        ref(Utils.javaLiteralToJavaExpression),
                                                        inject(hydra.java.syntax.Literal.TYPE_,
                                                            hydra.java.syntax.Literal.INTEGER,
                                                            wrap(
                                                                IntegerLiteral.TYPE_,
                                                                Literals.int32ToBigint(
                                                                    Logic.ifElse(
                                                                        Equality.gt(var("w"), int32(127)),
                                                                        Math_.sub(var("w"), int32(256)),
                                                                        var("w"))))))),
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
                                apply(ref(Utils.javaString), var("s")))))));

    public static final Def encodeLiteralType = def("encodeLiteralType")
        .lam("lt").lam("cx").lam("g")
        .to(() ->
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
                                var("g"))))));

    public static final Def encodeLiteralType_simple = def("encodeLiteralType_simple")
        .lam("n").lam("cx").lam("g")
        .to(() ->
                right(apply(ref(Utils.javaRefType), list(), nothing(), var("n"))));

    public static final Def encodeLiteral_encodeFloat = def("encodeLiteral_encodeFloat")
        .lam("f")
        .to(() ->
                cases(FloatValue.TYPE_,
                    var("f"),
                    field(
                        FloatValue.FLOAT32,
                        lambda("v", apply(ref(Coder.encodeLiteral_encodeFloat32), var("v")))),
                    field(
                        FloatValue.FLOAT64,
                        lambda("v", apply(ref(Coder.encodeLiteral_encodeFloat64), var("v"))))));

    public static final Def encodeLiteral_encodeFloat32 = def("encodeLiteral_encodeFloat32")
        .lam("v")
        .to(() ->
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
                                                Literals.float32ToFloat64(var("v")))))))))));

    public static final Def encodeLiteral_encodeFloat64 = def("encodeLiteral_encodeFloat64")
        .lam("v")
        .to(() ->
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
                                                var("v"))))))))));

    public static final Def encodeLiteral_encodeInteger = def("encodeLiteral_encodeInteger")
        .lam("i")
        .to(() ->
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
                                nothing())))));

    public static final Def encodeLiteral_javaParseDouble = def("encodeLiteral_javaParseDouble")
        .lam("value")
        .to(() ->
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
                                    var("value")))))));

    public static final Def encodeLiteral_javaSpecialFloatExpr = def("encodeLiteral_javaSpecialFloatExpr")
        .lam("className").lam("fieldName")
        .to(() ->
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
                            wrap(Identifier.TYPE_, var("fieldName"))))));

    public static final Def encodeLiteral_litExp = def("encodeLiteral_litExp")
        .lam("l")
        .to(() ->
                apply(ref(Utils.javaLiteralToJavaExpression), var("l")));

    public static final Def encodeLiteral_primCast = def("encodeLiteral_primCast")
        .lam("pt").lam("expr")
        .to(() ->
                apply(
                    ref(Utils.javaCastExpressionToJavaExpression),
                    apply(
                        ref(Utils.javaCastPrimitive),
                        var("pt"),
                        apply(
                            ref(Utils.javaExpressionToJavaUnaryExpression),
                            var("expr")))));

    public static final Def encodeNullaryConstant = def("encodeNullaryConstant")
        .lam("env").lam("typ").lam("funTerm").lam("cx").lam("g")
        .to(() ->
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
                                        apply(tterm(Refs.showRef(Core.termTerm())), var("funTerm")))))))));

    public static final Def encodeNullaryConstant_typeArgsFromReturnType = def("encodeNullaryConstant_typeArgsFromReturnType")
        .lam("aliases").lam("t").lam("cx").lam("g")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    right(list()),
                    field(
                        Type.SET,
                        lambda("st",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    hydra.dsl.lib.Sets.empty(),
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
                                    hydra.dsl.lib.Sets.empty(),
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
                        Type.OPTIONAL,
                        lambda("mt",
                            Eithers.bind(
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    hydra.dsl.lib.Sets.empty(),
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
                                    hydra.dsl.lib.Sets.empty(),
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
                                                    hydra.dsl.lib.Sets.empty(),
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
                                                                        var("rv"))))))))))))))));

    public static final Def encodeNullaryPrimitiveByName = def("encodeNullaryPrimitiveByName")
        .lam("env").lam("typ").lam("name").lam("cx").lam("g")
        .to(() ->
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
                                                Optionals.fromOptional(
                                                    list(),
                                                    Lists.maybeInit(var("parts")))))),
                                    field("methodName",
                                        wrap(Identifier.TYPE_,
                                            Optionals.fromOptional(
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
                                                list())))))))));

    public static final Def encodeTerm = def("encodeTerm")
        .lam("env").lam("term").lam("cx").lam("g")
        .to(() ->
                apply(
                    ref(Coder.encodeTermInternal),
                    var("env"),
                    list(),
                    list(),
                    var("term"),
                    var("cx"),
                    var("g")));

    public static final Def encodeTermDefinition = def("encodeTermDefinition")
        .lam("env").lam("tdef").lam("cx").lam("g")
        .to(() ->
                let(
                    field("name",
                        proj(TermDefinition.TYPE_, TermDefinition.NAME, "tdef")),
                    field("term0",
                        proj(TermDefinition.TYPE_, TermDefinition.BODY, "tdef")),
                    Eithers.bind(
                        hydra.dsl.Annotations.getTermDescription(
                            var("cx"),
                            var("g"),
                            var("term0")),
                        lambda("mDoc",
                            let(
                                field("ts",
                                    Optionals.cases(proj(TermDefinition.TYPE_, TermDefinition.SIGNATURE, "tdef"), record(TypeScheme.TYPE_,
                                            field(TypeScheme.VARIABLES, list()),
                                            field(
                                                TypeScheme.BODY,
                                                inject(Type.TYPE_,
                                                    Type.VARIABLE,
                                                    wrap(Name.TYPE_,
                                                        string("hydra.core.Unit")))),
                                            field(TypeScheme.CONSTRAINTS, nothing())), lambda("sig",
                                            hydra.dsl.Scoping.termSignatureToTypeScheme(
                                                var("sig"))))),
                                field("term",
                                    hydra.dsl.Variables.unshadowVariables( var("term0"))),
                                Eithers.bind(
                                    apply(
                                        ref(Coder.analyzeJavaFunction),
                                        var("env"),
                                        var("term"),
                                        var("cx"),
                                        var("g")),
                                    lambda("fs",
                                        let(
                                            binds(    field("schemeVars",
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
                                                    right(hydra.dsl.lib.Maps.empty()),
                                                    apply(
                                                        ref(Coder.buildSubstFromAnnotations),
                                                        var("schemeVarSet"),
                                                        var("term"),
                                                        var("cx"),
                                                        var("g"))),
                                                lambda("typeVarSubst",
                                                    let(
                                                        binds(    field("overgenSubst",
                                                            apply(
                                                                ref(Coder.detectAccumulatorUnification),
                                                                var("schemeDoms"),
                                                                var("cod"),
                                                                var("tparams"))),
    field("overgenVarSubst",
                                                            Maps.fromList(
                                                                Optionals.cat(
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
                                                                        Logic.not(
                                                                            Maps.member(
                                                                                var("v"),
                                                                                var("overgenSubst")))),
                                                                    var("tparams")))),
    field("constraints",
                                                            Optionals.fromOptional(
                                                                hydra.dsl.lib.Maps.empty(),
                                                                proj(TypeScheme.TYPE_, TypeScheme.CONSTRAINTS, "ts"))),
    field("jparams",
                                                            Lists.map(
                                                                lambda("v",
                                                                    apply(
                                                                        ref(Utils.javaTypeParameter),
                                                                        hydra.dsl.Formatting.capitalize(
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
                                                                                                    hydra.dsl.lib.Sets.empty(),
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
                                                                                                hydra.dsl.lib.Sets.empty(),
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
                                                                                                            hydra.dsl.Formatting.decapitalize(
                                                                                                                hydra.dsl.Names.localNameOf(
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
                                                                                                                    Optionals.cases(var("mDoc"), apply(
                                                                                                                            ref(Coder.noInterfaceComment),
                                                                                                                            var("imdMember")), lambda(
                                                                                                                            "doc",
                                                                                                                            apply(
                                                                                                                                ref(Coder.withInterfaceCommentString),
                                                                                                                                var("doc"),
                                                                                                                                var("imdMember"))))))))))))))))))))))))))))));

    public static final Def encodeTermInternal = def("encodeTermInternal")
        .lam("env").lam("anns").lam("tyapps").lam("term").lam("cx").lam("g0")
        .to(() ->
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
                                        hydra.dsl.Annotations.getAnnotationMap(
                                            proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at")),
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
                                                hydra.dsl.lib.Maps.empty(),
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
                                                    hydra.dsl.Annotations.getType(
                                                        var("g"),
                                                        var("combinedAnns"))),
                                                lambda("mEitherType",
                                                    let(
                                                        field("branchTypes",
                                                            Optionals.bind(
                                                                var("mEitherType"),
                                                                lambda("etyp",
                                                                    casesWithDefault(
                                                                        Type.TYPE_,
                                                                        hydra.dsl.Strip.deannotateType(
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
                                                                    hydra.dsl.Annotations.setTermAnnotation(
                                                                        hydra.dsl.Constants.keyType(),
                                                                        just(
                                                                            apply(
                                                                                tterm(Refs.encodeRef(Core.typeType())),
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
                                                                Optionals.cases(
                                                                    var("mtargs"),
                                                                    apply(
                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                        apply(
                                                                            ref(Utils.methodInvocationStatic),
                                                                            wrap(
                                                                                Identifier.TYPE_,
                                                                                string("hydra.overlay.java.util.Either")),
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
                                                                                    string("hydra.overlay.java.util.Either")),
                                                                                wrap(
                                                                                    Identifier.TYPE_,
                                                                                    var("methodName")),
                                                                                var("targs"),
                                                                                list(var("expr")))))))),
                                                        Eithers.either(
                                                            lambda("term1",
                                                                Eithers.bind(
                                                                    Optionals.cases(
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
                                                                    Optionals.cases(
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
                                                                        hydra.dsl.lib.Maps.empty(),
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
                                                                        hydra.dsl.Annotations.getType(
                                                                            var("g"),
                                                                            var("combinedAnns"))),
                                                                    lambda("mt",
                                                                        Eithers.bind(
                                                                            Optionals.cases(
                                                                                var("mt"),
                                                                                hydra.dsl.Checking.typeOfTerm(
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
                                                                                        hydra.dsl.lib.Sets.empty(),
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
                                                        string("hydra.overlay.java.util.ConsList")),
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
                                                                string("hydra.overlay.java.util.ConsList")),
                                                            wrap(Identifier.TYPE_,
                                                                string("empty")),
                                                            var("targs"),
                                                            list())))))),
                                    Eithers.bind(
                                        Eithers.mapList(var("encode"), var("els")),
                                        lambda("jels",
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.collectionTypeArgs),
                                                    string("list"),
                                                    int32(1),
                                                    var("aliases"),
                                                    var("anns"),
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
                                                                    string("hydra.overlay.java.util.ConsList")),
                                                                wrap(Identifier.TYPE_,
                                                                    string("of")),
                                                                var("targs"),
                                                                var("jels"))))))))))),
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
                                                        string("hydra.overlay.java.util.PersistentMap")),
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
                                                                string("hydra.overlay.java.util.PersistentMap")),
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
                                                        Eithers.bind(
                                                            apply(
                                                                ref(Coder.collectionTypeArgs),
                                                                string("map"),
                                                                int32(2),
                                                                var("aliases"),
                                                                var("anns"),
                                                                var("tyapps"),
                                                                var("cx"),
                                                                var("g")),
                                                            lambda("targs",
                                                                right(
                                                                    apply(
                                                                        ref(Utils.javaMethodInvocationToJavaExpression),
                                                                        apply(
                                                                            ref(Utils.methodInvocationStaticWithTypeArgs),
                                                                            wrap(
                                                                                Identifier.TYPE_,
                                                                                string("hydra.overlay.java.util.PersistentMap")),
                                                                            wrap(
                                                                                Identifier.TYPE_,
                                                                                string("ofEntries")),
                                                                            var("targs"),
                                                                            var("pairExprs")))))))))))))),
                        field(
                            Term.OPTIONAL,
                            lambda("mt",
                                Optionals.cases(
                                    var("mt"),
                                    Logic.ifElse(
                                        Lists.null_(var("tyapps")),
                                        right(
                                            apply(
                                                ref(Utils.javaMethodInvocationToJavaExpression),
                                                apply(
                                                    ref(Utils.methodInvocationStatic),
                                                    wrap(Identifier.TYPE_,
                                                        string("hydra.overlay.java.util.Optional")),
                                                    wrap(Identifier.TYPE_,
                                                        string("none")),
                                                    list()))),
                                        Eithers.bind(
                                            apply(
                                                ref(Coder.takeTypeArgs),
                                                string("optional"),
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
                                                                string("hydra.overlay.java.util.Optional")),
                                                            wrap(Identifier.TYPE_,
                                                                string("none")),
                                                            var("targs"),
                                                            list())))))),
                                    lambda("term1",
                                        Eithers.bind(
                                            apply(var("encode"), var("term1")),
                                            lambda("expr",
                                                Eithers.bind(
                                                    apply(
                                                        ref(Coder.collectionTypeArgs),
                                                        string("optional"),
                                                        int32(1),
                                                        var("aliases"),
                                                        var("anns"),
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
                                                                        string("hydra.overlay.java.util.Optional")),
                                                                    wrap(Identifier.TYPE_,
                                                                        string("given")),
                                                                    var("targs"),
                                                                    list(var("expr"))))))))))))),
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
                                                                        string("hydra.overlay.java.util.Pair")),
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
                                        Eithers.either(
                                            constant(nothing()),
                                            lambda("t", just(var("t"))),
                                            hydra.dsl.Resolution.requireType(
                                                var("cx"),
                                                var("g"),
                                                var("recName")))),
                                    field("strippedRecTyp",
                                        Optionals.map(
                                            lambda("recTyp",
                                                apply(
                                                    ref(Coder.stripForalls),
                                                    hydra.dsl.Strip.deannotateType(
                                                        var("recTyp")))),
                                            var("mRecordType"))),
                                    field("mFieldTypeMap",
                                        Optionals.bind(
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
                                            hydra.dsl.lib.Maps.empty(),
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
                                            hydra.dsl.Annotations.getType(
                                                var("g"),
                                                var("combinedAnnsRec"))),
                                        lambda("mAnnotType",
                                            let(
                                                field("mTypeSubst",
                                                    Optionals.bind(
                                                        var("mAnnotType"),
                                                        lambda("annTyp",
                                                            Optionals.bind(
                                                                var("mRecordType"),
                                                                lambda("recTyp",
                                                                    let(
                                                                        field("args",
                                                                            apply(
                                                                                ref(Coder.extractTypeApplicationArgs),
                                                                                hydra.dsl.Strip.deannotateType(
                                                                                    var("annTyp")))),
                                                                        field("params",
                                                                            apply(
                                                                                ref(Coder.collectForallParams),
                                                                                hydra.dsl.Strip.deannotateType(
                                                                                    var("recTyp")))),
                                                                        Logic.ifElse(
                                                                            Logic.or(
                                                                                Lists.null_(
                                                                                    var("args")),
                                                                                Logic.not(
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
                                                        Optionals.cases(
                                                            var("mFieldTypeMap"),
                                                            apply(
                                                                var("encode"),
                                                                proj(Field.TYPE_, Field.TERM, "fld")),
                                                            lambda("ftmap",
                                                                let("mftyp",
                                                                    Maps.lookup(
                                                                        proj(Field.TYPE_, Field.NAME, "fld"),
                                                                        var("ftmap")),
                                                                    Optionals.cases(
                                                                        var("mftyp"),
                                                                        apply(
                                                                            var("encode"),
                                                                            proj(Field.TYPE_, Field.TERM, "fld")),
                                                                        lambda("ftyp",
                                                                            let(
                                                                                field(
                                                                                    "resolvedType",
                                                                                    Optionals.cases(
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
                                                                                    hydra.dsl.Annotations.setTermAnnotation(
                                                                                        hydra.dsl.Constants.keyType(),
                                                                                        just(
                                                                                            apply(
                                                                                                tterm(Refs.encodeRef(Core.typeType())),
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
                                                                    Logic.not(
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
                                                                            hydra.dsl.lib.Maps.empty(),
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
                                                                                hydra.dsl.Annotations.getType(
                                                                                    var("g"),
                                                                                    var("combinedAnns"))),
                                                                            lambda("mtyp",
                                                                                Optionals.cases(
                                                                                    var("mtyp"),
                                                                                    right(nothing()),
                                                                                    lambda("annTyp",
                                                                                        let(
                                                                                            "typeArgs",
                                                                                            apply(
                                                                                                ref(Coder.extractTypeApplicationArgs),
                                                                                                hydra.dsl.Strip.deannotateType(
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
                                                                                                                    hydra.dsl.lib.Sets.empty(),
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
                                                        string("hydra.overlay.java.util.PersistentSet")),
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
                                                                string("hydra.overlay.java.util.PersistentSet")),
                                                            wrap(Identifier.TYPE_,
                                                                string("empty")),
                                                            var("targs"),
                                                            list())))))),
                                    let("slist",
                                        Sets.toList(var("s")),
                                        Eithers.bind(
                                            Eithers.mapList(var("encode"), var("slist")),
                                            lambda("jels",
                                                Eithers.bind(
                                                    apply(
                                                        ref(Coder.collectionTypeArgs),
                                                        string("set"),
                                                        int32(1),
                                                        var("aliases"),
                                                        var("anns"),
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
                                                                        string("hydra.overlay.java.util.PersistentSet")),
                                                                    wrap(Identifier.TYPE_,
                                                                        string("of")),
                                                                    var("targs"),
                                                                    var("jels")))))))))))),
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
                                                hydra.dsl.lib.Maps.empty(),
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
                                                    hydra.dsl.Annotations.getType(
                                                        var("g"),
                                                        var("combinedAnns"))),
                                                lambda("mtyp",
                                                    let("annotatedBody",
                                                        Optionals.cases(
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
                                                                            hydra.dsl.Annotations.setTermAnnotation(
                                                                                hydra.dsl.Constants.keyType(),
                                                                                just(
                                                                                    apply(
                                                                                        tterm(Refs.encodeRef(Core.typeType())),
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
                                                        hydra.dsl.Formatting.capitalize(
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
                                                    Logic.or(
                                                        hydra.dsl.Predicates.isUnitTerm(
                                                            hydra.dsl.Strip.deannotateTerm(
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
                                Optionals.cases(
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
                                                hydra.dsl.lib.Maps.empty(),
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
                                                    hydra.dsl.Annotations.getType(
                                                        var("g"),
                                                        var("combinedAnns"))),
                                                lambda("mt",
                                                    Eithers.bind(
                                                        Optionals.cases(
                                                            var("mt"),
                                                            hydra.dsl.Checking.typeOfTerm(
                                                                var("cx"),
                                                                var("g"),
                                                                var("term")),
                                                            lambda("t", right(var("t")))),
                                                        lambda("typ",
                                                            casesWithDefault(Type.TYPE_,
                                                                hydra.dsl.Strip.deannotateType(
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
                                            hydra.dsl.lib.Sets.empty(),
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
                                                    hydra.dsl.lib.Maps.empty(),
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
                                                        hydra.dsl.Annotations.getType(
                                                            var("g"),
                                                            var("combinedAnns"))),
                                                    lambda("mtyp",
                                                        Eithers.bind(
                                                            Optionals.cases(
                                                                var("mtyp"),
                                                                hydra.dsl.Checking.typeOfTerm(
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
                                                                                                        Optionals.fromOptional(
                                                                                                            var("correctedTyp"),
                                                                                                            Lists.maybeAt(
                                                                                                                int32(0),
                                                                                                                var("allTypeArgs"))),
                                                                                                        Optionals.fromOptional(
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
                                                                                                                        hydra.dsl.lib.Sets.empty(),
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
                                                                                                                            hydra.dsl.Annotations.setTermAnnotation(
                                                                                                                                hydra.dsl.Constants.keyType(),
                                                                                                                                just(
                                                                                                                                    apply(
                                                                                                                                        tterm(Refs.encodeRef(Core.typeType())),
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
                                                                                                                Eithers.either(
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
                                                                                                                                                string("hydra.overlay.java.util.Either")),
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
                                                                                                                                                string("hydra.overlay.java.util.Either")),
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
                                                                                                    var("g")))))))))))))))))))))));

    public static final Def encodeTermTCO = def("encodeTermTCO")
        .to(() ->
                lambda(
                params("env0", "funcName", "paramNames", "tcoVarRenames", "tcoDepth", "term", "cx", "g"),
                let(
                    binds(    field("aliases0",
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env0")),
    field("env",
                        record(JavaEnvironment.TYPE_,
                            field(
                                JavaEnvironment.ALIASES,
                                recordWith(Aliases.TYPE_, "aliases0", Utils.ALIASES_FIELDS,
                                    field(
                                        Aliases.VAR_RENAMES,
                                        Maps.union(
                                            var("tcoVarRenames"),
                                            proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases0"))))),
                            field(
                                JavaEnvironment.GRAPH,
                                proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env0")))),
    field("stripped",
                        hydra.dsl.Strip.deannotateAndDetypeTerm( var("term"))),
    field("gathered",
                        hydra.dsl.Analysis.gatherApplications( var("stripped"))),
    field("gatherArgs",
                        Pairs.first(var("gathered"))),
    field("gatherFun",
                        Pairs.second(var("gathered"))),
    field("strippedFun",
                        hydra.dsl.Strip.deannotateAndDetypeTerm( var("gatherFun"))),
    field("isSelfCall",
                        casesWithDefault(Term.TYPE_,
                            var("strippedFun"),
                            bool(false),
                            field(
                                Term.VARIABLE,
                                lambda("n", Equality.equal(var("n"), var("funcName"))))))),
                    Logic.ifElse(
                        Logic.and(
                            var("isSelfCall"),
                            Equality.equal(
                                Lists.length(var("gatherArgs")),
                                Lists.length(var("paramNames")))),
                        let(
                            field("changePairs",
                                Lists.filter(
                                    lambda("pair",
                                        Logic.not(
                                            casesWithDefault(Term.TYPE_,
                                                hydra.dsl.Strip.deannotateAndDetypeTerm(
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
                                    hydra.dsl.Analysis.gatherApplications( var("term"))),
                                field("args2",
                                    Pairs.first(var("gathered2"))),
                                field("body2",
                                    Pairs.second(var("gathered2"))),
                                Logic.ifElse(
                                    Equality.equal(Lists.length(var("args2")), int32(1)),
                                    let("arg",
                                        Optionals.fromOptional(
                                            inject(Term.TYPE_,
                                                Term.UNIT,
                                                unit()),
                                            Lists.maybeHead(var("args2"))),
                                        casesWithDefault(Term.TYPE_,
                                            hydra.dsl.Strip.deannotateAndDetypeTerm(
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
                                                                hydra.dsl.Resolution.nominalApplication(
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
                                                                                            hydra.dsl.Formatting.decapitalize(
                                                                                                hydra.dsl.Names.localNameOf(
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
                                                                                                proj(CaseAlternative.TYPE_, CaseAlternative.NAME, "field")),
                                                                                            field(
                                                                                                "variantRefType",
                                                                                                apply(
                                                                                                    ref(Utils.nameToJavaReferenceType),
                                                                                                    var("aliases"),
                                                                                                    bool(true),
                                                                                                    var("domArgs"),
                                                                                                    var("tname"),
                                                                                                    just(
                                                                                                        hydra.dsl.Formatting.capitalize(
                                                                                                            apply(
                                                                                                                unwrap(Name.TYPE_),
                                                                                                                var("fieldName")))))),
                                                                                            casesWithDefault(
                                                                                                Term.TYPE_,
                                                                                                hydra.dsl.Strip.deannotateTerm(
                                                                                                    proj(CaseAlternative.TYPE_, CaseAlternative.HANDLER, "field")),
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
                                                                                                                        hydra.dsl.Analysis.isTailRecursiveInTailPosition(
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
                                                                                        Optionals.cases(
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

    public static final Def encodeType = def("encodeType")
        .lam("aliases").lam("boundVars").lam("t").lam("cx").lam("g")
        .to(() ->
                let(
                    field("inScopeTypeParams",
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_TYPE_PARAMS, "aliases")),
                    field("typeVarSubst",
                        proj(Aliases.TYPE_, Aliases.TYPE_VAR_SUBST, "aliases")),
                    casesWithDefault(Type.TYPE_,
                        hydra.dsl.Strip.deannotateType( var("t")),
                        left(
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    Strings.cat2(
                                        string("can't encode unsupported type in Java: "),
                                        apply(tterm(Refs.showRef(Core.typeType())), var("t")))))),
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
                            Type.EFFECT,
                            // effect<t> is transparent in Java: it encodes to the same type as its
                            // inner type t. Java has no distinguished effect wrapper (#494).
                            lambda("et",
                                apply(
                                    ref(Coder.encodeType),
                                    var("aliases"),
                                    var("boundVars"),
                                    var("et"),
                                    var("cx"),
                                    var("g")))),
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
                            Type.OPTIONAL,
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
                                                string("Optional"))))))),
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
                                    Optionals.fromOptional(
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
                                            Optionals.cases(
                                                var("resolved"),
                                                right(
                                                    Logic.ifElse(
                                                        Logic.or(
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
                                            string("unexpected anonymous wrap type")))))))));

    public static final Def encodeTypeDefinition = def("encodeTypeDefinition")
        .lam("pkg").lam("aliases").lam("tdef").lam("cx").lam("g")
        .to(() ->
                let(
                    field("name",
                        proj(TypeDefinition.TYPE_, TypeDefinition.NAME, "tdef")),
                    field("typ",
                        apply(
                            project(TypeScheme.TYPE_, TypeScheme.BODY),
                            proj(TypeDefinition.TYPE_, TypeDefinition.BODY, "tdef"))),
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
                                hydra.dsl.Annotations.getTypeDescription(
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
                                                            list(var("tdecl"))))))))))))));

    public static final Def encodeType_resolveIfTypedef = def("encodeType_resolveIfTypedef")
        .lam("aliases").lam("boundVars").lam("inScopeTypeParams").lam("name").lam("cx").lam("g")
        .to(() ->
                Logic.ifElse(
                    Logic.or(
                        Sets.member(var("name"), var("boundVars")),
                        Sets.member(var("name"), var("inScopeTypeParams"))),
                    right(nothing()),
                    Logic.ifElse(
                        apply(ref(Coder.isLambdaBoundVariable), var("name")),
                        right(nothing()),
                        let("schemaTypes",
                            proj(Graph.TYPE_, Graph.SCHEMA_TYPES, "g"),
                            Optionals.cases(
                                Maps.lookup(var("name"), var("schemaTypes")),
                                right(nothing()),
                                lambda("ts",
                                    Logic.ifElse(
                                        Logic.not(
                                            Lists.null_(
                                                proj(TypeScheme.TYPE_, TypeScheme.VARIABLES, "ts"))),
                                        right(nothing()),
                                        casesWithDefault(Type.TYPE_,
                                            hydra.dsl.Strip.deannotateType(
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                                            right(
                                                just(
                                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"))),
                                            field(
                                                Type.RECORD,
                                                constant(right(nothing()))),
                                            field(Type.UNION, constant(right(nothing()))),
                                            field(Type.WRAP, constant(right(nothing())))))))))));

    public static final Def encodeVariable = def("encodeVariable")
        .lam("env").lam("name").lam("cx").lam("g")
        .to(() ->
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
                            Logic.and(
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
                                Logic.and(
                                    apply(
                                        ref(Coder.isRecursiveVariable),
                                        var("aliases"),
                                        var("name")),
                                    Logic.not(
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
                                    Logic.and(
                                        Sets.member(
                                            var("name"),
                                            proj(Aliases.TYPE_, Aliases.THUNKED_VARS, "aliases")),
                                        Logic.not(
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
                                                                            var("name")))))))))))))))));

    public static final Def encodeVariable_buildCurried = def("encodeVariable_buildCurried")
        .lam("params").lam("inner")
        .to(() ->
                Optionals.fromOptional(
                    var("inner"),
                    Optionals.map(
                        lambda("p",
                            apply(
                                ref(Utils.javaLambda),
                                Pairs.first(var("p")),
                                apply(
                                    ref(Coder.encodeVariable_buildCurried),
                                    Pairs.second(var("p")),
                                    var("inner")))),
                        Lists.uncons(var("params")))));

    public static final Def encodeVariable_hoistedLambdaCase = def("encodeVariable_hoistedLambdaCase")
        .lam("aliases").lam("name").lam("arity").lam("cx").lam("g")
        .to(() ->
                let(
                    field("paramNames",
                        Lists.map(
                            lambda("i",
                                wrap(Name.TYPE_,
                                    Strings.cat2(string("p"), Literals.showInt32(var("i"))))),
                            Math_.range(int32(0), Math_.sub(var("arity"), int32(1))))),
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
                        right(hydra.dsl.Lexical.lookupBinding( var("g"), var("name"))),
                        lambda("mel",
                            Optionals.cases(
                                var("mel"),
                                right(var("lam")),
                                lambda("el",
                                    Optionals.cases(
                                        proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                        right(var("lam")),
                                        lambda("ts",
                                            let("typ",
                                                proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts"),
                                                Eithers.bind(
                                                    apply(
                                                        ref(Coder.encodeType),
                                                        var("aliases"),
                                                        hydra.dsl.lib.Sets.empty(),
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
                                                                                var("lam"))))))))))))))))));

    public static final Def eqClause = def("eqClause")
        .lam("tmpName").lam("ft")
        .to(() ->
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
                            apply(ref(Coder.equalsClause), var("tmpName"), var("fname"))))));

    public static final Def equalsClause = def("equalsClause")
        .lam("tmpName").lam("fname")
        .to(() ->
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
                                    list(var("thisArg"), var("otherArg"))))))));

    public static final Def extractArgType = def("extractArgType")
        .lam("_lhs").lam("typ")
        .to(() ->
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
                                        proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at1"))))))));

    public static final Def extractDirectReturn = def("extractDirectReturn")
        .lam("tparamSet").lam("t")
        .to(() ->
                apply(ref(Coder.extractDirectReturn_go), var("tparamSet"), var("t")));

    public static final Def extractDirectReturn_go = def("extractDirectReturn_go")
        .lam("tparamSet").lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    list(),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            let(
                                field("dom",
                                    hydra.dsl.Strip.deannotateType(
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
                                                    hydra.dsl.Strip.deannotateType(
                                                        var("cod")),
                                                    list(),
                                                    field(
                                                        Type.FUNCTION,
                                                        lambda("ft2",
                                                            let(
                                                                field("midArg",
                                                                    hydra.dsl.Strip.deannotateType(
                                                                        proj(FunctionType.TYPE_, FunctionType.DOMAIN, "ft2"))),
                                                                field("retPart",
                                                                    hydra.dsl.Strip.deannotateType(
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
                                                    var("cod")))))))))));

    public static final Def extractInOutPair = def("extractInOutPair")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    list(),
                    field(
                        Type.FUNCTION,
                        lambda("ft",
                            casesWithDefault(Type.TYPE_,
                                hydra.dsl.Strip.deannotateType(
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
                                                hydra.dsl.Strip.deannotateType(
                                                    var("retType")),
                                                list(),
                                                field(
                                                    Type.PAIR,
                                                    lambda("pt",
                                                        casesWithDefault(Type.TYPE_,
                                                            hydra.dsl.Strip.deannotateType(
                                                                proj(PairType.TYPE_, PairType.FIRST, "pt")),
                                                            list(),
                                                            field(
                                                                Type.VARIABLE,
                                                                lambda("outVar",
                                                                    list(
                                                                        pair(
                                                                            var("inVar"),
                                                                            var("outVar")))))))))))))))));

    public static final Def extractTypeApplicationArgs = def("extractTypeApplicationArgs")
        .lam("typ")
        .to(() ->
                Lists.reverse(apply(ref(Coder.extractTypeApplicationArgs_go), var("typ"))));

    public static final Def extractTypeApplicationArgs_go = def("extractTypeApplicationArgs_go")
        .lam("t")
        .to(() ->
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
                                    proj(ApplicationType.TYPE_, ApplicationType.FUNCTION, "at")))))));

    public static final Def fieldTypeToFormalParam = def("fieldTypeToFormalParam")
        .lam("aliases").lam("ft").lam("cx").lam("g")
        .to(() ->
                Eithers.bind(
                    apply(
                        ref(Coder.encodeType),
                        var("aliases"),
                        hydra.dsl.lib.Sets.empty(),
                        proj(FieldType.TYPE_, FieldType.TYPE, "ft"),
                        var("cx"),
                        var("g")),
                    lambda("jt",
                        right(
                            apply(
                                ref(Utils.javaTypeToJavaFormalParameter),
                                var("jt"),
                                proj(FieldType.TYPE_, FieldType.NAME, "ft"))))));

    public static final Def filterByFlags = def("filterByFlags")
        .lam("xs").lam("flags")
        .to(() ->
                Lists.map(
                    lambda("p", Pairs.first(var("p"))),
                    Lists.filter(
                        lambda("p", Pairs.second(var("p"))),
                        Lists.zip(var("xs"), var("flags")))));

    public static final Def filterPhantomTypeArgs = def("filterPhantomTypeArgs")
        .lam("calleeName").lam("allTypeArgs").lam("cx").lam("g")
        .to(() ->
                Eithers.bind(
                    right(hydra.dsl.Lexical.lookupBinding( var("g"), var("calleeName"))),
                    lambda("mel",
                        Optionals.cases(
                            var("mel"),
                            right(var("allTypeArgs")),
                            lambda("el",
                                Optionals.cases(
                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "el"),
                                    right(var("allTypeArgs")),
                                    lambda("ts",
                                        let(
                                            binds(    field("schemeVars",
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
                                                        Logic.and(
                                                            Sets.member(
                                                                var("v"),
                                                                var("schemeTypeVars")),
                                                            Logic.not(
                                                                Maps.member(
                                                                    var("v"),
                                                                    var("overgenSubst"))))),
                                                    var("schemeVars")))),
                                            Logic.ifElse(
                                                Logic.not(
                                                    Equality.equal(
                                                        Lists.length(var("schemeVars")),
                                                        Lists.length(var("allTypeArgs")))),
                                                right(var("allTypeArgs")),
                                                right(
                                                    apply(
                                                        ref(Coder.filterPhantomTypeArgs_filterAndApply),
                                                        var("allTypeArgs"),
                                                        var("keepFlags"),
                                                        var("overgenSubst"))))))))))));

    public static final Def filterPhantomTypeArgs_filterAndApply = def("filterPhantomTypeArgs_filterAndApply")
        .lam("allTypeArgs").lam("keepFlags").lam("overgenSubst")
        .to(() ->
                let("filtered",
                    Lists.map(
                        lambda("p", Pairs.first(var("p"))),
                        Lists.filter(
                            lambda("p", Pairs.second(var("p"))),
                            Lists.zip(var("allTypeArgs"), var("keepFlags")))),
                    Logic.ifElse(
                        Logic.not(Maps.null_(var("overgenSubst"))),
                        Lists.map(
                            lambda("t",
                                apply(
                                    ref(Coder.substituteTypeVarsWithTypes),
                                    var("overgenSubst"),
                                    var("t"))),
                            var("filtered")),
                        var("filtered"))));

    public static final Def findMatchingLambdaVar = def("findMatchingLambdaVar")
        .lam("name").lam("lambdaVars")
        .to(() ->
                Logic.ifElse(
                    Sets.member(var("name"), var("lambdaVars")),
                    var("name"),
                    Logic.ifElse(
                        apply(ref(Coder.isLambdaBoundIn_isQualified), var("name")),
                        Optionals.fromOptional(
                            var("name"),
                            Lists.find(
                                lambda("lv",
                                    Logic.and(
                                        apply(ref(Coder.isLambdaBoundIn_isQualified), var("lv")),
                                        Equality.equal(
                                            hydra.dsl.Names.localNameOf( var("lv")),
                                            hydra.dsl.Names.localNameOf( var("name"))))),
                                Sets.toList(var("lambdaVars")))),
                        Logic.ifElse(
                            Sets.member(
                                wrap(Name.TYPE_,
                                    hydra.dsl.Names.localNameOf( var("name"))),
                                var("lambdaVars")),
                            wrap(Name.TYPE_,
                                hydra.dsl.Names.localNameOf( var("name"))),
                            var("name")))));

    public static final Def findPairFirst = def("findPairFirst")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    nothing(),
                    field(
                        Type.PAIR,
                        lambda("pt",
                            casesWithDefault(Type.TYPE_,
                                hydra.dsl.Strip.deannotateType(
                                    proj(PairType.TYPE_, PairType.FIRST, "pt")),
                                nothing(),
                                field(Type.VARIABLE, lambda("v", just(var("v")))))))));

    public static final Def findSelfRefVar = def("findSelfRefVar")
        .lam("grouped")
        .to(() ->
                let("selfRefs",
                    Lists.filter(
                        lambda("entry",
                            Lists.elem(Pairs.first(var("entry")), Pairs.second(var("entry")))),
                        Maps.toList(var("grouped"))),
                    Optionals.map(
                        lambda("entry", Pairs.first(var("entry"))),
                        Lists.maybeHead(var("selfRefs")))));

    public static final Def first20Primes = def("first20Primes")
        .to(() ->
                list(
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

    public static final Def flattenApps = def("flattenApps")
        .lam("t").lam("acc")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("t")),
                    pair(var("acc"), var("t")),
                    field(
                        Term.APPLICATION,
                        lambda("app",
                            apply(
                                ref(Coder.flattenApps),
                                proj(Application.TYPE_, Application.FUNCTION, "app"),
                                Lists.cons(
                                    proj(Application.TYPE_, Application.ARGUMENT, "app"),
                                    var("acc")))))));

    public static final Def flattenBindings = def("flattenBindings")
        .lam("bindings")
        .to(() ->
                Lists.bind(
                    var("bindings"),
                    lambda("b",
                        casesWithDefault(Term.TYPE_,
                            hydra.dsl.Strip.deannotateTerm(
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
                                                    proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b")))))))))));

    public static final Def freshJavaName = def("freshJavaName")
        .lam("base").lam("avoid")
        .to(() ->
                apply(ref(Coder.freshJavaName_go), var("base"), var("avoid"), int32(2)));

    public static final Def freshJavaName_go = def("freshJavaName_go")
        .lam("base").lam("avoid").lam("i")
        .to(() ->
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
                        var("candidate"))));

    public static final Def functionCall = def("functionCall")
        .lam("env").lam("isPrim").lam("name").lam("args").lam("typeApps").lam("cx").lam("g")
        .to(() ->
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
                                    apply(ref(Coder.wrapLazyArguments), var("g"), var("name"), var("jargs0"))),
                                field("jargs",
                                    Pairs.first(var("wrapResult"))),
                                field("mMethodOverride",
                                    Pairs.second(var("wrapResult"))),
                                Logic.ifElse(
                                    Logic.or(
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
                                            Optionals.cases(
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
                                                    hydra.dsl.Names.qualifyName(
                                                        var("name"))),
                                                field("mns",
                                                    proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                                                field("localName",
                                                    proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                                                Optionals.cases(
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
                                                                                        hydra.dsl.Names.unqualifyName(
                                                                                            record(
                                                                                                QualifiedName.TYPE_,
                                                                                                field(
                                                                                                    QualifiedName.MODULE_NAME,
                                                                                                    just(
                                                                                                        var("ns_"))),
                                                                                                field(
                                                                                                    QualifiedName.LOCAL,
                                                                                                    hydra.dsl.Formatting.capitalize(
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
                                                                                hydra.dsl.lib.Sets.empty(),
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
                                                                                var("jargs"))))))))))))))))));

    public static final Def getCodomain = def("getCodomain")
        .lam("ann").lam("cx").lam("g")
        .to(() ->
                Eithers.map(
                    lambda("ft",
                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft")),
                    apply(ref(Coder.getFunctionType), var("ann"), var("cx"), var("g"))));

    public static final Def getFunctionType = def("getFunctionType")
        .lam("ann").lam("cx").lam("g")
        .to(() ->
                Eithers.bind(
                    Eithers.bimap(
                        lambda("__de",
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    apply(unwrap(DecodingError.TYPE_), var("__de"))))),
                        lambda("__a", var("__a")),
                        hydra.dsl.Annotations.getType( var("g"), var("ann"))),
                    lambda("mt",
                        Optionals.cases(
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
                                                    apply(tterm(Refs.showRef(Core.typeType())), var("t")))))),
                                    field(Type.FUNCTION, lambda("ft", right(var("ft"))))))))));

    public static final Def groupPairsByFirst = def("groupPairsByFirst")
        .lam("pairs")
        .to(() ->
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
                                    Optionals.cases(var("mv"), just(list(var("v"))), lambda("vs", just(Lists.concat2(var("vs"), list(var("v"))))))),
                                var("k"),
                                var("m")))),
                    hydra.dsl.lib.Maps.empty(),
                    var("pairs")));

    public static final Def hashCodeCompareExpr = def("hashCodeCompareExpr")
        .lam("otherVar").lam("fname")
        .to(() ->
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
                                list(var("thisHashCode"), var("otherHashCode")))))));

    public static final Def hashCodeMultPair = def("hashCodeMultPair")
        .lam("i").lam("fname")
        .to(() ->
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
                            field(MultiplicativeExpression_Binary.RHS, var("rhs"))))));

    public static final Def innerClassRef = def("innerClassRef")
        .lam("aliases").lam("name").lam("local")
        .to(() ->
                let("id",
                    apply(
                        unwrap(Identifier.TYPE_),
                        apply(
                            ref(Utils.nameToJavaName),
                            var("aliases"),
                            var("name"))),
                    wrap(Identifier.TYPE_,
                        Strings.cat2(Strings.cat2(var("id"), string(".")), var("local")))));

    public static final Def insertBranchVar = def("insertBranchVar")
        .lam("name").lam("env")
        .to(() ->
                let("aliases",
                    proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env"),
                    record(JavaEnvironment.TYPE_,
                        field(
                            JavaEnvironment.ALIASES,
                            recordWith(Aliases.TYPE_, "aliases", Utils.ALIASES_FIELDS,
                                field(
                                    Aliases.BRANCH_VARS,
                                    Sets.insert(
                                        var("name"),
                                        proj(Aliases.TYPE_, Aliases.BRANCH_VARS, "aliases"))),
                                field(Aliases.METHOD_CODOMAIN, nothing()))),
                        field(
                            JavaEnvironment.GRAPH,
                            proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env")))));

    public static final Def interfaceTypes = def("interfaceTypes")
        .lam("isSer").lam("aliases").lam("tparams").lam("elName")
        .to(() ->
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
                        list())));

    public static final Def isBigNumericType = def("isBigNumericType")
        .lam("typ")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("typ")),
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
                                                constant(bool(true)))))))))));

    public static final Def isBinaryType = def("isBinaryType")
        .lam("typ")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("typ")),
                    bool(false),
                    field(
                        Type.LITERAL,
                        lambda("lt",
                            casesWithDefault(LiteralType.TYPE_,
                                var("lt"),
                                bool(false),
                                field(LiteralType.BINARY, constant(bool(true))))))));

    public static final Def isFieldUnitType = def("isFieldUnitType")
        .lam("typeName").lam("fieldName").lam("cx").lam("g")
        .to(() ->
                let("schemaTypes",
                    proj(Graph.TYPE_, Graph.SCHEMA_TYPES, "g"),
                    Optionals.cases(
                        Maps.lookup(var("typeName"), var("schemaTypes")),
                        right(bool(false)),
                        lambda("ts",
                            casesWithDefault(Type.TYPE_,
                                hydra.dsl.Strip.deannotateType(
                                    proj(TypeScheme.TYPE_, TypeScheme.BODY, "ts")),
                                right(bool(false)),
                                field(
                                    Type.UNION,
                                    lambda("rt",
                                        right(
                                            Optionals.cases(
                                                Lists.find(
                                                    lambda("ft",
                                                        Equality.equal(
                                                            proj(FieldType.TYPE_, FieldType.NAME, "ft"),
                                                            var("fieldName"))),
                                                    var("rt")),
                                                bool(false),
                                                lambda("ft",
                                                    hydra.dsl.Predicates.isUnitType(
                                                        hydra.dsl.Strip.deannotateType(
                                                            proj(FieldType.TYPE_, FieldType.TYPE, "ft")))))))))))));

    public static final Def isLambdaBoundIn = def("isLambdaBoundIn")
        .lam("name").lam("lambdaVars")
        .to(() ->
                Logic.or(
                    Sets.member(var("name"), var("lambdaVars")),
                    Logic.or(
                        Logic.and(
                            apply(ref(Coder.isLambdaBoundIn_isQualified), var("name")),
                            Optionals.isGiven(
                                Lists.find(
                                    lambda("lv",
                                        Logic.and(
                                            apply(ref(Coder.isLambdaBoundIn_isQualified), var("lv")),
                                            Equality.equal(
                                                hydra.dsl.Names.localNameOf( var("lv")),
                                                hydra.dsl.Names.localNameOf( var("name"))))),
                                    Sets.toList(var("lambdaVars"))))),
                        Logic.and(
                            Logic.not(apply(ref(Coder.isLambdaBoundIn_isQualified), var("name"))),
                            Sets.member(
                                wrap(Name.TYPE_,
                                    hydra.dsl.Names.localNameOf( var("name"))),
                                var("lambdaVars"))))));

    public static final Def isLambdaBoundIn_isQualified = def("isLambdaBoundIn_isQualified")
        .lam("n")
        .to(() ->
                Optionals.isGiven(
                    apply(
                        project(QualifiedName.TYPE_, QualifiedName.MODULE_NAME),
                        hydra.dsl.Names.qualifyName( var("n")))));

    public static final Def isLambdaBoundVariable = def("isLambdaBoundVariable")
        .lam("name")
        .to(() ->
                let("v",
                    apply(unwrap(Name.TYPE_), var("name")),
                    Equality.lte(Strings.length(var("v")), int32(4))));

    public static final Def isLocalVariable = def("isLocalVariable")
        .lam("name")
        .to(() ->
                Optionals.isNone(
                    apply(
                        project(QualifiedName.TYPE_, QualifiedName.MODULE_NAME),
                        hydra.dsl.Names.qualifyName( var("name")))));

    public static final Def isNonComparableType = def("isNonComparableType")
        .lam("typ")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("typ")),
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
                                proj(ForallType.TYPE_, ForallType.BODY, "ft"))))));

    public static final Def isRecursiveVariable = def("isRecursiveVariable")
        .lam("aliases").lam("name")
        .to(() ->
                Sets.member(
                    var("name"),
                    proj(Aliases.TYPE_, Aliases.RECURSIVE_VARS, "aliases")));

    public static final Def isSerializableJavaType = def("isSerializableJavaType")
        .lam("typ")
        .to(() ->
                hydra.dsl.Predicates.isNominalType( var("typ")));

    public static final Def isSimpleName = def("isSimpleName")
        .lam("name")
        .to(() ->
                Equality.equal(
                    Lists.length(
                        Strings.splitOn(
                            string("."),
                            apply(unwrap(Name.TYPE_), var("name")))),
                    int32(1)));

    public static final Def isUnresolvedInferenceVar = def("isUnresolvedInferenceVar")
        .lam("name")
        .to(() ->
                let("chars",
                    Strings.toList(apply(unwrap(Name.TYPE_), var("name"))),
                    Optionals.fromOptional(
                        bool(false),
                        Optionals.map(
                            lambda("p",
                                let(
                                    field("firstCh",
                                        Pairs.first(var("p"))),
                                    field("rest",
                                        Pairs.second(var("p"))),
                                    Logic.ifElse(
                                        Logic.not(Equality.equal(var("firstCh"), int32(116))),
                                        bool(false),
                                        Logic.and(
                                            Logic.not(Lists.null_(var("rest"))),
                                            Lists.null_(
                                                Lists.filter(
                                                    lambda("c",
                                                        Logic.not(
                                                            apply(
                                                                ref(Coder.isUnresolvedInferenceVar_isDigit),
                                                                var("c")))),
                                                    var("rest"))))))),
                            Lists.uncons(var("chars"))))));

    public static final Def isUnresolvedInferenceVar_isDigit = def("isUnresolvedInferenceVar_isDigit")
        .lam("c")
        .to(() ->
                Logic.and(Equality.gte(var("c"), int32(48)), Equality.lte(var("c"), int32(57))));

    public static final Def java11Features = def("java11Features")
        .to(() ->
                record(JavaFeatures.TYPE_,
                field(JavaFeatures.SUPPORTS_DIAMOND_OPERATOR, bool(true))));

    public static final Def java8Features = def("java8Features")
        .to(() ->
                record(JavaFeatures.TYPE_,
                field(JavaFeatures.SUPPORTS_DIAMOND_OPERATOR, bool(false))));

    public static final Def javaComparableRefType = def("javaComparableRefType")
        .to(() ->
                inject(ReferenceType.TYPE_,
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

    public static final Def javaEnvGetGraph = def("javaEnvGetGraph")
        .lam("env")
        .to(() ->
                proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env"));

    public static final Def javaEnvSetGraph = def("javaEnvSetGraph")
        .lam("g").lam("env")
        .to(() ->
                record(JavaEnvironment.TYPE_,
                    field(
                        JavaEnvironment.ALIASES,
                        proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env")),
                    field(JavaEnvironment.GRAPH, var("g"))));

    public static final Def javaFeatures = def("javaFeatures")
        .to(() ->
                ref(Coder.java11Features));

    public static final Def javaIdentifierToString = def("javaIdentifierToString")
        .lam("id")
        .to(() ->
                apply(unwrap(Identifier.TYPE_), var("id")));

    public static final Def javaTypeArgumentsForNamedType = def("javaTypeArgumentsForNamedType")
        .lam("tname").lam("cx").lam("g")
        .to(() ->
                Eithers.bind(
                    hydra.dsl.Resolution.requireType( var("cx"), var("g"), var("tname")),
                    lambda("typ",
                        right(
                            Lists.map(
                                lambda("tp_",
                                    apply(
                                        ref(Utils.typeParameterToTypeArgument),
                                        var("tp_"))),
                                apply(ref(Coder.javaTypeParametersForType), var("typ")))))));

    public static final Def javaTypeArgumentsForType = def("javaTypeArgumentsForType")
        .lam("typ")
        .to(() ->
                Lists.reverse(
                    Lists.map(
                        ref(Utils.typeParameterToTypeArgument),
                        apply(ref(Coder.javaTypeParametersForType), var("typ")))));

    public static final Def javaTypeParametersForType = def("javaTypeParametersForType")
        .lam("typ")
        .to(() ->
                let(
                    field("toParam",
                        lambda("name",
                            apply(
                                ref(Utils.javaTypeParameter),
                                hydra.dsl.Formatting.capitalize(
                                    apply(unwrap(Name.TYPE_), var("name")))))),
                    field("boundVars",
                        apply(ref(Coder.javaTypeParametersForType_bvars), var("typ"))),
                    field("freeVars",
                        Lists.filter(
                            lambda("v", apply(ref(Coder.isLambdaBoundVariable), var("v"))),
                            Sets.toList(
                                hydra.dsl.Variables.freeVariablesInType( var("typ"))))),
                    field("vars",
                        Lists.nub(Lists.concat2(var("boundVars"), var("freeVars")))),
                    Lists.map(var("toParam"), var("vars"))));

    public static final Def javaTypeParametersForType_bvars = def("javaTypeParametersForType_bvars")
        .lam("t")
        .to(() ->
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
                                    proj(ForallType.TYPE_, ForallType.BODY, "ft")))))));

    public static final Def moduleToJava = def("moduleToJava")
        .lam("mod").lam("defs").lam("cx").lam("g")
        .to(() ->
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
                                                hydra.dsl.Serialization.printExpr(
                                                    hydra.dsl.Serialization.parenthesize(
                                                        apply(
                                                            ref(Serde.compilationUnitToExpr),
                                                            var("unit"))))))),
                                    Maps.toList(var("units"))))))));

    public static final Def nameMapToTypeMap = def("nameMapToTypeMap")
        .lam("m")
        .to(() ->
                Maps.map(
                    lambda("v", inject(Type.TYPE_, Type.VARIABLE, var("v"))),
                    var("m")));

    public static final Def namespaceParent = def("namespaceParent")
        .lam("ns")
        .to(() ->
                let(
                    field("parts",
                        Strings.splitOn(
                            string("."),
                            apply(unwrap(ModuleName.TYPE_), var("ns")))),
                    field("initParts",
                        Optionals.fromOptional(list(), Lists.maybeInit(var("parts")))),
                    Logic.ifElse(
                        Lists.null_(var("initParts")),
                        nothing(),
                        just(
                            wrap(ModuleName.TYPE_,
                                Strings.intercalate(string("."), var("initParts")))))));

    public static final Def noComment = def("noComment")
        .lam("decl")
        .to(() ->
                record(ClassBodyDeclarationWithComments.TYPE_,
                    field(ClassBodyDeclarationWithComments.VALUE, var("decl")),
                    field(ClassBodyDeclarationWithComments.COMMENTS, nothing())));

    public static final Def noInterfaceComment = def("noInterfaceComment")
        .lam("decl")
        .to(() ->
                record(InterfaceMemberDeclarationWithComments.TYPE_,
                    field(
                        InterfaceMemberDeclarationWithComments.VALUE,
                        var("decl")),
                    field(
                        InterfaceMemberDeclarationWithComments.COMMENTS,
                        nothing())));

    public static final Def otherwiseBranch = def("otherwiseBranch")
        .to(() ->
                lambda(
                params("env", "aliases", "dom", "cod", "tname", "jcod", "targs", "d", "cx", "g"),
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

    public static final Def peelDomainTypes = def("peelDomainTypes")
        .lam("n").lam("t")
        .to(() ->
                Logic.ifElse(
                    Equality.lte(var("n"), int32(0)),
                    pair(list(), var("t")),
                    casesWithDefault(Type.TYPE_,
                        hydra.dsl.Strip.deannotateType( var("t")),
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
                                        Pairs.second(var("rest")))))))));

    public static final Def peelDomainsAndCod = def("peelDomainsAndCod")
        .lam("n").lam("t")
        .to(() ->
                Logic.ifElse(
                    Equality.lte(var("n"), int32(0)),
                    pair(list(), var("t")),
                    casesWithDefault(Type.TYPE_,
                        hydra.dsl.Strip.deannotateType( var("t")),
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
                                        Pairs.second(var("rest")))))))));

    public static final Def peelExpectedTypes = def("peelExpectedTypes")
        .lam("subst").lam("n").lam("t")
        .to(() ->
                Logic.ifElse(
                    Equality.equal(var("n"), int32(0)),
                    list(),
                    casesWithDefault(Type.TYPE_,
                        hydra.dsl.Strip.deannotateType( var("t")),
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
                                        proj(FunctionType.TYPE_, FunctionType.CODOMAIN, "ft"))))))));

    public static final Def propagateType = def("propagateType")
        .lam("typ").lam("term")
        .to(() ->
                let("setTypeAnn",
                    lambda("t",
                        hydra.dsl.Annotations.setTermAnnotation(
                            hydra.dsl.Constants.keyType(),
                            just(apply(tterm(Refs.encodeRef(Core.typeType())), var("typ"))),
                            var("t"))),
                    casesWithDefault(Term.TYPE_,
                        hydra.dsl.Strip.deannotateTerm( var("term")),
                        apply(var("setTypeAnn"), var("term")),
                        field(
                            Term.LAMBDA,
                            constant(
                                let("annotated",
                                    apply(var("setTypeAnn"), var("term")),
                                    casesWithDefault(Type.TYPE_,
                                        hydra.dsl.Strip.deannotateType( var("typ")),
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
                                            Optionals.cases(proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b"), var("b"), lambda("ts",
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
                                                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "b")))))),
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
                                            hydra.dsl.Strip.deannotateTerm( var("fun")),
                                            var("fun"),
                                            field(
                                                Term.CASES,
                                                lambda("cs",
                                                    let(
                                                        field("dom",
                                                            hydra.dsl.Resolution.nominalApplication(
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
                                                        hydra.dsl.Annotations.setTermAnnotation(
                                                            hydra.dsl.Constants.keyType(),
                                                            just(
                                                                apply(
                                                                    tterm(Refs.encodeRef(Core.typeType())),
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
                                                field(Application.ARGUMENT, var("arg")))))))))));

    public static final Def propagateType_propagateIntoLambda = def("propagateType_propagateIntoLambda")
        .lam("cod").lam("t")
        .to(() ->
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
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam")))))))));

    public static final Def propagateType_rebuildLet = def("propagateType_rebuildLet")
        .lam("t").lam("bindings").lam("newBody")
        .to(() ->
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
                                    field(Let.BODY, var("newBody"))))))));

    public static final Def propagateTypesInAppChain = def("propagateTypesInAppChain")
        .lam("fixedCod").lam("resultType").lam("t")
        .to(() ->
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
                        Logic.and(
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
                                hydra.dsl.Annotations.setTermAnnotation(
                                    hydra.dsl.Constants.keyType(),
                                    just(apply(tterm(Refs.encodeRef(Core.typeType())), var("funType"))),
                                    var("fun"))),
                            apply(
                                ref(Coder.rebuildApps),
                                var("annotatedFun"),
                                var("args"),
                                var("funType"))),
                        casesWithDefault(Term.TYPE_,
                            hydra.dsl.Strip.deannotateTerm( var("t")),
                            hydra.dsl.Annotations.setTermAnnotation(
                                hydra.dsl.Constants.keyType(),
                                just(apply(tterm(Refs.encodeRef(Core.typeType())), var("resultType"))),
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
                                                hydra.dsl.Strip.deannotateTerm( var("lhs")),
                                                var("lhs"),
                                                field(
                                                    Term.CASES,
                                                    lambda("cs",
                                                        let(
                                                            field("dom",
                                                                hydra.dsl.Resolution.nominalApplication(
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
                                                            hydra.dsl.Annotations.setTermAnnotation(
                                                                hydra.dsl.Constants.keyType(),
                                                                just(
                                                                    apply(
                                                                        tterm(Refs.encodeRef(Core.typeType())),
                                                                        var("ft"))),
                                                                var("lhs"))))))),
                                        hydra.dsl.Annotations.setTermAnnotation(
                                            hydra.dsl.Constants.keyType(),
                                            just(
                                                apply(
                                                    tterm(Refs.encodeRef(Core.typeType())),
                                                    var("resultType"))),
                                            inject(Term.TYPE_,
                                                Term.APPLICATION,
                                                record(Application.TYPE_,
                                                    field(
                                                        Application.FUNCTION,
                                                        var("annotatedLhs")),
                                                    field(
                                                        Application.ARGUMENT,
                                                        var("rhs"))))))))))));

    public static final Def rebuildApps = def("rebuildApps")
        .lam("f").lam("args").lam("fType")
        .to(() ->
                Logic.ifElse(
                    Lists.null_(var("args")),
                    var("f"),
                    casesWithDefault(Type.TYPE_,
                        hydra.dsl.Strip.deannotateType( var("fType")),
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
                                Optionals.fromOptional(
                                    var("f"),
                                    Optionals.map(
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
                                                    hydra.dsl.Annotations.setTermAnnotation(
                                                        hydra.dsl.Constants.keyType(),
                                                        just(
                                                            apply(
                                                                tterm(Refs.encodeRef(Core.typeType())),
                                                                var("remainingType"))),
                                                        var("app"))),
                                                apply(
                                                    ref(Coder.rebuildApps),
                                                    var("annotatedApp"),
                                                    var("rest"),
                                                    var("remainingType")))),
                                        Lists.uncons(var("args")))))))));

    // Escapes a builder setter name that would collide with the generated builder() / build() methods.
    // The builder setter name for a field: the sanitized field name (reserved words like
    // `implements`/`static` get a trailing underscore via sanitizeJavaName, matching the private field
    // and parameter), plus an extra escape for `build`/`builder`, which would collide with the
    // generated build()/builder() methods but are not Java reserved words.
    public static final Def builderSetterName = def("builderSetterName")
        .lam("fname")
        .to(() ->
                let("base",
                    apply(
                        ref(Utils.sanitizeJavaName),
                        apply(unwrap(Name.TYPE_), var("fname"))),
                    Logic.ifElse(
                        Logic.or(
                            Equality.equal(var("base"), string("build")),
                            Equality.equal(var("base"), string("builder"))),
                        Strings.cat2(var("base"), string("_")),
                        var("base"))));

    // Emits the per-record fluent builder: a static generic builder() factory plus a nested
    // 'public static final class Builder' with one mutable field and one fluent setter per record
    // field and a build() method that calls the record's all-args constructor. Generic type
    // parameters are threaded through so that Builder<V>, its setters, and build() are type-safe.
    public static final Def recordBuilderClass = def("recordBuilderClass")
        .lams("aliases", "tparams", "elName", "fields", "cx", "g")
        .to(() ->
                let(binds(                    // ReferenceTypes for the record's type parameters, e.g. [V] for Vertex<V>.
                    field("typeArgRefs",
                        Lists.map(
                            ref(Utils.typeParameterToReferenceType),
                            var("tparams"))),
                    // The same type parameters as TypeArguments, for constructor calls (new Builder<V>(), new R<V>(...)).
                    field("typeArgs",
                        Lists.map(
                            lambda("rt",
                                inject(TypeArgument.TYPE_,
                                    TypeArgument.REFERENCE,
                                    var("rt"))),
                            var("typeArgRefs"))),
                    // Optional type-arguments-or-diamond for constructor calls: nothing() for a
                    // non-generic record (new R(...)), or just(<V...>) to thread the type params.
                    field("mTypeArgs",
                        Logic.ifElse(
                            Lists.null_(var("tparams")),
                            nothing(),
                            just(
                                inject(TypeArgumentsOrDiamond.TYPE_,
                                    TypeArgumentsOrDiamond.ARGUMENTS,
                                    var("typeArgs"))))),
                    field("recordLocalName",
                        apply(
                            ref(Utils.sanitizeJavaName),
                            hydra.dsl.Names.localNameOf( var("elName")))),
                    // Type of Builder<...>, as a Java syntax Type.
                    field("builderType",
                        apply(
                            ref(Utils.javaRefType),
                            var("typeArgRefs"),
                            nothing(),
                            string("Builder"))),
                    field("builderResult",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            var("builderType"))),
                    // Type of the enclosing record R<...>, as a Java syntax Type.
                    field("recordType",
                        apply(
                            ref(Utils.javaRefType),
                            var("typeArgRefs"),
                            nothing(),
                            var("recordLocalName"))),
                    field("recordResult",
                        apply(
                            ref(Utils.javaTypeToJavaResult),
                            var("recordType"))),
                    // 'return this;' — every fluent setter ends with this.
                    field("returnThisStmt",
                        inject(BlockStatement.TYPE_,
                            BlockStatement.STATEMENT,
                            apply(
                                ref(Utils.javaReturnStatement),
                                just(ref(Utils.javaThis)))))),
                    // The private mutable member fields of the builder (one per record field).
                    Eithers.bind(
                        Eithers.mapList(
                            lambda("f",
                                let(
                                    field("mods",
                                    list(
                                        inject(FieldModifier.TYPE_,
                                            FieldModifier.PRIVATE,
                                            unit()))),
                                    field("ftype",
                                        proj(FieldType.TYPE_, FieldType.TYPE, "f")),
                                    field("fname",
                                        proj(FieldType.TYPE_, FieldType.NAME, "f")),
                                    Eithers.bind(
                                        apply(
                                            ref(Coder.encodeType),
                                            var("aliases"),
                                            hydra.dsl.lib.Sets.empty(),
                                            var("ftype"),
                                            var("cx"),
                                            var("g")),
                                        lambda("jt",
                                            right(
                                                apply(
                                                    ref(Coder.noComment),
                                                    apply(
                                                        ref(Utils.javaMemberField),
                                                        var("mods"),
                                                        var("jt"),
                                                        apply(
                                                            ref(Utils.fieldNameToJavaVariableDeclarator),
                                                            var("fname"))))))))),
                            var("fields")),
                        lambda("builderFields",
                            // The fluent setters (one per record field): 'this.f = f; return this;' returning Builder<...>.
                            Eithers.bind(
                                Eithers.mapList(
                                    lambda("f",
                                        let(
                                            field("mods",
                                            list(
                                                inject(MethodModifier.TYPE_,
                                                    MethodModifier.PUBLIC,
                                                    unit()))),
                                            field("fname",
                                                proj(FieldType.TYPE_, FieldType.NAME, "f")),
                                            field("assignStmt",
                                                inject(BlockStatement.TYPE_,
                                                    BlockStatement.STATEMENT,
                                                    apply(
                                                        ref(Utils.toAssignStmt),
                                                        var("fname")))),
                                            Eithers.bind(
                                                apply(
                                                    ref(Coder.fieldTypeToFormalParam),
                                                    var("aliases"),
                                                    var("f"),
                                                    var("cx"),
                                                    var("g")),
                                                lambda("param",
                                                    right(
                                                        apply(
                                                            ref(Coder.noComment),
                                                            apply(
                                                                ref(Utils.methodDeclaration),
                                                                var("mods"),
                                                                list(),
                                                                list(),
                                                                apply(
                                                                    ref(Coder.builderSetterName),
                                                                    var("fname")),
                                                                list(var("param")),
                                                                var("builderResult"),
                                                                just(
                                                                    list(
                                                                        var("assignStmt"),
                                                                        var("returnThisStmt")))))))))),
                                    var("fields")),
                                lambda("setterMethods",
                                    let(binds(                                        // build(): 'return new R<...>(f1, ..., fn);'
                                        field("buildMods",
                                            list(
                                                inject(MethodModifier.TYPE_,
                                                    MethodModifier.PUBLIC,
                                                    unit()))),
                                        field("buildArgs",
                                            Lists.map(
                                                lambda("f",
                                                    apply(
                                                        ref(Utils.fieldNameToJavaExpression),
                                                        proj(FieldType.TYPE_, FieldType.NAME, "f"))),
                                                var("fields"))),
                                        field("buildReturnStmt",
                                            inject(BlockStatement.TYPE_,
                                                BlockStatement.STATEMENT,
                                                apply(
                                                    ref(Utils.javaReturnStatement),
                                                    just(
                                                        apply(
                                                            ref(Utils.javaConstructorCall),
                                                            apply(
                                                                ref(Utils.javaConstructorName),
                                                                wrap(Identifier.TYPE_, var("recordLocalName")),
                                                                var("mTypeArgs")),
                                                            var("buildArgs"),
                                                            nothing()))))),
                                        field("buildMethod",
                                            apply(
                                                ref(Coder.withCommentString),
                                                Strings.cat2(
                                                    string("Builds an immutable {@link "),
                                                    Strings.cat2(
                                                        var("recordLocalName"),
                                                        string("}."))),
                                                apply(
                                                    ref(Utils.methodDeclaration),
                                                    var("buildMods"),
                                                    list(),
                                                    list(),
                                                    string("build"),
                                                    list(),
                                                    var("recordResult"),
                                                    just(list(var("buildReturnStmt")))))),
                                        // The nested 'public static final class Builder<...>'.
                                        field("builderClassDecl",
                                            apply(
                                                ref(Utils.javaClassDeclaration),
                                                var("aliases"),
                                                var("tparams"),
                                                wrap(Name.TYPE_, string("Builder")),
                                                list(
                                                    inject(ClassModifier.TYPE_, ClassModifier.PUBLIC, unit()),
                                                    inject(ClassModifier.TYPE_, ClassModifier.STATIC, unit()),
                                                    inject(ClassModifier.TYPE_, ClassModifier.FINAL, unit())),
                                                nothing(),
                                                list(),
                                                Lists.concat(
                                                    list(
                                                        var("builderFields"),
                                                        var("setterMethods"),
                                                        list(var("buildMethod")))))),
                                        field("builderClassMember",
                                            apply(
                                                ref(Coder.withCommentString),
                                                Strings.cat2(
                                                    string("A fluent builder for {@link "),
                                                    Strings.cat2(
                                                        var("recordLocalName"),
                                                        string("}."))),
                                                inject(ClassBodyDeclaration.TYPE_,
                                                    ClassBodyDeclaration.CLASS_MEMBER,
                                                    inject(ClassMemberDeclaration.TYPE_,
                                                        ClassMemberDeclaration.CLASS,
                                                        var("builderClassDecl"))))),
                                        // The static generic factory: 'public static <...> Builder<...> builder() { return new Builder<...>(); }'.
                                        field("factoryMods",
                                            list(
                                                inject(MethodModifier.TYPE_, MethodModifier.PUBLIC, unit()),
                                                inject(MethodModifier.TYPE_, MethodModifier.STATIC, unit()))),
                                        field("factoryReturnStmt",
                                            inject(BlockStatement.TYPE_,
                                                BlockStatement.STATEMENT,
                                                apply(
                                                    ref(Utils.javaReturnStatement),
                                                    just(
                                                        apply(
                                                            ref(Utils.javaConstructorCall),
                                                            apply(
                                                                ref(Utils.javaConstructorName),
                                                                wrap(Identifier.TYPE_, string("Builder")),
                                                                var("mTypeArgs")),
                                                            list(),
                                                            nothing()))))),
                                        field("factoryMethod",
                                            apply(
                                                ref(Coder.withCommentString),
                                                Strings.cat2(
                                                    string("Creates a new fluent builder for {@link "),
                                                    Strings.cat2(
                                                        var("recordLocalName"),
                                                        string("}."))),
                                                apply(
                                                    ref(Utils.methodDeclaration),
                                                    var("factoryMods"),
                                                    var("tparams"),
                                                    list(),
                                                    string("builder"),
                                                    list(),
                                                    var("builderResult"),
                                                    just(list(var("factoryReturnStmt"))))))),
                                        right(
                                            list(
                                                var("factoryMethod"),
                                                var("builderClassMember"))))))))));

    public static final Def recordCompareToMethod = def("recordCompareToMethod")
        .lam("aliases").lam("tparams").lam("elName").lam("fields")
        .to(() ->
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
                                var("fields"))))));

    public static final Def recordConstructor = def("recordConstructor")
        .lam("aliases").lam("elName").lam("fields").lam("cx").lam("g")
        .to(() ->
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
                                    var("assignStmts")))))));

    public static final Def recordEqualsMethod = def("recordEqualsMethod")
        .lam("aliases").lam("elName").lam("fields")
        .to(() ->
                let(
                    binds(    field("anns",
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
                                var("returnAllFieldsEqual"))))));

    public static final Def recordHashCodeMethod = def("recordHashCodeMethod")
        .lam("fields")
        .to(() ->
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
                        just(list(var("returnSum"))))));

    public static final Def recordMemberVar = def("recordMemberVar")
        .lam("aliases").lam("ft").lam("cx").lam("g")
        .to(() ->
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
                            hydra.dsl.lib.Sets.empty(),
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
                                        var("fname"))))))));

    public static final Def recordWithMethod = def("recordWithMethod")
        .lam("aliases").lam("elName").lam("fields").lam("field").lam("cx").lam("g")
        .to(() ->
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
                            hydra.dsl.Formatting.nonAlnumToUnderscores(
                                hydra.dsl.Formatting.capitalize(
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
                                hydra.dsl.Names.localNameOf( var("elName"))))),
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
                                    just(list(var("returnStmt")))))))));

    public static final Def resolveTypeApps = def("resolveTypeApps")
        .lam("schemeVars").lam("fallbackTypeApps").lam("argSubst")
        .to(() ->
                let(
                    field("resolvedVars",
                        Sets.fromList(Maps.keys(var("argSubst")))),
                    field("unresolvedVars",
                        Lists.filter(
                            lambda("v", Logic.not(Sets.member(var("v"), var("resolvedVars")))),
                            var("schemeVars"))),
                    field("usedTypes",
                        Sets.fromList(Maps.elems(var("argSubst")))),
                    field("unusedIrTypes",
                        Lists.filter(
                            lambda("t", Logic.not(Sets.member(var("t"), var("usedTypes")))),
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
                        var("schemeVars"))));

    public static final Def selfRefSubstitution = def("selfRefSubstitution")
        .lam("grouped")
        .to(() ->
                Lists.foldl(
                    lambda(
                        "subst",
                        "entry",
                        apply(
                            ref(Coder.selfRefSubstitution_processGroup),
                            var("subst"),
                            Pairs.first(var("entry")),
                            Pairs.second(var("entry")))),
                    hydra.dsl.lib.Maps.empty(),
                    Maps.toList(var("grouped"))));

    public static final Def selfRefSubstitution_processGroup = def("selfRefSubstitution_processGroup")
        .lam("subst").lam("inVar").lam("outVars")
        .to(() ->
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
                    var("subst")));

    public static final Def serializableTypes = def("serializableTypes")
        .lam("isSer")
        .to(() ->
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
                    Logic.ifElse(var("isSer"), list(var("javaSerializableType")), list())));

    public static final Def splitConstantInitializer = def("splitConstantInitializer")
        .lam("member")
        .to(() ->
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
                                    proj(ConstantDeclaration.TYPE_, ConstantDeclaration.TYPE, "cd")))))));

    public static final Def splitConstantInitializer_splitVar = def("splitConstantInitializer_splitVar")
        .lam("mods").lam("utype").lam("vd")
        .to(() ->
                let(
                    field("vid",
                        proj(VariableDeclarator.TYPE_, VariableDeclarator.ID, "vd")),
                    field("mInit",
                        proj(VariableDeclarator.TYPE_, VariableDeclarator.INITIALIZER, "vd")),
                    Optionals.cases(
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
                                            list(var("field"), var("helper"))))))))));

    public static final Def stripForalls = def("stripForalls")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    var("t"),
                    field(
                        Type.FORALL,
                        lambda("fa",
                            apply(
                                ref(Coder.stripForalls),
                                proj(ForallType.TYPE_, ForallType.BODY, "fa"))))));

    public static final Def substituteTypeVarsWithTypes = def("substituteTypeVarsWithTypes")
        .lam("subst").lam("t")
        .to(() ->
                apply(
                    ref(Coder.substituteTypeVarsWithTypes_go),
                    var("subst"),
                    hydra.dsl.Strip.deannotateType( var("t"))));

    public static final Def substituteTypeVarsWithTypes_go = def("substituteTypeVarsWithTypes_go")
        .lam("subst").lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
                    var("t"),
                    field(
                        Type.VARIABLE,
                        lambda("v",
                            Optionals.cases(
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
                        Type.OPTIONAL,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.OPTIONAL,
                                apply(
                                    ref(Coder.substituteTypeVarsWithTypes_go),
                                    var("subst"),
                                    var("inner"))))),
                    field(
                        Type.EFFECT,
                        lambda("inner",
                            inject(Type.TYPE_,
                                Type.EFFECT,
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
                                            proj(ForallType.TYPE_, ForallType.BODY, "ft")))))))));

    public static final Def tagCmpNotZeroExpr = def("tagCmpNotZeroExpr")
        .to(() ->
                let(
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

    public static final Def tagCompareExpr = def("tagCompareExpr")
        .to(() ->
                let(
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

    // Derive the explicit type arguments for a non-empty collection literal
    // (list, set, map, optional). When the caller already has type applications
    // in scope (descended through a Type.APPLICATION wrapper), reuse them via
    // takeTypeArgs. Otherwise recover the term's type with typeOfTerm, strip
    // annotations, and pull the component types out of the expected
    // ListType/SetType/MapType/OptionalType shape. Each component is encoded to
    // a Java reference type and wrapped as a TypeArgument, ready for
    // methodInvocationStaticWithTypeArgs. This lets the coder emit e.g.
    // hydra.util.PersistentMap.<Name, Type>ofEntries(...) so javac is not forced
    // to infer K and V from deeply-nested varargs (issue #394).
    public static final Def collectionTypeArgs = def("collectionTypeArgs")
        .to(() ->
                lambda(
                params("label", "n", "aliases", "anns", "tyapps", "cx", "g"),
                Logic.ifElse(
                    Logic.not(Lists.null_(var("tyapps"))),
                    apply(
                        ref(Coder.takeTypeArgs),
                        var("label"),
                        var("n"),
                        var("tyapps"),
                        var("cx"),
                        var("g")),
                    let("combinedAnns",
                        Lists.foldl(
                            lambda(
                                "acc",
                                "m",
                                Maps.union(var("acc"), var("m"))),
                            hydra.dsl.lib.Maps.empty(),
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
                                hydra.dsl.Annotations.getType(
                                    var("g"),
                                    var("combinedAnns"))),
                            lambda("mtyp",
                                Eithers.bind(
                                    Optionals.cases(
                                        var("mtyp"),
                                        // No retained type (neither a wrapping type application nor a
                                        // keyType annotation reached this collection term, e.g. a literal
                                        // nested below the points the type-propagation pass annotates).
                                        // Fall back to bare emission — same as the empty-collection case
                                        // does when tyapps is empty. javac can still infer the simpler
                                        // (non-varargs-heavy) cases; the cases that defeat inference, like
                                        // the 94-entry typesByName map, do carry the annotation. (#394)
                                        right(list()),
                                        lambda("typ",
                                            casesWithDefault(Type.TYPE_,
                                                hydra.dsl.Strip.deannotateType( var("typ")),
                                                right(list()),
                                                field(
                                                    Type.LIST,
                                                    lambda("elemType",
                                                        right(list(var("elemType"))))),
                                                field(
                                                    Type.SET,
                                                    lambda("elemType",
                                                        right(list(var("elemType"))))),
                                                field(
                                                    Type.OPTIONAL,
                                                    lambda("elemType",
                                                        right(list(var("elemType"))))),
                                                field(
                                                    Type.MAP,
                                                    lambda("mt",
                                                        right(
                                                            list(
                                                                proj(MapType.TYPE_, MapType.KEYS, "mt"),
                                                                proj(MapType.TYPE_, MapType.VALUES, "mt")))))))),
                                    lambda("compTypes",
                                        Eithers.mapList(
                                            lambda("ct",
                                                Eithers.bind(
                                                    apply(
                                                        ref(Coder.encodeType),
                                                        var("aliases"),
                                                        hydra.dsl.lib.Sets.empty(),
                                                        var("ct"),
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
                                            var("compTypes"))))))))));

    public static final Def takeTypeArgs = def("takeTypeArgs")
        .lam("label").lam("n").lam("tyapps").lam("cx").lam("g")
        .to(() ->
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
                        Lists.take(var("n"), var("tyapps")))));

    public static final Def toClassDecl = def("toClassDecl")
        .to(() ->
                lambda(
                params("isInner", "isSer", "aliases", "tparams", "elName", "t", "cx", "g"),
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
                                        hydra.dsl.Strip.deannotateType( var("t'"))))),
                            var("cx"),
                            var("g"))),
                    casesWithDefault(Type.TYPE_,
                        hydra.dsl.Strip.deannotateType( var("t")),
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
                                            hydra.dsl.Formatting.capitalize(
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

    public static final Def toDeclInit = def("toDeclInit")
        .lam("aliasesExt").lam("gExt").lam("recursiveVars").lam("flatBindings").lam("name").lam("cx").lam("g")
        .to(() ->
                Logic.ifElse(
                    Sets.member(var("name"), var("recursiveVars")),
                    let(
                        field("binding",
                            Optionals.fromOptional(
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
                            Optionals.cases(
                                proj(Binding.TYPE_, Binding.TYPE_SCHEME, "binding"),
                                hydra.dsl.Checking.typeOfTerm(
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
                                        hydra.dsl.lib.Sets.empty(),
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
                    right(nothing())));

    public static final Def toDeclStatement = def("toDeclStatement")
        .to(() ->
                lambda(
                params("envExt", "aliasesExt", "gExt", "recursiveVars", "thunkedVars", "flatBindings", "name", "cx", "g"),
                let(
                    field("binding",
                        Optionals.fromOptional(
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
                        Optionals.cases(
                            proj(Binding.TYPE_, Binding.TYPE_SCHEME, "binding"),
                            hydra.dsl.Checking.typeOfTerm(
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
                                    hydra.dsl.lib.Sets.empty(),
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
                                            hydra.dsl.Annotations.setTermAnnotation(
                                                hydra.dsl.Constants.keyType(),
                                                just(
                                                    apply(tterm(Refs.encodeRef(Core.typeType())), var("typ"))),
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
                                                                                    string("hydra.overlay.java.util.Lazy")),
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

    public static final Def tryInferFunctionType = def("tryInferFunctionType")
        .lam("funTerm")
        .to(() ->
                casesWithDefault(Term.TYPE_,
                    hydra.dsl.Strip.deannotateTerm( var("funTerm")),
                    nothing(),
                    field(
                        Term.LAMBDA,
                        lambda("lam",
                            Optionals.bind(
                                proj(Lambda.TYPE_, Lambda.DOMAIN, "lam"),
                                lambda("dom",
                                    let("mCod",
                                        casesWithDefault(Term.TYPE_,
                                            proj(Lambda.TYPE_, Lambda.BODY, "lam"),
                                            nothing(),
                                            field(
                                                Term.ANNOTATED,
                                                lambda("at",
                                                    Optionals.bind(
                                                        Maps.lookup(
                                                            hydra.dsl.Constants.keyType(),
                                                            hydra.dsl.Annotations.getAnnotationMap(
                                                                proj(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION, "at"))),
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
                                        Optionals.map(
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
                                            var("mCod")))))))));

    public static final Def typeAppFallbackCast = def("typeAppFallbackCast")
        .to(() ->
                lambda(
                params("env", "aliases", "anns", "tyapps", "jatyp", "body", "typ", "cx", "g"),
                let("annotatedBody",
                    hydra.dsl.Annotations.setTermAnnotation(
                        hydra.dsl.Constants.keyType(),
                        just(apply(tterm(Refs.encodeRef(Core.typeType())), var("typ"))),
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
                                    hydra.dsl.lib.Sets.empty(),
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

    public static final Def typeAppNullaryOrHoisted = def("typeAppNullaryOrHoisted")
        .to(() ->
                lambda(
                params("env", "aliases", "anns", "tyapps", "jatyp", "body", "correctedTyp", "varName", "cls", "allTypeArgs", "cx", "g"),
                let(
                    field("qn",
                        hydra.dsl.Names.qualifyName( var("varName"))),
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
                                Optionals.cases(
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
                                                                        hydra.dsl.lib.Sets.empty(),
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
                                Optionals.cases(
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
                                                                        hydra.dsl.lib.Sets.empty(),
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
                                                                        Math_.range(
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

    public static final Def typeArgsOrDiamond = def("typeArgsOrDiamond")
        .lam("args")
        .to(() ->
                Logic.ifElse(
                    apply(
                        project(JavaFeatures.TYPE_, JavaFeatures.SUPPORTS_DIAMOND_OPERATOR),
                        ref(Coder.javaFeatures)),
                    inject(TypeArgumentsOrDiamond.TYPE_,
                        TypeArgumentsOrDiamond.DIAMOND,
                        unit()),
                    inject(TypeArgumentsOrDiamond.TYPE_,
                        TypeArgumentsOrDiamond.ARGUMENTS,
                        var("args"))));

    public static final Def typesMatch = def("typesMatch")
        .lam("a").lam("b")
        .to(() ->
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
                                    lambda("wb", Equality.equal(var("wa"), var("wb")))))))));

    public static final Def unwrapReturnType = def("unwrapReturnType")
        .lam("t")
        .to(() ->
                casesWithDefault(Type.TYPE_,
                    hydra.dsl.Strip.deannotateType( var("t")),
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
                                proj(ApplicationType.TYPE_, ApplicationType.ARGUMENT, "at"))))));

    public static final Def variantCompareToMethod = def("variantCompareToMethod")
        .lam("aliases").lam("tparams").lam("parentName").lam("variantName").lam("fields")
        .to(() ->
                let(
                    binds(    field("anns",
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
                        just(var("body")))));

    public static final Def visitBranch = def("visitBranch")
        .to(() ->
                lambda(
                params("env", "aliases", "dom", "tname", "jcod", "targs", "field", "cx", "g"),
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
                                    hydra.dsl.Formatting.capitalize(
                                        apply(
                                            unwrap(Name.TYPE_),
                                            proj(CaseAlternative.TYPE_, CaseAlternative.NAME, "field"))))))),
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
                        hydra.dsl.Strip.deannotateTerm(
                            proj(CaseAlternative.TYPE_, CaseAlternative.HANDLER, "field")),
                        left(
                            inject(Error_.TYPE_,
                                Error_.OTHER,
                                wrap(OtherError.TYPE_,
                                    Strings.cat2(
                                        string("visitBranch: field term is not a lambda: "),
                                        apply(
                                            tterm(Refs.showRef(Core.termTerm())),
                                            proj(CaseAlternative.TYPE_, CaseAlternative.HANDLER, "field")))))),
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

    public static final Def withCommentString = def("withCommentString")
        .lam("comment").lam("decl")
        .to(() ->
                record(ClassBodyDeclarationWithComments.TYPE_,
                    field(ClassBodyDeclarationWithComments.VALUE, var("decl")),
                    field(
                        ClassBodyDeclarationWithComments.COMMENTS,
                        just(var("comment")))));

    public static final Def withInterfaceCommentString = def("withInterfaceCommentString")
        .lam("comment").lam("decl")
        .to(() ->
                record(InterfaceMemberDeclarationWithComments.TYPE_,
                    field(
                        InterfaceMemberDeclarationWithComments.VALUE,
                        var("decl")),
                    field(
                        InterfaceMemberDeclarationWithComments.COMMENTS,
                        just(var("comment")))));

    public static final Def withLambda = def("withLambda")
        .lam("env").lam("lam").lam("k")
        .to(() ->
                hydra.dsl.Environment.withLambdaContext(
                    ref(Coder.javaEnvGetGraph),
                    ref(Coder.javaEnvSetGraph),
                    var("env"),
                    var("lam"),
                    lambda("env1",
                        let(
                            field("aliases",
                                proj(JavaEnvironment.TYPE_, JavaEnvironment.ALIASES, "env1")),
                            field("aliases2",
                                recordWith(Aliases.TYPE_, "aliases", Utils.ALIASES_FIELDS,
                                    field(
                                        Aliases.LAMBDA_VARS,
                                        Sets.insert(
                                            proj(Lambda.TYPE_, Lambda.PARAMETER, "lam"),
                                            proj(Aliases.TYPE_, Aliases.LAMBDA_VARS, "aliases"))))),
                            field("env2",
                                record(JavaEnvironment.TYPE_,
                                    field(
                                        JavaEnvironment.ALIASES,
                                        var("aliases2")),
                                    field(
                                        JavaEnvironment.GRAPH,
                                        proj(JavaEnvironment.TYPE_, JavaEnvironment.GRAPH, "env1")))),
                            apply(var("k"), var("env2"))))));

    public static final Def withTypeLambda = def("withTypeLambda")
        .to(() ->
                apply(
                var("hydra.environment.withTypeLambdaContext"),
                ref(Coder.javaEnvGetGraph),
                ref(Coder.javaEnvSetGraph)));

    public static final Def wrapInSupplierLambda = def("wrapInSupplierLambda")
        .lam("expr")
        .to(() ->
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
                                var("expr"))))));

    // Look up a primitive by name and return its per-parameter laziness flags
    // (the isLazy flag of each signature parameter), in order. Empty if the name
    // is not a registered primitive. The single source of truth for which
    // arguments must be thunked, replacing the former hard-coded name table
    // (issue #391).
    public static final Def lazyFlagsForPrimitive = def("lazyFlagsForPrimitive")
        .lam("g").lam("name")
        .to(() ->
                Optionals.cases(
                    Maps.lookup(var("name"), proj(Graph.TYPE_, Graph.PRIMITIVES, "g")),
                    list(),
                    lambda("prim",
                        Lists.map(
                            lambda("p", proj(Parameter.TYPE_, Parameter.IS_LAZY, "p")),
                            proj(TermSignature.TYPE_, TermSignature.PARAMETERS,
                                proj(PrimitiveDefinition.TYPE_, PrimitiveDefinition.SIGNATURE,
                                    proj(Primitive.TYPE_, Primitive.DEFINITION, "prim")))))));

    // For primitives requiring lazy evaluation, wrap the lazy-flagged arguments
    // in Supplier lambdas. Java eagerly evaluates all method arguments, so e.g.
    // ifElse branches must be wrapped in () -> expr and called via IfElse.lazy();
    // maybe's default must be wrapped to avoid constructing expensive values on
    // the success path. Which positions are lazy comes from the primitive's
    // isLazy metadata (issue #391), not a hard-coded name table. Only fires when
    // the primitive is fully applied (argc == parameter count) and has at least
    // one lazy parameter. The returned Optional String is the Java method-name
    // override: ifElse dispatches to `lazy`, the others to `applyLazy`.
    public static final Def wrapLazyArguments = def("wrapLazyArguments")
        .lam("g").lam("name").lam("args")
        .to(() ->
                let(
                    field("lazyFlags",
                        apply(ref(Coder.lazyFlagsForPrimitive), var("g"), var("name"))),
                    field("anyLazy",
                        Lists.foldl(
                            lambda("b", "f", Logic.or(var("b"), var("f"))),
                            bool(false),
                            var("lazyFlags"))),
                    Logic.ifElse(
                        Logic.and(
                            var("anyLazy"),
                            Equality.equal(Lists.length(var("args")), Lists.length(var("lazyFlags")))),
                        pair(
                            Lists.map(
                                lambda("pair",
                                    Logic.ifElse(
                                        Pairs.second(var("pair")),
                                        apply(ref(Coder.wrapInSupplierLambda), Pairs.first(var("pair"))),
                                        Pairs.first(var("pair")))),
                                Lists.zip(var("args"), var("lazyFlags"))),
                            just(Logic.ifElse(
                                Equality.equal(var("name"), wrap(Name.TYPE_, string("hydra.lib.logic.ifElse"))),
                                string("lazy"),
                                string("applyLazy")))),
                        pair(var("args"), nothing()))));









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
            builderSetterName,
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
            collectionTypeArgs,
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
            lazyFlagsForPrimitive,
            moduleToJava,
            nameMapToTypeMap,
            namespaceParent,
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
            recordBuilderClass,
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
        new ModuleName("hydra.refs"),
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
        Optional.given(new EntityMetadata(
            Optional.given("Java code generator: converts Hydra modules to Java source code"),
            List.of(),
            List.of(),
            Optional.none())),
        DEPENDENCIES,
        DEFINITIONS);
}
