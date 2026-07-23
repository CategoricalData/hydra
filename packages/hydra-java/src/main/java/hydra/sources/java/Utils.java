package hydra.sources.java;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Core;
import hydra.dsl.Errors;
import hydra.dsl.Packaging;
import hydra.overlay.java.dsl.Types;
import hydra.dsl.java.Environment;
import hydra.dsl.java.Language;
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
import hydra.packaging.EntityMetadata;
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
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.java.environment.Aliases;
import hydra.java.syntax.AdditiveExpression;
import hydra.java.syntax.AdditiveExpression_Binary;
import hydra.java.syntax.AmbiguousName;
import hydra.java.syntax.AndExpression;
import hydra.java.syntax.AnnotatedIdentifier;
import hydra.java.syntax.Annotation;
import hydra.java.syntax.ArrayCreationExpression;
import hydra.java.syntax.ArrayCreationExpressionWithInitializer;
import hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive;
import hydra.java.syntax.ArrayInitializer;
import hydra.java.syntax.ArrayType;
import hydra.java.syntax.ArrayType_Variant;
import hydra.java.syntax.Assignment;
import hydra.java.syntax.AssignmentExpression;
import hydra.java.syntax.AssignmentOperator;
import hydra.java.syntax.Block;
import hydra.java.syntax.BlockStatement;
import hydra.java.syntax.CastExpression;
import hydra.java.syntax.CastExpression_NotPlusMinus;
import hydra.java.syntax.CastExpression_Primitive;
import hydra.java.syntax.CastExpression_RefAndBounds;
import hydra.java.syntax.ClassBody;
import hydra.java.syntax.ClassBodyDeclaration;
import hydra.java.syntax.ClassDeclaration;
import hydra.java.syntax.ClassInstanceCreationExpression;
import hydra.java.syntax.ClassMemberDeclaration;
import hydra.java.syntax.ClassOrInterfaceType;
import hydra.java.syntax.ClassOrInterfaceTypeToInstantiate;
import hydra.java.syntax.ClassType;
import hydra.java.syntax.ClassTypeQualifier;
import hydra.java.syntax.ConditionalAndExpression;
import hydra.java.syntax.ConditionalExpression;
import hydra.java.syntax.ConditionalOrExpression;
import hydra.java.syntax.ConstructorBody;
import hydra.java.syntax.ConstructorDeclaration;
import hydra.java.syntax.ConstructorDeclarator;
import hydra.java.syntax.ConstructorModifier;
import hydra.java.syntax.Dims;
import hydra.java.syntax.ElementValue;
import hydra.java.syntax.EqualityExpression;
import hydra.java.syntax.EqualityExpression_Binary;
import hydra.java.syntax.ExclusiveOrExpression;
import hydra.java.syntax.Expression;
import hydra.java.syntax.ExpressionName;
import hydra.java.syntax.ExpressionStatement;
import hydra.java.syntax.FieldAccess;
import hydra.java.syntax.FieldAccess_Qualifier;
import hydra.java.syntax.FieldDeclaration;
import hydra.java.syntax.FormalParameter;
import hydra.java.syntax.FormalParameter_Simple;
import hydra.java.syntax.Identifier;
import hydra.java.syntax.InclusiveOrExpression;
import hydra.java.syntax.InstanceofExpression;
import hydra.java.syntax.InstanceofExpression_Rhs;
import hydra.java.syntax.IntegerLiteral;
import hydra.java.syntax.IntegralType;
import hydra.java.syntax.InterfaceDeclaration;
import hydra.java.syntax.InterfaceMemberDeclaration;
import hydra.java.syntax.InterfaceMethodDeclaration;
import hydra.java.syntax.InterfaceType;
import hydra.java.syntax.LambdaBody;
import hydra.java.syntax.LambdaExpression;
import hydra.java.syntax.LambdaParameters;
import hydra.java.syntax.LeftHandSide;
import hydra.java.syntax.Literal;
import hydra.java.syntax.LocalVariableDeclaration;
import hydra.java.syntax.LocalVariableDeclarationStatement;
import hydra.java.syntax.LocalVariableType;
import hydra.java.syntax.MarkerAnnotation;
import hydra.java.syntax.MethodBody;
import hydra.java.syntax.MethodDeclaration;
import hydra.java.syntax.MethodDeclarator;
import hydra.java.syntax.MethodHeader;
import hydra.java.syntax.MethodInvocation;
import hydra.java.syntax.MethodInvocation_Complex;
import hydra.java.syntax.MethodInvocation_Header;
import hydra.java.syntax.MethodInvocation_Variant;
import hydra.java.syntax.MethodModifier;
import hydra.java.syntax.MethodName;
import hydra.java.syntax.MultiplicativeExpression;
import hydra.java.syntax.NormalClassDeclaration;
import hydra.java.syntax.NumericType;
import hydra.java.syntax.PackageDeclaration;
import hydra.java.syntax.PackageName;
import hydra.java.syntax.PostfixExpression;
import hydra.java.syntax.Primary;
import hydra.java.syntax.PrimaryNoNewArrayExpression;
import hydra.java.syntax.PrimitiveType;
import hydra.java.syntax.PrimitiveTypeWithAnnotations;
import hydra.java.syntax.ReferenceType;
import hydra.java.syntax.RelationalExpression;
import hydra.java.syntax.Result;
import hydra.java.syntax.ReturnStatement;
import hydra.java.syntax.ShiftExpression;
import hydra.java.syntax.SimpleTypeName;
import hydra.java.syntax.SingleElementAnnotation;
import hydra.java.syntax.Statement;
import hydra.java.syntax.StatementExpression;
import hydra.java.syntax.StatementWithoutTrailingSubstatement;
import hydra.java.syntax.StringLiteral;
import hydra.java.syntax.ThrowStatement;
import hydra.java.syntax.TypeArgument;
import hydra.java.syntax.TypeIdentifier;
import hydra.java.syntax.TypeName;
import hydra.java.syntax.TypeParameter;
import hydra.java.syntax.TypeVariable;
import hydra.java.syntax.UnannType;
import hydra.java.syntax.UnaryExpression;
import hydra.java.syntax.UnaryExpressionNotPlusMinus;
import hydra.java.syntax.UnqualifiedClassInstanceCreationExpression;
import hydra.java.syntax.VariableDeclarator;
import hydra.java.syntax.VariableDeclaratorId;
import hydra.java.syntax.VariableInitializer;
import hydra.java.syntax.VariableModifier;
import hydra.java.syntax.Wildcard;
import hydra.util.QualifiedName;
import hydra.sources.java.Names;

/**
 * Java utilities for constructing Java syntax trees — Java DSL port of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Utils.hs}.
 */
public class Utils {
    public static final ModuleName NS = new ModuleName("hydra.java.utils");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").lam("x").to(() -> body)}. See Defs.DefBuilder. */
    private static Defs.DefBuilder def(String localName) {
        return define(NS, localName);
    }

    /**
     * The {@link Aliases} fields in declaration order (mirrors the record in
     * {@code Environment.aliases()}). Passed to {@link hydra.overlay.java.dsl.meta.Phantoms#recordWith}
     * so a copy-with-update over an Aliases value need name only the fields it changes.
     * Keep in sync with the Aliases type definition.
     */
    public static final List<Name> ALIASES_FIELDS = List.of(
        Aliases.CURRENT_NAMESPACE, Aliases.PACKAGES, Aliases.BRANCH_VARS, Aliases.RECURSIVE_VARS,
        Aliases.IN_SCOPE_TYPE_PARAMS, Aliases.POLYMORPHIC_LOCALS, Aliases.IN_SCOPE_JAVA_VARS,
        Aliases.VAR_RENAMES, Aliases.LAMBDA_VARS, Aliases.TYPE_VAR_SUBST, Aliases.TRUSTED_TYPE_VARS,
        Aliases.METHOD_CODOMAIN, Aliases.THUNKED_VARS);

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def addExpressions = def("addExpressions")
        .lam("exprs")
        .to(() ->
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
                            Optionals.withDefault(var("dummyMult"), Lists.head(var("exprs")))),
                        Lists.drop(int32(1), var("exprs")))));

    public static final Def addInScopeVar = def("addInScopeVar")
        .lam("name").lam("aliases")
        .to(() ->
                recordWith(Aliases.TYPE_, "aliases", ALIASES_FIELDS,
                    field(
                        Aliases.IN_SCOPE_JAVA_VARS,
                        Sets.insert(
                            var("name"),
                            proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")))));

    public static final Def addInScopeVars = def("addInScopeVars")
        .lam("names").lam("aliases")
        .to(() ->
                Lists.foldl(
                    lambda("a", lambda("n", apply(ref(Utils.addInScopeVar), var("n"), var("a")))),
                    var("aliases"),
                    var("names")));

    public static final Def addJavaTypeParameter = def("addJavaTypeParameter")
        .lam("rt").lam("t").lam("cx")
        .to(() ->
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
                                        string("expected a reference type"))))))));

    public static final Def addVarRename = def("addVarRename")
        .lam("original").lam("renamed").lam("aliases")
        .to(() ->
                recordWith(Aliases.TYPE_, "aliases", ALIASES_FIELDS,
                    field(
                        Aliases.VAR_RENAMES,
                        Maps.insert(
                            var("original"),
                            var("renamed"),
                            proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases")))));

    public static final Def fieldExpression = def("fieldExpression")
        .lam("varId").lam("fieldId")
        .to(() ->
                record(ExpressionName.TYPE_,
                    field(
                        ExpressionName.QUALIFIER,
                        just(wrap(AmbiguousName.TYPE_, list(var("varId"))))),
                    field(ExpressionName.IDENTIFIER, var("fieldId"))));

    public static final Def fieldNameToJavaExpression = def("fieldNameToJavaExpression")
        .lam("fname")
        .to(() ->
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
                                                                                                            var("fname")))))))))))))))))))))))));

    public static final Def fieldNameToJavaIdentifier = def("fieldNameToJavaIdentifier")
        .lam("fname")
        .to(() ->
                apply(ref(Utils.javaIdentifier), apply(unwrap(Name.TYPE_), var("fname"))));

    public static final Def fieldNameToJavaVariableDeclarator = def("fieldNameToJavaVariableDeclarator")
        .lam("fname")
        .to(() ->
                apply(
                    ref(Utils.javaVariableDeclarator),
                    apply(
                        ref(Utils.javaIdentifier),
                        apply(unwrap(Name.TYPE_), var("fname"))),
                    nothing()));

    public static final Def fieldNameToJavaVariableDeclaratorId = def("fieldNameToJavaVariableDeclaratorId")
        .lam("fname")
        .to(() ->
                apply(
                    ref(Utils.javaVariableDeclaratorId),
                    apply(
                        ref(Utils.javaIdentifier),
                        apply(unwrap(Name.TYPE_), var("fname")))));

    public static final Def finalVarDeclarationStatement = def("finalVarDeclarationStatement")
        .lam("id").lam("rhs")
        .to(() ->
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
                                                var("rhs"))))))))));

    private static TypedTerm overlayLibPair(String sub) {
        return pair(
            wrap(new Name("hydra.packaging.ModuleName"), string("hydra.lib." + sub)),
            apply(ref(Names.javaPackageName), list(
                string("hydra"), string("overlay"), string("java"), string("lib"), string(sub))));
    }

    public static final Def overlayJavaLibPackageAliases = def("overlayJavaLibPackageAliases")
        .to(() ->
                Maps.fromList(list(
            overlayLibPair("chars"),
            overlayLibPair("effects"),
            overlayLibPair("eithers"),
            overlayLibPair("equality"),
            overlayLibPair("files"),
            overlayLibPair("functions"),
            overlayLibPair("hashing"),
            overlayLibPair("lists"),
            overlayLibPair("literals"),
            overlayLibPair("logic"),
            overlayLibPair("maps"),
            overlayLibPair("math"),
            overlayLibPair("optionals"),
            overlayLibPair("ordering"),
            overlayLibPair("pairs"),
            overlayLibPair("regex"),
            overlayLibPair("sets"),
            overlayLibPair("strings"),
            overlayLibPair("system"),
            overlayLibPair("text"))));

    public static final Def importAliasesForModule = def("importAliasesForModule")
        .lam("mod")
        .to(() ->
                record(Aliases.TYPE_,
                    field(
                        Aliases.CURRENT_NAMESPACE,
                        proj(Module.TYPE_, Module.NAME, "mod")),
                    field(Aliases.PACKAGES, ref(overlayJavaLibPackageAliases)),
                    field(Aliases.BRANCH_VARS, hydra.dsl.lib.Sets.empty()),
                    field(
                        Aliases.RECURSIVE_VARS,
                        hydra.dsl.lib.Sets.empty()),
                    field(
                        Aliases.IN_SCOPE_TYPE_PARAMS,
                        hydra.dsl.lib.Sets.empty()),
                    field(
                        Aliases.POLYMORPHIC_LOCALS,
                        hydra.dsl.lib.Sets.empty()),
                    field(
                        Aliases.IN_SCOPE_JAVA_VARS,
                        hydra.dsl.lib.Sets.empty()),
                    field(Aliases.VAR_RENAMES, hydra.dsl.lib.Maps.empty()),
                    field(Aliases.LAMBDA_VARS, hydra.dsl.lib.Sets.empty()),
                    field(
                        Aliases.TYPE_VAR_SUBST,
                        hydra.dsl.lib.Maps.empty()),
                    field(
                        Aliases.TRUSTED_TYPE_VARS,
                        hydra.dsl.lib.Sets.empty()),
                    field(Aliases.METHOD_CODOMAIN, nothing()),
                    field(Aliases.THUNKED_VARS, hydra.dsl.lib.Sets.empty())));

    public static final Def interfaceMethodDeclaration = def("interfaceMethodDeclaration")
        .lam("mods").lam("tparams").lam("methodName").lam("params").lam("result").lam("stmts")
        .to(() ->
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
                            apply(ref(Utils.javaMethodBody), var("stmts"))))));

    public static final Def isEscaped = def("isEscaped")
        .lam("s")
        .to(() ->
                Equality.equal(
                    Optionals.withDefault(int32(0), Strings.charAt(int32(0), var("s"))),
                    int32(36)));

    public static final Def javaAdditiveExpressionToJavaExpression = def("javaAdditiveExpressionToJavaExpression")
        .lam("ae")
        .to(() ->
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
                                                                                var("ae"))))))))))))))))));

    public static final Def javaArrayCreation = def("javaArrayCreation")
        .lam("primType").lam("minit")
        .to(() ->
                let("init_",
                    Optionals.cases(
                        var("minit"),
                        wrap(ArrayInitializer.TYPE_, list()),
                        lambda("i", var("i"))),
                    apply(
                        ref(Utils.javaPrimaryToJavaExpression),
                        inject(Primary.TYPE_,
                            Primary.ARRAY_CREATION,
                            inject(ArrayCreationExpression.TYPE_,
                                ArrayCreationExpression.WITH_INIT,
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
                                            var("init_")))))))));

    public static final Def javaArrayInitializer = def("javaArrayInitializer")
        .lam("exprs")
        .to(() ->
                wrap(ArrayInitializer.TYPE_,
                    list(
                        Lists.map(
                            lambda("e",
                                inject(VariableInitializer.TYPE_,
                                    VariableInitializer.EXPRESSION,
                                    var("e"))),
                            var("exprs")))));

    public static final Def javaAssignmentStatement = def("javaAssignmentStatement")
        .lam("lhs").lam("rhs")
        .to(() ->
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
                                    field(Assignment.EXPRESSION, var("rhs"))))))));

    public static final Def javaBoolean = def("javaBoolean")
        .lam("b")
        .to(() ->
                inject(Literal.TYPE_, Literal.BOOLEAN, var("b")));

    public static final Def javaBooleanExpression = def("javaBooleanExpression")
        .lam("b")
        .to(() ->
                apply(
                    ref(Utils.javaPrimaryToJavaExpression),
                    apply(
                        ref(Utils.javaLiteralToJavaPrimary),
                        apply(ref(Utils.javaBoolean), var("b")))));

    public static final Def javaBooleanType = def("javaBooleanType")
        .to(() ->
                apply(
                ref(Utils.javaPrimitiveTypeToJavaType),
                inject(PrimitiveType.TYPE_,
                    PrimitiveType.BOOLEAN,
                    unit())));

    public static final Def javaBytePrimitiveType = def("javaBytePrimitiveType")
        .to(() ->
                record(PrimitiveTypeWithAnnotations.TYPE_,
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

    public static final Def javaCastExpression = def("javaCastExpression")
        .lam("rt").lam("expr")
        .to(() ->
                inject(CastExpression.TYPE_,
                    CastExpression.NOT_PLUS_MINUS,
                    record(CastExpression_NotPlusMinus.TYPE_,
                        field(
                            CastExpression_NotPlusMinus.REF_AND_BOUNDS,
                            record(CastExpression_RefAndBounds.TYPE_,
                                field(CastExpression_RefAndBounds.TYPE, var("rt")),
                                field(CastExpression_RefAndBounds.BOUNDS, list()))),
                        field(CastExpression_NotPlusMinus.EXPRESSION, var("expr")))));

    public static final Def javaCastExpressionToJavaExpression = def("javaCastExpressionToJavaExpression")
        .lam("ce")
        .to(() ->
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
                                                                                                var("ce"))))))))))))))))))))));

    public static final Def javaCastPrimitive = def("javaCastPrimitive")
        .lam("pt").lam("expr")
        .to(() ->
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
                        field(CastExpression_Primitive.EXPRESSION, var("expr")))));

    public static final Def javaClassDeclaration = def("javaClassDeclaration")
        .lam("aliases").lam("tparams").lam("elName").lam("mods").lam("supname").lam("impls").lam("bodyDecls")
        .to(() ->
                let("extends_",
                    Optionals.map(
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
                                wrap(ClassBody.TYPE_, var("bodyDecls")))))));

    public static final Def javaClassType = def("javaClassType")
        .lam("args").lam("pkg").lam("id")
        .to(() ->
                let(
                    field("qual",
                        Optionals.cases(
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
                        field(ClassType.ARGUMENTS, var("targs")))));

    public static final Def javaClassTypeToJavaType = def("javaClassTypeToJavaType")
        .lam("ct")
        .to(() ->
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.CLASS_OR_INTERFACE,
                        inject(ClassOrInterfaceType.TYPE_,
                            ClassOrInterfaceType.CLASS,
                            var("ct")))));

    public static final Def javaConditionalAndExpressionToJavaExpression = def("javaConditionalAndExpressionToJavaExpression")
        .lam("cae")
        .to(() ->
                inject(Expression.TYPE_,
                    Expression.ASSIGNMENT,
                    inject(AssignmentExpression.TYPE_,
                        AssignmentExpression.CONDITIONAL,
                        inject(ConditionalExpression.TYPE_,
                            ConditionalExpression.SIMPLE,
                            wrap(ConditionalOrExpression.TYPE_, list(var("cae")))))));

    public static final Def javaConstructorCall = def("javaConstructorCall")
        .lam("ci").lam("args").lam("mbody")
        .to(() ->
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
                                                                                                                            var("mbody")))))))))))))))))))))))))))));

    public static final Def javaConstructorName = def("javaConstructorName")
        .lam("id").lam("targs")
        .to(() ->
                record(ClassOrInterfaceTypeToInstantiate.TYPE_,
                    field(
                        ClassOrInterfaceTypeToInstantiate.IDENTIFIERS,
                        list(
                            record(AnnotatedIdentifier.TYPE_,
                                field(AnnotatedIdentifier.ANNOTATIONS, list()),
                                field(AnnotatedIdentifier.IDENTIFIER, var("id"))))),
                    field(
                        ClassOrInterfaceTypeToInstantiate.TYPE_ARGUMENTS,
                        var("targs"))));

    public static final Def javaDeclName = def("javaDeclName")
        .lam("name")
        .to(() ->
                wrap(TypeIdentifier.TYPE_,
                    apply(ref(Utils.javaVariableName), var("name"))));

    public static final Def javaDoubleCastExpression = def("javaDoubleCastExpression")
        .lam("rawRt").lam("targetRt").lam("expr")
        .to(() ->
                let("firstCast",
                    apply(
                        ref(Utils.javaCastExpressionToJavaExpression),
                        apply(ref(Utils.javaCastExpression), var("rawRt"), var("expr"))),
                    apply(
                        ref(Utils.javaCastExpression),
                        var("targetRt"),
                        apply(ref(Utils.javaExpressionToJavaUnaryExpression), var("firstCast")))));

    public static final Def javaDoubleCastExpressionToJavaExpression = def("javaDoubleCastExpressionToJavaExpression")
        .lam("rawRt").lam("targetRt").lam("expr")
        .to(() ->
                apply(
                    ref(Utils.javaCastExpressionToJavaExpression),
                    apply(
                        ref(Utils.javaDoubleCastExpression),
                        var("rawRt"),
                        var("targetRt"),
                        var("expr"))));

    public static final Def javaEmptyStatement = def("javaEmptyStatement")
        .to(() ->
                inject(Statement.TYPE_,
                Statement.WITHOUT_TRAILING,
                inject(StatementWithoutTrailingSubstatement.TYPE_,
                    StatementWithoutTrailingSubstatement.EMPTY,
                    unit())));

    public static final Def javaEqualityExpressionToJavaExpression = def("javaEqualityExpressionToJavaExpression")
        .lam("ee")
        .to(() ->
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
                                                                list(var("ee")))))))))))))));

    public static final Def javaEqualityExpressionToJavaInclusiveOrExpression = def("javaEqualityExpressionToJavaInclusiveOrExpression")
        .lam("ee")
        .to(() ->
                wrap(InclusiveOrExpression.TYPE_,
                    list(
                        wrap(ExclusiveOrExpression.TYPE_,
                            list(wrap(AndExpression.TYPE_, list(var("ee"))))))));

    public static final Def javaEquals = def("javaEquals")
        .lam("lhs").lam("rhs")
        .to(() ->
                inject(EqualityExpression.TYPE_,
                    EqualityExpression.EQUAL,
                    record(EqualityExpression_Binary.TYPE_,
                        field(EqualityExpression_Binary.LHS, var("lhs")),
                        field(EqualityExpression_Binary.RHS, var("rhs")))));

    public static final Def javaEqualsNull = def("javaEqualsNull")
        .lam("lhs")
        .to(() ->
                apply(
                    ref(Utils.javaEquals),
                    var("lhs"),
                    apply(
                        ref(Utils.javaLiteralToJavaRelationalExpression),
                        inject(Literal.TYPE_,
                            Literal.NULL,
                            unit()))));

    public static final Def javaExpressionNameToJavaExpression = def("javaExpressionNameToJavaExpression")
        .lam("en")
        .to(() ->
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
                                                                                                    var("en")))))))))))))))))))))));

    public static final Def javaExpressionToJavaPrimary = def("javaExpressionToJavaPrimary")
        .doc("Convert an Expression to a Primary, avoiding unnecessary parentheses when the expression is already a simple primary chain")
        .lam("e")
        .to(() ->
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
                                                                Optionals.withDefault(
                                                                    var("fallback"),
                                                                    Optionals.bind(
                                                                        Lists.head(
                                                                            var("cands")),
                                                                        lambda("candHead",
                                                                            let("iors",
                                                                                apply(
                                                                                    unwrap(ConditionalAndExpression.TYPE_),
                                                                                    var("candHead")),
                                                                                Optionals.bind(
                                                                                    Lists.head(
                                                                                        var("iors")),
                                                                                    lambda(
                                                                                        "iorHead",
                                                                                        let("xors",
                                                                                            apply(
                                                                                                unwrap(InclusiveOrExpression.TYPE_),
                                                                                                var("iorHead")),
                                                                                            Optionals.bind(
                                                                                                Lists.head(
                                                                                                    var("xors")),
                                                                                                lambda(
                                                                                                    "xorHead",
                                                                                                    let(
                                                                                                        "ands",
                                                                                                        apply(
                                                                                                            unwrap(ExclusiveOrExpression.TYPE_),
                                                                                                            var("xorHead")),
                                                                                                        Optionals.bind(
                                                                                                            Lists.head(
                                                                                                                var("ands")),
                                                                                                            lambda(
                                                                                                                "andHead",
                                                                                                                let(
                                                                                                                    "eqs",
                                                                                                                    apply(
                                                                                                                        unwrap(AndExpression.TYPE_),
                                                                                                                        var("andHead")),
                                                                                                                    Optionals.bind(
                                                                                                                        Lists.head(
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
                                                                                                                                                                                                                                var("p")))))))))))))))))))))))))))))))))))))))))))))))))))));

    public static final Def javaExpressionToJavaUnaryExpression = def("javaExpressionToJavaUnaryExpression")
        .lam("e")
        .to(() ->
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
                                    var("e")))))));

    public static final Def javaFieldAccessToJavaExpression = def("javaFieldAccessToJavaExpression")
        .lam("fa")
        .to(() ->
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
                                                                                                            var("fa")))))))))))))))))))))))));

    public static final Def javaIdentifier = def("javaIdentifier")
        .lam("s")
        .to(() ->
                wrap(Identifier.TYPE_,
                    apply(ref(Utils.sanitizeJavaName), var("s"))));

    public static final Def javaIdentifierToJavaExpression = def("javaIdentifierToJavaExpression")
        .lam("id")
        .to(() ->
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
                                                                                                            var("id")))))))))))))))))))))))));

    public static final Def javaIdentifierToJavaExpressionName = def("javaIdentifierToJavaExpressionName")
        .lam("id")
        .to(() ->
                record(ExpressionName.TYPE_,
                    field(ExpressionName.QUALIFIER, nothing()),
                    field(ExpressionName.IDENTIFIER, var("id"))));

    public static final Def javaIdentifierToJavaRelationalExpression = def("javaIdentifierToJavaRelationalExpression")
        .lam("id")
        .to(() ->
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
                                                    var("id")))))))))));

    public static final Def javaIdentifierToJavaUnaryExpression = def("javaIdentifierToJavaUnaryExpression")
        .lam("id")
        .to(() ->
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.NAME,
                            record(ExpressionName.TYPE_,
                                field(ExpressionName.QUALIFIER, nothing()),
                                field(ExpressionName.IDENTIFIER, var("id")))))));

    public static final Def javaInstanceOf = def("javaInstanceOf")
        .lam("lhs").lam("rhs")
        .to(() ->
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.INSTANCEOF_EXPRESSION,
                    record(InstanceofExpression.TYPE_,
                        field(InstanceofExpression.LHS, var("lhs")),
                        field(
                            InstanceofExpression.RHS,
                            inject(InstanceofExpression_Rhs.TYPE_,
                                InstanceofExpression_Rhs.REFERENCE_TYPE,
                                var("rhs"))))));

    public static final Def javaInt = def("javaInt")
        .lam("i")
        .to(() ->
                inject(Literal.TYPE_,
                    Literal.INTEGER,
                    wrap(IntegerLiteral.TYPE_, var("i"))));

    public static final Def javaIntExpression = def("javaIntExpression")
        .lam("i")
        .to(() ->
                apply(
                    ref(Utils.javaPrimaryToJavaExpression),
                    apply(ref(Utils.javaLiteralToJavaPrimary), apply(ref(Utils.javaInt), var("i")))));

    public static final Def javaIntType = def("javaIntType")
        .to(() ->
                apply(
                ref(Utils.javaPrimitiveTypeToJavaType),
                inject(PrimitiveType.TYPE_,
                    PrimitiveType.NUMERIC,
                    inject(NumericType.TYPE_,
                        NumericType.INTEGRAL,
                        inject(IntegralType.TYPE_,
                            IntegralType.INT,
                            unit())))));

    public static final Def javaInterfaceDeclarationToJavaClassBodyDeclaration = def("javaInterfaceDeclarationToJavaClassBodyDeclaration")
        .lam("nid")
        .to(() ->
                inject(ClassBodyDeclaration.TYPE_,
                    ClassBodyDeclaration.CLASS_MEMBER,
                    inject(ClassMemberDeclaration.TYPE_,
                        ClassMemberDeclaration.INTERFACE,
                        inject(InterfaceDeclaration.TYPE_,
                            InterfaceDeclaration.NORMAL_INTERFACE,
                            var("nid")))));

    public static final Def javaLambda = def("javaLambda")
        .lam("v").lam("body")
        .to(() ->
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
                                var("body"))))));

    public static final Def javaLambdaFromBlock = def("javaLambdaFromBlock")
        .lam("v").lam("block")
        .to(() ->
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
                                var("block"))))));

    public static final Def javaLiteralToJavaExpression = def("javaLiteralToJavaExpression")
        .lam("lit")
        .to(() ->
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
                                                                                                            var("lit")))))))))))))))))))))))));

    public static final Def javaLiteralToJavaMultiplicativeExpression = def("javaLiteralToJavaMultiplicativeExpression")
        .lam("lit")
        .to(() ->
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
                                        var("lit"))))))));

    public static final Def javaLiteralToJavaPrimary = def("javaLiteralToJavaPrimary")
        .lam("lit")
        .to(() ->
                inject(Primary.TYPE_,
                    Primary.NO_NEW_ARRAY,
                    inject(PrimaryNoNewArrayExpression.TYPE_,
                        PrimaryNoNewArrayExpression.LITERAL,
                        var("lit"))));

    public static final Def javaLiteralToJavaRelationalExpression = def("javaLiteralToJavaRelationalExpression")
        .lam("lit")
        .to(() ->
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
                                                    var("lit")))))))))));

    public static final Def javaMemberField = def("javaMemberField")
        .lam("mods").lam("jt").lam("v")
        .to(() ->
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
                                list(var("v")))))));

    public static final Def javaMethodBody = def("javaMethodBody")
        .lam("mstmts")
        .to(() ->
                Optionals.cases(
                    var("mstmts"),
                    inject(MethodBody.TYPE_,
                        MethodBody.NONE,
                        unit()),
                    lambda("stmts",
                        inject(MethodBody.TYPE_,
                            MethodBody.BLOCK,
                            wrap(Block.TYPE_, var("stmts"))))));

    public static final Def javaMethodDeclarationToJavaClassBodyDeclaration = def("javaMethodDeclarationToJavaClassBodyDeclaration")
        .lam("md")
        .to(() ->
                inject(ClassBodyDeclaration.TYPE_,
                    ClassBodyDeclaration.CLASS_MEMBER,
                    inject(ClassMemberDeclaration.TYPE_,
                        ClassMemberDeclaration.METHOD,
                        var("md"))));

    public static final Def javaMethodHeader = def("javaMethodHeader")
        .lam("tparams").lam("methodName").lam("params").lam("result")
        .to(() ->
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
                    field(MethodHeader.THROWS, nothing())));

    public static final Def javaMethodInvocationToJavaExpression = def("javaMethodInvocationToJavaExpression")
        .lam("mi")
        .to(() ->
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
                                                                                                            var("mi")))))))))))))))))))))))));

    public static final Def javaMethodInvocationToJavaPostfixExpression = def("javaMethodInvocationToJavaPostfixExpression")
        .lam("mi")
        .to(() ->
                inject(PostfixExpression.TYPE_,
                    PostfixExpression.PRIMARY,
                    inject(Primary.TYPE_,
                        Primary.NO_NEW_ARRAY,
                        inject(PrimaryNoNewArrayExpression.TYPE_,
                            PrimaryNoNewArrayExpression.METHOD_INVOCATION,
                            var("mi")))));

    public static final Def javaMethodInvocationToJavaPrimary = def("javaMethodInvocationToJavaPrimary")
        .lam("mi")
        .to(() ->
                inject(Primary.TYPE_,
                    Primary.NO_NEW_ARRAY,
                    inject(PrimaryNoNewArrayExpression.TYPE_,
                        PrimaryNoNewArrayExpression.METHOD_INVOCATION,
                        var("mi"))));

    public static final Def javaMethodInvocationToJavaStatement = def("javaMethodInvocationToJavaStatement")
        .lam("mi")
        .to(() ->
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.EXPRESSION,
                        wrap(ExpressionStatement.TYPE_,
                            inject(StatementExpression.TYPE_,
                                StatementExpression.METHOD_INVOCATION,
                                var("mi"))))));

    public static final Def javaMultiplicativeExpressionToJavaRelationalExpression = def("javaMultiplicativeExpressionToJavaRelationalExpression")
        .lam("me")
        .to(() ->
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            var("me")))));

    public static final Def javaPackageDeclaration = def("javaPackageDeclaration")
        .lam("ns")
        .to(() ->
                record(PackageDeclaration.TYPE_,
                    field(PackageDeclaration.MODIFIERS, list()),
                    field(
                        PackageDeclaration.IDENTIFIERS,
                        Lists.map(
                            lambda("s", wrap(Identifier.TYPE_, var("s"))),
                            Strings.splitOn(
                                string("."),
                                apply(unwrap(ModuleName.TYPE_), var("ns")))))));

    public static final Def javaPostfixExpressionToJavaEqualityExpression = def("javaPostfixExpressionToJavaEqualityExpression")
        .lam("pe")
        .to(() ->
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
                                            var("pe")))))))));

    public static final Def javaPostfixExpressionToJavaExpression = def("javaPostfixExpressionToJavaExpression")
        .lam("pe")
        .to(() ->
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
                                                                                                var("pe"))))))))))))))))))))));

    public static final Def javaPostfixExpressionToJavaInclusiveOrExpression = def("javaPostfixExpressionToJavaInclusiveOrExpression")
        .lam("pe")
        .to(() ->
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
                                                                    var("pe")))))))))))))));

    public static final Def javaPostfixExpressionToJavaRelationalExpression = def("javaPostfixExpressionToJavaRelationalExpression")
        .lam("pe")
        .to(() ->
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
                                        var("pe"))))))));

    public static final Def javaPostfixExpressionToJavaUnaryExpression = def("javaPostfixExpressionToJavaUnaryExpression")
        .lam("pe")
        .to(() ->
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        var("pe"))));

    public static final Def javaPrimaryToJavaExpression = def("javaPrimaryToJavaExpression")
        .lam("p")
        .to(() ->
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
                                                                                                    var("p")))))))))))))))))))))));

    public static final Def javaPrimaryToJavaUnaryExpression = def("javaPrimaryToJavaUnaryExpression")
        .lam("p")
        .to(() ->
                inject(UnaryExpression.TYPE_,
                    UnaryExpression.OTHER,
                    inject(UnaryExpressionNotPlusMinus.TYPE_,
                        UnaryExpressionNotPlusMinus.POSTFIX,
                        inject(PostfixExpression.TYPE_,
                            PostfixExpression.PRIMARY,
                            var("p")))));

    public static final Def javaPrimitiveTypeToJavaType = def("javaPrimitiveTypeToJavaType")
        .lam("pt")
        .to(() ->
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.PRIMITIVE,
                    record(PrimitiveTypeWithAnnotations.TYPE_,
                        field(PrimitiveTypeWithAnnotations.TYPE, var("pt")),
                        field(PrimitiveTypeWithAnnotations.ANNOTATIONS, list()))));

    public static final Def javaRefType = def("javaRefType")
        .lam("args").lam("pkg").lam("id")
        .to(() ->
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.CLASS_OR_INTERFACE,
                        inject(ClassOrInterfaceType.TYPE_,
                            ClassOrInterfaceType.CLASS,
                            apply(ref(Utils.javaClassType), var("args"), var("pkg"), var("id"))))));

    public static final Def javaReferenceTypeToRawType = def("javaReferenceTypeToRawType")
        .lam("rt")
        .to(() ->
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
                                                                list())))))))))))));

    public static final Def javaRelationalExpressionToJavaEqualityExpression = def("javaRelationalExpressionToJavaEqualityExpression")
        .lam("re")
        .to(() ->
                inject(EqualityExpression.TYPE_,
                    EqualityExpression.UNARY,
                    var("re")));

    public static final Def javaRelationalExpressionToJavaExpression = def("javaRelationalExpressionToJavaExpression")
        .lam("re")
        .to(() ->
                apply(
                    ref(Utils.javaEqualityExpressionToJavaExpression),
                    inject(EqualityExpression.TYPE_,
                        EqualityExpression.UNARY,
                        var("re"))));

    public static final Def javaRelationalExpressionToJavaUnaryExpression = def("javaRelationalExpressionToJavaUnaryExpression")
        .lam("re")
        .to(() ->
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
                                                                                            var("re")))))))))))))))))))));

    public static final Def javaReturnStatement = def("javaReturnStatement")
        .lam("mex")
        .to(() ->
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.RETURN,
                        wrap(ReturnStatement.TYPE_, var("mex")))));

    public static final Def javaStatementsToBlock = def("javaStatementsToBlock")
        .lam("stmts")
        .to(() ->
                wrap(Block.TYPE_,
                    Lists.map(
                        lambda("s",
                            inject(BlockStatement.TYPE_,
                                BlockStatement.STATEMENT,
                                var("s"))),
                        var("stmts"))));

    public static final Def javaString = def("javaString")
        .lam("s")
        .to(() ->
                inject(Literal.TYPE_,
                    Literal.STRING,
                    wrap(StringLiteral.TYPE_, var("s"))));

    public static final Def javaStringMultiplicativeExpression = def("javaStringMultiplicativeExpression")
        .lam("s")
        .to(() ->
                apply(
                    ref(Utils.javaLiteralToJavaMultiplicativeExpression),
                    apply(ref(Utils.javaString), var("s"))));

    public static final Def javaThis = def("javaThis")
        .to(() ->
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

    public static final Def javaThrowIllegalArgumentException = def("javaThrowIllegalArgumentException")
        .lam("args")
        .to(() ->
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
                        nothing())));

    public static final Def javaThrowIllegalStateException = def("javaThrowIllegalStateException")
        .lam("args")
        .to(() ->
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
                        nothing())));

    public static final Def javaThrowStatement = def("javaThrowStatement")
        .lam("e")
        .to(() ->
                inject(Statement.TYPE_,
                    Statement.WITHOUT_TRAILING,
                    inject(StatementWithoutTrailingSubstatement.TYPE_,
                        StatementWithoutTrailingSubstatement.THROW,
                        wrap(ThrowStatement.TYPE_, var("e")))));

    public static final Def javaTypeFromTypeName = def("javaTypeFromTypeName")
        .lam("aliases").lam("elName")
        .to(() ->
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
                                var("elName"))))));

    public static final Def javaTypeIdentifier = def("javaTypeIdentifier")
        .lam("s")
        .to(() ->
                wrap(TypeIdentifier.TYPE_,
                    wrap(Identifier.TYPE_, var("s"))));

    public static final Def javaTypeIdentifierToJavaTypeArgument = def("javaTypeIdentifierToJavaTypeArgument")
        .lam("id")
        .to(() ->
                inject(TypeArgument.TYPE_,
                    TypeArgument.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.VARIABLE,
                        record(TypeVariable.TYPE_,
                            field(TypeVariable.ANNOTATIONS, list()),
                            field(TypeVariable.IDENTIFIER, var("id"))))));

    public static final Def javaTypeName = def("javaTypeName")
        .lam("id")
        .to(() ->
                record(TypeName.TYPE_,
                    field(
                        TypeName.IDENTIFIER,
                        wrap(TypeIdentifier.TYPE_, var("id"))),
                    field(TypeName.QUALIFIER, nothing())));

    public static final Def javaTypeParameter = def("javaTypeParameter")
        .lam("v")
        .to(() ->
                record(TypeParameter.TYPE_,
                    field(TypeParameter.MODIFIERS, list()),
                    field(
                        TypeParameter.IDENTIFIER,
                        apply(ref(Utils.javaTypeIdentifier), var("v"))),
                    field(TypeParameter.BOUND, nothing())));

    public static final Def javaTypeToJavaFormalParameter = def("javaTypeToJavaFormalParameter")
        .lam("jt").lam("fname")
        .to(() ->
                inject(FormalParameter.TYPE_,
                    FormalParameter.SIMPLE,
                    record(FormalParameter_Simple.TYPE_,
                        field(FormalParameter_Simple.MODIFIERS, list()),
                        field(
                            FormalParameter_Simple.TYPE,
                            wrap(UnannType.TYPE_, var("jt"))),
                        field(
                            FormalParameter_Simple.ID,
                            apply(ref(Utils.fieldNameToJavaVariableDeclaratorId), var("fname"))))));

    public static final Def javaTypeToJavaReferenceType = def("javaTypeToJavaReferenceType")
        .lam("t").lam("cx")
        .to(() ->
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
                                        string("expected a Java reference type"))))))));

    public static final Def javaTypeToJavaResult = def("javaTypeToJavaResult")
        .lam("jt")
        .to(() ->
                inject(Result.TYPE_,
                    Result.TYPE,
                    wrap(UnannType.TYPE_, var("jt"))));

    public static final Def javaTypeToJavaTypeArgument = def("javaTypeToJavaTypeArgument")
        .lam("t")
        .to(() ->
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
                                    field(Wildcard.WILDCARD, nothing())))))));

    public static final Def javaTypeVariable = def("javaTypeVariable")
        .lam("v")
        .to(() ->
                inject(ReferenceType.TYPE_,
                    ReferenceType.VARIABLE,
                    record(TypeVariable.TYPE_,
                        field(TypeVariable.ANNOTATIONS, list()),
                        field(
                            TypeVariable.IDENTIFIER,
                            apply(
                                ref(Utils.javaTypeIdentifier),
                                hydra.dsl.Formatting.capitalize( var("v")))))));

    public static final Def javaTypeVariableToType = def("javaTypeVariableToType")
        .lam("tv")
        .to(() ->
                inject(hydra.java.syntax.Type.TYPE_,
                    hydra.java.syntax.Type.REFERENCE,
                    inject(ReferenceType.TYPE_,
                        ReferenceType.VARIABLE,
                        var("tv"))));

    public static final Def javaUnaryExpressionToJavaExpression = def("javaUnaryExpressionToJavaExpression")
        .lam("ue")
        .to(() ->
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
                                                                                        var("ue"))))))))))))))))))));

    public static final Def javaUnaryExpressionToJavaRelationalExpression = def("javaUnaryExpressionToJavaRelationalExpression")
        .lam("ue")
        .to(() ->
                inject(RelationalExpression.TYPE_,
                    RelationalExpression.SIMPLE,
                    inject(ShiftExpression.TYPE_,
                        ShiftExpression.UNARY,
                        inject(AdditiveExpression.TYPE_,
                            AdditiveExpression.UNARY,
                            inject(MultiplicativeExpression.TYPE_,
                                MultiplicativeExpression.UNARY,
                                var("ue"))))));

    public static final Def javaVariableDeclarator = def("javaVariableDeclarator")
        .lam("id").lam("minit")
        .to(() ->
                record(VariableDeclarator.TYPE_,
                    field(
                        VariableDeclarator.ID,
                        apply(ref(Utils.javaVariableDeclaratorId), var("id"))),
                    field(VariableDeclarator.INITIALIZER, var("minit"))));

    public static final Def javaVariableDeclaratorId = def("javaVariableDeclaratorId")
        .lam("id")
        .to(() ->
                record(VariableDeclaratorId.TYPE_,
                    field(VariableDeclaratorId.IDENTIFIER, var("id")),
                    field(VariableDeclaratorId.DIMS, nothing())));

    public static final Def javaVariableName = def("javaVariableName")
        .lam("name")
        .to(() ->
                apply(ref(Utils.javaIdentifier), hydra.dsl.Names.localNameOf( var("name"))));

    public static final Def lookupJavaVarName = def("lookupJavaVarName")
        .lam("aliases").lam("name")
        .to(() ->
                Optionals.cases(
                    Maps.lookup(
                        var("name"),
                        proj(Aliases.TYPE_, Aliases.VAR_RENAMES, "aliases")),
                    var("name"),
                    lambda("renamed", var("renamed"))));

    public static final Def makeConstructor = def("makeConstructor")
        .lam("aliases").lam("elName").lam("private").lam("params").lam("stmts")
        .to(() ->
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
                            field(ConstructorDeclaration.BODY, var("body"))))));

    public static final Def methodDeclaration = def("methodDeclaration")
        .lam("mods").lam("tparams").lam("anns").lam("methodName").lam("params").lam("result").lam("stmts")
        .to(() ->
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
                            apply(ref(Utils.javaMethodBody), var("stmts"))))));

    public static final Def methodInvocation = def("methodInvocation")
        .lam("lhs").lam("methodName").lam("args")
        .to(() ->
                let("header",
                    Optionals.cases(
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
                                        Eithers.either(
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
                        field(MethodInvocation.ARGUMENTS, var("args")))));

    public static final Def methodInvocationStatic = def("methodInvocationStatic")
        .lam("self").lam("methodName").lam("args")
        .to(() ->
                apply(
                    ref(Utils.methodInvocation),
                    just(left(apply(ref(Utils.javaIdentifierToJavaExpressionName), var("self")))),
                    var("methodName"),
                    var("args")));

    public static final Def methodInvocationStaticWithTypeArgs = def("methodInvocationStaticWithTypeArgs")
        .to(() ->
                lambda(
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

    public static final Def nameToJavaClassType = def("nameToJavaClassType")
        .lam("aliases").lam("qualify").lam("args").lam("name").lam("mlocal")
        .to(() ->
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
                        field(ClassType.ARGUMENTS, var("args")))));

    public static final Def nameToJavaName = def("nameToJavaName")
        .lam("aliases").lam("name")
        .to(() ->
                let(
                    field("qn",
                        hydra.dsl.Names.qualifyName( var("name"))),
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
                        Optionals.cases(
                            var("ns_"),
                            wrap(Identifier.TYPE_, var("local")),
                            lambda("gname",
                                let(
                                    field("parts",
                                        Optionals.cases(
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
                                        Strings.join(string("."), var("allParts")))))))));

    public static final Def nameToJavaReferenceType = def("nameToJavaReferenceType")
        .lam("aliases").lam("qualify").lam("args").lam("name").lam("mlocal")
        .to(() ->
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
                            var("mlocal")))));

    public static final Def nameToJavaTypeIdentifier = def("nameToJavaTypeIdentifier")
        .lam("aliases").lam("qualify").lam("name")
        .to(() ->
                Pairs.first(
                    apply(
                        ref(Utils.nameToQualifiedJavaName),
                        var("aliases"),
                        var("qualify"),
                        var("name"),
                        nothing())));

    public static final Def nameToQualifiedJavaName = def("nameToQualifiedJavaName")
        .lam("aliases").lam("qualify").lam("name").lam("mlocal")
        .to(() ->
                let(
                    field("qn",
                        hydra.dsl.Names.qualifyName( var("name"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("alias",
                        Optionals.cases(
                            var("ns_"),
                            nothing(),
                            lambda("n",
                                just(
                                    Optionals.cases(
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
                            Optionals.cases(
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
                            Optionals.cases(
                                var("mlocal"),
                                apply(ref(Utils.sanitizeJavaName), var("local")),
                                lambda("l",
                                    Strings.concat2(
                                        Strings.concat2(
                                            apply(ref(Utils.sanitizeJavaName), var("local")),
                                            string(".")),
                                        apply(ref(Utils.sanitizeJavaName), var("l"))))))),
                    pair(var("jid"), var("pkg"))));

    public static final Def overrideAnnotation = def("overrideAnnotation")
        .to(() ->
                inject(Annotation.TYPE_,
                Annotation.MARKER,
                wrap(MarkerAnnotation.TYPE_,
                    apply(
                        ref(Utils.javaTypeName),
                        wrap(Identifier.TYPE_, string("Override"))))));

    public static final Def referenceTypeToResult = def("referenceTypeToResult")
        .lam("rt")
        .to(() ->
                apply(
                    ref(Utils.javaTypeToJavaResult),
                    inject(hydra.java.syntax.Type.TYPE_,
                        hydra.java.syntax.Type.REFERENCE,
                        var("rt"))));

    public static final Def sanitizeJavaName = def("sanitizeJavaName")
        .lam("name")
        .to(() ->
                Logic.ifElse(
                    apply(ref(Utils.isEscaped), var("name")),
                    apply(ref(Utils.unescape), var("name")),
                    Logic.ifElse(
                        Equality.equal(var("name"), string("_")),
                        string("ignored"),
                        hydra.dsl.Formatting.sanitizeWithUnderscores(
                            Language.reservedWords(),
                            var("name")))));

    public static final Def suppressWarningsUncheckedAnnotation = def("suppressWarningsUncheckedAnnotation")
        .to(() ->
                inject(Annotation.TYPE_,
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

    public static final Def toAcceptMethod = def("toAcceptMethod")
        .lam("abstract").lam("vtparams")
        .to(() ->
                let(
                    binds(
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
                        var("body"))));

    public static final Def toAssignStmt = def("toAssignStmt")
        .lam("fname")
        .to(() ->
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
                    apply(ref(Utils.javaAssignmentStatement), var("lhs"), var("rhs"))));

    public static final Def toJavaArrayType = def("toJavaArrayType")
        .lam("t").lam("cx")
        .to(() ->
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
                                        string("don't know how to make Java type into array type"))))))));

    public static final Def typeParameterToReferenceType = def("typeParameterToReferenceType")
        .lam("tp")
        .to(() ->
                apply(
                    ref(Utils.javaTypeVariable),
                    apply(
                        unwrap(Identifier.TYPE_),
                        apply(
                            unwrap(TypeIdentifier.TYPE_),
                            proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp")))));

    public static final Def typeParameterToTypeArgument = def("typeParameterToTypeArgument")
        .lam("tp")
        .to(() ->
                apply(
                    ref(Utils.javaTypeIdentifierToJavaTypeArgument),
                    proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp")));

    public static final Def unTypeParameter = def("unTypeParameter")
        .lam("tp")
        .to(() ->
                apply(
                    unwrap(Identifier.TYPE_),
                    apply(
                        unwrap(TypeIdentifier.TYPE_),
                        proj(TypeParameter.TYPE_, TypeParameter.IDENTIFIER, "tp"))));

    public static final Def unescape = def("unescape")
        .lam("s")
        .to(() ->
                Strings.fromList(Lists.drop(int32(1), Strings.toList(var("s")))));

    public static final Def uniqueVarName = def("uniqueVarName")
        .lam("aliases").lam("name")
        .to(() ->
                Logic.ifElse(
                    Sets.member(
                        var("name"),
                        proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")),
                    apply(
                        ref(Utils.uniqueVarName_go),
                        var("aliases"),
                        apply(unwrap(Name.TYPE_), var("name")),
                        int32(2)),
                    var("name")));

    public static final Def uniqueVarName_go = def("uniqueVarName_go")
        .lam("aliases").lam("base").lam("n")
        .to(() ->
                let("candidate",
                    wrap(Name.TYPE_,
                        Strings.concat2(var("base"), Literals.showInt32(var("n")))),
                    Logic.ifElse(
                        Sets.member(
                            var("candidate"),
                            proj(Aliases.TYPE_, Aliases.IN_SCOPE_JAVA_VARS, "aliases")),
                        apply(
                            ref(Utils.uniqueVarName_go),
                            var("aliases"),
                            var("base"),
                            Math_.add(var("n"), int32(1))),
                        var("candidate"))));

    public static final Def varDeclarationStatement = def("varDeclarationStatement")
        .lam("id").lam("rhs")
        .to(() ->
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
                                                var("rhs"))))))))));

    public static final Def variableDeclarationStatement = def("variableDeclarationStatement")
        .lam("aliases").lam("jtype").lam("id").lam("rhs")
        .to(() ->
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
                                    list(var("vdec"))))))));

    public static final Def variableToJavaIdentifier = def("variableToJavaIdentifier")
        .lam("name")
        .to(() ->
                let("v",
                    apply(unwrap(Name.TYPE_), var("name")),
                    Logic.ifElse(
                        Equality.equal(var("v"), string("_")),
                        wrap(Identifier.TYPE_, string("ignored")),
                        wrap(Identifier.TYPE_,
                            apply(ref(Utils.sanitizeJavaName), var("v"))))));

    public static final Def variantClassName = def("variantClassName")
        .lam("qualify").lam("elName").lam("fname")
        .to(() ->
                let(
                    field("qn",
                        hydra.dsl.Names.qualifyName( var("elName"))),
                    field("ns_",
                        proj(QualifiedName.TYPE_, QualifiedName.MODULE_NAME, "qn")),
                    field("local",
                        proj(QualifiedName.TYPE_, QualifiedName.LOCAL, "qn")),
                    field("flocal",
                        hydra.dsl.Formatting.capitalize(
                            apply(unwrap(Name.TYPE_), var("fname")))),
                    field("local1",
                        Logic.ifElse(
                            var("qualify"),
                            Strings.concat2(Strings.concat2(var("local"), string(".")), var("flocal")),
                            Logic.ifElse(
                                Equality.equal(var("flocal"), var("local")),
                                Strings.concat2(var("flocal"), string("_")),
                                var("flocal")))),
                    hydra.dsl.Names.unqualifyName(
                        record(QualifiedName.TYPE_,
                            field(QualifiedName.MODULE_NAME, var("ns_")),
                            field(QualifiedName.LOCAL, var("local1"))))));

    public static final Def visitorTypeVariable = def("visitorTypeVariable")
        .to(() ->
                apply(ref(Utils.javaTypeVariable), string("r")));









    private static final Def[] ALL_DEFS = {
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
            overlayJavaLibPackageAliases,
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
            visitorTypeVariable
    };

    static {
        Defs.checkComplete(Utils.class, ALL_DEFS);
    }

    private static final List<Definition> DEFINITIONS = definitionsOf(ALL_DEFS);

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
            Optional.given("Java utilities for constructing Java syntax trees"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none())),
        DEPENDENCIES,
        DEFINITIONS);
}
