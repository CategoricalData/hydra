package hydra.dsl.meta;

import hydra.core.*;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Meta-DSL for constructing Hydra core terms and types as first-class values.
 *
 * <p>Mirrors Hydra.Dsl.Meta.Core (Haskell), providing phantom-typed constructors,
 * accessors, and modifiers for all core Hydra types.
 *
 * <p>Usage: {@code import hydra.dsl.meta.Core;}
 */
public interface Core {

    // ============================================================
    // AnnotatedTerm
    // ============================================================

    static Expr<AnnotatedTerm> annotatedTerm(TTerm<Term> body, TTerm<?> annotation) {
        return record(AnnotatedTerm.TYPE_,
                field(AnnotatedTerm.BODY, body),
                field(AnnotatedTerm.ANNOTATION, annotation));
    }

    static Expr<Term> annotatedTermBody(TTerm<AnnotatedTerm> at) {
        return apply(project(AnnotatedTerm.TYPE_, AnnotatedTerm.BODY), at);
    }

    static Expr<?> annotatedTermAnnotation(TTerm<AnnotatedTerm> at) {
        return apply(project(AnnotatedTerm.TYPE_, AnnotatedTerm.ANNOTATION), at);
    }

    static Expr<AnnotatedTerm> annotatedTermWithBody(TTerm<AnnotatedTerm> at, TTerm<Term> body) {
        return annotatedTerm(body, annotatedTermAnnotation(at));
    }

    // ============================================================
    // AnnotatedType
    // ============================================================

    static Expr<AnnotatedType> annotatedType(TTerm<Type> body, TTerm<?> annotation) {
        return record(AnnotatedType.TYPE_,
                field(AnnotatedType.BODY, body),
                field(AnnotatedType.ANNOTATION, annotation));
    }

    static Expr<Type> annotatedTypeBody(TTerm<AnnotatedType> at) {
        return apply(project(AnnotatedType.TYPE_, AnnotatedType.BODY), at);
    }

    static Expr<?> annotatedTypeAnnotation(TTerm<AnnotatedType> at) {
        return apply(project(AnnotatedType.TYPE_, AnnotatedType.ANNOTATION), at);
    }

    // ============================================================
    // Application
    // ============================================================

    static Expr<Application> application(TTerm<Term> function, TTerm<Term> argument) {
        return record(Application.TYPE_,
                field(Application.FUNCTION, function),
                field(Application.ARGUMENT, argument));
    }

    static Expr<Term> applicationFunction(TTerm<Application> app) {
        return apply(project(Application.TYPE_, Application.FUNCTION), app);
    }

    static Expr<Term> applicationArgument(TTerm<Application> app) {
        return apply(project(Application.TYPE_, Application.ARGUMENT), app);
    }

    // ============================================================
    // ApplicationType
    // ============================================================

    static Expr<ApplicationType> applicationType(TTerm<Type> function, TTerm<Type> argument) {
        return record(ApplicationType.TYPE_,
                field(ApplicationType.FUNCTION, function),
                field(ApplicationType.ARGUMENT, argument));
    }

    static Expr<Type> applicationTypeFunction(TTerm<ApplicationType> app) {
        return apply(project(ApplicationType.TYPE_, ApplicationType.FUNCTION), app);
    }

    static Expr<Type> applicationTypeArgument(TTerm<ApplicationType> app) {
        return apply(project(ApplicationType.TYPE_, ApplicationType.ARGUMENT), app);
    }

    // ============================================================
    // Binding
    // ============================================================

    static Expr<Binding> binding(TTerm<Name> nameTerm, TTerm<Term> term, TTerm<?> mtype) {
        return record(Binding.TYPE_,
                field(Binding.NAME, nameTerm),
                field(Binding.TERM, term),
                field(Binding.TYPE, mtype));
    }

    static Expr<Name> bindingName(TTerm<Binding> b) {
        return apply(project(Binding.TYPE_, Binding.NAME), b);
    }

    static Expr<Term> bindingTerm(TTerm<Binding> b) {
        return apply(project(Binding.TYPE_, Binding.TERM), b);
    }

    static Expr<?> bindingType(TTerm<Binding> b) {
        return apply(project(Binding.TYPE_, Binding.TYPE), b);
    }

    static Expr<Binding> bindingWithTerm(TTerm<Binding> b, TTerm<Term> term) {
        return binding(bindingName(b), term, bindingType(b));
    }

    // ============================================================
    // CaseStatement
    // ============================================================

    static Expr<CaseStatement> caseStatement(TTerm<Name> typeName, TTerm<?> defaultTerm, TTerm<?> cases) {
        return record(CaseStatement.TYPE_,
                field(CaseStatement.TYPE_NAME, typeName),
                field(CaseStatement.DEFAULT, defaultTerm),
                field(CaseStatement.CASES, cases));
    }

    static Expr<Name> caseStatementTypeName(TTerm<CaseStatement> cs) {
        return apply(project(CaseStatement.TYPE_, CaseStatement.TYPE_NAME), cs);
    }

    static Expr<?> caseStatementDefault(TTerm<CaseStatement> cs) {
        return apply(project(CaseStatement.TYPE_, CaseStatement.DEFAULT), cs);
    }

    static Expr<?> caseStatementCases(TTerm<CaseStatement> cs) {
        return apply(project(CaseStatement.TYPE_, CaseStatement.CASES), cs);
    }

    // ============================================================
    // EitherType
    // ============================================================

    static Expr<EitherType> eitherType(TTerm<Type> left, TTerm<Type> right) {
        return record(EitherType.TYPE_,
                field(EitherType.LEFT, left),
                field(EitherType.RIGHT, right));
    }

    static Expr<Type> eitherTypeLeft(TTerm<EitherType> et) {
        return apply(project(EitherType.TYPE_, EitherType.LEFT), et);
    }

    static Expr<Type> eitherTypeRight(TTerm<EitherType> et) {
        return apply(project(EitherType.TYPE_, EitherType.RIGHT), et);
    }

    // ============================================================
    // PairType
    // ============================================================

    static Expr<PairType> pairType(TTerm<Type> first, TTerm<Type> second) {
        return record(PairType.TYPE_,
                field(PairType.FIRST, first),
                field(PairType.SECOND, second));
    }

    static Expr<Type> pairTypeFirst(TTerm<PairType> pt) {
        return apply(project(PairType.TYPE_, PairType.FIRST), pt);
    }

    static Expr<Type> pairTypeSecond(TTerm<PairType> pt) {
        return apply(project(PairType.TYPE_, PairType.SECOND), pt);
    }

    // ============================================================
    // Elimination
    // ============================================================

    static Expr<Elimination> eliminationRecord(TTerm<Projection> proj) {
        return inject(Elimination.TYPE_, Elimination.RECORD, proj);
    }

    static Expr<Elimination> eliminationUnion(TTerm<CaseStatement> cs) {
        return inject(Elimination.TYPE_, Elimination.UNION, cs);
    }

    static Expr<Elimination> eliminationWrap(TTerm<Name> name) {
        return inject(Elimination.TYPE_, Elimination.WRAP, name);
    }

    // ============================================================
    // Field
    // ============================================================

    static Expr<Field> fieldRecord(TTerm<Name> name, TTerm<Term> term) {
        return record(Field.TYPE_,
                field(Field.NAME, name),
                field(Field.TERM, term));
    }

    static Expr<Name> fieldName(TTerm<Field> f) {
        return apply(project(Field.TYPE_, Field.NAME), f);
    }

    static Expr<Term> fieldTerm(TTerm<Field> f) {
        return apply(project(Field.TYPE_, Field.TERM), f);
    }

    static Expr<Field> fieldWithTerm(TTerm<Term> t, TTerm<Field> ft) {
        return fieldRecord(fieldName(ft), t);
    }

    // ============================================================
    // FieldType
    // ============================================================

    static Expr<FieldType> fieldType(TTerm<Name> name, TTerm<Type> type) {
        return record(FieldType.TYPE_,
                field(FieldType.NAME, name),
                field(FieldType.TYPE, type));
    }

    static Expr<Name> fieldTypeName(TTerm<FieldType> ft) {
        return apply(project(FieldType.TYPE_, FieldType.NAME), ft);
    }

    static Expr<Type> fieldTypeType(TTerm<FieldType> ft) {
        return apply(project(FieldType.TYPE_, FieldType.TYPE), ft);
    }

    static Expr<FieldType> fieldTypeWithType(TTerm<FieldType> ft, TTerm<Type> t) {
        return fieldType(fieldTypeName(ft), t);
    }

    // ============================================================
    // FloatType
    // ============================================================

    static Expr<FloatType> floatTypeBigfloat() {
        return injectUnit(FloatType.TYPE_, FloatType.BIGFLOAT);
    }

    static Expr<FloatType> floatTypeFloat32() {
        return injectUnit(FloatType.TYPE_, FloatType.FLOAT32);
    }

    static Expr<FloatType> floatTypeFloat64() {
        return injectUnit(FloatType.TYPE_, FloatType.FLOAT64);
    }

    // ============================================================
    // FloatValue
    // ============================================================

    static Expr<FloatValue> floatValueBigfloat(TTerm<?> v) {
        return inject(FloatValue.TYPE_, FloatValue.BIGFLOAT, v);
    }

    static Expr<FloatValue> floatValueFloat32(TTerm<?> v) {
        return inject(FloatValue.TYPE_, FloatValue.FLOAT32, v);
    }

    static Expr<FloatValue> floatValueFloat64(TTerm<?> v) {
        return inject(FloatValue.TYPE_, FloatValue.FLOAT64, v);
    }

    // ============================================================
    // ForallType
    // ============================================================

    static Expr<ForallType> forallType(TTerm<Name> parameter, TTerm<Type> body) {
        return record(ForallType.TYPE_,
                field(ForallType.PARAMETER, parameter),
                field(ForallType.BODY, body));
    }

    static Expr<Name> forallTypeParameter(TTerm<ForallType> ft) {
        return apply(project(ForallType.TYPE_, ForallType.PARAMETER), ft);
    }

    static Expr<Type> forallTypeBody(TTerm<ForallType> ft) {
        return apply(project(ForallType.TYPE_, ForallType.BODY), ft);
    }

    // ============================================================
    // Function
    // ============================================================

    static Expr<Function> functionElimination(TTerm<Elimination> elim) {
        return inject(Function.TYPE_, Function.ELIMINATION, elim);
    }

    static Expr<Function> functionLambda(TTerm<Lambda> lam) {
        return inject(Function.TYPE_, Function.LAMBDA, lam);
    }

    static Expr<Function> functionPrimitive(TTerm<Name> name) {
        return inject(Function.TYPE_, Function.PRIMITIVE, name);
    }

    // ============================================================
    // FunctionType
    // ============================================================

    static Expr<FunctionType> functionType(TTerm<Type> domain, TTerm<Type> codomain) {
        return record(FunctionType.TYPE_,
                field(FunctionType.DOMAIN, domain),
                field(FunctionType.CODOMAIN, codomain));
    }

    static Expr<Type> functionTypeDomain(TTerm<FunctionType> ft) {
        return apply(project(FunctionType.TYPE_, FunctionType.DOMAIN), ft);
    }

    static Expr<Type> functionTypeCodomain(TTerm<FunctionType> ft) {
        return apply(project(FunctionType.TYPE_, FunctionType.CODOMAIN), ft);
    }

    // ============================================================
    // Injection
    // ============================================================

    static Expr<Injection> injection(TTerm<Name> typeName, TTerm<Field> fld) {
        return record(Injection.TYPE_,
                field(Injection.TYPE_NAME, typeName),
                field(Injection.FIELD, fld));
    }

    static Expr<Name> injectionTypeName(TTerm<Injection> inj) {
        return apply(project(Injection.TYPE_, Injection.TYPE_NAME), inj);
    }

    static Expr<Field> injectionField(TTerm<Injection> inj) {
        return apply(project(Injection.TYPE_, Injection.FIELD), inj);
    }

    // ============================================================
    // IntegerType
    // ============================================================

    static Expr<IntegerType> integerTypeBigint() {
        return injectUnit(IntegerType.TYPE_, IntegerType.BIGINT);
    }

    static Expr<IntegerType> integerTypeInt8() {
        return injectUnit(IntegerType.TYPE_, IntegerType.INT8);
    }

    static Expr<IntegerType> integerTypeInt16() {
        return injectUnit(IntegerType.TYPE_, IntegerType.INT16);
    }

    static Expr<IntegerType> integerTypeInt32() {
        return injectUnit(IntegerType.TYPE_, IntegerType.INT32);
    }

    static Expr<IntegerType> integerTypeInt64() {
        return injectUnit(IntegerType.TYPE_, IntegerType.INT64);
    }

    static Expr<IntegerType> integerTypeUint8() {
        return injectUnit(IntegerType.TYPE_, IntegerType.UINT8);
    }

    static Expr<IntegerType> integerTypeUint16() {
        return injectUnit(IntegerType.TYPE_, IntegerType.UINT16);
    }

    static Expr<IntegerType> integerTypeUint32() {
        return injectUnit(IntegerType.TYPE_, IntegerType.UINT32);
    }

    static Expr<IntegerType> integerTypeUint64() {
        return injectUnit(IntegerType.TYPE_, IntegerType.UINT64);
    }

    // ============================================================
    // IntegerValue
    // ============================================================

    static Expr<IntegerValue> integerValueBigint(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.BIGINT, v);
    }

    static Expr<IntegerValue> integerValueInt8(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.INT8, v);
    }

    static Expr<IntegerValue> integerValueInt16(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.INT16, v);
    }

    static Expr<IntegerValue> integerValueInt32(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.INT32, v);
    }

    static Expr<IntegerValue> integerValueInt64(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.INT64, v);
    }

    static Expr<IntegerValue> integerValueUint8(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.UINT8, v);
    }

    static Expr<IntegerValue> integerValueUint16(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.UINT16, v);
    }

    static Expr<IntegerValue> integerValueUint32(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.UINT32, v);
    }

    static Expr<IntegerValue> integerValueUint64(TTerm<?> v) {
        return inject(IntegerValue.TYPE_, IntegerValue.UINT64, v);
    }

    // ============================================================
    // Lambda
    // ============================================================

    static Expr<Lambda> lambdaRecord(TTerm<Name> parameter, TTerm<?> mdom, TTerm<Term> body) {
        return record(Lambda.TYPE_,
                field(Lambda.PARAMETER, parameter),
                field(Lambda.DOMAIN, mdom),
                field(Lambda.BODY, body));
    }

    static Expr<Name> lambdaParameter(TTerm<Lambda> l) {
        return apply(project(Lambda.TYPE_, Lambda.PARAMETER), l);
    }

    static Expr<Term> lambdaBody(TTerm<Lambda> l) {
        return apply(project(Lambda.TYPE_, Lambda.BODY), l);
    }

    static Expr<?> lambdaDomain(TTerm<Lambda> l) {
        return apply(project(Lambda.TYPE_, Lambda.DOMAIN), l);
    }

    static Expr<Lambda> lambdaWithBody(TTerm<Lambda> l, TTerm<Term> body) {
        return lambdaRecord(lambdaParameter(l), lambdaDomain(l), body);
    }

    // ============================================================
    // Let
    // ============================================================

    static Expr<Let> letRecord(TTerm<?> bindings, TTerm<Term> body) {
        return record(Let.TYPE_,
                field(Let.BINDINGS, bindings),
                field(Let.BODY, body));
    }

    static Expr<?> letBindings(TTerm<Let> l) {
        return apply(project(Let.TYPE_, Let.BINDINGS), l);
    }

    static Expr<Term> letBody(TTerm<Let> l) {
        return apply(project(Let.TYPE_, Let.BODY), l);
    }

    static Expr<Let> letWithBody(TTerm<Let> l, TTerm<Term> body) {
        return letRecord(letBindings(l), body);
    }

    // ============================================================
    // Literal
    // ============================================================

    static Expr<Literal> literalBinary(TTerm<?> v) {
        return inject(Literal.TYPE_, Literal.BINARY, v);
    }

    static Expr<Literal> literalBoolean(TTerm<Boolean> v) {
        return inject(Literal.TYPE_, Literal.BOOLEAN, v);
    }

    static Expr<Literal> literalFloat(TTerm<FloatValue> v) {
        return inject(Literal.TYPE_, Literal.FLOAT, v);
    }

    static Expr<Literal> literalInteger(TTerm<IntegerValue> v) {
        return inject(Literal.TYPE_, Literal.INTEGER, v);
    }

    static Expr<Literal> literalString(TTerm<String> v) {
        return inject(Literal.TYPE_, Literal.STRING, v);
    }

    // ============================================================
    // LiteralType
    // ============================================================

    static Expr<LiteralType> literalTypeBinary() {
        return injectUnit(LiteralType.TYPE_, LiteralType.BINARY);
    }

    static Expr<LiteralType> literalTypeBoolean() {
        return injectUnit(LiteralType.TYPE_, LiteralType.BOOLEAN);
    }

    static Expr<LiteralType> literalTypeFloat(TTerm<FloatType> ft) {
        return inject(LiteralType.TYPE_, LiteralType.FLOAT, ft);
    }

    static Expr<LiteralType> literalTypeInteger(TTerm<IntegerType> it) {
        return inject(LiteralType.TYPE_, LiteralType.INTEGER, it);
    }

    static Expr<LiteralType> literalTypeString() {
        return injectUnit(LiteralType.TYPE_, LiteralType.STRING);
    }

    // ============================================================
    // MapType
    // ============================================================

    static Expr<MapType> mapType(TTerm<Type> keys, TTerm<Type> values) {
        return record(MapType.TYPE_,
                field(MapType.KEYS, keys),
                field(MapType.VALUES, values));
    }

    static Expr<Type> mapTypeKeys(TTerm<MapType> mt) {
        return apply(project(MapType.TYPE_, MapType.KEYS), mt);
    }

    static Expr<Type> mapTypeValues(TTerm<MapType> mt) {
        return apply(project(MapType.TYPE_, MapType.VALUES), mt);
    }

    // ============================================================
    // Projection
    // ============================================================

    static Expr<Projection> projectionRecord(TTerm<Name> tname, TTerm<Name> fname) {
        return record(Projection.TYPE_,
                field(Projection.TYPE_NAME, tname),
                field(Projection.FIELD, fname));
    }

    static Expr<Name> projectionTypeName(TTerm<Projection> p) {
        return apply(project(Projection.TYPE_, Projection.TYPE_NAME), p);
    }

    static Expr<Name> projectionField(TTerm<Projection> p) {
        return apply(project(Projection.TYPE_, Projection.FIELD), p);
    }

    // ============================================================
    // Record
    // ============================================================

    static Expr<hydra.core.Record> recordValue(TTerm<Name> typeName, TTerm<?> fields) {
        return record(hydra.core.Record.TYPE_,
                field(hydra.core.Record.TYPE_NAME, typeName),
                field(hydra.core.Record.FIELDS, fields));
    }

    static Expr<Name> recordTypeName(TTerm<hydra.core.Record> r) {
        return apply(project(hydra.core.Record.TYPE_, hydra.core.Record.TYPE_NAME), r);
    }

    static Expr<?> recordFields(TTerm<hydra.core.Record> r) {
        return apply(project(hydra.core.Record.TYPE_, hydra.core.Record.FIELDS), r);
    }

    // ============================================================
    // Term variants (injection functions)
    // ============================================================

    static Expr<Term> termAnnotated(TTerm<AnnotatedTerm> at) {
        return inject(Term.TYPE_, Term.ANNOTATED, at);
    }

    static Expr<Term> termApplication(TTerm<Application> app) {
        return inject(Term.TYPE_, Term.APPLICATION, app);
    }

    static Expr<Term> termEither(TTerm<?> e) {
        return inject(Term.TYPE_, Term.EITHER, e);
    }

    static Expr<Term> termFunction(TTerm<Function> f) {
        return inject(Term.TYPE_, Term.FUNCTION, f);
    }

    static Expr<Term> termLet(TTerm<Let> l) {
        return inject(Term.TYPE_, Term.LET, l);
    }

    static Expr<Term> termList(TTerm<?> l) {
        return inject(Term.TYPE_, Term.LIST, l);
    }

    static Expr<Term> termLiteral(TTerm<Literal> lit) {
        return inject(Term.TYPE_, Term.LITERAL, lit);
    }

    static Expr<Term> termMap(TTerm<?> m) {
        return inject(Term.TYPE_, Term.MAP, m);
    }

    static Expr<Term> termMaybe(TTerm<?> m) {
        return inject(Term.TYPE_, Term.MAYBE, m);
    }

    static Expr<Term> termPair(TTerm<?> p) {
        return inject(Term.TYPE_, Term.PAIR, p);
    }

    static Expr<Term> termRecord(TTerm<hydra.core.Record> r) {
        return inject(Term.TYPE_, Term.RECORD, r);
    }

    static Expr<Term> termSet(TTerm<?> s) {
        return inject(Term.TYPE_, Term.SET, s);
    }

    static Expr<Term> termTypeApplication(TTerm<TypeApplicationTerm> ta) {
        return inject(Term.TYPE_, Term.TYPE_APPLICATION, ta);
    }

    static Expr<Term> termTypeLambda(TTerm<TypeLambda> tl) {
        return inject(Term.TYPE_, Term.TYPE_LAMBDA, tl);
    }

    static Expr<Term> termUnion(TTerm<Injection> inj) {
        return inject(Term.TYPE_, Term.UNION, inj);
    }

    static Expr<Term> termUnit() {
        return injectUnit(Term.TYPE_, Term.UNIT);
    }

    static Expr<Term> termVariable(TTerm<Name> name) {
        return inject(Term.TYPE_, Term.VARIABLE, name);
    }

    static Expr<Term> termWrap(TTerm<WrappedTerm> wt) {
        return inject(Term.TYPE_, Term.WRAP, wt);
    }

    // ============================================================
    // Type variants (injection functions)
    // ============================================================

    static Expr<Type> typeAnnotated(TTerm<AnnotatedType> at) {
        return inject(Type.TYPE_, Type.ANNOTATED, at);
    }

    static Expr<Type> typeApplication(TTerm<ApplicationType> app) {
        return inject(Type.TYPE_, Type.APPLICATION, app);
    }

    static Expr<Type> typeEither(TTerm<EitherType> et) {
        return inject(Type.TYPE_, Type.EITHER, et);
    }

    static Expr<Type> typeForall(TTerm<ForallType> ft) {
        return inject(Type.TYPE_, Type.FORALL, ft);
    }

    static Expr<Type> typeFunction(TTerm<FunctionType> ft) {
        return inject(Type.TYPE_, Type.FUNCTION, ft);
    }

    static Expr<Type> typeList(TTerm<Type> t) {
        return inject(Type.TYPE_, Type.LIST, t);
    }

    static Expr<Type> typeLiteral(TTerm<LiteralType> lt) {
        return inject(Type.TYPE_, Type.LITERAL, lt);
    }

    static Expr<Type> typeMap(TTerm<MapType> mt) {
        return inject(Type.TYPE_, Type.MAP, mt);
    }

    static Expr<Type> typeMaybe(TTerm<Type> t) {
        return inject(Type.TYPE_, Type.MAYBE, t);
    }

    static Expr<Type> typePair(TTerm<PairType> pt) {
        return inject(Type.TYPE_, Type.PAIR, pt);
    }

    static Expr<Type> typeRecord(TTerm<?> fields) {
        return inject(Type.TYPE_, Type.RECORD, fields);
    }

    static Expr<Type> typeSet(TTerm<Type> t) {
        return inject(Type.TYPE_, Type.SET, t);
    }

    static Expr<Type> typeUnion(TTerm<?> fields) {
        return inject(Type.TYPE_, Type.UNION, fields);
    }

    static Expr<Type> typeUnit() {
        return injectUnit(Type.TYPE_, Type.UNIT);
    }

    static Expr<Type> typeVariable(TTerm<Name> name) {
        return inject(Type.TYPE_, Type.VARIABLE, name);
    }

    static Expr<Type> typeWrap(TTerm<Type> t) {
        return inject(Type.TYPE_, Type.WRAP, t);
    }

    // ============================================================
    // TypeApplicationTerm
    // ============================================================

    static Expr<TypeApplicationTerm> typeApplicationTerm(TTerm<Term> body, TTerm<Type> type) {
        return record(TypeApplicationTerm.TYPE_,
                field(TypeApplicationTerm.BODY, body),
                field(TypeApplicationTerm.TYPE, type));
    }

    static Expr<Term> typeApplicationTermBody(TTerm<TypeApplicationTerm> tt) {
        return apply(project(TypeApplicationTerm.TYPE_, TypeApplicationTerm.BODY), tt);
    }

    static Expr<Type> typeApplicationTermType(TTerm<TypeApplicationTerm> tt) {
        return apply(project(TypeApplicationTerm.TYPE_, TypeApplicationTerm.TYPE), tt);
    }

    // ============================================================
    // TypeLambda
    // ============================================================

    static Expr<TypeLambda> typeLambda(TTerm<Name> parameter, TTerm<Term> body) {
        return record(TypeLambda.TYPE_,
                field(TypeLambda.PARAMETER, parameter),
                field(TypeLambda.BODY, body));
    }

    static Expr<Name> typeLambdaParameter(TTerm<TypeLambda> tl) {
        return apply(project(TypeLambda.TYPE_, TypeLambda.PARAMETER), tl);
    }

    static Expr<Term> typeLambdaBody(TTerm<TypeLambda> tl) {
        return apply(project(TypeLambda.TYPE_, TypeLambda.BODY), tl);
    }

    static Expr<TypeLambda> typeLambdaWithBody(TTerm<TypeLambda> tl, TTerm<Term> body) {
        return typeLambda(typeLambdaParameter(tl), body);
    }

    // ============================================================
    // TypeScheme
    // ============================================================

    static Expr<TypeScheme> typeScheme(TTerm<?> variables, TTerm<Type> body, TTerm<?> constraints) {
        return record(TypeScheme.TYPE_,
                field(TypeScheme.VARIABLES, variables),
                field(TypeScheme.TYPE, body),
                field(TypeScheme.CONSTRAINTS, constraints));
    }

    static Expr<?> typeSchemeVariables(TTerm<TypeScheme> ts) {
        return apply(project(TypeScheme.TYPE_, TypeScheme.VARIABLES), ts);
    }

    static Expr<Type> typeSchemeType(TTerm<TypeScheme> ts) {
        return apply(project(TypeScheme.TYPE_, TypeScheme.TYPE), ts);
    }

    static Expr<?> typeSchemeConstraints(TTerm<TypeScheme> ts) {
        return apply(project(TypeScheme.TYPE_, TypeScheme.CONSTRAINTS), ts);
    }

    // ============================================================
    // WrappedTerm
    // ============================================================

    static Expr<WrappedTerm> wrappedTerm(TTerm<Name> typeName, TTerm<Term> body) {
        return record(WrappedTerm.TYPE_,
                field(WrappedTerm.TYPE_NAME, typeName),
                field(WrappedTerm.BODY, body));
    }

    static Expr<Name> wrappedTermTypeName(TTerm<WrappedTerm> wt) {
        return apply(project(WrappedTerm.TYPE_, WrappedTerm.TYPE_NAME), wt);
    }

    static Expr<Term> wrappedTermBody(TTerm<WrappedTerm> wt) {
        return apply(project(WrappedTerm.TYPE_, WrappedTerm.BODY), wt);
    }

    // ============================================================
    // Name helpers
    // ============================================================

    /**
     * Wrap a string as a Name.
     */
    static Expr<Name> name(TTerm<String> s) {
        return Phantoms.wrap(Name.TYPE_, s);
    }

    /**
     * Lift a Java Name value into a TTerm&lt;Name&gt;.
     */
    static Expr<Name> nameLift(Name n) {
        return Phantoms.wrap(Name.TYPE_, Phantoms.string(n.value));
    }

    /**
     * Unwrap a Name to its underlying string.
     */
    static Expr<String> unName(TTerm<Name> n) {
        return apply(Phantoms.unwrap(Name.TYPE_), n);
    }
}
