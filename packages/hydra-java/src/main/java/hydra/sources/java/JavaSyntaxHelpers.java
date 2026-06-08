package hydra.sources.java;
import hydra.dsl.java.Syntax;
import hydra.dsl.meta.Phantoms;
import hydra.java.syntax.AdditionalBound;
import hydra.java.syntax.AdditiveExpression;
import hydra.java.syntax.AdditiveExpression_Binary;
import hydra.java.syntax.Annotation;
import hydra.java.syntax.ArrayAccess;
import hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive;
import hydra.java.syntax.ArrayInitializer;
import hydra.java.syntax.ArrayType_Variant;
import hydra.java.syntax.Assignment;
import hydra.java.syntax.Block;
import hydra.java.syntax.CastExpression;
import hydra.java.syntax.CastExpression_NotPlusMinus;
import hydra.java.syntax.CastExpression_Primitive;
import hydra.java.syntax.CastExpression_RefAndBounds;
import hydra.java.syntax.ClassBodyDeclaration;
import hydra.java.syntax.ClassDeclaration;
import hydra.java.syntax.ClassInstanceCreationExpression;
import hydra.java.syntax.ClassMemberDeclaration;
import hydra.java.syntax.ConditionalAndExpression;
import hydra.java.syntax.ConditionalExpression;
import hydra.java.syntax.ConstantDeclaration;
import hydra.java.syntax.ConstructorDeclaration;
import hydra.java.syntax.Dims;
import hydra.java.syntax.ElementValue;
import hydra.java.syntax.ElementValueArrayInitializer;
import hydra.java.syntax.EqualityExpression;
import hydra.java.syntax.EqualityExpression_Binary;
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
import hydra.java.syntax.InterfaceDeclaration;
import hydra.java.syntax.InterfaceMemberDeclaration;
import hydra.java.syntax.InterfaceMethodDeclaration;
import hydra.java.syntax.InterfaceMethodModifier;
import hydra.java.syntax.LambdaParameters;
import hydra.java.syntax.Literal;
import hydra.java.syntax.MethodBody;
import hydra.java.syntax.MethodDeclaration;
import hydra.java.syntax.MethodHeader;
import hydra.java.syntax.MethodInvocation;
import hydra.java.syntax.MethodInvocation_Complex;
import hydra.java.syntax.MethodInvocation_Header;
import hydra.java.syntax.MethodInvocation_Variant;
import hydra.java.syntax.MethodModifier;
import hydra.java.syntax.MethodName;
import hydra.java.syntax.MultiplicativeExpression;
import hydra.java.syntax.MultiplicativeExpression_Binary;
import hydra.java.syntax.PostfixExpression;
import hydra.java.syntax.Primary;
import hydra.java.syntax.PrimaryNoNewArrayExpression;
import hydra.java.syntax.PrimitiveTypeWithAnnotations;
import hydra.java.syntax.ReferenceType;
import hydra.java.syntax.RelationalExpression;
import hydra.java.syntax.ReturnStatement;
import hydra.java.syntax.Statement;
import hydra.java.syntax.StatementExpression;
import hydra.java.syntax.StatementWithoutTrailingSubstatement;
import hydra.java.syntax.ThrowStatement;
import hydra.java.syntax.TypeArgument;
import hydra.java.syntax.TypeName;
import hydra.java.syntax.UnannType;
import hydra.java.syntax.UnaryExpression;
import hydra.java.syntax.UnaryExpressionNotPlusMinus;
import hydra.java.syntax.VariableDeclaratorId;
import hydra.java.syntax.VariableInitializer;
import hydra.java.syntax.VariableModifier;
import hydra.typed.TypedTerm;
import hydra.util.Optional;

import java.util.List;

import static hydra.dsl.meta.Phantoms.list;

/**
 * Convenience layer over the generated {@link Syntax} DSL. Java analogue of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Dsl/Java/Helpers.hs}.
 *
 * <p>Provides phantom-typed smart constructors and the long
 * Primary→Expression-style "lift" chains that thread one wrapper layer
 * into another. Use this when the generated single-step DSL alone is too
 * tedious; use {@link Syntax} directly for everything else.</p>
 */
public final class JavaSyntaxHelpers {
    private JavaSyntaxHelpers() {}

    // ---- Wrappers whose signatures differ from the generated versions ----

    /** {@code arrayInitializer} wrapping a list of variable initializers. */
    public static TypedTerm<ArrayInitializer> arrayInitializer(TypedTerm<List<VariableInitializer>> variableInitializers) {
        return Phantoms.wrap("hydra.java.syntax.ArrayInitializer", variableInitializers);
    }

    public static TypedTerm<ClassInstanceCreationExpression> classInstanceCreationExpression(
            TypedTerm<Optional<Expression>> qual,
            TypedTerm<hydra.java.syntax.UnqualifiedClassInstanceCreationExpression> unqual) {
        return Phantoms.record("hydra.java.syntax.ClassInstanceCreationExpression",
            Phantoms.field("qualifier", qual),
            Phantoms.field("expression", unqual));
    }

    public static TypedTerm<LambdaParameters> lambdaParametersTuple(TypedTerm<List<FormalParameter>> formalParams) {
        return Phantoms.inject("hydra.java.syntax.LambdaParameters", "tuple", formalParams);
    }

    public static TypedTerm<VariableDeclaratorId> variableDeclaratorId(TypedTerm<Identifier> ident, TypedTerm<Optional<Dims>> dims) {
        return Phantoms.record("hydra.java.syntax.VariableDeclaratorId",
            Phantoms.field("identifier", ident),
            Phantoms.field("dims", dims));
    }

    // ---- Expression-tower conversions ----

    public static TypedTerm<Primary> literalToPrimary(TypedTerm<Literal> lit) {
        return Syntax.primaryNoNewArray(primaryLiteral(lit));
    }

    public static TypedTerm<Expression> literalToExpression(TypedTerm<Literal> lit) {
        return primaryToExpression(literalToPrimary(lit));
    }

    public static TypedTerm<RelationalExpression> literalToRelationalExpression(TypedTerm<Literal> lit) {
        return Syntax.relationalExpressionSimple(
            Syntax.shiftExpressionUnary(
                Syntax.additiveExpressionUnary(
                    Syntax.multiplicativeExpressionUnary(
                        Syntax.unaryExpressionOther(
                            Syntax.unaryExpressionNotPlusMinusPostfix(
                                Syntax.postfixExpressionPrimary(literalToPrimary(lit))))))));
    }

    public static TypedTerm<MultiplicativeExpression> literalToMultiplicativeExpression(TypedTerm<Literal> lit) {
        return Syntax.multiplicativeExpressionUnary(
            Syntax.unaryExpressionOther(
                Syntax.unaryExpressionNotPlusMinusPostfix(
                    Syntax.postfixExpressionPrimary(literalToPrimary(lit)))));
    }

    public static TypedTerm<Expression> primaryToExpression(TypedTerm<Primary> p) {
        return postfixToExpression(Syntax.postfixExpressionPrimary(p));
    }

    public static TypedTerm<Expression> postfixToExpression(TypedTerm<PostfixExpression> pe) {
        TypedTerm<UnaryExpression> unary = Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(pe));
        TypedTerm<MultiplicativeExpression> mul = Syntax.multiplicativeExpressionUnary(unary);
        TypedTerm<AdditiveExpression> add = Syntax.additiveExpressionUnary(mul);
        TypedTerm<hydra.java.syntax.ShiftExpression> shift = Syntax.shiftExpressionUnary(add);
        TypedTerm<RelationalExpression> rel = Syntax.relationalExpressionSimple(shift);
        TypedTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(rel);
        TypedTerm<hydra.java.syntax.AndExpression> and_ = Syntax.andExpression(list(eq));
        TypedTerm<hydra.java.syntax.ExclusiveOrExpression> xor = Syntax.exclusiveOrExpression(list(and_));
        TypedTerm<InclusiveOrExpression> ior = Syntax.inclusiveOrExpression(list(xor));
        TypedTerm<ConditionalAndExpression> cand = Syntax.conditionalAndExpression(list(ior));
        TypedTerm<hydra.java.syntax.ConditionalOrExpression> cor = Syntax.conditionalOrExpression(list(cand));
        TypedTerm<ConditionalExpression> cond = Syntax.conditionalExpressionSimple(cor);
        TypedTerm<hydra.java.syntax.AssignmentExpression> ae = Syntax.assignmentExpressionConditional(cond);
        return Syntax.expressionAssignment(ae);
    }

    public static TypedTerm<InclusiveOrExpression> postfixToInclusiveOr(TypedTerm<PostfixExpression> pe) {
        return Syntax.inclusiveOrExpression(list(
            Syntax.exclusiveOrExpression(list(
                Syntax.andExpression(list(
                    Syntax.equalityExpressionUnary(
                        Syntax.relationalExpressionSimple(
                            Syntax.shiftExpressionUnary(
                                Syntax.additiveExpressionUnary(
                                    Syntax.multiplicativeExpressionUnary(
                                        Syntax.unaryExpressionOther(
                                            Syntax.unaryExpressionNotPlusMinusPostfix(pe)))))))))))));
    }

    public static TypedTerm<RelationalExpression> postfixToRelationalExpression(TypedTerm<PostfixExpression> pe) {
        return Syntax.relationalExpressionSimple(
            Syntax.shiftExpressionUnary(
                Syntax.additiveExpressionUnary(
                    Syntax.multiplicativeExpressionUnary(
                        Syntax.unaryExpressionOther(
                            Syntax.unaryExpressionNotPlusMinusPostfix(pe))))));
    }

    public static TypedTerm<Expression> unaryToExpression(TypedTerm<UnaryExpression> ue) {
        TypedTerm<MultiplicativeExpression> mul = Syntax.multiplicativeExpressionUnary(ue);
        TypedTerm<AdditiveExpression> add = Syntax.additiveExpressionUnary(mul);
        TypedTerm<hydra.java.syntax.ShiftExpression> shift = Syntax.shiftExpressionUnary(add);
        TypedTerm<RelationalExpression> rel = Syntax.relationalExpressionSimple(shift);
        TypedTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(rel);
        return conditionalAndToExpression(
            Syntax.conditionalAndExpression(list(
                Syntax.inclusiveOrExpression(list(
                    Syntax.exclusiveOrExpression(list(
                        Syntax.andExpression(list(eq)))))))));
    }

    public static TypedTerm<Expression> additiveToExpression(TypedTerm<AdditiveExpression> ae) {
        TypedTerm<hydra.java.syntax.ShiftExpression> shift = Syntax.shiftExpressionUnary(ae);
        TypedTerm<RelationalExpression> rel = Syntax.relationalExpressionSimple(shift);
        TypedTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(rel);
        return equalityToExpression(eq);
    }

    public static TypedTerm<Expression> equalityToExpression(TypedTerm<EqualityExpression> ee) {
        return conditionalAndToExpression(
            Syntax.conditionalAndExpression(list(equalityToInclusiveOr(ee))));
    }

    public static TypedTerm<InclusiveOrExpression> equalityToInclusiveOr(TypedTerm<EqualityExpression> ee) {
        return Syntax.inclusiveOrExpression(list(
            Syntax.exclusiveOrExpression(list(
                Syntax.andExpression(list(ee))))));
    }

    public static TypedTerm<Expression> conditionalAndToExpression(TypedTerm<ConditionalAndExpression> cae) {
        TypedTerm<hydra.java.syntax.ConditionalOrExpression> cor = Syntax.conditionalOrExpression(list(cae));
        return Syntax.expressionAssignment(
            Syntax.assignmentExpressionConditional(
                Syntax.conditionalExpressionSimple(cor)));
    }

    public static TypedTerm<Expression> castExpressionToExpression(TypedTerm<CastExpression> ce) {
        TypedTerm<UnaryExpression> unary = Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusCast(ce));
        return unaryToExpression(unary);
    }

    public static TypedTerm<Expression> expressionNameToExpression(TypedTerm<ExpressionName> en) {
        return postfixToExpression(Syntax.postfixExpressionName(en));
    }

    public static TypedTerm<Expression> identifierToExpression(TypedTerm<Identifier> id) {
        return expressionNameToExpression(Syntax.expressionName(Phantoms.<hydra.java.syntax.AmbiguousName>nothing(), id));
    }

    public static TypedTerm<RelationalExpression> identifierToRelationalExpression(TypedTerm<Identifier> id) {
        return postfixToRelationalExpression(
            Syntax.postfixExpressionName(
                Syntax.expressionName(Phantoms.<hydra.java.syntax.AmbiguousName>nothing(), id)));
    }

    public static TypedTerm<UnaryExpression> identifierToUnary(TypedTerm<Identifier> id) {
        return Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(
                Syntax.postfixExpressionName(
                    Syntax.expressionName(Phantoms.<hydra.java.syntax.AmbiguousName>nothing(), id))));
    }

    public static TypedTerm<Expression> fieldAccessToExpression(TypedTerm<FieldAccess> fa) {
        return primaryToExpression(Syntax.primaryNoNewArray(primaryFieldAccess(fa)));
    }

    public static TypedTerm<Expression> methodInvocationToExpression(TypedTerm<MethodInvocation> mi) {
        return primaryToExpression(Syntax.primaryNoNewArray(primaryMethodInvocation(mi)));
    }

    public static TypedTerm<Primary> methodInvocationToPrimary(TypedTerm<MethodInvocation> mi) {
        return Syntax.primaryNoNewArray(primaryMethodInvocation(mi));
    }

    public static TypedTerm<PostfixExpression> methodInvocationToPostfix(TypedTerm<MethodInvocation> mi) {
        return Syntax.postfixExpressionPrimary(Syntax.primaryNoNewArray(primaryMethodInvocation(mi)));
    }

    public static TypedTerm<Statement> methodInvocationToStatement(TypedTerm<MethodInvocation> mi) {
        return Syntax.statementWithoutTrailing(
            stmtExpression(Syntax.expressionStatement(stmtExprMethodInvocation(mi))));
    }

    public static TypedTerm<Primary> expressionToPrimary(TypedTerm<Expression> e) {
        return Syntax.primaryNoNewArray(primaryParens(e));
    }

    public static TypedTerm<UnaryExpression> expressionToUnary(TypedTerm<Expression> e) {
        return Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(
                Syntax.postfixExpressionPrimary(expressionToPrimary(e))));
    }

    public static TypedTerm<Primary> primaryToUnary(TypedTerm<UnaryExpression> ue) {
        return Syntax.primaryNoNewArray(primaryParens(unaryToExpression(ue)));
    }

    public static TypedTerm<UnaryExpression> relationalToUnary(TypedTerm<RelationalExpression> re) {
        TypedTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(re);
        TypedTerm<hydra.java.syntax.AndExpression> and_ = Syntax.andExpression(list(eq));
        TypedTerm<hydra.java.syntax.ExclusiveOrExpression> xor = Syntax.exclusiveOrExpression(list(and_));
        TypedTerm<InclusiveOrExpression> ior = Syntax.inclusiveOrExpression(list(xor));
        TypedTerm<ConditionalAndExpression> cand = Syntax.conditionalAndExpression(list(ior));
        TypedTerm<hydra.java.syntax.ConditionalOrExpression> cor = Syntax.conditionalOrExpression(list(cand));
        TypedTerm<ConditionalExpression> cond = Syntax.conditionalExpressionSimple(cor);
        TypedTerm<hydra.java.syntax.AssignmentExpression> aexp = Syntax.assignmentExpressionConditional(cond);
        TypedTerm<Expression> expr = Syntax.expressionAssignment(aexp);
        return Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(
                Syntax.postfixExpressionPrimary(
                    Syntax.primaryNoNewArray(primaryParens(expr)))));
    }

    // ---- Binary expression constructors ----

    public static TypedTerm<AdditiveExpression_Binary> additiveExpressionBinary(
            TypedTerm<AdditiveExpression> lhs, TypedTerm<MultiplicativeExpression> rhs) {
        return Phantoms.record("hydra.java.syntax.AdditiveExpression_Binary",
            Phantoms.field("lhs", lhs),
            Phantoms.field("rhs", rhs));
    }

    public static TypedTerm<EqualityExpression_Binary> equalityExpressionBinary(
            TypedTerm<EqualityExpression> lhs, TypedTerm<RelationalExpression> rhs) {
        return Phantoms.record("hydra.java.syntax.EqualityExpression_Binary",
            Phantoms.field("lhs", lhs),
            Phantoms.field("rhs", rhs));
    }

    public static TypedTerm<MultiplicativeExpression_Binary> multiplicativeExpressionBinary(
            TypedTerm<MultiplicativeExpression> lhs, TypedTerm<UnaryExpression> rhs) {
        return Phantoms.record("hydra.java.syntax.MultiplicativeExpression_Binary",
            Phantoms.field("lhs", lhs),
            Phantoms.field("rhs", rhs));
    }

    // ---- Primary constructors (injections into PrimaryNoNewArrayExpression) ----

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryLiteral(TypedTerm<Literal> lit) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "literal", lit);
    }

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryParens(TypedTerm<Expression> e) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "parens", e);
    }

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryFieldAccess(TypedTerm<FieldAccess> fa) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "fieldAccess", fa);
    }

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryMethodInvocation(TypedTerm<MethodInvocation> mi) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "methodInvocation", mi);
    }

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryThis() {
        return Phantoms.injectUnit("hydra.java.syntax.PrimaryNoNewArrayExpression", "this");
    }

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryClassInstance(TypedTerm<ClassInstanceCreationExpression> ci) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "classInstance", ci);
    }

    public static TypedTerm<PrimaryNoNewArrayExpression> primaryArrayAccess(TypedTerm<ArrayAccess> aa) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "arrayAccess", aa);
    }

    // ---- Statement constructors ----

    public static TypedTerm<StatementWithoutTrailingSubstatement> stmtExpression(TypedTerm<ExpressionStatement> es) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "expression", es);
    }

    public static TypedTerm<StatementWithoutTrailingSubstatement> stmtReturn(TypedTerm<ReturnStatement> rs) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "return", rs);
    }

    public static TypedTerm<StatementWithoutTrailingSubstatement> stmtThrow(TypedTerm<ThrowStatement> ts) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "throw", ts);
    }

    public static TypedTerm<StatementWithoutTrailingSubstatement> stmtEmpty() {
        return Phantoms.injectUnit("hydra.java.syntax.StatementWithoutTrailingSubstatement", "empty");
    }

    public static TypedTerm<StatementWithoutTrailingSubstatement> stmtBlock(TypedTerm<Block> b) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "block", b);
    }

    public static TypedTerm<StatementExpression> stmtExprAssignment(TypedTerm<Assignment> a) {
        return Phantoms.inject("hydra.java.syntax.StatementExpression", "assignment", a);
    }

    public static TypedTerm<StatementExpression> stmtExprMethodInvocation(TypedTerm<MethodInvocation> mi) {
        return Phantoms.inject("hydra.java.syntax.StatementExpression", "methodInvocation", mi);
    }

    public static TypedTerm<StatementExpression> stmtExprClassInstance(TypedTerm<ClassInstanceCreationExpression> ci) {
        return Phantoms.inject("hydra.java.syntax.StatementExpression", "classInstanceCreation", ci);
    }

    // ---- Other constructors ----

    public static TypedTerm<ArrayCreationExpressionWithInitializer_Primitive> arrayCreationExpressionPrimitiveArray_(
            TypedTerm<PrimitiveTypeWithAnnotations> pt, TypedTerm<List<Dims>> dims, TypedTerm<ArrayInitializer> arr) {
        return Phantoms.record("hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive",
            Phantoms.field("type", pt),
            Phantoms.field("dims", dims),
            Phantoms.field("array", arr));
    }

    public static TypedTerm<ArrayType_Variant> arrayTypeVariantPrimitive(TypedTerm<PrimitiveTypeWithAnnotations> pta) {
        return Phantoms.inject("hydra.java.syntax.ArrayType_Variant", "primitive", pta);
    }

    public static TypedTerm<CastExpression_NotPlusMinus> castExpressionNotPlusMinus_(
            TypedTerm<CastExpression_RefAndBounds> rb, TypedTerm<UnaryExpressionNotPlusMinus> expr) {
        return Phantoms.record("hydra.java.syntax.CastExpression_NotPlusMinus",
            Phantoms.field("refAndBounds", rb),
            Phantoms.field("expression", expr));
    }

    public static TypedTerm<CastExpression_Primitive> castExpressionPrimitive_(
            TypedTerm<PrimitiveTypeWithAnnotations> pt, TypedTerm<UnaryExpression> expr) {
        return Phantoms.record("hydra.java.syntax.CastExpression_Primitive",
            Phantoms.field("type", pt),
            Phantoms.field("expression", expr));
    }

    public static TypedTerm<CastExpression_RefAndBounds> castExpressionRefAndBounds(
            TypedTerm<ReferenceType> rt, TypedTerm<List<AdditionalBound>> bounds) {
        return Phantoms.record("hydra.java.syntax.CastExpression_RefAndBounds",
            Phantoms.field("type", rt),
            Phantoms.field("bounds", bounds));
    }

    public static TypedTerm<FieldAccess_Qualifier> fieldAccessQualifierPrimary(TypedTerm<Primary> p) {
        return Phantoms.inject("hydra.java.syntax.FieldAccess_Qualifier", "primary", p);
    }

    public static TypedTerm<RelationalExpression> relationalExpressionInstanceOf(TypedTerm<InstanceofExpression> ie) {
        return Phantoms.inject("hydra.java.syntax.RelationalExpression", "instanceofExpression", ie);
    }

    public static TypedTerm<InstanceofExpression> relationalExpressionInstanceOf_(
            TypedTerm<RelationalExpression> expr, TypedTerm<ReferenceType> rt) {
        return Phantoms.record("hydra.java.syntax.InstanceofExpression",
            Phantoms.field("lhs", expr),
            Phantoms.field("rhs",
                Phantoms.<hydra.java.syntax.InstanceofExpression_Rhs>inject(
                    "hydra.java.syntax.InstanceofExpression_Rhs", "referenceType", rt)));
    }

    public static TypedTerm<MethodInvocation> methodInvocation_(
            TypedTerm<MethodInvocation_Header> header, TypedTerm<List<Expression>> args) {
        return Phantoms.record("hydra.java.syntax.MethodInvocation",
            Phantoms.field("header", header),
            Phantoms.field("arguments", args));
    }

    public static TypedTerm<MethodInvocation_Header> methodInvocationHeaderSimple(TypedTerm<MethodName> mn) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Header", "simple", mn);
    }

    public static TypedTerm<MethodInvocation_Header> methodInvocationHeaderComplex(TypedTerm<MethodInvocation_Complex> mc) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Header", "complex", mc);
    }

    public static TypedTerm<MethodInvocation_Complex> methodInvocationComplex(
            TypedTerm<MethodInvocation_Variant> variant, TypedTerm<List<TypeArgument>> targs, TypedTerm<Identifier> ident) {
        return Phantoms.record("hydra.java.syntax.MethodInvocation_Complex",
            Phantoms.field("variant", variant),
            Phantoms.field("typeArguments", targs),
            Phantoms.field("identifier", ident));
    }

    public static TypedTerm<MethodInvocation_Variant> methodInvocationVariantExpression(TypedTerm<ExpressionName> en) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Variant", "expression", en);
    }

    public static TypedTerm<MethodInvocation_Variant> methodInvocationVariantPrimary(TypedTerm<Primary> p) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Variant", "primary", p);
    }

    public static TypedTerm<MethodInvocation_Variant> methodInvocationVariantType(TypedTerm<TypeName> tn) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Variant", "type", tn);
    }

    public static TypedTerm<MethodDeclaration> methodDeclaration_(
            TypedTerm<List<Annotation>> anns,
            TypedTerm<List<MethodModifier>> mods,
            TypedTerm<MethodHeader> header,
            TypedTerm<MethodBody> body) {
        return Phantoms.record("hydra.java.syntax.MethodDeclaration",
            Phantoms.field("annotations", anns),
            Phantoms.field("modifiers", mods),
            Phantoms.field("header", header),
            Phantoms.field("body", body));
    }

    public static TypedTerm<InterfaceMethodDeclaration> interfaceMethodDeclaration_(
            TypedTerm<List<InterfaceMethodModifier>> mods,
            TypedTerm<MethodHeader> header,
            TypedTerm<MethodBody> body) {
        return Phantoms.record("hydra.java.syntax.InterfaceMethodDeclaration",
            Phantoms.field("modifiers", mods),
            Phantoms.field("header", header),
            Phantoms.field("body", body));
    }

    public static TypedTerm<FormalParameter_Simple> formalParameterSimple_(
            TypedTerm<List<VariableModifier>> mods, TypedTerm<UnannType> ut, TypedTerm<VariableDeclaratorId> vid) {
        return Phantoms.record("hydra.java.syntax.FormalParameter_Simple",
            Phantoms.field("modifiers", mods),
            Phantoms.field("type", ut),
            Phantoms.field("id", vid));
    }

    public static TypedTerm<ClassBodyDeclaration> classBodyDeclClassMember(TypedTerm<ClassMemberDeclaration> cm) {
        return Phantoms.inject("hydra.java.syntax.ClassBodyDeclaration", "classMember", cm);
    }

    public static TypedTerm<ClassBodyDeclaration> classBodyDeclConstructor(TypedTerm<ConstructorDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.ClassBodyDeclaration", "constructorDeclaration", cd);
    }

    public static TypedTerm<ClassMemberDeclaration> classMemberDeclField(TypedTerm<FieldDeclaration> fd) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "field", fd);
    }

    public static TypedTerm<ClassMemberDeclaration> classMemberDeclMethod(TypedTerm<MethodDeclaration> md) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "method", md);
    }

    public static TypedTerm<ClassMemberDeclaration> classMemberDeclClass(TypedTerm<ClassDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "class", cd);
    }

    public static TypedTerm<ClassMemberDeclaration> classMemberDeclInterface(TypedTerm<InterfaceDeclaration> id) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "interface", id);
    }

    public static TypedTerm<InterfaceMemberDeclaration> interfaceMemberDeclConstant(TypedTerm<ConstantDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.InterfaceMemberDeclaration", "constant", cd);
    }

    public static TypedTerm<InterfaceMemberDeclaration> interfaceMemberDeclInterfaceMethod(TypedTerm<InterfaceMethodDeclaration> imd) {
        return Phantoms.inject("hydra.java.syntax.InterfaceMemberDeclaration", "interfaceMethod", imd);
    }

    public static TypedTerm<InterfaceMemberDeclaration> interfaceMemberDeclClass(TypedTerm<ClassDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.InterfaceMemberDeclaration", "class", cd);
    }

    public static TypedTerm<ElementValue> elementValueConditional(TypedTerm<ConditionalExpression> ce) {
        return Phantoms.inject("hydra.java.syntax.ElementValue", "conditionalExpression", ce);
    }

    public static TypedTerm<ElementValue> elementValueArray(TypedTerm<ElementValueArrayInitializer> evai) {
        return Phantoms.inject("hydra.java.syntax.ElementValue", "elementValueArrayInitializer", evai);
    }

    public static TypedTerm<VariableInitializer> variableInitializerArray(TypedTerm<ArrayInitializer> ai) {
        return Phantoms.inject("hydra.java.syntax.VariableInitializer", "arrayInitializer", ai);
    }
}
