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
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

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
    public static TTerm<ArrayInitializer> arrayInitializer(TTerm<List<VariableInitializer>> variableInitializers) {
        return Phantoms.wrap("hydra.java.syntax.ArrayInitializer", variableInitializers);
    }

    public static TTerm<ClassInstanceCreationExpression> classInstanceCreationExpression(
            TTerm<Maybe<Expression>> qual,
            TTerm<hydra.java.syntax.UnqualifiedClassInstanceCreationExpression> unqual) {
        return Phantoms.record("hydra.java.syntax.ClassInstanceCreationExpression",
            Phantoms.field("qualifier", qual),
            Phantoms.field("expression", unqual));
    }

    public static TTerm<LambdaParameters> lambdaParametersTuple(TTerm<List<FormalParameter>> formalParams) {
        return Phantoms.inject("hydra.java.syntax.LambdaParameters", "tuple", formalParams);
    }

    public static TTerm<VariableDeclaratorId> variableDeclaratorId(TTerm<Identifier> ident, TTerm<Maybe<Dims>> dims) {
        return Phantoms.record("hydra.java.syntax.VariableDeclaratorId",
            Phantoms.field("identifier", ident),
            Phantoms.field("dims", dims));
    }

    // ---- Expression-tower conversions ----

    public static TTerm<Primary> literalToPrimary(TTerm<Literal> lit) {
        return Syntax.primaryNoNewArray(primaryLiteral(lit));
    }

    public static TTerm<Expression> literalToExpression(TTerm<Literal> lit) {
        return primaryToExpression(literalToPrimary(lit));
    }

    public static TTerm<RelationalExpression> literalToRelationalExpression(TTerm<Literal> lit) {
        return Syntax.relationalExpressionSimple(
            Syntax.shiftExpressionUnary(
                Syntax.additiveExpressionUnary(
                    Syntax.multiplicativeExpressionUnary(
                        Syntax.unaryExpressionOther(
                            Syntax.unaryExpressionNotPlusMinusPostfix(
                                Syntax.postfixExpressionPrimary(literalToPrimary(lit))))))));
    }

    public static TTerm<MultiplicativeExpression> literalToMultiplicativeExpression(TTerm<Literal> lit) {
        return Syntax.multiplicativeExpressionUnary(
            Syntax.unaryExpressionOther(
                Syntax.unaryExpressionNotPlusMinusPostfix(
                    Syntax.postfixExpressionPrimary(literalToPrimary(lit)))));
    }

    public static TTerm<Expression> primaryToExpression(TTerm<Primary> p) {
        return postfixToExpression(Syntax.postfixExpressionPrimary(p));
    }

    public static TTerm<Expression> postfixToExpression(TTerm<PostfixExpression> pe) {
        TTerm<UnaryExpression> unary = Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(pe));
        TTerm<MultiplicativeExpression> mul = Syntax.multiplicativeExpressionUnary(unary);
        TTerm<AdditiveExpression> add = Syntax.additiveExpressionUnary(mul);
        TTerm<hydra.java.syntax.ShiftExpression> shift = Syntax.shiftExpressionUnary(add);
        TTerm<RelationalExpression> rel = Syntax.relationalExpressionSimple(shift);
        TTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(rel);
        TTerm<hydra.java.syntax.AndExpression> and_ = Syntax.andExpression(list(eq));
        TTerm<hydra.java.syntax.ExclusiveOrExpression> xor = Syntax.exclusiveOrExpression(list(and_));
        TTerm<InclusiveOrExpression> ior = Syntax.inclusiveOrExpression(list(xor));
        TTerm<ConditionalAndExpression> cand = Syntax.conditionalAndExpression(list(ior));
        TTerm<hydra.java.syntax.ConditionalOrExpression> cor = Syntax.conditionalOrExpression(list(cand));
        TTerm<ConditionalExpression> cond = Syntax.conditionalExpressionSimple(cor);
        TTerm<hydra.java.syntax.AssignmentExpression> ae = Syntax.assignmentExpressionConditional(cond);
        return Syntax.expressionAssignment(ae);
    }

    public static TTerm<InclusiveOrExpression> postfixToInclusiveOr(TTerm<PostfixExpression> pe) {
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

    public static TTerm<RelationalExpression> postfixToRelationalExpression(TTerm<PostfixExpression> pe) {
        return Syntax.relationalExpressionSimple(
            Syntax.shiftExpressionUnary(
                Syntax.additiveExpressionUnary(
                    Syntax.multiplicativeExpressionUnary(
                        Syntax.unaryExpressionOther(
                            Syntax.unaryExpressionNotPlusMinusPostfix(pe))))));
    }

    public static TTerm<Expression> unaryToExpression(TTerm<UnaryExpression> ue) {
        TTerm<MultiplicativeExpression> mul = Syntax.multiplicativeExpressionUnary(ue);
        TTerm<AdditiveExpression> add = Syntax.additiveExpressionUnary(mul);
        TTerm<hydra.java.syntax.ShiftExpression> shift = Syntax.shiftExpressionUnary(add);
        TTerm<RelationalExpression> rel = Syntax.relationalExpressionSimple(shift);
        TTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(rel);
        return conditionalAndToExpression(
            Syntax.conditionalAndExpression(list(
                Syntax.inclusiveOrExpression(list(
                    Syntax.exclusiveOrExpression(list(
                        Syntax.andExpression(list(eq)))))))));
    }

    public static TTerm<Expression> additiveToExpression(TTerm<AdditiveExpression> ae) {
        TTerm<hydra.java.syntax.ShiftExpression> shift = Syntax.shiftExpressionUnary(ae);
        TTerm<RelationalExpression> rel = Syntax.relationalExpressionSimple(shift);
        TTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(rel);
        return equalityToExpression(eq);
    }

    public static TTerm<Expression> equalityToExpression(TTerm<EqualityExpression> ee) {
        return conditionalAndToExpression(
            Syntax.conditionalAndExpression(list(equalityToInclusiveOr(ee))));
    }

    public static TTerm<InclusiveOrExpression> equalityToInclusiveOr(TTerm<EqualityExpression> ee) {
        return Syntax.inclusiveOrExpression(list(
            Syntax.exclusiveOrExpression(list(
                Syntax.andExpression(list(ee))))));
    }

    public static TTerm<Expression> conditionalAndToExpression(TTerm<ConditionalAndExpression> cae) {
        TTerm<hydra.java.syntax.ConditionalOrExpression> cor = Syntax.conditionalOrExpression(list(cae));
        return Syntax.expressionAssignment(
            Syntax.assignmentExpressionConditional(
                Syntax.conditionalExpressionSimple(cor)));
    }

    public static TTerm<Expression> castExpressionToExpression(TTerm<CastExpression> ce) {
        TTerm<UnaryExpression> unary = Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusCast(ce));
        return unaryToExpression(unary);
    }

    public static TTerm<Expression> expressionNameToExpression(TTerm<ExpressionName> en) {
        return postfixToExpression(Syntax.postfixExpressionName(en));
    }

    public static TTerm<Expression> identifierToExpression(TTerm<Identifier> id) {
        return expressionNameToExpression(Syntax.expressionName(Phantoms.<hydra.java.syntax.AmbiguousName>nothing(), id));
    }

    public static TTerm<RelationalExpression> identifierToRelationalExpression(TTerm<Identifier> id) {
        return postfixToRelationalExpression(
            Syntax.postfixExpressionName(
                Syntax.expressionName(Phantoms.<hydra.java.syntax.AmbiguousName>nothing(), id)));
    }

    public static TTerm<UnaryExpression> identifierToUnary(TTerm<Identifier> id) {
        return Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(
                Syntax.postfixExpressionName(
                    Syntax.expressionName(Phantoms.<hydra.java.syntax.AmbiguousName>nothing(), id))));
    }

    public static TTerm<Expression> fieldAccessToExpression(TTerm<FieldAccess> fa) {
        return primaryToExpression(Syntax.primaryNoNewArray(primaryFieldAccess(fa)));
    }

    public static TTerm<Expression> methodInvocationToExpression(TTerm<MethodInvocation> mi) {
        return primaryToExpression(Syntax.primaryNoNewArray(primaryMethodInvocation(mi)));
    }

    public static TTerm<Primary> methodInvocationToPrimary(TTerm<MethodInvocation> mi) {
        return Syntax.primaryNoNewArray(primaryMethodInvocation(mi));
    }

    public static TTerm<PostfixExpression> methodInvocationToPostfix(TTerm<MethodInvocation> mi) {
        return Syntax.postfixExpressionPrimary(Syntax.primaryNoNewArray(primaryMethodInvocation(mi)));
    }

    public static TTerm<Statement> methodInvocationToStatement(TTerm<MethodInvocation> mi) {
        return Syntax.statementWithoutTrailing(
            stmtExpression(Syntax.expressionStatement(stmtExprMethodInvocation(mi))));
    }

    public static TTerm<Primary> expressionToPrimary(TTerm<Expression> e) {
        return Syntax.primaryNoNewArray(primaryParens(e));
    }

    public static TTerm<UnaryExpression> expressionToUnary(TTerm<Expression> e) {
        return Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(
                Syntax.postfixExpressionPrimary(expressionToPrimary(e))));
    }

    public static TTerm<Primary> primaryToUnary(TTerm<UnaryExpression> ue) {
        return Syntax.primaryNoNewArray(primaryParens(unaryToExpression(ue)));
    }

    public static TTerm<UnaryExpression> relationalToUnary(TTerm<RelationalExpression> re) {
        TTerm<EqualityExpression> eq = Syntax.equalityExpressionUnary(re);
        TTerm<hydra.java.syntax.AndExpression> and_ = Syntax.andExpression(list(eq));
        TTerm<hydra.java.syntax.ExclusiveOrExpression> xor = Syntax.exclusiveOrExpression(list(and_));
        TTerm<InclusiveOrExpression> ior = Syntax.inclusiveOrExpression(list(xor));
        TTerm<ConditionalAndExpression> cand = Syntax.conditionalAndExpression(list(ior));
        TTerm<hydra.java.syntax.ConditionalOrExpression> cor = Syntax.conditionalOrExpression(list(cand));
        TTerm<ConditionalExpression> cond = Syntax.conditionalExpressionSimple(cor);
        TTerm<hydra.java.syntax.AssignmentExpression> aexp = Syntax.assignmentExpressionConditional(cond);
        TTerm<Expression> expr = Syntax.expressionAssignment(aexp);
        return Syntax.unaryExpressionOther(
            Syntax.unaryExpressionNotPlusMinusPostfix(
                Syntax.postfixExpressionPrimary(
                    Syntax.primaryNoNewArray(primaryParens(expr)))));
    }

    // ---- Binary expression constructors ----

    public static TTerm<AdditiveExpression_Binary> additiveExpressionBinary(
            TTerm<AdditiveExpression> lhs, TTerm<MultiplicativeExpression> rhs) {
        return Phantoms.record("hydra.java.syntax.AdditiveExpression_Binary",
            Phantoms.field("lhs", lhs),
            Phantoms.field("rhs", rhs));
    }

    public static TTerm<EqualityExpression_Binary> equalityExpressionBinary(
            TTerm<EqualityExpression> lhs, TTerm<RelationalExpression> rhs) {
        return Phantoms.record("hydra.java.syntax.EqualityExpression_Binary",
            Phantoms.field("lhs", lhs),
            Phantoms.field("rhs", rhs));
    }

    public static TTerm<MultiplicativeExpression_Binary> multiplicativeExpressionBinary(
            TTerm<MultiplicativeExpression> lhs, TTerm<UnaryExpression> rhs) {
        return Phantoms.record("hydra.java.syntax.MultiplicativeExpression_Binary",
            Phantoms.field("lhs", lhs),
            Phantoms.field("rhs", rhs));
    }

    // ---- Primary constructors (injections into PrimaryNoNewArrayExpression) ----

    public static TTerm<PrimaryNoNewArrayExpression> primaryLiteral(TTerm<Literal> lit) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "literal", lit);
    }

    public static TTerm<PrimaryNoNewArrayExpression> primaryParens(TTerm<Expression> e) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "parens", e);
    }

    public static TTerm<PrimaryNoNewArrayExpression> primaryFieldAccess(TTerm<FieldAccess> fa) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "fieldAccess", fa);
    }

    public static TTerm<PrimaryNoNewArrayExpression> primaryMethodInvocation(TTerm<MethodInvocation> mi) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "methodInvocation", mi);
    }

    public static TTerm<PrimaryNoNewArrayExpression> primaryThis() {
        return Phantoms.injectUnit("hydra.java.syntax.PrimaryNoNewArrayExpression", "this");
    }

    public static TTerm<PrimaryNoNewArrayExpression> primaryClassInstance(TTerm<ClassInstanceCreationExpression> ci) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "classInstance", ci);
    }

    public static TTerm<PrimaryNoNewArrayExpression> primaryArrayAccess(TTerm<ArrayAccess> aa) {
        return Phantoms.inject("hydra.java.syntax.PrimaryNoNewArrayExpression", "arrayAccess", aa);
    }

    // ---- Statement constructors ----

    public static TTerm<StatementWithoutTrailingSubstatement> stmtExpression(TTerm<ExpressionStatement> es) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "expression", es);
    }

    public static TTerm<StatementWithoutTrailingSubstatement> stmtReturn(TTerm<ReturnStatement> rs) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "return", rs);
    }

    public static TTerm<StatementWithoutTrailingSubstatement> stmtThrow(TTerm<ThrowStatement> ts) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "throw", ts);
    }

    public static TTerm<StatementWithoutTrailingSubstatement> stmtEmpty() {
        return Phantoms.injectUnit("hydra.java.syntax.StatementWithoutTrailingSubstatement", "empty");
    }

    public static TTerm<StatementWithoutTrailingSubstatement> stmtBlock(TTerm<Block> b) {
        return Phantoms.inject("hydra.java.syntax.StatementWithoutTrailingSubstatement", "block", b);
    }

    public static TTerm<StatementExpression> stmtExprAssignment(TTerm<Assignment> a) {
        return Phantoms.inject("hydra.java.syntax.StatementExpression", "assignment", a);
    }

    public static TTerm<StatementExpression> stmtExprMethodInvocation(TTerm<MethodInvocation> mi) {
        return Phantoms.inject("hydra.java.syntax.StatementExpression", "methodInvocation", mi);
    }

    public static TTerm<StatementExpression> stmtExprClassInstance(TTerm<ClassInstanceCreationExpression> ci) {
        return Phantoms.inject("hydra.java.syntax.StatementExpression", "classInstanceCreation", ci);
    }

    // ---- Other constructors ----

    public static TTerm<ArrayCreationExpressionWithInitializer_Primitive> arrayCreationExpressionPrimitiveArray_(
            TTerm<PrimitiveTypeWithAnnotations> pt, TTerm<List<Dims>> dims, TTerm<ArrayInitializer> arr) {
        return Phantoms.record("hydra.java.syntax.ArrayCreationExpressionWithInitializer_Primitive",
            Phantoms.field("type", pt),
            Phantoms.field("dims", dims),
            Phantoms.field("array", arr));
    }

    public static TTerm<ArrayType_Variant> arrayTypeVariantPrimitive(TTerm<PrimitiveTypeWithAnnotations> pta) {
        return Phantoms.inject("hydra.java.syntax.ArrayType_Variant", "primitive", pta);
    }

    public static TTerm<CastExpression_NotPlusMinus> castExpressionNotPlusMinus_(
            TTerm<CastExpression_RefAndBounds> rb, TTerm<UnaryExpressionNotPlusMinus> expr) {
        return Phantoms.record("hydra.java.syntax.CastExpression_NotPlusMinus",
            Phantoms.field("refAndBounds", rb),
            Phantoms.field("expression", expr));
    }

    public static TTerm<CastExpression_Primitive> castExpressionPrimitive_(
            TTerm<PrimitiveTypeWithAnnotations> pt, TTerm<UnaryExpression> expr) {
        return Phantoms.record("hydra.java.syntax.CastExpression_Primitive",
            Phantoms.field("type", pt),
            Phantoms.field("expression", expr));
    }

    public static TTerm<CastExpression_RefAndBounds> castExpressionRefAndBounds(
            TTerm<ReferenceType> rt, TTerm<List<AdditionalBound>> bounds) {
        return Phantoms.record("hydra.java.syntax.CastExpression_RefAndBounds",
            Phantoms.field("type", rt),
            Phantoms.field("bounds", bounds));
    }

    public static TTerm<FieldAccess_Qualifier> fieldAccessQualifierPrimary(TTerm<Primary> p) {
        return Phantoms.inject("hydra.java.syntax.FieldAccess_Qualifier", "primary", p);
    }

    public static TTerm<RelationalExpression> relationalExpressionInstanceOf(TTerm<InstanceofExpression> ie) {
        return Phantoms.inject("hydra.java.syntax.RelationalExpression", "instanceofExpression", ie);
    }

    public static TTerm<InstanceofExpression> relationalExpressionInstanceOf_(
            TTerm<RelationalExpression> expr, TTerm<ReferenceType> rt) {
        return Phantoms.record("hydra.java.syntax.InstanceofExpression",
            Phantoms.field("lhs", expr),
            Phantoms.field("rhs",
                Phantoms.<hydra.java.syntax.InstanceofExpression_Rhs>inject(
                    "hydra.java.syntax.InstanceofExpression_Rhs", "referenceType", rt)));
    }

    public static TTerm<MethodInvocation> methodInvocation_(
            TTerm<MethodInvocation_Header> header, TTerm<List<Expression>> args) {
        return Phantoms.record("hydra.java.syntax.MethodInvocation",
            Phantoms.field("header", header),
            Phantoms.field("arguments", args));
    }

    public static TTerm<MethodInvocation_Header> methodInvocationHeaderSimple(TTerm<MethodName> mn) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Header", "simple", mn);
    }

    public static TTerm<MethodInvocation_Header> methodInvocationHeaderComplex(TTerm<MethodInvocation_Complex> mc) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Header", "complex", mc);
    }

    public static TTerm<MethodInvocation_Complex> methodInvocationComplex(
            TTerm<MethodInvocation_Variant> variant, TTerm<List<TypeArgument>> targs, TTerm<Identifier> ident) {
        return Phantoms.record("hydra.java.syntax.MethodInvocation_Complex",
            Phantoms.field("variant", variant),
            Phantoms.field("typeArguments", targs),
            Phantoms.field("identifier", ident));
    }

    public static TTerm<MethodInvocation_Variant> methodInvocationVariantExpression(TTerm<ExpressionName> en) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Variant", "expression", en);
    }

    public static TTerm<MethodInvocation_Variant> methodInvocationVariantPrimary(TTerm<Primary> p) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Variant", "primary", p);
    }

    public static TTerm<MethodInvocation_Variant> methodInvocationVariantType(TTerm<TypeName> tn) {
        return Phantoms.inject("hydra.java.syntax.MethodInvocation_Variant", "type", tn);
    }

    public static TTerm<MethodDeclaration> methodDeclaration_(
            TTerm<List<Annotation>> anns,
            TTerm<List<MethodModifier>> mods,
            TTerm<MethodHeader> header,
            TTerm<MethodBody> body) {
        return Phantoms.record("hydra.java.syntax.MethodDeclaration",
            Phantoms.field("annotations", anns),
            Phantoms.field("modifiers", mods),
            Phantoms.field("header", header),
            Phantoms.field("body", body));
    }

    public static TTerm<InterfaceMethodDeclaration> interfaceMethodDeclaration_(
            TTerm<List<InterfaceMethodModifier>> mods,
            TTerm<MethodHeader> header,
            TTerm<MethodBody> body) {
        return Phantoms.record("hydra.java.syntax.InterfaceMethodDeclaration",
            Phantoms.field("modifiers", mods),
            Phantoms.field("header", header),
            Phantoms.field("body", body));
    }

    public static TTerm<FormalParameter_Simple> formalParameterSimple_(
            TTerm<List<VariableModifier>> mods, TTerm<UnannType> ut, TTerm<VariableDeclaratorId> vid) {
        return Phantoms.record("hydra.java.syntax.FormalParameter_Simple",
            Phantoms.field("modifiers", mods),
            Phantoms.field("type", ut),
            Phantoms.field("id", vid));
    }

    public static TTerm<ClassBodyDeclaration> classBodyDeclClassMember(TTerm<ClassMemberDeclaration> cm) {
        return Phantoms.inject("hydra.java.syntax.ClassBodyDeclaration", "classMember", cm);
    }

    public static TTerm<ClassBodyDeclaration> classBodyDeclConstructor(TTerm<ConstructorDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.ClassBodyDeclaration", "constructorDeclaration", cd);
    }

    public static TTerm<ClassMemberDeclaration> classMemberDeclField(TTerm<FieldDeclaration> fd) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "field", fd);
    }

    public static TTerm<ClassMemberDeclaration> classMemberDeclMethod(TTerm<MethodDeclaration> md) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "method", md);
    }

    public static TTerm<ClassMemberDeclaration> classMemberDeclClass(TTerm<ClassDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "class", cd);
    }

    public static TTerm<ClassMemberDeclaration> classMemberDeclInterface(TTerm<InterfaceDeclaration> id) {
        return Phantoms.inject("hydra.java.syntax.ClassMemberDeclaration", "interface", id);
    }

    public static TTerm<InterfaceMemberDeclaration> interfaceMemberDeclConstant(TTerm<ConstantDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.InterfaceMemberDeclaration", "constant", cd);
    }

    public static TTerm<InterfaceMemberDeclaration> interfaceMemberDeclInterfaceMethod(TTerm<InterfaceMethodDeclaration> imd) {
        return Phantoms.inject("hydra.java.syntax.InterfaceMemberDeclaration", "interfaceMethod", imd);
    }

    public static TTerm<InterfaceMemberDeclaration> interfaceMemberDeclClass(TTerm<ClassDeclaration> cd) {
        return Phantoms.inject("hydra.java.syntax.InterfaceMemberDeclaration", "class", cd);
    }

    public static TTerm<ElementValue> elementValueConditional(TTerm<ConditionalExpression> ce) {
        return Phantoms.inject("hydra.java.syntax.ElementValue", "conditionalExpression", ce);
    }

    public static TTerm<ElementValue> elementValueArray(TTerm<ElementValueArrayInitializer> evai) {
        return Phantoms.inject("hydra.java.syntax.ElementValue", "elementValueArrayInitializer", evai);
    }

    public static TTerm<VariableInitializer> variableInitializerArray(TTerm<ArrayInitializer> ai) {
        return Phantoms.inject("hydra.java.syntax.VariableInitializer", "arrayInitializer", ai);
    }
}
