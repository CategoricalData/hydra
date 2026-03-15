package hydra.dsl.meta.examples;

import hydra.coders.TraversalOrder;
import hydra.core.AnnotatedType;
import hydra.core.Binding;
import hydra.core.Field;
import hydra.core.Let;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.dsl.meta.DefineBuilder;
import hydra.dsl.meta.Expr;
import hydra.dsl.TypeParameters.*;
import hydra.module.Module;
import hydra.module.Namespace;
import hydra.phantoms.TBinding;
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.meta.Phantoms.*;
import static hydra.dsl.meta.Core.*;
import hydra.dsl.meta.lib.Equality;
import hydra.dsl.meta.lib.Lists;
import hydra.dsl.meta.lib.Logic;
import hydra.dsl.meta.lib.Sets;

/**
 * Partial implementation of hydra.rewriting as a complete Module,
 * using the Java phantom-typed DSLs.
 *
 * <p>This mirrors the Haskell source at:
 * {@code hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs}
 *
 * <p>The functions chosen range from simple (deannotateTerm) to complex (freeVariablesInTerm,
 * replaceFreeTermVariable) to showcase the DSL's expressiveness.
 */
public interface Rewriting {

    Namespace ns = new Namespace("hydra.rewriting");

    /**
     * Create a definition in the hydra.rewriting namespace.
     */
    static DefineBuilder define(String localName) {
        return hydra.dsl.meta.Phantoms.define(ns, localName);
    }

    /**
     * Reference a function in this module by local name (qualified with namespace).
     */
    static <A> Expr<A> local(String localName) {
        return var(ns.value + "." + localName);
    }

    /**
     * Partially apply foldl to a combining function (curried helper).
     */
    static <R> Expr<R> fold(TTerm<?> f) {
        return primitive(Lists._lists_foldl).of(f);
    }

    // =========================================================================
    // TraversalOrder constructors
    // =========================================================================

    static Expr<TraversalOrder> traversalOrderPre() {
        return injectUnit(TraversalOrder.TYPE_, TraversalOrder.PRE);
    }

    static Expr<TraversalOrder> traversalOrderPost() {
        return injectUnit(TraversalOrder.TYPE_, TraversalOrder.POST);
    }

    // =========================================================================
    // Simple deannotation functions
    // =========================================================================

    TBinding<Function<Term, Term>> deannotateTerm = define("deannotateTerm")
        .doc("Strip all annotations from the top levels of a term")
        .lam("t")
        .to(cases(Term.TYPE_, var("t"),
            Maybe.just(var("t")),
            field(Term.ANNOTATED,
                lam("at", local("deannotateTerm")
                    .of(annotatedTermBody(var("at")))))));

    TBinding<Function<Term, Term>> deannotateAndDetypeTerm = define("deannotateAndDetypeTerm")
        .doc("Strip type annotations from the top levels of a term")
        .lam("t")
        .to(cases(Term.TYPE_, var("t"),
            Maybe.just(var("t")),
            field(Term.ANNOTATED,
                lam("at", local("deannotateAndDetypeTerm")
                    .of(annotatedTermBody(var("at"))))),
            field(Term.TYPE_APPLICATION,
                lam("tt", local("deannotateAndDetypeTerm")
                    .of(typeApplicationTermBody(var("tt"))))),
            field(Term.TYPE_LAMBDA,
                lam("ta", local("deannotateAndDetypeTerm")
                    .of(typeLambdaBody(var("ta")))))));

    TBinding<Function<Type, Type>> deannotateType = define("deannotateType")
        .doc("Strip all annotations from a type")
        .lam("t")
        .to(cases(Type.TYPE_, var("t"),
            Maybe.just(var("t")),
            field(Type.ANNOTATED,
                compose(local("deannotateType"),
                    project(AnnotatedType.TYPE_, AnnotatedType.BODY)))));

    // =========================================================================
    // Deannotation with let bindings and composition
    // =========================================================================

    TBinding<Function<Type, Type>> deannotateTypeRecursive = define("deannotateTypeRecursive")
        .doc("Recursively strip all annotations from a type")
        .lam("typ")
        .let("strip",
            lams("recurse", "typ")
                .let("rewritten", var("recurse").of("typ"))
                .to(cases(Type.TYPE_, var("rewritten"),
                    Maybe.just(var("rewritten")),
                    field(Type.ANNOTATED,
                        lam("at", annotatedTypeBody(var("at")))))))
        .to(local("rewriteType").of("strip").of("typ"));

    TBinding<Function<TypeScheme, TypeScheme>> deannotateTypeSchemeRecursive = define("deannotateTypeSchemeRecursive")
        .doc("Recursively strip all annotations from a type scheme")
        .lam("ts")
        .let("vars", typeSchemeVariables(var("ts")))
        .let("typ", typeSchemeType(var("ts")))
        .let("constraints", typeSchemeConstraints(var("ts")))
        .to(typeScheme(var("vars"),
            local("deannotateTypeRecursive").of("typ"),
            var("constraints")));

    // =========================================================================
    // detypeTerm
    // =========================================================================

    TBinding<Function<Term, Term>> detypeTerm = define("detypeTerm")
        .doc("Strip System F type annotations but leave application-specific annotations intact")
        .lam("t")
        .to(cases(Term.TYPE_, var("t"),
            Maybe.just(var("t")),
            field(Term.ANNOTATED,
                lam("at")
                    .let("subj", annotatedTermBody(var("at")))
                    .let("ann", annotatedTermAnnotation(var("at")))
                    .to(termAnnotated(annotatedTerm(
                        local("detypeTerm").of("subj"),
                        var("ann"))))),
            field(Term.TYPE_APPLICATION,
                lam("tt", local("deannotateAndDetypeTerm")
                    .of(typeApplicationTermBody(var("tt"))))),
            field(Term.TYPE_LAMBDA,
                lam("ta", local("deannotateAndDetypeTerm")
                    .of(typeLambdaBody(var("ta")))))));

    // =========================================================================
    // mapBeneathTypeAnnotations
    // =========================================================================

    TBinding<Function<Function<Type, Type>, Function<Type, Type>>> mapBeneathTypeAnnotations = define("mapBeneathTypeAnnotations")
        .doc("Apply a transformation to the first type beneath a chain of annotations")
        .lams("f", "t")
        .to(cases(Type.TYPE_, var("t"),
            Maybe.just(var("f").of("t")),
            field(Type.ANNOTATED,
                lam("at", typeAnnotated(annotatedType(
                    local("mapBeneathTypeAnnotations").of("f")
                        .of(annotatedTypeBody(var("at"))),
                    annotatedTypeAnnotation(var("at"))))))));

    // =========================================================================
    // isLambda
    // =========================================================================

    TBinding<Function<Term, Boolean>> isLambda = define("isLambda")
        .doc("Check whether a term is a lambda, possibly nested within let/annotation terms")
        .lam("term")
        .to(cases(Term.TYPE_,
            local("deannotateTerm").of("term"),
            Maybe.just(false_()),
            field(Term.FUNCTION,
                match(hydra.core.Function.TYPE_,
                    Maybe.just(false_()),
                    field(hydra.core.Function.LAMBDA,
                        constant(true_())))),
            field(Term.LET,
                lam("lt", local("isLambda")
                    .of(project(Let.TYPE_, Let.BODY).of("lt"))))));

    // =========================================================================
    // Free variable analysis
    // =========================================================================

    TBinding<Function<Term, Set<Name>>> freeVariablesInTerm = define("freeVariablesInTerm")
        .doc("Find the free variables (not bound by a lambda or let) in a term")
        .lam("term")
        .let("dfltVars",
            Lists.foldl(
                lams("s", "t")
                    .to(Sets.union(var("s"),
                        local("freeVariablesInTerm").of("t"))),
                Sets.empty(),
                local("subterms").of("term")))
        .to(cases(Term.TYPE_, var("term"),
            Maybe.just(var("dfltVars")),
            field(Term.FUNCTION,
                match(hydra.core.Function.TYPE_,
                    Maybe.just(var("dfltVars")),
                    field(hydra.core.Function.LAMBDA,
                        lam("l", Sets.delete(
                            lambdaParameter(var("l")),
                            local("freeVariablesInTerm")
                                .of(lambdaBody(var("l")))))))),
            field(Term.LET,
                lam("l", Sets.difference(
                    var("dfltVars"),
                    Sets.fromList(Lists.map(
                        project(Binding.TYPE_, Binding.NAME),
                        letBindings(var("l"))))))),
            field(Term.VARIABLE,
                lam("v", Sets.singleton(var("v"))))));

    TBinding<Function<Type, Set<Name>>> freeVariablesInType = define("freeVariablesInType")
        .doc("Find the free variables (not bound by forall) in a type")
        .lam("typ")
        .let("dfltVars",
            fold(lams("s", "t")
                    .to(Sets.union(var("s"),
                        local("freeVariablesInType").of("t"))))
                .of(Sets.empty())
                .of(local("subtypes").of("typ")))
        .to(cases(Type.TYPE_, var("typ"),
            Maybe.just(var("dfltVars")),
            field(Type.FORALL,
                lam("lt", Sets.delete(
                    forallTypeParameter(var("lt")),
                    local("freeVariablesInType")
                        .of(forallTypeBody(var("lt")))))),
            field(Type.VARIABLE,
                lam("v", Sets.singleton(var("v"))))));

    TBinding<Function<TypeScheme, Set<Name>>> freeVariablesInTypeScheme = define("freeVariablesInTypeScheme")
        .doc("Find free variables in a type scheme")
        .lam("ts")
        .let("vars", typeSchemeVariables(var("ts")))
        .let("t", typeSchemeType(var("ts")))
        .to(Sets.difference(
            local("freeVariablesInType").of("t"),
            Sets.fromList(var("vars"))));

    TBinding<Function<Name, Function<Term, Boolean>>> isFreeVariableInTerm = define("isFreeVariableInTerm")
        .doc("Check whether a variable is free (not bound) in a term")
        .lams("v", "term")
        .to(Logic.not(Sets.member(var("v"),
            local("freeVariablesInTerm").of("term"))));

    // =========================================================================
    // Recursive rewriting with binding awareness
    // =========================================================================

    TBinding<Function<Name, Function<Term, Function<Term, Term>>>> replaceFreeTermVariable = define("replaceFreeTermVariable")
        .doc("Replace free occurrences of a variable name with a new term")
        .lams("vold", "tnew", "term")
        .let("rewrite",
            lams("recurse", "t")
                .to(cases(Term.TYPE_, var("t"),
                    Maybe.just(var("recurse").of("t")),
                    field(Term.FUNCTION,
                        lam("f", cases(hydra.core.Function.TYPE_, var("f"),
                            Maybe.just(var("recurse").of("t")),
                            field(hydra.core.Function.LAMBDA,
                                lam("l")
                                    .let("v", lambdaParameter(var("l")))
                                    .to(Logic.ifElse(
                                        Equality.equal(var("v"), var("vold")),
                                        var("t"),
                                        var("recurse").of("t"))))))),
                    field(Term.VARIABLE,
                        lam("v", Logic.ifElse(
                            Equality.equal(var("v"), var("vold")),
                            var("tnew"),
                            termVariable(var("v"))))))))
        .to(local("rewriteTerm").of("rewrite").of("term"));

    TBinding<Function<Name, Function<Type, Function<Type, Type>>>> replaceFreeTypeVariable = define("replaceFreeTypeVariable")
        .doc("Replace free occurrences of a name in a type")
        .lams("v", "rep", "typ")
        .let("mapExpr",
            lams("recurse", "t")
                .to(cases(Type.TYPE_, var("t"),
                    Maybe.just(var("recurse").of("t")),
                    field(Type.FORALL,
                        lam("ft", Logic.ifElse(
                            Equality.equal(var("v"), forallTypeParameter(var("ft"))),
                            var("t"),
                            typeForall(forallType(
                                forallTypeParameter(var("ft")),
                                var("recurse").of(forallTypeBody(var("ft")))))))),
                    field(Type.VARIABLE,
                        lam("v2", Logic.ifElse(
                            Equality.equal(var("v"), var("v2")),
                            var("rep"),
                            var("t")))))))
        .to(local("rewriteType").of("mapExpr").of("typ"));

    // =========================================================================
    // Recursive annotation removal
    // =========================================================================

    TBinding<Function<Term, Term>> removeTermAnnotations = define("removeTermAnnotations")
        .doc("Recursively remove term annotations, including within subterms")
        .lam("term")
        .let("remove",
            lams("recurse", "term")
                .let("rewritten", var("recurse").of("term"))
                .to(cases(Term.TYPE_, var("term"),
                    Maybe.just(var("rewritten")),
                    field(Term.ANNOTATED,
                        lam("at", annotatedTermBody(var("at")))))))
        .to(local("rewriteTerm").of("remove").of("term"));

    TBinding<Function<Type, Type>> removeTypeAnnotations = define("removeTypeAnnotations")
        .doc("Recursively remove type annotations, including within subtypes")
        .lam("typ")
        .let("remove",
            lams("recurse", "typ")
                .let("rewritten", var("recurse").of("typ"))
                .to(cases(Type.TYPE_, var("rewritten"),
                    Maybe.just(var("rewritten")),
                    field(Type.ANNOTATED,
                        lam("at", annotatedTypeBody(var("at")))))))
        .to(local("rewriteType").of("remove").of("typ"));

    // =========================================================================
    // Fold operations
    // =========================================================================

    TBinding<Function<TraversalOrder, Function<Function<X, Function<Term, X>>, Function<X, Function<Term, X>>>>> foldOverTerm = define("foldOverTerm")
        .doc("Fold over a term, traversing its subterms in the specified order")
        .lams("order", "fld", "b0", "term")
        .to(cases(TraversalOrder.TYPE_, var("order"),
            Maybe.nothing(),
            field(TraversalOrder.PRE,
                constant(
                    fold(local("foldOverTerm").of("order").of("fld"))
                        .of(var("fld").of("b0").of("term"))
                        .of(local("subterms").of("term")))),
            field(TraversalOrder.POST,
                constant(var("fld")
                    .of(fold(local("foldOverTerm").of("order").of("fld"))
                        .of("b0")
                        .of(local("subterms").of("term")))
                    .of("term")))));

    TBinding<Function<TraversalOrder, Function<Function<X, Function<Type, X>>, Function<X, Function<Type, X>>>>> foldOverType = define("foldOverType")
        .doc("Fold over a type, traversing its subtypes in the specified order")
        .lams("order", "fld", "b0", "typ")
        .to(cases(TraversalOrder.TYPE_, var("order"),
            Maybe.nothing(),
            field(TraversalOrder.PRE,
                constant(
                    fold(local("foldOverType").of("order").of("fld"))
                        .of(var("fld").of("b0").of("typ"))
                        .of(local("subtypes").of("typ")))),
            field(TraversalOrder.POST,
                constant(var("fld")
                    .of(fold(local("foldOverType").of("order").of("fld"))
                        .of("b0")
                        .of(local("subtypes").of("typ")))
                    .of("typ")))));

    // =========================================================================
    // Subterms / subtypes extraction
    // =========================================================================

    TBinding<Function<Type, List<Type>>> subtypes = define("subtypes")
        .doc("Extract the immediate subtypes of a type")
        .lam("typ")
        .to(cases(Type.TYPE_, var("typ"),
            Maybe.just(list()),
            field(Type.ANNOTATED,
                lam("at", list(annotatedTypeBody(var("at"))))),
            field(Type.FUNCTION,
                lam("ft", list(
                    functionTypeDomain(var("ft")),
                    functionTypeCodomain(var("ft"))))),
            field(Type.FORALL,
                lam("ft", list(forallTypeBody(var("ft"))))),
            field(Type.LIST,
                lam("t", list(var("t")))),
            field(Type.SET,
                lam("t", list(var("t")))),
            field(Type.MAYBE,
                lam("t", list(var("t")))),
            field(Type.MAP,
                lam("mt", list(
                    mapTypeKeys(var("mt")),
                    mapTypeValues(var("mt"))))),
            field(Type.EITHER,
                lam("et", list(
                    eitherTypeLeft(var("et")),
                    eitherTypeRight(var("et"))))),
            field(Type.PAIR,
                lam("pt", list(
                    pairTypeFirst(var("pt")),
                    pairTypeSecond(var("pt")))))));

    // =========================================================================
    // Module definition
    // =========================================================================

    List<Binding> elements = Arrays.asList(
        toBinding(deannotateTerm),
        toBinding(deannotateAndDetypeTerm),
        toBinding(deannotateType),
        toBinding(deannotateTypeRecursive),
        toBinding(deannotateTypeSchemeRecursive),
        toBinding(detypeTerm),
        toBinding(foldOverTerm),
        toBinding(foldOverType),
        toBinding(freeVariablesInTerm),
        toBinding(freeVariablesInType),
        toBinding(freeVariablesInTypeScheme),
        toBinding(isFreeVariableInTerm),
        toBinding(isLambda),
        toBinding(mapBeneathTypeAnnotations),
        toBinding(removeTermAnnotations),
        toBinding(removeTypeAnnotations),
        toBinding(replaceFreeTermVariable),
        toBinding(replaceFreeTypeVariable),
        toBinding(subtypes));

    Module module = new Module(
        ns,
        hydra.util.ConsList.fromList(elements),
        hydra.util.ConsList.empty(),
        hydra.util.ConsList.empty(),
        Maybe.just("Utilities for type and term rewriting and analysis."));
}
