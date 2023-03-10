package hydra;

import hydra.compute.Flow;
import hydra.core.Annotated;
import hydra.core.Field;
import hydra.core.Injection;
import hydra.core.Nominal;
import hydra.core.Record;
import hydra.core.Sum;
import hydra.core.Term;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import static hydra.dsl.Flows.*;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

public interface Rewriting {
    static <X, Y> Function<X, Y> rewrite(
            Function<Function<X, Y>, Function<X, Y>> fsub,
            Function<Function<X, Y>, Function<X, Y>> f) {
        final AtomicReference<Function<X, Y>> ref = new AtomicReference<>();
        ref.set(f.apply(fsub.apply(x -> ref.get().apply(x))));
        return ref.get();
    }

    static <A, B, S> Flow<S, Field<B>> rewriteFieldM(
            Function<Term<A>, Flow<S, Term<B>>> recurse,
            Field<A> original) {
        return map(recurse.apply(original.term), bTerm -> new Field<>(original.name, bTerm));
    }

    static <A, B, S> Flow<S, Term<B>> rewriteTermM(
            Function<Function<Term<A>, Flow<S, Term<B>>>, Function<Term<A>, Flow<S, Term<B>>>> f,
            Function<A, Flow<S, B>> mf,
            Term<A> original) {
        Function<Function<Term<A>, Flow<S, Term<B>>>, Function<Term<A>, Flow<S, Term<B>>>> fsub
                = recurse -> term -> term.accept(new Term.Visitor<Flow<S, Term<B>>>() {
            @Override
            public Flow<S, Term<B>> visit(Term.Annotated instance) {
                Annotated<Term<A>, A> ann = instance.value;
                return map2(
                        recurse.apply(ann.subject),
                        mf.apply(ann.annotation),
                        (term1, ann1) -> Terms.annot(ann1, term1));
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Application instance) {
                return null; // TODO
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Element instance) {
                return pure(instance);
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Function instance) {
                return null;
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Let instance) {
                return null;
            }

            @Override
            public Flow<S, Term<B>> visit(Term.List instance) {
                return null;
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Literal instance) {
                return null;
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Map instance) {
                return null;
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Optional instance) {

//                Flow<S, Optional<Term<B>>> termB = recurse.apply()
                return null;
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Product instance) {
                List<Term<A>> termsA = instance.value;
                Flow<S, List<Term<B>>> termsB = mapM(termsA, recurse);
                return map(termsB, Term.Product::new);
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Record instance) {
                List<Field<A>> fieldsA = instance.value.fields;
                Flow<S, List<Field<B>>> fieldsB = mapM(fieldsA, aField -> rewriteFieldM(recurse, aField));
                return map(fieldsB, fields -> new Term.Record<>(new Record<>(instance.value.typeName, fields)));
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Set instance) {
                Flow<S, Set<Term<B>>> els = mapM(instance.value, recurse);
                return map(els, Term.Set::new);
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Stream instance) {
                throw new UnsupportedOperationException();
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Sum instance) {
                Term<A> t0 = instance.value.term;
                Flow<S, Term<B>> t1 = recurse.apply(t0);
                return map(t1, t2 -> new Term.Sum<B>(new Sum<B>(instance.value.index, instance.value.size, t2)));
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Union instance) {
                Flow<S, Field<B>> t = rewriteFieldM(recurse, instance.value.field);
                return map(t, bField -> new Term.Union<>(new Injection<>(instance.value.typeName, bField)));
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Variable instance) {
                return pure(new Term.Variable<B>(instance.value));
            }

            @Override
            public Flow<S, Term<B>> visit(Term.Wrap instance) {
                Flow<S, Term<B>> obj2 = recurse.apply(((Term.Wrap<A>) instance).value.object);
                return map(obj2, bTerm -> new Term.Wrap<>(new Nominal<>(instance.value.typeName, bTerm)));
            }
        });
        return rewrite(fsub, f).apply(original);
    }
}
