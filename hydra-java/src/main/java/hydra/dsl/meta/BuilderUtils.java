package hydra.dsl.meta;

import hydra.core.Binding;
import hydra.core.Lambda;
import hydra.core.Let;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Function;
import hydra.phantoms.TTerm;
import hydra.util.ConsList;
import hydra.util.Maybe;

import java.util.Collections;
import java.util.List;

/**
 * Shared utilities for building terms from introduction lists.
 * Used by both ExprBuilder and DefineBuilder.
 */
class BuilderUtils {

    @SuppressWarnings("unchecked")
    static <B> Expr<B> buildTerm(List<Intro> intros, TTerm<?> body) {
        Term result = body.value;
        for (int i = intros.size() - 1; i >= 0; i--) {
            Intro intro = intros.get(i);
            switch (intro.kind) {
                case DOC:
                    result = hydra.dsl.Terms.annot(intro.name, result);
                    break;
                case LAMBDA:
                    result = new Term.Function(new Function.Lambda(
                            new Lambda(new Name(intro.name), Maybe.nothing(), result)));
                    break;
                case LET:
                    ConsList<Binding> bindings = ConsList.singleton(
                            new Binding(new Name(intro.name), intro.value.value, Maybe.nothing()));
                    result = new Term.Let(new Let(bindings, result));
                    break;
            }
        }
        return new Expr<>(result);
    }
}
