package hydra.dsl.meta;

import hydra.phantoms.TTerm;

/**
 * An introduction form used by ExprBuilder and DefineBuilder.
 * Represents either a doc annotation, a lambda parameter, or a let binding.
 */
class Intro {

    enum Kind { DOC, LAMBDA, LET }

    final Kind kind;
    final String name;
    final TTerm<?> value; // null for DOC and LAMBDA

    private Intro(Kind kind, String name, TTerm<?> value) {
        this.kind = kind;
        this.name = name;
        this.value = value;
    }

    static Intro doc(String description) {
        return new Intro(Kind.DOC, description, null);
    }

    static Intro lambda(String param) {
        return new Intro(Kind.LAMBDA, param, null);
    }

    static Intro let(String name, TTerm<?> value) {
        return new Intro(Kind.LET, name, value);
    }
}
