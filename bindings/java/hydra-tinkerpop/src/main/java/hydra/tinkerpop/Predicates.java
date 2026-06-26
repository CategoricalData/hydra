package hydra.tinkerpop;

import hydra.tinkerpop.gremlin.RangeArgument;
import hydra.tinkerpop.gremlin.TraversalPredicate;

import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.TextP;

/**
 * Maps Hydra {@code TraversalPredicate} values to TinkerPop {@link P} / {@link TextP} predicates
 * (the objects passed as step arguments to {@code has}/{@code is}/{@code where}/…).
 *
 * <p>Covers the comparison predicates (eq/neq/lt/lte/gt/gte), range predicates (inside/outside/between),
 * collection predicates (within/without), the text predicates (startingWith/endingWith/containing/regex
 * and their negations), and boolean composition (and/or/negate/not). The {@code typeOf} predicate
 * (3.8.1) is mapped where the runtime exposes it; otherwise it throws "not yet mapped".
 */
final class Predicates {

    private Predicates() {
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    static P<?> toP(TraversalPredicate pred) {
        return pred.accept(new TraversalPredicate.PartialVisitor<P<?>>() {
            @Override
            public P<?> otherwise(TraversalPredicate instance) {
                throw new UnsupportedOperationException(
                        "TraversalPredicate→P mapping not yet implemented for: "
                                + instance.getClass().getSimpleName());
            }

            // Comparison predicates: single generic-literal argument → boxed value.
            @Override
            public P<?> visit(TraversalPredicate.Eq x) {
                return P.eq(value(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Neq x) {
                return P.neq(value(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Lt x) {
                return P.lt((Comparable) value(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Lte x) {
                return P.lte((Comparable) value(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Gt x) {
                return P.gt((Comparable) value(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Gte x) {
                return P.gte((Comparable) value(x.value));
            }

            // Range predicates: (min, max).
            @Override
            public P<?> visit(TraversalPredicate.Inside x) {
                return P.inside((Comparable) rangeMin(x.value), (Comparable) rangeMax(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Outside x) {
                return P.outside((Comparable) rangeMin(x.value), (Comparable) rangeMax(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Between x) {
                return P.between((Comparable) rangeMin(x.value), (Comparable) rangeMax(x.value));
            }

            // Collection predicates. The Hydra payload wraps the collection in a single optional
            // generic-literal argument; a present GenericLiteralCollection becomes the varargs.
            @Override
            public P<?> visit(TraversalPredicate.Within x) {
                return P.within(collection(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Without x) {
                return P.without(collection(x.value));
            }

            // Boolean composition.
            @Override
            public P<?> visit(TraversalPredicate.Not x) {
                return P.not((P) toP(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Negate x) {
                return ((P) toP(x.value)).negate();
            }

            @Override
            public P<?> visit(TraversalPredicate.And x) {
                return ((P) toP(x.value.left)).and((P) toP(x.value.right));
            }

            @Override
            public P<?> visit(TraversalPredicate.Or x) {
                return ((P) toP(x.value.left)).or((P) toP(x.value.right));
            }

            // Text predicates.
            @Override
            public P<?> visit(TraversalPredicate.StartingWith x) {
                return TextP.startingWith(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.NotStartingWith x) {
                return TextP.notStartingWith(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.EndingWith x) {
                return TextP.endingWith(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.NotEndingWith x) {
                return TextP.notEndingWith(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Containing x) {
                return TextP.containing(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.NotContaining x) {
                return TextP.notContaining(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.Regex x) {
                return TextP.regex(HydraToBytecode.stringArg(x.value));
            }

            @Override
            public P<?> visit(TraversalPredicate.NotRegex x) {
                return TextP.notRegex(HydraToBytecode.stringArg(x.value));
            }
        });
    }

    private static Object value(hydra.tinkerpop.gremlin.GenericLiteralArgument a) {
        return HydraToBytecode.literalArg(a);
    }

    private static Object rangeMin(RangeArgument r) {
        return HydraToBytecode.literalArg(r.min);
    }

    private static Object rangeMax(RangeArgument r) {
        return HydraToBytecode.literalArg(r.max);
    }

    /** Unpacks the optional collection argument of within/without into an {@code Object[]} of values. */
    private static Object[] collection(
            hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.GenericLiteralArgument> opt) {
        if (opt.isNone()) {
            return new Object[0];
        }
        // The argument is a single generic literal; if it is a collection/set, unpack its elements,
        // otherwise treat it as a single-element membership test.
        Object v = HydraToBytecode.literalArg(opt.fromGiven());
        if (v instanceof java.util.Collection) {
            return ((java.util.Collection<?>) v).toArray();
        }
        return new Object[]{v};
    }

    // -- Reverse: TinkerPop P / TextP → Hydra TraversalPredicate --------------------------------

    /**
     * Reverse of {@link #toP}. Dispatches on {@code P.getPredicateName()}; handles comparison, range,
     * collection, text, and boolean-composition predicates. Connective predicates (and/or) are read via
     * {@code AndP}/{@code OrP}'s {@code getPredicates()} where available.
     */
    @SuppressWarnings("unchecked")
    static TraversalPredicate fromP(P<?> p) {
        // Boolean composition first (these are P subclasses, not leaf comparisons).
        if (p instanceof org.apache.tinkerpop.gremlin.process.traversal.util.AndP) {
            java.util.List<? extends P<?>> ps = ((org.apache.tinkerpop.gremlin.process.traversal.util.AndP<?>) p).getPredicates();
            return new TraversalPredicate.And(new hydra.tinkerpop.gremlin.TwoTraversalPredicates(fromP(ps.get(0)), fromP(ps.get(1))));
        }
        if (p instanceof org.apache.tinkerpop.gremlin.process.traversal.util.OrP) {
            java.util.List<? extends P<?>> ps = ((org.apache.tinkerpop.gremlin.process.traversal.util.OrP<?>) p).getPredicates();
            return new TraversalPredicate.Or(new hydra.tinkerpop.gremlin.TwoTraversalPredicates(fromP(ps.get(0)), fromP(ps.get(1))));
        }

        String name = p.getPredicateName();
        Object value = p.getValue();
        switch (name) {
            case "eq":  return new TraversalPredicate.Eq(genLit(value));
            case "neq": return new TraversalPredicate.Neq(genLit(value));
            case "lt":  return new TraversalPredicate.Lt(genLit(value));
            case "lte": return new TraversalPredicate.Lte(genLit(value));
            case "gt":  return new TraversalPredicate.Gt(genLit(value));
            case "gte": return new TraversalPredicate.Gte(genLit(value));
            case "inside":  return new TraversalPredicate.Inside(range(value));
            case "outside": return new TraversalPredicate.Outside(range(value));
            case "between": return new TraversalPredicate.Between(range(value));
            case "within":  return new TraversalPredicate.Within(hydra.overlay.java.util.Optional.given(genLit(value)));
            case "without": return new TraversalPredicate.Without(hydra.overlay.java.util.Optional.given(genLit(value)));
            // Text predicates (TextP) share getPredicateName().
            case "startingWith":    return new TraversalPredicate.StartingWith(strArg(value));
            case "notStartingWith": return new TraversalPredicate.NotStartingWith(strArg(value));
            case "endingWith":      return new TraversalPredicate.EndingWith(strArg(value));
            case "notEndingWith":   return new TraversalPredicate.NotEndingWith(strArg(value));
            case "containing":      return new TraversalPredicate.Containing(strArg(value));
            case "notContaining":   return new TraversalPredicate.NotContaining(strArg(value));
            case "regex":           return new TraversalPredicate.Regex(strArg(value));
            case "notRegex":        return new TraversalPredicate.NotRegex(strArg(value));
            default:
                throw new UnsupportedOperationException("P→TraversalPredicate not implemented for predicate: " + name);
        }
    }

    private static hydra.tinkerpop.gremlin.GenericLiteralArgument genLit(Object v) {
        return new hydra.tinkerpop.gremlin.GenericLiteralArgument.Value(BytecodeToHydra.literal(v));
    }

    private static hydra.tinkerpop.gremlin.RangeArgument range(Object v) {
        // inside/outside/between store the value as a 2-element list/array [lo, hi].
        Object lo, hi;
        if (v instanceof java.util.List) {
            java.util.List<?> l = (java.util.List<?>) v;
            lo = l.get(0); hi = l.get(1);
        } else if (v instanceof Object[]) {
            Object[] arr = (Object[]) v;
            lo = arr[0]; hi = arr[1];
        } else {
            throw new UnsupportedOperationException("range predicate value is not a 2-element collection: " + v);
        }
        return new hydra.tinkerpop.gremlin.RangeArgument(genLit(lo), genLit(hi));
    }

    private static hydra.tinkerpop.gremlin.StringArgument strArg(Object v) {
        return new hydra.tinkerpop.gremlin.StringArgument.Value((String) v);
    }
}
