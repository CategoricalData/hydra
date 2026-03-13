package hydra.lib.equality;

import hydra.context.Context;
import hydra.context.InContext;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.error.Error_;
import hydra.graph.Graph;
import hydra.lib.PrimitiveType;
import hydra.tools.PrimitiveFunction;
import hydra.util.Either;

import java.util.Comparator;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


public abstract class EqualityFunction<T> extends PrimitiveFunction {
    public enum Relation {
        EQUALS("equal"),
        NOT_EQUALS("notEqual"),
        LESS_THAN("lt"),
        LESS_THAN_OR_EQUAL("lte"),
        GREATER_THAN("gt"),
        GREATER_THAN_OR_EQUAL("gte");

        private final String prefix;

        Relation(String prefix) {
            this.prefix = prefix;
        }
    }

    protected final Name name;
    protected final TypeScheme type;
    protected final PrimitiveType.TriFunction<Context, Graph, Term, Either<InContext<Error_>, T>> expect;
    protected final BiFunction<T, T, Boolean> criterion;

    public EqualityFunction(PrimitiveType<T> type, Relation relation) {
        this(type.name, type.type, type.expect, type.comparator, relation);
    }

    private EqualityFunction(String typeName,
                             Type datatype,
                             PrimitiveType.TriFunction<Context, Graph, Term, Either<InContext<Error_>, T>> expect,
                             Comparator<T> comparator,
                             Relation relation) {
        this.name = new Name("hydra.lib.equality." + relation.prefix + capitalize(typeName));
        this.type = scheme(function(datatype, datatype, boolean_()));
        this.expect = expect;
        switch (relation) {
            case EQUALS:
                this.criterion = Object::equals;
                break;
            case NOT_EQUALS:
                this.criterion = (a, b) -> !a.equals(b);
                break;
            case LESS_THAN:
                this.criterion = (a, b) -> comparator.compare(a, b) < 0;
                break;
            case LESS_THAN_OR_EQUAL:
                this.criterion = (a, b) -> comparator.compare(a, b) <= 0;
                break;
            case GREATER_THAN:
                this.criterion = (a, b) -> comparator.compare(a, b) > 0;
                break;
            case GREATER_THAN_OR_EQUAL:
                this.criterion = (a, b) -> comparator.compare(a, b) >= 0;
                break;
            default:
                throw new IllegalStateException();
        };
    }

    public Name name() {
        return name;
    }

    @Override
    public TypeScheme type() {
        return type;
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(expect.apply(cx, graph, args.get(0)), arg0 ->
                hydra.lib.eithers.Map.apply(arg1 ->
                    Terms.boolean_(criterion.apply(arg0, arg1)),
                    expect.apply(cx, graph, args.get(1))));
    }

    // TODO: inline implementation until hydra.basics.Basics is generated
    private static String capitalize(String s) {
        if (s == null || s.isEmpty()) {
            return s;
        }
        return Character.toUpperCase(s.charAt(0)) + s.substring(1);
    }
}
