package hydra.lib.equality;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.lib.PrimitiveType;
import hydra.tools.PrimitiveFunction;

import java.util.Comparator;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.Flows.map2;
import static hydra.basics.Basics.capitalize;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;


public abstract class EqualityFunction<A, T> extends PrimitiveFunction<A> {
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
    protected final Type<A> type;
    protected final Function<Term<A>, Flow<Graph<A>, T>> expect;
    protected final BiFunction<T, T, Boolean> criterion;

    public EqualityFunction(PrimitiveType<A, T> type, Relation relation) {
        this(type.name, type.type, type.expect, type.comparator, relation);
    }

    private EqualityFunction(String typeName,
                             Type<A> datatype,
                             Function<Term<A>, Flow<Graph<A>, T>> expect,
                             Comparator<T> comparator,
                             Relation relation) {
        this.name = new Name("hydra/lib/equality." + relation.prefix + capitalize(typeName));
        this.type = function(datatype, datatype, boolean_());
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
    public Type<A> type() {
        return type;
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map2(expect.apply(args.get(0)), expect.apply(args.get(1)),
                (arg0, arg1) -> Terms.boolean_(criterion.apply(arg0, arg1)));
    }
}
