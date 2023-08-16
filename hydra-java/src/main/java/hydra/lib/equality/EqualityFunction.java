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

import static hydra.Flows.*;
import static hydra.basics.Basics.capitalize;
import static hydra.dsl.Types.*;


public abstract class EqualityFunction<A, T> extends PrimitiveFunction<A> {

    protected final Name name;
    protected final Type<A> type;
    protected final Function<Term<A>, Flow<Graph<A>, T>> expect;
    protected final BiFunction<T, T, Boolean> criterion;

    public EqualityFunction(PrimitiveType<A, T> type) {
        this(type.name, type.type, type.expect, type.comparator);
    }

    private EqualityFunction(String typeName,
                            Type<A> datatype,
                            Function<Term<A>, Flow<Graph<A>, T>> expect,
                            Comparator<T> comparator) {
        this.name = new Name("hydra/lib/equality." + capitalize(typeName));
        this.type = function(datatype, datatype, boolean_());
        this.expect = expect;
        this.criterion = Object::equals;
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
