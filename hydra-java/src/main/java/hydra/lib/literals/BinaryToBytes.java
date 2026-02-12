package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.binary;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


public class BinaryToBytes extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.binaryToBytes");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(binary(), list(int32())));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term term = args.get(0);
            if (term instanceof Term.Literal) {
                hydra.core.Literal lit = ((Term.Literal) term).value;
                if (lit instanceof hydra.core.Literal.Binary) {
                    byte[] bytes = ((hydra.core.Literal.Binary) lit).value;
                    List<Integer> result = apply(bytes);
                    List<Term> terms = new ArrayList<>();
                    for (int v : result) {
                        terms.add(Terms.int32(v));
                    }
                    return Flows.pure(Terms.list(terms.toArray(new Term[0])));
                }
            }
            return Flows.fail("expected binary literal");
        };
    }

    public static List<Integer> apply(byte[] binary) {
        List<Integer> result = new ArrayList<>(binary.length);
        for (byte b : binary) {
            result.add(b & 0xFF);
        }
        return result;
    }
}
