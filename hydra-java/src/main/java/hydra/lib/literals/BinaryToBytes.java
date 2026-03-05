package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
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
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


public class BinaryToBytes extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.binaryToBytes");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(binary(), list(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> {
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
                    return Either.right(Terms.list(terms.toArray(new Term[0])));
                }
            }
            return Either.left(new InContext<>(new OtherError("expected binary literal"), cx));
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
