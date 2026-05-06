package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.binary;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.util.ConsList;
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term term = args.get(0);
            if (term instanceof Term.Literal) {
                hydra.core.Literal lit = ((Term.Literal) term).value;
                if (lit instanceof hydra.core.Literal.Binary) {
                    byte[] bytes = ((hydra.core.Literal.Binary) lit).value;
                    ConsList<Term> terms = ConsList.empty();
                    for (int i = bytes.length - 1; i >= 0; i--) {
                        terms = ConsList.cons(Terms.int32(bytes[i] & 0xFF), terms);
                    }
                    return Either.right(Terms.list(terms));
                }
            }
            return Either.left(new Error_.Other(new OtherError("expected binary literal")));
        };
    }

    public static List<Integer> apply(byte[] binary) {
        ConsList<Integer> result = ConsList.empty();
        for (int i = binary.length - 1; i >= 0; i--) {
            result = ConsList.cons(binary[i] & 0xFF, result);
        }
        return result;
    }
}
