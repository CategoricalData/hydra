package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.binary;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


public class BinaryToBytes extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Literals.binaryToBytes().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(binary(), list(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
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
