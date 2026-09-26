package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.binary;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.errors.OtherError;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


public class BinaryToBytes extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Literals.binaryToBytes().name;
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
                hydra.core.model.Literal lit = ((Term.Literal) term).value;
                if (lit instanceof hydra.core.model.Literal.Binary) {
                    byte[] bytes = ((hydra.core.model.Literal.Binary) lit).value;
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
