package hydra.tools;

import hydra.util.Maybe;
import org.antlr.v4.runtime.ParserRuleContext;

import java.util.function.Function;

/**
 * A base class for utilities which convert ANTLR parse trees into domain-specific objects.
 */
public class AntlrReaderBase extends MapperBase {

    /**
     * Match a parser context against a list of functions, returning the result of the first match.
     * @param <P0> the parser context type
     * @param <P> the result type
     * @param ctx the parser context
     * @param funs the functions to try
     * @return the result of the first matching function
     */
    protected static <P0 extends ParserRuleContext, P> P match(P0 ctx,
                                                               Function<P0, Maybe<P>>... funs) {
        if (null != ctx.exception) {
            throw new MapperException(ctx.exception);
        }

        for (Function<P0, Maybe<P>> f : funs) {
            Maybe<P> res = f.apply(ctx);
            if (res.isJust()) {
                return res.fromJust();
            }
        }
        return invalid("union failed to match");
    }
}
