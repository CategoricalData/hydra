package hydra.tools;

import org.antlr.v4.runtime.ParserRuleContext;

import java.util.Optional;
import java.util.function.Function;

/**
 * A base class for utilities which convert ANTLR parse trees into domain-specific objects
 */
public class AntlrReaderBase extends MapperBase {

    protected static <P0 extends ParserRuleContext, P> P match(P0 ctx,
                                                               Function<P0, Optional<P>>... funs) {
        if (null != ctx.exception) {
            throw new MapperException(ctx.exception);
        }

        for (Function<P0, Optional<P>> f : funs) {
            Optional<P> res = f.apply(ctx);
            if (res.isPresent()) {
                return res.get();
            }
        }
        return invalid("union failed to match");
    }
}
