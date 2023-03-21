package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.flows.Apply;
import hydra.lib.flows.Bind;
import hydra.lib.flows.Map;
import hydra.lib.flows.Pure;


public interface Flows {
    static <A> Term<A> apply() {
        return new Apply<A>().term();
    }
    static <A> Term<A> bind() {
        return new Bind<A>().term();
    }
    static <A> Term<A> map() {
        return new Map<A>().term();
    }
    static <A> Term<A> pure() {
        return new Pure<A>().term();
    }
}
