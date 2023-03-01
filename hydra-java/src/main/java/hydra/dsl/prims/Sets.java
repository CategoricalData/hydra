package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.sets.*;

public interface Sets {
    static <M> Term<M> contains() {
        return new Contains().term();
    }

    static <M> Term<M> empty() {
        return new Empty().term();
    }

    static <M> Term<M> fromList() {
        return new FromList().term();
    }

    static <M> Term<M> insert() {
        return new Insert().term();
    }

    static <M> Term<M> isEmpty() {
        return new IsEmpty().term();
    }

    static <M> Term<M> map() {
        return new Map().term();
    }

    static <M> Term<M> remove() {
        return new Remove().term();
    }

    static <M> Term<M> singleton() {
        return new Singleton().term();
    }

    static <M> Term<M> size() {
        return new Size().term();
    }

    static <M> Term<M> toList() {
        return new ToList().term();
    }
}
