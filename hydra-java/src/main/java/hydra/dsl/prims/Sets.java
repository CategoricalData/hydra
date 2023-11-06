package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.sets.Contains;
import hydra.lib.sets.Empty;
import hydra.lib.sets.FromList;
import hydra.lib.sets.Insert;
import hydra.lib.sets.IsEmpty;
import hydra.lib.sets.Map;
import hydra.lib.sets.Remove;
import hydra.lib.sets.Singleton;
import hydra.lib.sets.Size;
import hydra.lib.sets.ToList;

public interface Sets {
    static <A> Term<A> contains() {
        return new Contains().term();
    }

    static <A> Term<A> empty() {
        return new Empty().term();
    }

    static <A> Term<A> fromList() {
        return new FromList().term();
    }

    static <A> Term<A> insert() {
        return new Insert().term();
    }

    static <A> Term<A> isEmpty() {
        return new IsEmpty().term();
    }

    static <A> Term<A> map() {
        return new Map().term();
    }

    static <A> Term<A> remove() {
        return new Remove().term();
    }

    static <A> Term<A> singleton() {
        return new Singleton().term();
    }

    static <A> Term<A> size() {
        return new Size().term();
    }

    static <A> Term<A> toList() {
        return new ToList().term();
    }
}
