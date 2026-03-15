package hydra.dsl.meta;

import hydra.util.Adapter;
import hydra.util.Coder;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Meta-DSL for constructing computation-related terms (Adapter, Coder).
 *
 * <p>Mirrors Hydra.Dsl.Meta.Compute (Haskell).
 */
public interface Compute {

    // ============================================================
    // Adapter
    // ============================================================

    static Expr<Adapter> adapter(TTerm<?> isLossy, TTerm<?> source, TTerm<?> target, TTerm<?> coder) {
        return record(Adapter.TYPE_,
                field(Adapter.IS_LOSSY, isLossy),
                field(Adapter.SOURCE, source),
                field(Adapter.TARGET, target),
                field(Adapter.CODER, coder));
    }

    static Expr<Boolean> adapterIsLossy(TTerm<Adapter> a) {
        return apply(project(Adapter.TYPE_, Adapter.IS_LOSSY), a);
    }

    static Expr<?> adapterSource(TTerm<Adapter> a) {
        return apply(project(Adapter.TYPE_, Adapter.SOURCE), a);
    }

    static Expr<?> adapterTarget(TTerm<Adapter> a) {
        return apply(project(Adapter.TYPE_, Adapter.TARGET), a);
    }

    static Expr<?> adapterCoder(TTerm<Adapter> a) {
        return apply(project(Adapter.TYPE_, Adapter.CODER), a);
    }

    static Expr<Adapter> adapterWithCoder(TTerm<Adapter> a, TTerm<?> coder) {
        return adapter(adapterIsLossy(a), adapterSource(a), adapterTarget(a), coder);
    }

    static Expr<Adapter> adapterWithTarget(TTerm<Adapter> a, TTerm<?> target) {
        return adapter(adapterIsLossy(a), adapterSource(a), target, adapterCoder(a));
    }

    // ============================================================
    // Coder
    // ============================================================

    static Expr<Coder> coder(TTerm<?> encode, TTerm<?> decode) {
        return record(Coder.TYPE_,
                field(Coder.ENCODE, encode),
                field(Coder.DECODE, decode));
    }

    static Expr<?> coderEncode(TTerm<Coder> c) {
        return apply(project(Coder.TYPE_, Coder.ENCODE), c);
    }

    static Expr<?> coderDecode(TTerm<Coder> c) {
        return apply(project(Coder.TYPE_, Coder.DECODE), c);
    }
}
