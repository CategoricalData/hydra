package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.literals.*;


public interface Literals {
    static <A> Term<A> equalBigfloat() {
        return new EqualBigfloat<A>().term();
    }

    static <A> Term<A> equalBigint() {
        return new EqualBigint<A>().term();
    }

    static <A> Term<A> equalBinary() {
        return new EqualBinary<A>().term();
    }

    static <A> Term<A> equalBoolean() {
        return new EqualBoolean<A>().term();
    }

    static <A> Term<A> equalFloat32() {
        return new EqualFloat32<A>().term();
    }

    static <A> Term<A> equalFloat64() {
        return new EqualFloat64<A>().term();
    }

    static <A> Term<A> equalInt8() {
        return new EqualInt8<A>().term();
    }

    static <A> Term<A> equalInt16() {
        return new EqualInt16<A>().term();
    }

    static <A> Term<A> equalInt32() {
        return new EqualInt32<A>().term();
    }

    static <A> Term<A> equalInt64() {
        return new EqualInt64<A>().term();
    }

    static <A> Term<A> equalString() {
        return new EqualString<A>().term();
    }

    static <A> Term<A> equalUint8() {
        return new EqualUint8<A>().term();
    }

    static <A> Term<A> equalUint16() {
        return new EqualUint16<A>().term();
    }

    static <A> Term<A> equalUint32() {
        return new EqualUint32<A>().term();
    }

    static <A> Term<A> equalUint64() {
        return new EqualUint64<A>().term();
    }

    static <A> Term<A> showInt32() {
        return new ShowInt32<A>().term();
    }

    static <A> Term<A> showString() {
        return new ShowString<A>().term();
    }
}
