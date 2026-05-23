package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.literals} library. */
public final class Literals {
    private Literals() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.literals." + name);
    }

    public static <A> TTerm<A> bigintToDecimal(TTerm<?> x) { return Phantoms.apply(prim("bigintToDecimal"), x); }
    public static <A> TTerm<A> bigintToInt8(TTerm<?> x) { return Phantoms.apply(prim("bigintToInt8"), x); }
    public static <A> TTerm<A> bigintToInt16(TTerm<?> x) { return Phantoms.apply(prim("bigintToInt16"), x); }
    public static <A> TTerm<A> bigintToInt32(TTerm<?> x) { return Phantoms.apply(prim("bigintToInt32"), x); }
    public static <A> TTerm<A> bigintToInt64(TTerm<?> x) { return Phantoms.apply(prim("bigintToInt64"), x); }
    public static <A> TTerm<A> bigintToUint8(TTerm<?> x) { return Phantoms.apply(prim("bigintToUint8"), x); }
    public static <A> TTerm<A> bigintToUint16(TTerm<?> x) { return Phantoms.apply(prim("bigintToUint16"), x); }
    public static <A> TTerm<A> bigintToUint32(TTerm<?> x) { return Phantoms.apply(prim("bigintToUint32"), x); }
    public static <A> TTerm<A> bigintToUint64(TTerm<?> x) { return Phantoms.apply(prim("bigintToUint64"), x); }
    public static <A> TTerm<A> binaryToBytes(TTerm<?> x) { return Phantoms.apply(prim("binaryToBytes"), x); }
    public static <A> TTerm<A> binaryToString(TTerm<?> s) { return Phantoms.apply(prim("binaryToString"), s); }
    public static <A> TTerm<A> decimalToBigint(TTerm<?> x) { return Phantoms.apply(prim("decimalToBigint"), x); }
    public static <A> TTerm<A> decimalToFloat32(TTerm<?> x) { return Phantoms.apply(prim("decimalToFloat32"), x); }
    public static <A> TTerm<A> decimalToFloat64(TTerm<?> x) { return Phantoms.apply(prim("decimalToFloat64"), x); }
    public static <A> TTerm<A> float32ToDecimal(TTerm<?> x) { return Phantoms.apply(prim("float32ToDecimal"), x); }
    public static <A> TTerm<A> float32ToFloat64(TTerm<?> x) { return Phantoms.apply(prim("float32ToFloat64"), x); }
    public static <A> TTerm<A> float64ToDecimal(TTerm<?> x) { return Phantoms.apply(prim("float64ToDecimal"), x); }
    public static <A> TTerm<A> float64ToFloat32(TTerm<?> x) { return Phantoms.apply(prim("float64ToFloat32"), x); }
    public static <A> TTerm<A> int8ToBigint(TTerm<?> x) { return Phantoms.apply(prim("int8ToBigint"), x); }
    public static <A> TTerm<A> int16ToBigint(TTerm<?> x) { return Phantoms.apply(prim("int16ToBigint"), x); }
    public static <A> TTerm<A> int32ToBigint(TTerm<?> x) { return Phantoms.apply(prim("int32ToBigint"), x); }
    public static <A> TTerm<A> int64ToBigint(TTerm<?> x) { return Phantoms.apply(prim("int64ToBigint"), x); }
    public static <A> TTerm<A> readBoolean(TTerm<?> s) { return Phantoms.apply(prim("readBoolean"), s); }
    public static <A> TTerm<A> readDecimal(TTerm<?> s) { return Phantoms.apply(prim("readDecimal"), s); }
    public static <A> TTerm<A> readFloat32(TTerm<?> s) { return Phantoms.apply(prim("readFloat32"), s); }
    public static <A> TTerm<A> readFloat64(TTerm<?> s) { return Phantoms.apply(prim("readFloat64"), s); }
    public static <A> TTerm<A> readInt32(TTerm<?> s) { return Phantoms.apply(prim("readInt32"), s); }
    public static <A> TTerm<A> readInt64(TTerm<?> s) { return Phantoms.apply(prim("readInt64"), s); }
    public static <A> TTerm<A> readString(TTerm<?> s) { return Phantoms.apply(prim("readString"), s); }
    public static <A> TTerm<A> showBigint(TTerm<?> x) { return Phantoms.apply(prim("showBigint"), x); }
    public static <A> TTerm<A> showBoolean(TTerm<?> b) { return Phantoms.apply(prim("showBoolean"), b); }
    public static <A> TTerm<A> showDecimal(TTerm<?> x) { return Phantoms.apply(prim("showDecimal"), x); }
    public static <A> TTerm<A> showFloat32(TTerm<?> x) { return Phantoms.apply(prim("showFloat32"), x); }
    public static <A> TTerm<A> showFloat64(TTerm<?> x) { return Phantoms.apply(prim("showFloat64"), x); }
    public static <A> TTerm<A> showInt8(TTerm<?> x) { return Phantoms.apply(prim("showInt8"), x); }
    public static <A> TTerm<A> showInt16(TTerm<?> x) { return Phantoms.apply(prim("showInt16"), x); }
    public static <A> TTerm<A> showInt32(TTerm<?> x) { return Phantoms.apply(prim("showInt32"), x); }
    public static <A> TTerm<A> showInt64(TTerm<?> x) { return Phantoms.apply(prim("showInt64"), x); }
    public static <A> TTerm<A> showUint8(TTerm<?> x) { return Phantoms.apply(prim("showUint8"), x); }
    public static <A> TTerm<A> showUint16(TTerm<?> x) { return Phantoms.apply(prim("showUint16"), x); }
    public static <A> TTerm<A> showUint32(TTerm<?> x) { return Phantoms.apply(prim("showUint32"), x); }
    public static <A> TTerm<A> showUint64(TTerm<?> x) { return Phantoms.apply(prim("showUint64"), x); }
    public static <A> TTerm<A> showString(TTerm<?> s) { return Phantoms.apply(prim("showString"), s); }
    public static <A> TTerm<A> stringToBinary(TTerm<?> s) { return Phantoms.apply(prim("stringToBinary"), s); }
    public static <A> TTerm<A> uint8ToBigint(TTerm<?> x) { return Phantoms.apply(prim("uint8ToBigint"), x); }
    public static <A> TTerm<A> uint16ToBigint(TTerm<?> x) { return Phantoms.apply(prim("uint16ToBigint"), x); }
    public static <A> TTerm<A> uint32ToBigint(TTerm<?> x) { return Phantoms.apply(prim("uint32ToBigint"), x); }
    public static <A> TTerm<A> uint64ToBigint(TTerm<?> x) { return Phantoms.apply(prim("uint64ToBigint"), x); }
}
