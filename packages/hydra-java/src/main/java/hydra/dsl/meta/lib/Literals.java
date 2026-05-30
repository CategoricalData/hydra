package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.literals} library. */
public final class Literals {
    private Literals() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.literals." + name);
    }

    public static <A> TypedTerm<A> bigintToDecimal(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToDecimal"), x); }
    public static <A> TypedTerm<A> bigintToInt8(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToInt8"), x); }
    public static <A> TypedTerm<A> bigintToInt16(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToInt16"), x); }
    public static <A> TypedTerm<A> bigintToInt32(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToInt32"), x); }
    public static <A> TypedTerm<A> bigintToInt64(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToInt64"), x); }
    public static <A> TypedTerm<A> bigintToUint8(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToUint8"), x); }
    public static <A> TypedTerm<A> bigintToUint16(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToUint16"), x); }
    public static <A> TypedTerm<A> bigintToUint32(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToUint32"), x); }
    public static <A> TypedTerm<A> bigintToUint64(TypedTerm<?> x) { return Phantoms.apply(prim("bigintToUint64"), x); }
    public static <A> TypedTerm<A> binaryToBytes(TypedTerm<?> x) { return Phantoms.apply(prim("binaryToBytes"), x); }
    public static <A> TypedTerm<A> binaryToString(TypedTerm<?> s) { return Phantoms.apply(prim("binaryToString"), s); }
    public static <A> TypedTerm<A> decimalToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("decimalToBigint"), x); }
    public static <A> TypedTerm<A> decimalToFloat32(TypedTerm<?> x) { return Phantoms.apply(prim("decimalToFloat32"), x); }
    public static <A> TypedTerm<A> decimalToFloat64(TypedTerm<?> x) { return Phantoms.apply(prim("decimalToFloat64"), x); }
    public static <A> TypedTerm<A> float32ToDecimal(TypedTerm<?> x) { return Phantoms.apply(prim("float32ToDecimal"), x); }
    public static <A> TypedTerm<A> float32ToFloat64(TypedTerm<?> x) { return Phantoms.apply(prim("float32ToFloat64"), x); }
    public static <A> TypedTerm<A> float64ToDecimal(TypedTerm<?> x) { return Phantoms.apply(prim("float64ToDecimal"), x); }
    public static <A> TypedTerm<A> float64ToFloat32(TypedTerm<?> x) { return Phantoms.apply(prim("float64ToFloat32"), x); }
    public static <A> TypedTerm<A> int8ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("int8ToBigint"), x); }
    public static <A> TypedTerm<A> int16ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("int16ToBigint"), x); }
    public static <A> TypedTerm<A> int32ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("int32ToBigint"), x); }
    public static <A> TypedTerm<A> int64ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("int64ToBigint"), x); }
    public static <A> TypedTerm<A> readBoolean(TypedTerm<?> s) { return Phantoms.apply(prim("readBoolean"), s); }
    public static <A> TypedTerm<A> readDecimal(TypedTerm<?> s) { return Phantoms.apply(prim("readDecimal"), s); }
    public static <A> TypedTerm<A> readFloat32(TypedTerm<?> s) { return Phantoms.apply(prim("readFloat32"), s); }
    public static <A> TypedTerm<A> readFloat64(TypedTerm<?> s) { return Phantoms.apply(prim("readFloat64"), s); }
    public static <A> TypedTerm<A> readInt32(TypedTerm<?> s) { return Phantoms.apply(prim("readInt32"), s); }
    public static <A> TypedTerm<A> readInt64(TypedTerm<?> s) { return Phantoms.apply(prim("readInt64"), s); }
    public static <A> TypedTerm<A> readString(TypedTerm<?> s) { return Phantoms.apply(prim("readString"), s); }
    public static <A> TypedTerm<A> showBigint(TypedTerm<?> x) { return Phantoms.apply(prim("showBigint"), x); }
    public static <A> TypedTerm<A> showBoolean(TypedTerm<?> b) { return Phantoms.apply(prim("showBoolean"), b); }
    public static <A> TypedTerm<A> showDecimal(TypedTerm<?> x) { return Phantoms.apply(prim("showDecimal"), x); }
    public static <A> TypedTerm<A> showFloat32(TypedTerm<?> x) { return Phantoms.apply(prim("showFloat32"), x); }
    public static <A> TypedTerm<A> showFloat64(TypedTerm<?> x) { return Phantoms.apply(prim("showFloat64"), x); }
    public static <A> TypedTerm<A> showInt8(TypedTerm<?> x) { return Phantoms.apply(prim("showInt8"), x); }
    public static <A> TypedTerm<A> showInt16(TypedTerm<?> x) { return Phantoms.apply(prim("showInt16"), x); }
    public static <A> TypedTerm<A> showInt32(TypedTerm<?> x) { return Phantoms.apply(prim("showInt32"), x); }
    public static <A> TypedTerm<A> showInt64(TypedTerm<?> x) { return Phantoms.apply(prim("showInt64"), x); }
    public static <A> TypedTerm<A> showUint8(TypedTerm<?> x) { return Phantoms.apply(prim("showUint8"), x); }
    public static <A> TypedTerm<A> showUint16(TypedTerm<?> x) { return Phantoms.apply(prim("showUint16"), x); }
    public static <A> TypedTerm<A> showUint32(TypedTerm<?> x) { return Phantoms.apply(prim("showUint32"), x); }
    public static <A> TypedTerm<A> showUint64(TypedTerm<?> x) { return Phantoms.apply(prim("showUint64"), x); }
    public static <A> TypedTerm<A> showString(TypedTerm<?> s) { return Phantoms.apply(prim("showString"), s); }
    public static <A> TypedTerm<A> stringToBinary(TypedTerm<?> s) { return Phantoms.apply(prim("stringToBinary"), s); }
    public static <A> TypedTerm<A> uint8ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("uint8ToBigint"), x); }
    public static <A> TypedTerm<A> uint16ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("uint16ToBigint"), x); }
    public static <A> TypedTerm<A> uint32ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("uint32ToBigint"), x); }
    public static <A> TypedTerm<A> uint64ToBigint(TypedTerm<?> x) { return Phantoms.apply(prim("uint64ToBigint"), x); }
}
