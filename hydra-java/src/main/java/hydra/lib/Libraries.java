package hydra.lib;

import hydra.lib.equality.Equal;
import hydra.lib.equality.EqualBigfloat;
import hydra.lib.equality.EqualBigint;
import hydra.lib.equality.EqualBinary;
import hydra.lib.equality.EqualBoolean;
import hydra.lib.equality.EqualFloat32;
import hydra.lib.equality.EqualFloat64;
import hydra.lib.equality.EqualInt16;
import hydra.lib.equality.EqualInt32;
import hydra.lib.equality.EqualInt64;
import hydra.lib.equality.EqualInt8;
import hydra.lib.equality.EqualString;
import hydra.lib.equality.EqualTerm;
import hydra.lib.equality.EqualType;
import hydra.lib.equality.EqualUint16;
import hydra.lib.equality.EqualUint32;
import hydra.lib.equality.EqualUint64;
import hydra.lib.equality.EqualUint8;
import hydra.lib.equality.GtInt32;
import hydra.lib.equality.GteInt32;
import hydra.lib.equality.Identity;
import hydra.lib.equality.LtInt32;
import hydra.lib.equality.LteInt32;
import hydra.lib.io.ShowTerm;
import hydra.lib.maps.Elems;
import hydra.lib.sets.Delete;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * A registry of all of the primitive functions available in Hydra-Java.
 */
public class Libraries {
    private Libraries() {
    }

    /**
     * All standard primitive functions as a list.
     */
    public static List<PrimitiveFunction> standardPrimitives() {
        List<PrimitiveFunction> prims = new ArrayList<>();
        prims.addAll(equalityPrimitives());
        prims.addAll(flowsPrimitives());
        prims.addAll(ioPrimitives());
        prims.addAll(listsPrimitives());
        prims.addAll(literalsPrimitives());
        prims.addAll(logicPrimitives());
        prims.addAll(mapsPrimitives());
        prims.addAll(mathPrimitives());
        prims.addAll(optionalsPrimitives());
        prims.addAll(setsPrimitives());
        prims.addAll(stringsPrimitives());

        return prims;
    }

    private static List<PrimitiveFunction> equalityPrimitives() {
        return Arrays.asList(
                new Equal(),
                new EqualBigfloat(),
                new EqualBigint(),
                new EqualBinary(),
                new EqualBoolean(),
                new EqualFloat32(),
                new EqualFloat64(),
                new EqualInt8(),
                new EqualInt16(),
                new EqualInt32(),
                new EqualInt64(),
                new EqualString(),
                new EqualTerm(),
                new EqualType(),
                new EqualUint8(),
                new EqualUint16(),
                new EqualUint32(),
                new EqualUint64(),
                new GteInt32(),
                new GtInt32(),
                new Identity(),
                new LteInt32(),
                new LtInt32());
    }

    private static List<PrimitiveFunction> flowsPrimitives() {
        return Arrays.asList(
                new hydra.lib.flows.Apply(),
                new hydra.lib.flows.Bind(),
                new hydra.lib.flows.Map(),
                new hydra.lib.flows.MapList(),
                new hydra.lib.flows.Pure());
    }

    private static List<PrimitiveFunction> ioPrimitives() {
        return List.of(new ShowTerm());
    }

    private static List<PrimitiveFunction> listsPrimitives() {
        return Arrays.asList(
                new hydra.lib.lists.Apply(),
                new hydra.lib.lists.At(),
                new hydra.lib.lists.Bind(),
                new hydra.lib.lists.Concat(),
                new hydra.lib.lists.Concat2(),
                new hydra.lib.lists.Cons(),
                new hydra.lib.lists.Foldl(),
                new hydra.lib.lists.Head(),
                new hydra.lib.lists.Intercalate(),
                new hydra.lib.lists.Intersperse(),
                new hydra.lib.lists.Last(),
                new hydra.lib.lists.Length(),
                new hydra.lib.lists.Map(),
                new hydra.lib.lists.Nub(),
                new hydra.lib.lists.Null(),
                new hydra.lib.lists.Pure(),
                new hydra.lib.lists.Reverse(),
                new hydra.lib.lists.SafeHead(),
                new hydra.lib.lists.Tail());
    }

    private static List<PrimitiveFunction> literalsPrimitives() {
        return Arrays.asList(
                new hydra.lib.literals.BigfloatToBigint(),
                new hydra.lib.literals.BigfloatToFloat32(),
                new hydra.lib.literals.BigfloatToFloat64(),
                new hydra.lib.literals.BigintToBigfloat(),
                new hydra.lib.literals.BigintToInt8(),
                new hydra.lib.literals.BigintToInt16(),
                new hydra.lib.literals.BigintToInt32(),
                new hydra.lib.literals.BigintToInt64(),
                new hydra.lib.literals.BigintToUint8(),
                new hydra.lib.literals.BigintToUint16(),
                new hydra.lib.literals.BigintToUint32(),
                new hydra.lib.literals.BigintToUint64(),
                new hydra.lib.literals.Float32ToBigfloat(),
                new hydra.lib.literals.Float64ToBigfloat(),
                new hydra.lib.literals.Int8ToBigint(),
                new hydra.lib.literals.Int16ToBigint(),
                new hydra.lib.literals.Int32ToBigint(),
                new hydra.lib.literals.Int64ToBigint(),
                new hydra.lib.literals.ShowInt32(),
                new hydra.lib.literals.ShowString(),
                new hydra.lib.literals.Uint8ToBigint(),
                new hydra.lib.literals.Uint16ToBigint(),
                new hydra.lib.literals.Uint32ToBigint(),
                new hydra.lib.literals.Uint64ToBigint());
    }

    private static List<PrimitiveFunction> logicPrimitives() {
        return Arrays.asList(
                new hydra.lib.logic.And(),
                new hydra.lib.logic.IfElse(),
                new hydra.lib.logic.Not(),
                new hydra.lib.logic.Or());
    }

    private static List<PrimitiveFunction> mapsPrimitives() {
        return Arrays.asList(
                new hydra.lib.maps.Empty(),
                new hydra.lib.maps.FromList(),
                new hydra.lib.maps.Insert(),
                new hydra.lib.maps.IsEmpty(),
                new hydra.lib.maps.Keys(),
                new hydra.lib.maps.Lookup(),
                new hydra.lib.maps.Map(),
                new hydra.lib.maps.MapKeys(),
                new hydra.lib.maps.Remove(),
                new hydra.lib.maps.ToList(),
                new Elems());
    }

    private static List<PrimitiveFunction> mathPrimitives() {
        return Arrays.asList(
                new hydra.lib.math.Add(),
                new hydra.lib.math.Div(),
                new hydra.lib.math.Mod(),
                new hydra.lib.math.Mul(),
                new hydra.lib.math.Neg(),
                new hydra.lib.math.Rem(),
                new hydra.lib.math.Sub());
    }

    private static List<PrimitiveFunction> optionalsPrimitives() {
        return Arrays.asList(
                new hydra.lib.math.Add(),
                new hydra.lib.optionals.Apply(),
                new hydra.lib.optionals.Bind(),
                new hydra.lib.optionals.Cat(),
                new hydra.lib.optionals.Compose(),
                new hydra.lib.optionals.IsJust(),
                new hydra.lib.optionals.IsNothing(),
                new hydra.lib.optionals.Map(),
                new hydra.lib.optionals.Pure());
    }

    private static List<PrimitiveFunction> setsPrimitives() {
        return Arrays.asList(
                new hydra.lib.sets.Difference(),
                new hydra.lib.sets.Empty(),
                new hydra.lib.sets.FromList(),
                new hydra.lib.sets.Insert(),
                new hydra.lib.sets.Intersection(),
                new hydra.lib.sets.IsEmpty(),
                new hydra.lib.sets.Map(),
                new hydra.lib.sets.Member(),
                new Delete(),
                new hydra.lib.sets.Singleton(),
                new hydra.lib.sets.Size(),
                new hydra.lib.sets.ToList(),
                new hydra.lib.sets.Union());
    }

    private static List<PrimitiveFunction> stringsPrimitives() {
        return Arrays.asList(
                new hydra.lib.strings.Cat(),
                new hydra.lib.strings.Cat2(),
                new hydra.lib.strings.FromList(),
                new hydra.lib.strings.Intercalate(),
                new hydra.lib.strings.IsEmpty(),
                new hydra.lib.strings.Length(),
                new hydra.lib.strings.SplitOn(),
                new hydra.lib.strings.ToList(),
                new hydra.lib.strings.ToLower(),
                new hydra.lib.strings.ToUpper());
    }
}