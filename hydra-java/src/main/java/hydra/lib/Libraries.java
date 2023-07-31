package hydra.lib;

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
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class Libraries {
    private Libraries() {
    }

    public static <A> List<PrimitiveFunction<A>> standardPrimitives() {
        List<PrimitiveFunction<A>> prims = new ArrayList<>();
        prims.addAll(equalityPrimitives());
        prims.addAll(flowsPrimitives());
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

    private static <A> List<PrimitiveFunction<A>> equalityPrimitives() {
        return Arrays.asList(
                new EqualBigfloat<>(),
                new EqualBigint<>(),
                new EqualBinary<>(),
                new EqualBoolean<>(),
                new EqualFloat32<>(),
                new EqualFloat64<>(),
                new EqualInt8<>(),
                new EqualInt16<>(),
                new EqualInt32<>(),
                new EqualInt64<>(),
                new EqualString<>(),
                new EqualTerm<>(),
                new EqualType<>(),
                new EqualUint8<>(),
                new EqualUint16<>(),
                new EqualUint32<>(),
                new EqualUint64<>());
    }

    private static <A> List<PrimitiveFunction<A>> flowsPrimitives() {
        return Arrays.asList(
                new hydra.lib.flows.Apply<>(),
                new hydra.lib.flows.Bind<>(),
                new hydra.lib.flows.Map<>(),
                new hydra.lib.flows.Pure<>());
    }

    private static <A> List<PrimitiveFunction<A>> listsPrimitives() {
        return Arrays.asList(
                new hydra.lib.lists.Apply<>(),
                new hydra.lib.lists.Bind<>(),
                new hydra.lib.lists.Concat<>(),
                new hydra.lib.lists.Head<>(),
                new hydra.lib.lists.Intercalate<>(),
                new hydra.lib.lists.Intersperse<>(),
                new hydra.lib.lists.Last<>(),
                new hydra.lib.lists.Length<>(),
                new hydra.lib.lists.Map<>(),
                new hydra.lib.lists.Pure<>(),
                new hydra.lib.lists.Tail<>());
    }

    private static <A> List<PrimitiveFunction<A>> literalsPrimitives() {
        return Arrays.asList(
                new hydra.lib.literals.ShowInt32<>(),
                new hydra.lib.literals.ShowString<>());
    }

    private static <A> List<PrimitiveFunction<A>> logicPrimitives() {
        return Arrays.asList(
                new hydra.lib.logic.And<>(),
                new hydra.lib.logic.IfElse<>(),
                new hydra.lib.logic.Not<>(),
                new hydra.lib.logic.Or<>());
    }

    private static <A> List<PrimitiveFunction<A>> mapsPrimitives() {
        return Arrays.asList(
                new hydra.lib.maps.Empty<>(),
                new hydra.lib.maps.FromList<>(),
                new hydra.lib.maps.Insert<>(),
                new hydra.lib.maps.Lookup<>(),
                new hydra.lib.maps.Map<>(),
                new hydra.lib.maps.MapKeys<>());
    }

    private static <A> List<PrimitiveFunction<A>> mathPrimitives() {
        return Arrays.asList(
                new hydra.lib.math.Add<>(),
                new hydra.lib.math.Div<>(),
                new hydra.lib.math.Mod<>(),
                new hydra.lib.math.Mul<>(),
                new hydra.lib.math.Neg<>(),
                new hydra.lib.math.Rem<>(),
                new hydra.lib.math.Sub<>());
    }

    private static <A> List<PrimitiveFunction<A>> optionalsPrimitives() {
        return Arrays.asList(
                new hydra.lib.math.Add<>(),
                new hydra.lib.optionals.Apply<>(),
                new hydra.lib.optionals.Bind<>(),
                new hydra.lib.optionals.Map<>(),
                new hydra.lib.optionals.Pure<>());
    }

    private static <A> List<PrimitiveFunction<A>> setsPrimitives() {
        return Arrays.asList(
                new hydra.lib.sets.Contains<>(),
                new hydra.lib.sets.Empty<>(),
                new hydra.lib.sets.FromList<>(),
                new hydra.lib.sets.Insert<>(),
                new hydra.lib.sets.IsEmpty<>(),
                new hydra.lib.sets.Map<>(),
                new hydra.lib.sets.Remove<>(),
                new hydra.lib.sets.Singleton<>(),
                new hydra.lib.sets.Size<>(),
                new hydra.lib.sets.ToList<>());
    }

    private static <A> List<PrimitiveFunction<A>> stringsPrimitives() {
        return Arrays.asList(
                new hydra.lib.strings.Cat<>(),
                new hydra.lib.strings.IsEmpty<>(),
                new hydra.lib.strings.Length<>(),
                new hydra.lib.strings.SplitOn<>(),
                new hydra.lib.strings.ToLower<>(),
                new hydra.lib.strings.ToUpper<>());
    }
}