package hydra.lib;

import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class Libraries {
    private Libraries() {
    }

    public static <A> List<PrimitiveFunction<A>> standardPrimitives() {
        List<PrimitiveFunction<A>> prims = new ArrayList<>();
        prims.addAll(flowsPrimitives());
        prims.addAll(listsPrimitives());
        prims.addAll(literalsPrimitives());
        prims.addAll(mapsPrimitives());
        prims.addAll(mathPrimitives());
        prims.addAll(optionalsPrimitives());
        prims.addAll(setsPrimitives());
        prims.addAll(stringsPrimitives());

        return prims;
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
            new hydra.lib.lists.Pure<>());
    }

    private static <A> List<PrimitiveFunction<A>> literalsPrimitives() {
        return Arrays.asList(
            new hydra.lib.literals.EqualBigfloat<>(),
            new hydra.lib.literals.EqualBigint<>(),
            new hydra.lib.literals.EqualBinary<>(),
            new hydra.lib.literals.EqualBoolean<>(),
            new hydra.lib.literals.EqualFloat32<>(),
            new hydra.lib.literals.EqualFloat64<>(),
            new hydra.lib.literals.EqualInt8<>(),
            new hydra.lib.literals.EqualInt16<>(),
            new hydra.lib.literals.EqualInt32<>(),
            new hydra.lib.literals.EqualInt64<>(),
            new hydra.lib.literals.EqualUint8<>(),
            new hydra.lib.literals.EqualUint16<>(),
            new hydra.lib.literals.EqualUint32<>(),
            new hydra.lib.literals.EqualUint64<>(),
            new hydra.lib.literals.EqualString<>(),
            new hydra.lib.literals.ShowInt32<>(),
            new hydra.lib.literals.ShowString<>());
    }

    private static <A> List<PrimitiveFunction<A>> mapsPrimitives() {
        return Arrays.asList(
            new hydra.lib.maps.Empty<>(),
            new hydra.lib.maps.Insert<>(),
            new hydra.lib.maps.Lookup<>());
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
            new hydra.lib.strings.Length<>(),
            new hydra.lib.strings.SplitOn<>(),
            new hydra.lib.strings.ToLower<>(),
            new hydra.lib.strings.ToUpper<>());
    }
}