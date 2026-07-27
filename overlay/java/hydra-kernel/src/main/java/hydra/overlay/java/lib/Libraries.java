package hydra.overlay.java.lib;

import hydra.overlay.java.lib.equality.Equal;
import hydra.overlay.java.lib.functions.Identity;
import hydra.overlay.java.lib.ordering.Compare;
import hydra.overlay.java.lib.ordering.Gt;
import hydra.overlay.java.lib.ordering.Gte;
import hydra.overlay.java.lib.ordering.Lt;
import hydra.overlay.java.lib.ordering.Lte;
import hydra.overlay.java.lib.ordering.Max;
import hydra.overlay.java.lib.ordering.Min;
import hydra.overlay.java.lib.chars.IsAlphaNum;
import hydra.overlay.java.lib.chars.IsLower;
import hydra.overlay.java.lib.chars.IsSpace;
import hydra.overlay.java.lib.chars.IsUpper;
import hydra.overlay.java.lib.maps.Elems;
import hydra.overlay.java.lib.sets.Delete;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.Arrays;
import java.util.List;

import hydra.overlay.java.util.ConsList;


/**
 * A registry of all of the primitive functions available in Hydra-Java.
 */
public class Libraries {
    private Libraries() {
    }

    /**
     * All standard primitive functions as a list.
     *
     * @return a list containing all standard primitive functions from all categories
     */
    public static List<PrimitiveFunction> standardPrimitives() {
        ConsList<PrimitiveFunction> reversed = ConsList.empty();
        for (List<PrimitiveFunction> group : Arrays.asList(
                charsPrimitives(),
                effectsPrimitives(),
                eithersPrimitives(),
                equalityPrimitives(),
                filesPrimitives(),
                functionsPrimitives(),
                hashingPrimitives(),
                listsPrimitives(),
                literalsPrimitives(),
                logicPrimitives(),
                mapsPrimitives(),
                mathPrimitives(),
                optionalsPrimitives(),
                orderingPrimitives(),
                pairsPrimitives(),
                regexPrimitives(),
                setsPrimitives(),
                stringsPrimitives(),
                systemPrimitives(),
                textPrimitives())) {
            for (PrimitiveFunction p : group) {
                reversed = ConsList.cons(p, reversed);
            }
        }
        return reversed.reverse();
    }

    private static List<PrimitiveFunction> charsPrimitives() {
        return Arrays.asList(
                new IsAlphaNum(),
                new IsLower(),
                new IsSpace(),
                new IsUpper(),
                new hydra.overlay.java.lib.chars.ToLower(),
                new hydra.overlay.java.lib.chars.ToUpper());
    }

    private static List<PrimitiveFunction> effectsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.effects.Apply(),
                new hydra.overlay.java.lib.effects.Bind(),
                new hydra.overlay.java.lib.effects.Compose(),
                new hydra.overlay.java.lib.effects.FoldList(),
                new hydra.overlay.java.lib.effects.Map(),
                new hydra.overlay.java.lib.effects.MapList(),
                new hydra.overlay.java.lib.effects.MapOptional(),
                new hydra.overlay.java.lib.effects.Pure());
    }

    private static List<PrimitiveFunction> eithersPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.eithers.Bimap(),
                new hydra.overlay.java.lib.eithers.Bind(),
                new hydra.overlay.java.lib.eithers.Either(),
                new hydra.overlay.java.lib.eithers.FoldList(),
                new hydra.overlay.java.lib.eithers.IsLeft(),
                new hydra.overlay.java.lib.eithers.IsRight(),
                new hydra.overlay.java.lib.eithers.Lefts(),
                new hydra.overlay.java.lib.eithers.Map(),
                new hydra.overlay.java.lib.eithers.MapList(),
                new hydra.overlay.java.lib.eithers.MapOptional(),
                new hydra.overlay.java.lib.eithers.MapSet(),
                new hydra.overlay.java.lib.eithers.Partition(),
                new hydra.overlay.java.lib.eithers.Rights());
    }

    private static List<PrimitiveFunction> equalityPrimitives() {
        return Arrays.asList(
                new Equal());
    }

    private static List<PrimitiveFunction> filesPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.files.AppendFile(),
                new hydra.overlay.java.lib.files.Copy(),
                new hydra.overlay.java.lib.files.CreateDirectory(),
                new hydra.overlay.java.lib.files.Exists(),
                new hydra.overlay.java.lib.files.ListDirectory(),
                new hydra.overlay.java.lib.files.ReadFile(),
                new hydra.overlay.java.lib.files.RemoveDirectory(),
                new hydra.overlay.java.lib.files.RemoveFile(),
                new hydra.overlay.java.lib.files.Rename(),
                new hydra.overlay.java.lib.files.Status(),
                new hydra.overlay.java.lib.files.WriteFile());
    }

    private static List<PrimitiveFunction> functionsPrimitives() {
        return Arrays.asList(
                new Identity());
    }

    private static List<PrimitiveFunction> hashingPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.hashing.Sha256(),
                new hydra.overlay.java.lib.hashing.Sha256Hex());
    }

    private static List<PrimitiveFunction> listsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.lists.Apply(),
                new hydra.overlay.java.lib.lists.At(),
                new hydra.overlay.java.lib.lists.Bind(),
                new hydra.overlay.java.lib.lists.Concat(),
                new hydra.overlay.java.lib.lists.Concat2(),
                new hydra.overlay.java.lib.lists.Cons(),
                new hydra.overlay.java.lib.lists.Distinct(),
                new hydra.overlay.java.lib.lists.Drop(),
                new hydra.overlay.java.lib.lists.DropWhile(),
                new hydra.overlay.java.lib.lists.Filter(),
                new hydra.overlay.java.lib.lists.Find(),
                new hydra.overlay.java.lib.lists.Foldl(),
                new hydra.overlay.java.lib.lists.Foldr(),
                new hydra.overlay.java.lib.lists.Group(),
                new hydra.overlay.java.lib.lists.Head(),
                new hydra.overlay.java.lib.lists.Init(),
                new hydra.overlay.java.lib.lists.Intersperse(),
                new hydra.overlay.java.lib.lists.Join(),
                new hydra.overlay.java.lib.lists.Last(),
                new hydra.overlay.java.lib.lists.Length(),
                new hydra.overlay.java.lib.lists.Map(),
                new hydra.overlay.java.lib.lists.Member(),
                new hydra.overlay.java.lib.lists.Null(),
                new hydra.overlay.java.lib.lists.Partition(),
                new hydra.overlay.java.lib.lists.Pure(),
                new hydra.overlay.java.lib.lists.Replicate(),
                new hydra.overlay.java.lib.lists.Reverse(),
                new hydra.overlay.java.lib.lists.Singleton(),
                new hydra.overlay.java.lib.lists.Sort(),
                new hydra.overlay.java.lib.lists.SortBy(),
                new hydra.overlay.java.lib.lists.Span(),
                new hydra.overlay.java.lib.lists.Tail(),
                new hydra.overlay.java.lib.lists.Take(),
                new hydra.overlay.java.lib.lists.Transpose(),
                new hydra.overlay.java.lib.lists.Uncons(),
                new hydra.overlay.java.lib.lists.Zip(),
                new hydra.overlay.java.lib.lists.ZipWith());
    }

    private static List<PrimitiveFunction> literalsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.literals.BigintToDecimal(),
                new hydra.overlay.java.lib.literals.BigintToInt16(),
                new hydra.overlay.java.lib.literals.BigintToInt32(),
                new hydra.overlay.java.lib.literals.BigintToInt64(),
                new hydra.overlay.java.lib.literals.BigintToInt8(),
                new hydra.overlay.java.lib.literals.BigintToUint16(),
                new hydra.overlay.java.lib.literals.BigintToUint32(),
                new hydra.overlay.java.lib.literals.BigintToUint64(),
                new hydra.overlay.java.lib.literals.BigintToUint8(),
                new hydra.overlay.java.lib.literals.BinaryToBytes(),
                new hydra.overlay.java.lib.literals.BinaryToString(),
                new hydra.overlay.java.lib.literals.DecimalToBigint(),
                new hydra.overlay.java.lib.literals.DecimalToFloat32(),
                new hydra.overlay.java.lib.literals.DecimalToFloat64(),
                new hydra.overlay.java.lib.literals.Float32ToDecimal(),
                new hydra.overlay.java.lib.literals.Float32ToFloat64(),
                new hydra.overlay.java.lib.literals.Float64ToDecimal(),
                new hydra.overlay.java.lib.literals.Float64ToFloat32(),
                new hydra.overlay.java.lib.literals.Int16ToBigint(),
                new hydra.overlay.java.lib.literals.Int32ToBigint(),
                new hydra.overlay.java.lib.literals.Int64ToBigint(),
                new hydra.overlay.java.lib.literals.Int8ToBigint(),
                new hydra.overlay.java.lib.literals.ParseBoolean(),
                new hydra.overlay.java.lib.literals.ParseString(),
                new hydra.overlay.java.lib.literals.PrintBoolean(),
                new hydra.overlay.java.lib.literals.PrintString(),
                new hydra.overlay.java.lib.literals.ReadBigint(),
                new hydra.overlay.java.lib.literals.ReadDecimal(),
                new hydra.overlay.java.lib.literals.ReadFloat32(),
                new hydra.overlay.java.lib.literals.ReadFloat64(),
                new hydra.overlay.java.lib.literals.ReadInt16(),
                new hydra.overlay.java.lib.literals.ReadInt32(),
                new hydra.overlay.java.lib.literals.ReadInt64(),
                new hydra.overlay.java.lib.literals.ReadInt8(),
                new hydra.overlay.java.lib.literals.ReadUint16(),
                new hydra.overlay.java.lib.literals.ReadUint32(),
                new hydra.overlay.java.lib.literals.ReadUint64(),
                new hydra.overlay.java.lib.literals.ReadUint8(),
                new hydra.overlay.java.lib.literals.ShowBigint(),
                new hydra.overlay.java.lib.literals.ShowDecimal(),
                new hydra.overlay.java.lib.literals.ShowFloat32(),
                new hydra.overlay.java.lib.literals.ShowFloat64(),
                new hydra.overlay.java.lib.literals.ShowInt16(),
                new hydra.overlay.java.lib.literals.ShowInt32(),
                new hydra.overlay.java.lib.literals.ShowInt64(),
                new hydra.overlay.java.lib.literals.ShowInt8(),
                new hydra.overlay.java.lib.literals.ShowUint16(),
                new hydra.overlay.java.lib.literals.ShowUint32(),
                new hydra.overlay.java.lib.literals.ShowUint64(),
                new hydra.overlay.java.lib.literals.ShowUint8(),
                new hydra.overlay.java.lib.literals.StringToBinary(),
                new hydra.overlay.java.lib.literals.Uint16ToBigint(),
                new hydra.overlay.java.lib.literals.Uint32ToBigint(),
                new hydra.overlay.java.lib.literals.Uint64ToBigint(),
                new hydra.overlay.java.lib.literals.Uint8ToBigint());
    }

    private static List<PrimitiveFunction> logicPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.logic.And(),
                new hydra.overlay.java.lib.logic.IfElse(),
                new hydra.overlay.java.lib.logic.Not(),
                new hydra.overlay.java.lib.logic.Or());
    }

    private static List<PrimitiveFunction> mapsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.maps.Alter(),
                new hydra.overlay.java.lib.maps.Bimap(),
                new hydra.overlay.java.lib.maps.Delete(),
                new Elems(),
                new hydra.overlay.java.lib.maps.Empty(),
                new hydra.overlay.java.lib.maps.Filter(),
                new hydra.overlay.java.lib.maps.FilterWithKey(),
                new hydra.overlay.java.lib.maps.FindWithDefault(),
                new hydra.overlay.java.lib.maps.FromList(),
                new hydra.overlay.java.lib.maps.Insert(),
                new hydra.overlay.java.lib.maps.Keys(),
                new hydra.overlay.java.lib.maps.Lookup(),
                new hydra.overlay.java.lib.maps.Map(),
                new hydra.overlay.java.lib.maps.MapKeys(),
                new hydra.overlay.java.lib.maps.Member(),
                new hydra.overlay.java.lib.maps.Null(),
                new hydra.overlay.java.lib.maps.Singleton(),
                new hydra.overlay.java.lib.maps.Size(),
                new hydra.overlay.java.lib.maps.ToList(),
                new hydra.overlay.java.lib.maps.Union());
    }

    private static List<PrimitiveFunction> mathPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.math.Abs(),
                new hydra.overlay.java.lib.math.Acos(),
                new hydra.overlay.java.lib.math.Acosh(),
                new hydra.overlay.java.lib.math.Add(),
                new hydra.overlay.java.lib.math.AddFloat64(),
                new hydra.overlay.java.lib.math.Asin(),
                new hydra.overlay.java.lib.math.Asinh(),
                new hydra.overlay.java.lib.math.Atan(),
                new hydra.overlay.java.lib.math.Atan2(),
                new hydra.overlay.java.lib.math.Atanh(),
                new hydra.overlay.java.lib.math.Ceiling(),
                new hydra.overlay.java.lib.math.Cos(),
                new hydra.overlay.java.lib.math.Cosh(),
                new hydra.overlay.java.lib.math.Div(),
                new hydra.overlay.java.lib.math.E(),
                new hydra.overlay.java.lib.math.Even(),
                new hydra.overlay.java.lib.math.Exp(),
                new hydra.overlay.java.lib.math.Floor(),
                new hydra.overlay.java.lib.math.Log(),
                new hydra.overlay.java.lib.math.LogBase(),
                new hydra.overlay.java.lib.math.Mod(),
                new hydra.overlay.java.lib.math.Mul(),
                new hydra.overlay.java.lib.math.MulFloat64(),
                new hydra.overlay.java.lib.math.Negate(),
                new hydra.overlay.java.lib.math.NegateFloat64(),
                new hydra.overlay.java.lib.math.Odd(),
                new hydra.overlay.java.lib.math.Pi(),
                new hydra.overlay.java.lib.math.Pow(),
                new hydra.overlay.java.lib.math.Range(),
                new hydra.overlay.java.lib.math.Rem(),
                new hydra.overlay.java.lib.math.Round(),
                new hydra.overlay.java.lib.math.RoundFloat32(),
                new hydra.overlay.java.lib.math.RoundFloat64(),
                new hydra.overlay.java.lib.math.Signum(),
                new hydra.overlay.java.lib.math.Sin(),
                new hydra.overlay.java.lib.math.Sinh(),
                new hydra.overlay.java.lib.math.Sqrt(),
                new hydra.overlay.java.lib.math.Sub(),
                new hydra.overlay.java.lib.math.SubFloat64(),
                new hydra.overlay.java.lib.math.Tan(),
                new hydra.overlay.java.lib.math.Tanh(),
                new hydra.overlay.java.lib.math.Truncate());
    }

    private static List<PrimitiveFunction> optionalsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.optionals.Apply(),
                new hydra.overlay.java.lib.optionals.Bind(),
                new hydra.overlay.java.lib.optionals.Cases(),
                new hydra.overlay.java.lib.optionals.Compose(),
                new hydra.overlay.java.lib.optionals.Givens(),
                new hydra.overlay.java.lib.optionals.IsGiven(),
                new hydra.overlay.java.lib.optionals.IsNone(),
                new hydra.overlay.java.lib.optionals.Map(),
                new hydra.overlay.java.lib.optionals.MapOptional(),
                new hydra.overlay.java.lib.optionals.Pure(),
                new hydra.overlay.java.lib.optionals.ToList(),
                new hydra.overlay.java.lib.optionals.WithDefault());
    }

    private static List<PrimitiveFunction> orderingPrimitives() {
        return Arrays.asList(
                new Compare(),
                new Gt(),
                new Gte(),
                new Lt(),
                new Lte(),
                new Max(),
                new Min());
    }

    private static List<PrimitiveFunction> pairsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.pairs.Bimap(),
                new hydra.overlay.java.lib.pairs.First(),
                new hydra.overlay.java.lib.pairs.Second());
    }

    private static List<PrimitiveFunction> regexPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.regex.Find(),
                new hydra.overlay.java.lib.regex.FindAll(),
                new hydra.overlay.java.lib.regex.Matches(),
                new hydra.overlay.java.lib.regex.Replace(),
                new hydra.overlay.java.lib.regex.ReplaceAll(),
                new hydra.overlay.java.lib.regex.Split());
    }

    private static List<PrimitiveFunction> setsPrimitives() {
        return Arrays.asList(
                new Delete(),
                new hydra.overlay.java.lib.sets.Difference(),
                new hydra.overlay.java.lib.sets.Empty(),
                new hydra.overlay.java.lib.sets.FromList(),
                new hydra.overlay.java.lib.sets.Insert(),
                new hydra.overlay.java.lib.sets.Intersection(),
                new hydra.overlay.java.lib.sets.Map(),
                new hydra.overlay.java.lib.sets.Member(),
                new hydra.overlay.java.lib.sets.Null(),
                new hydra.overlay.java.lib.sets.Singleton(),
                new hydra.overlay.java.lib.sets.Size(),
                new hydra.overlay.java.lib.sets.ToList(),
                new hydra.overlay.java.lib.sets.Union(),
                new hydra.overlay.java.lib.sets.Unions());
    }

    private static List<PrimitiveFunction> stringsPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.strings.CharAt(),
                new hydra.overlay.java.lib.strings.Concat(),
                new hydra.overlay.java.lib.strings.Concat2(),
                new hydra.overlay.java.lib.strings.FromList(),
                new hydra.overlay.java.lib.strings.Join(),
                new hydra.overlay.java.lib.strings.Length(),
                new hydra.overlay.java.lib.strings.Lines(),
                new hydra.overlay.java.lib.strings.Null(),
                new hydra.overlay.java.lib.strings.SplitOn(),
                new hydra.overlay.java.lib.strings.ToList(),
                new hydra.overlay.java.lib.strings.ToLower(),
                new hydra.overlay.java.lib.strings.ToUpper(),
                new hydra.overlay.java.lib.strings.Unlines());
    }

    private static List<PrimitiveFunction> systemPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.system.Execute(),
                new hydra.overlay.java.lib.system.Exit(),
                new hydra.overlay.java.lib.system.GetEnvironment(),
                new hydra.overlay.java.lib.system.GetEnvironmentVariable(),
                new hydra.overlay.java.lib.system.GetTime(),
                new hydra.overlay.java.lib.system.GetWorkingDirectory(),
                new hydra.overlay.java.lib.system.ReadStdin(),
                new hydra.overlay.java.lib.system.WriteStderr(),
                new hydra.overlay.java.lib.system.WriteStdout());
    }

    private static List<PrimitiveFunction> textPrimitives() {
        return Arrays.asList(
                new hydra.overlay.java.lib.text.DecodeUtf8(),
                new hydra.overlay.java.lib.text.EncodeUtf8());
    }

}