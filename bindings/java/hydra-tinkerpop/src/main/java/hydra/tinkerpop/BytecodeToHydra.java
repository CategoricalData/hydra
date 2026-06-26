package hydra.tinkerpop;

import hydra.tinkerpop.gremlin.GenericLiteral;
import hydra.tinkerpop.gremlin.GenericLiteralArgument;
import hydra.tinkerpop.gremlin.IntegerLiteral;
import hydra.tinkerpop.gremlin.NumericLiteral;
import hydra.tinkerpop.gremlin.RootTraversal;
import hydra.tinkerpop.gremlin.StringNullableArgument;
import hydra.tinkerpop.gremlin.TraversalMethod;
import hydra.tinkerpop.gremlin.TraversalScope;
import hydra.tinkerpop.gremlin.TraversalScopeArgument;
import hydra.tinkerpop.gremlin.TraversalSource;
import hydra.tinkerpop.gremlin.TraversalSourceSelfMethod;
import hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod;

import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;
import org.apache.tinkerpop.gremlin.process.traversal.Scope;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Reverse mapping: TinkerPop {@link Bytecode} &rarr; Hydra {@code hydra.tinkerpop.gremlin} model.
 *
 * <p>Inverse of {@link HydraToBytecode}. Reads the source instructions (→ {@code withX} self methods),
 * then the step instructions: the first step instruction is the spawn method, the rest are chained
 * steps. Each instruction is dispatched on its operator name to the corresponding Hydra union variant.
 *
 * <p>This also serves the <b>Gremlin-text → Hydra</b> path: parse text with TinkerPop's own ANTLR
 * parser ({@code GremlinQueryParser.parse(text)} → a {@code Traversal}), take
 * {@code traversal.asAdmin().getBytecode()}, and feed it here. One reverse mapper covers both
 * "Bytecode → Hydra" and "Gremlin text → Hydra"; we do not vendor {@code Gremlin.g4}.
 *
 * <p><b>Coverage:</b> mirrors {@link HydraToBytecode} — the structural backbone plus the same step
 * subset (so the implemented steps round-trip). Unmapped operators throw
 * {@link UnsupportedOperationException} until filled in.
 */
public final class BytecodeToHydra {

    private BytecodeToHydra() {
    }

    /** Maps a TinkerPop {@link Bytecode} to a Hydra {@link RootTraversal}. */
    public static RootTraversal fromBytecode(Bytecode bc) {
        List<TraversalSourceSelfMethod> source = new ArrayList<>();
        for (Bytecode.Instruction i : bc.getSourceInstructions()) {
            source.add(sourceFrom(i));
        }

        List<Bytecode.Instruction> steps = new ArrayList<>();
        bc.getStepInstructions().forEach(steps::add);
        if (steps.isEmpty()) {
            throw new IllegalArgumentException("Bytecode has no step instructions; cannot form a RootTraversal");
        }
        TraversalSourceSpawnMethod spawn = spawnFrom(steps.get(0));
        List<TraversalMethod> chained = new ArrayList<>();
        for (int idx = 1; idx < steps.size(); idx++) {
            chained.add(stepFrom(steps.get(idx)));
        }
        return new RootTraversal(new TraversalSource(source), spawn, chained);
    }

    // -- Source / withX ------------------------------------------------------------------------

    private static TraversalSourceSelfMethod sourceFrom(Bytecode.Instruction i) {
        switch (i.getOperator()) {
            case "withBulk":
                return new TraversalSourceSelfMethod.WithBulk((Boolean) i.getArguments()[0]);
            case "withPath":
                return new TraversalSourceSelfMethod.WithPath();
            default:
                throw notYetMapped("source/withX", i.getOperator());
        }
    }

    // -- Spawn ---------------------------------------------------------------------------------

    private static TraversalSourceSpawnMethod spawnFrom(Bytecode.Instruction i) {
        Object[] a = i.getArguments();
        switch (i.getOperator()) {
            case "V":
                return new TraversalSourceSpawnMethod.V(literalArgs(a));
            case "E":
                return new TraversalSourceSpawnMethod.E(literalArgs(a));
            case "inject":
                return new TraversalSourceSpawnMethod.Inject(literalArgs(a));
            case "addV":
                return new TraversalSourceSpawnMethod.AddV(a.length == 0
                        ? hydra.overlay.java.util.Optional.none()
                        : hydra.overlay.java.util.Optional.given(stringOrNestedRev(a[0])));
            case "addE":
                return new TraversalSourceSpawnMethod.AddE(stringOrNestedRev(a[0]));
            case "mergeV":
                return new TraversalSourceSpawnMethod.MergeV(mergeArgsRev(a));
            case "mergeE":
                return new TraversalSourceSpawnMethod.MergeE(mergeArgsRev(a));
            case "union":
                return new TraversalSourceSpawnMethod.Union(nestedListRev(a));
            case "io":
                return new TraversalSourceSpawnMethod.Io(strArg(a[0]));
            default:
                throw notYetMapped("spawn", i.getOperator());
        }
    }

    // -- Steps ---------------------------------------------------------------------------------

    private static TraversalMethod stepFrom(Bytecode.Instruction i) {
        Object[] a = i.getArguments();
        switch (i.getOperator()) {
            // Element access
            case "V": return new TraversalMethod.V(literalArgs(a));
            case "E": return new TraversalMethod.E(literalArgs(a));

            // Adjacency (varargs of nullable edge labels)
            case "out":   return new TraversalMethod.Out(stringNullableArgs(a));
            case "in":    return new TraversalMethod.In(stringNullableArgs(a));
            case "both":  return new TraversalMethod.Both(stringNullableArgs(a));
            case "outE":  return new TraversalMethod.OutE(stringNullableArgs(a));
            case "inE":   return new TraversalMethod.InE(stringNullableArgs(a));
            case "bothE": return new TraversalMethod.BothE(stringNullableArgs(a));
            case "outV":  return new TraversalMethod.OutV();
            case "inV":   return new TraversalMethod.InV();
            case "bothV": return new TraversalMethod.BothV();
            case "otherV": return new TraversalMethod.OtherV();

            // Value access
            case "values": return new TraversalMethod.Values(stringNullableArgs(a));
            case "id":    return new TraversalMethod.Id();
            case "label": return new TraversalMethod.Label();
            case "key":   return new TraversalMethod.Key();
            case "value": return new TraversalMethod.Value();

            // No-argument utilities
            case "identity":           return new TraversalMethod.Identity();
            case "drop":               return new TraversalMethod.Drop();
            case "unfold":             return new TraversalMethod.Unfold();
            case "asBool":             return new TraversalMethod.AsBool();
            case "asDate":             return new TraversalMethod.AsDate();
            case "connectedComponent": return new TraversalMethod.ConnectedComponent();
            case "cyclicPath":         return new TraversalMethod.CyclicPath();
            case "discard":            return new TraversalMethod.Discard();
            case "index":              return new TraversalMethod.Index();
            case "path":               return new TraversalMethod.Path();
            case "peerPressure":       return new TraversalMethod.PeerPressure();
            case "read":               return new TraversalMethod.Read();
            case "reverse":            return new TraversalMethod.Reverse();
            case "shortestPath":       return new TraversalMethod.ShortestPath();
            case "simplePath":         return new TraversalMethod.SimplePath();
            case "write":              return new TraversalMethod.Write();

            // fold(): only the no-arg form is mapped (the 2-arg seed/biFunction form is a TODO)
            case "fold":
                if (a.length == 0) {
                    return new TraversalMethod.Fold(hydra.overlay.java.util.Optional.none());
                }
                throw notYetMapped("step", "fold(seed, biFunction)");

            // Steps with an optional TraversalScope argument
            case "count":    return new TraversalMethod.Count(scopeOpt(a));
            case "max":      return new TraversalMethod.Max(scopeOpt(a));
            case "mean":     return new TraversalMethod.Mean(scopeOpt(a));
            case "min":      return new TraversalMethod.Min(scopeOpt(a));
            case "sum":      return new TraversalMethod.Sum(scopeOpt(a));
            case "order":    return new TraversalMethod.Order(scopeOpt(a));
            case "asString": return new TraversalMethod.AsString(scopeOpt(a));
            case "length":   return new TraversalMethod.Length(scopeOpt(a));
            case "toUpper":  return new TraversalMethod.ToUpper(scopeOpt(a));
            case "toLower":  return new TraversalMethod.ToLower(scopeOpt(a));
            case "trim":     return new TraversalMethod.Trim(scopeOpt(a));
            case "lTrim":    return new TraversalMethod.LTrim(scopeOpt(a));
            case "rTrim":    return new TraversalMethod.RTrim(scopeOpt(a));

            // Label steps
            case "as":      return new TraversalMethod.As(asArgs(a));
            case "cap":     return new TraversalMethod.Cap(asArgs(a));
            case "project": return new TraversalMethod.Project(asArgs(a));

            case "constant":
                if (a.length != 1) {
                    throw new IllegalArgumentException("constant expects 1 argument, got " + a.length);
                }
                return new TraversalMethod.Constant(new GenericLiteralArgument.Value(literal(a[0])));

            // limit/skip: optional scope + long
            case "limit": return new TraversalMethod.Limit(scopeAndInteger(a));
            case "skip":  return new TraversalMethod.Skip(scopeAndInteger(a));

            // Predicate-bearing filters. Reverse of a P/TextP predicate argument back into a Hydra
            // TraversalPredicate is not yet implemented (reading P internals); the value/strings forms
            // (e.g. hasLabel("person"), hasId(1,2)) round-trip here.
            case "hasLabel": return new TraversalMethod.HasLabel(predicateOrStringsRev(a));
            case "hasKey":   return new TraversalMethod.HasKey(predicateOrStringsRev(a));
            case "hasId":    return new TraversalMethod.HasId(predicateOrObjectsRev(a));
            case "hasValue": return new TraversalMethod.HasValue(predicateOrObjectsRev(a));
            case "is":       return new TraversalMethod.Is(predicateOrObjectRev(a));

            // Single generic-literal-argument steps
            case "combine":    return new TraversalMethod.Combine(litArg(a[0]));
            case "difference": return new TraversalMethod.Difference(litArg(a[0]));
            case "disjunct":   return new TraversalMethod.Disjunct(litArg(a[0]));
            case "intersect":  return new TraversalMethod.Intersect(litArg(a[0]));
            case "merge":      return new TraversalMethod.Merge(litArg(a[0]));
            case "product":    return new TraversalMethod.Product(litArg(a[0]));

            // Single nested-traversal steps
            case "branch":     return new TraversalMethod.Branch(nestedRev(a[0]));
            case "flatMap":    return new TraversalMethod.FlatMap(nestedRev(a[0]));
            case "local":      return new TraversalMethod.Local(nestedRev(a[0]));
            case "map":        return new TraversalMethod.Map(nestedRev(a[0]));
            case "not":        return new TraversalMethod.Not(nestedRev(a[0]));
            case "optional":   return new TraversalMethod.Optional(nestedRev(a[0]));
            case "sideEffect": return new TraversalMethod.SideEffect(nestedRev(a[0]));

            // List-of-nested-traversal steps
            case "and":      return new TraversalMethod.And(nestedListRev(a));
            case "coalesce": return new TraversalMethod.Coalesce(nestedListRev(a));
            case "match":    return new TraversalMethod.Match(nestedListRev(a));
            case "or":       return new TraversalMethod.Or(nestedListRev(a));
            case "union":    return new TraversalMethod.Union(nestedListRev(a));

            // Single string-argument steps
            case "aggregate": return new TraversalMethod.Aggregate(strArg(a[0]));
            case "conjoin":   return new TraversalMethod.Conjoin(strArg(a[0]));
            case "format":    return new TraversalMethod.Format(strArg(a[0]));
            case "subgraph":  return new TraversalMethod.Subgraph(strArg(a[0]));
            case "math":      return new TraversalMethod.Math_(strArg(a[0]));

            // Optional-string-argument steps
            case "fail":       return new TraversalMethod.Fail(strOpt(a));
            case "group":      return new TraversalMethod.Group(strOpt(a));
            case "groupCount": return new TraversalMethod.GroupCount(strOpt(a));
            case "loops":      return new TraversalMethod.Loops(strOpt(a));
            case "profile":    return new TraversalMethod.Profile(strOpt(a));
            case "tree":       return new TraversalMethod.Tree(strOpt(a));

            // List-of-nullable-string steps
            case "element":     return new TraversalMethod.Element(stringNullableArgs(a));
            case "elementMap":  return new TraversalMethod.ElementMap(stringNullableArgs(a));
            case "properties":  return new TraversalMethod.Properties(stringNullableArgs(a));
            case "propertyMap": return new TraversalMethod.PropertyMap(stringNullableArgs(a));
            case "hasNot":      return new TraversalMethod.HasNot(stringNullableArg1(a[0]));

            // Integer-argument steps
            case "timeLimit": return new TraversalMethod.TimeLimit(intArg(a[0]));
            case "times":     return new TraversalMethod.Times(intArg(a[0]));

            // inject
            case "inject": return new TraversalMethod.Inject(literalArgs(a));

            // coin / pageRank
            case "coin": return new TraversalMethod.Coin(numArg(a[0]));
            case "pageRank":
                return new TraversalMethod.PageRank(a.length == 0
                        ? hydra.overlay.java.util.Optional.none()
                        : hydra.overlay.java.util.Optional.given(numArg(a[0])));

            // Record-args steps (reverse of the forward record mappers).
            case "range": {
                // [scope,] min, max
                boolean hasScope = a.length == 3 && a[0] instanceof Scope;
                Object minO = a[hasScope ? 1 : 0], maxO = a[hasScope ? 2 : 1];
                return new TraversalMethod.Range(new hydra.tinkerpop.gremlin.RangeArgs(
                        hasScope ? hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) a[0])) : hydra.overlay.java.util.Optional.none(),
                        intArg(minO), intArg(maxO)));
            }
            case "sample": {
                boolean hasScope = a.length == 2 && a[0] instanceof Scope;
                return new TraversalMethod.Sample(new hydra.tinkerpop.gremlin.SampleByScope(
                        hasScope ? hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) a[0])) : hydra.overlay.java.util.Optional.none(),
                        intArg(a[a.length - 1])));
            }
            case "replace": {
                boolean hasScope = a.length == 3 && a[0] instanceof Scope;
                int base = hasScope ? 1 : 0;
                return new TraversalMethod.Replace(new hydra.tinkerpop.gremlin.ReplaceArgs(
                        hasScope ? hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) a[0])) : hydra.overlay.java.util.Optional.none(),
                        stringNullableArg1(a[base]), stringNullableArg1(a[base + 1])));
            }
            case "split": {
                boolean hasScope = a.length == 2 && a[0] instanceof Scope;
                return new TraversalMethod.Split(new hydra.tinkerpop.gremlin.SplitArgs(
                        hasScope ? hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) a[0])) : hydra.overlay.java.util.Optional.none(),
                        stringNullableArg1(a[a.length - 1])));
            }
            case "substring": {
                int idx = 0;
                hydra.overlay.java.util.Optional<TraversalScopeArgument> scope = hydra.overlay.java.util.Optional.none();
                if (a.length > 0 && a[0] instanceof Scope) { scope = hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) a[0])); idx = 1; }
                hydra.tinkerpop.gremlin.IntegerArgument start = intArg(a[idx]);
                hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.IntegerArgument> end =
                        (idx + 1 < a.length) ? hydra.overlay.java.util.Optional.given(intArg(a[idx + 1])) : hydra.overlay.java.util.Optional.none();
                return new TraversalMethod.Substring(new hydra.tinkerpop.gremlin.SubstringArgs(scope, start, end));
            }
            case "tail": {
                if (a.length == 0) return new TraversalMethod.Tail(hydra.overlay.java.util.Optional.none());
                int idx = 0;
                hydra.overlay.java.util.Optional<TraversalScopeArgument> scope = hydra.overlay.java.util.Optional.none();
                if (a[0] instanceof Scope) { scope = hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) a[0])); idx = 1; }
                hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.IntegerArgument> n =
                        (idx < a.length) ? hydra.overlay.java.util.Optional.given(intArg(a[idx])) : hydra.overlay.java.util.Optional.none();
                return new TraversalMethod.Tail(hydra.overlay.java.util.Optional.given(new hydra.tinkerpop.gremlin.TailArgs(scope, n)));
            }
            case "repeat": {
                boolean hasLabel = a.length == 2;
                return new TraversalMethod.Repeat(new hydra.tinkerpop.gremlin.RepeatArgs(
                        hasLabel ? hydra.overlay.java.util.Optional.given(strArg(a[0])) : hydra.overlay.java.util.Optional.none(),
                        nestedRev(a[a.length - 1])));
            }
            case "dateAdd":
                return new TraversalMethod.DateAdd(new hydra.tinkerpop.gremlin.DateAddArgs(dtArgRev(a[0]), intArg(a[1])));
            case "dateDiff":
                if (a.length == 1 && a[0] instanceof Bytecode) {
                    return new TraversalMethod.DateDiff(new hydra.tinkerpop.gremlin.DateDiffArgs.Traversal(nestedRev(a[0])));
                }
                throw notYetMapped("step", "dateDiff(date) reverse");
            case "concat": {
                if (a.length > 0 && a[0] instanceof Bytecode) {
                    return new TraversalMethod.Concat(new hydra.tinkerpop.gremlin.ConcatArgs.Traversal(nestedListRev(a)));
                }
                return new TraversalMethod.Concat(new hydra.tinkerpop.gremlin.ConcatArgs.String_(stringNullableArgs(a)));
            }
            case "toE":
                return new TraversalMethod.ToE(directionAndVarargsRev(a));
            case "toV":
                return new TraversalMethod.ToV(new hydra.tinkerpop.gremlin.TraversalDirectionArgument.Value(
                        Directions.fromGremlin((org.apache.tinkerpop.gremlin.structure.Direction) a[0])));

            // addE/addV (string label or nested traversal)
            case "addE": return new TraversalMethod.AddE(stringOrNestedRev(a[0]));
            case "addV":
                return new TraversalMethod.AddV(a.length == 0
                        ? hydra.overlay.java.util.Optional.none()
                        : hydra.overlay.java.util.Optional.given(stringOrNestedRev(a[0])));

            // from/to (string | traversal | [to:] direction+labels). Vertex form not yet reversed.
            case "from":
                if (a[0] instanceof Bytecode) return new TraversalMethod.From(new hydra.tinkerpop.gremlin.FromArgs.Traversal(nestedRev(a[0])));
                if (a[0] instanceof String)   return new TraversalMethod.From(new hydra.tinkerpop.gremlin.FromArgs.String_(strArg(a[0])));
                throw notYetMapped("step", "from(" + clsOf(a[0]) + ") reverse");
            case "to":
                if (a.length >= 1 && a[0] instanceof org.apache.tinkerpop.gremlin.structure.Direction)
                    return new TraversalMethod.To(new hydra.tinkerpop.gremlin.ToArgs.Direction(directionAndVarargsRev(a)));
                if (a[0] instanceof Bytecode) return new TraversalMethod.To(new hydra.tinkerpop.gremlin.ToArgs.Traversal(nestedRev(a[0])));
                if (a[0] instanceof String)   return new TraversalMethod.To(new hydra.tinkerpop.gremlin.ToArgs.String_(strArg(a[0])));
                throw notYetMapped("step", "to(" + clsOf(a[0]) + ") reverse");

            // mergeE/mergeV (nested traversal; map-literal form not yet reversed)
            case "mergeE": return new TraversalMethod.MergeE(mergeArgsRev(a));
            case "mergeV": return new TraversalMethod.MergeV(mergeArgsRev(a));

            // valueMap (strings; boolean form not yet reversed)
            case "valueMap":
                return new TraversalMethod.ValueMap(new hydra.tinkerpop.gremlin.ValueMapArgs.String_(stringNullableArgs(a)));

            // dedup ([scope,] strings...)
            case "dedup": {
                if (a.length >= 1 && a[0] instanceof Scope) {
                    List<StringNullableArgument> strs = new ArrayList<>();
                    for (int k = 1; k < a.length; k++) strs.add(stringNullableArg1(a[k]));
                    return new TraversalMethod.Dedup(new hydra.tinkerpop.gremlin.DedupArgs.ScopeString(
                            new hydra.tinkerpop.gremlin.ScopeStringArgs(scopeArgRev((Scope) a[0]), strs)));
                }
                return new TraversalMethod.Dedup(new hydra.tinkerpop.gremlin.DedupArgs.String_(stringNullableArgs(a)));
            }

            // barrier ([maxBarrierSize]); sack ([Operator])
            case "barrier":
                if (a.length == 0) return new TraversalMethod.Barrier(hydra.overlay.java.util.Optional.none());
                return new TraversalMethod.Barrier(hydra.overlay.java.util.Optional.given(
                        new hydra.tinkerpop.gremlin.BarrierArgs.Int(intArg(a[0]))));
            case "sack":
                if (a.length == 0) return new TraversalMethod.Sack(hydra.overlay.java.util.Optional.none());
                return new TraversalMethod.Sack(hydra.overlay.java.util.Optional.given(biFunctionRev(a[0])));

            // Predicate-argument steps (now that P→predicate reverse exists).
            case "all":  return new TraversalMethod.All(predFrom(a[0]));
            case "any":  return new TraversalMethod.Any(predFrom(a[0]));
            case "none": return new TraversalMethod.None(predFrom(a[0]));

            // filter/until/emit (predicate or traversal)
            case "filter": return new TraversalMethod.Filter(predicateOrTraversalRev(a[0]));
            case "until":  return new TraversalMethod.Until(predicateOrTraversalRev(a[0]));
            case "emit":
                return new TraversalMethod.Emit(a.length == 0
                        ? hydra.overlay.java.util.Optional.none()
                        : hydra.overlay.java.util.Optional.given(predicateOrTraversalRev(a[0])));

            // where(predicate | string | traversal)
            case "where":
                if (a.length >= 1 && a[0] instanceof Bytecode) {
                    return new TraversalMethod.Where(new hydra.tinkerpop.gremlin.WhereArgs.Traversal(nestedRev(a[0])));
                }
                if (a.length == 1 && a[0] instanceof org.apache.tinkerpop.gremlin.process.traversal.P) {
                    return new TraversalMethod.Where(new hydra.tinkerpop.gremlin.WhereArgs.Predicate(
                            new hydra.tinkerpop.gremlin.WhereWithPredicateArgs(hydra.overlay.java.util.Optional.none(), predFrom(a[0]))));
                }
                if (a.length == 2 && a[0] instanceof String && a[1] instanceof org.apache.tinkerpop.gremlin.process.traversal.P) {
                    return new TraversalMethod.Where(new hydra.tinkerpop.gremlin.WhereArgs.Predicate(
                            new hydra.tinkerpop.gremlin.WhereWithPredicateArgs(hydra.overlay.java.util.Optional.given(strArg(a[0])), predFrom(a[1]))));
                }
                if (a.length == 1 && a[0] instanceof String) {
                    return new TraversalMethod.Where(new hydra.tinkerpop.gremlin.WhereArgs.String_(strArg(a[0])));
                }
                throw notYetMapped("step", "where(...) reverse with args " + a.length);

            // asNumber([GType])
            case "asNumber":
                return new TraversalMethod.AsNumber(a.length == 0
                        ? hydra.overlay.java.util.Optional.none()
                        : hydra.overlay.java.util.Optional.given(new hydra.tinkerpop.gremlin.TraversalGTypeArgument.Value(
                                Gtypes.fromGremlin((org.apache.tinkerpop.gremlin.process.traversal.GType) a[0]))));

            // select(column | [pop] keys... | [pop] traversal)
            case "select": {
                if (a.length == 1 && a[0] instanceof org.apache.tinkerpop.gremlin.structure.Column) {
                    return new TraversalMethod.Select(new hydra.tinkerpop.gremlin.SelectArgs.Column(
                            new hydra.tinkerpop.gremlin.TraversalColumnArgument.Value(columnRev((org.apache.tinkerpop.gremlin.structure.Column) a[0]))));
                }
                boolean hasPop = a.length >= 1 && a[0] instanceof org.apache.tinkerpop.gremlin.process.traversal.Pop;
                Object last = a[a.length - 1];
                if (last instanceof Bytecode) {
                    if (hasPop) return new TraversalMethod.Select(new hydra.tinkerpop.gremlin.SelectArgs.PopTraversal(
                            new hydra.tinkerpop.gremlin.PopAndTraversal(popRev((org.apache.tinkerpop.gremlin.process.traversal.Pop) a[0]), nestedRev(last))));
                    return new TraversalMethod.Select(new hydra.tinkerpop.gremlin.SelectArgs.Traversal(nestedRev(last)));
                }
                if (hasPop) {
                    List<hydra.tinkerpop.gremlin.StringArgument> keys = new ArrayList<>();
                    for (int k = 1; k < a.length; k++) keys.add(strArg(a[k]));
                    return new TraversalMethod.Select(new hydra.tinkerpop.gremlin.SelectArgs.PopStrings(
                            new hydra.tinkerpop.gremlin.SelectByKeys(popRev((org.apache.tinkerpop.gremlin.process.traversal.Pop) a[0]), keys)));
                }
                List<hydra.tinkerpop.gremlin.StringArgument> keys = new ArrayList<>();
                for (Object o : a) keys.add(strArg(o));
                return new TraversalMethod.Select(new hydra.tinkerpop.gremlin.SelectArgs.Strings(keys));
            }

            // choose(function | predicate,true[,false] | traversal)
            case "choose": {
                if (a.length == 1 && a[0] instanceof Bytecode) return new TraversalMethod.Choose(new hydra.tinkerpop.gremlin.ChooseArgs.Traversal(nestedRev(a[0])));
                if (a[0] instanceof org.apache.tinkerpop.gremlin.process.traversal.P) {
                    hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.NestedTraversal> f =
                            (a.length >= 3) ? hydra.overlay.java.util.Optional.given(nestedRev(a[2])) : hydra.overlay.java.util.Optional.none();
                    return new TraversalMethod.Choose(new hydra.tinkerpop.gremlin.ChooseArgs.PredicateTraversal(
                            new hydra.tinkerpop.gremlin.PredicateOrTraversalChoice(predFrom(a[0]), nestedRev(a[1]), f)));
                }
                throw notYetMapped("step", "choose(function) reverse");
            }

            // option(predicate,traversal | object,traversal | traversal)
            case "option": {
                if (a.length == 1 && a[0] instanceof Bytecode) return new TraversalMethod.Option(new hydra.tinkerpop.gremlin.OptionArgs.Traversal(nestedRev(a[0])));
                if (a.length == 2 && a[0] instanceof org.apache.tinkerpop.gremlin.process.traversal.P) {
                    return new TraversalMethod.Option(new hydra.tinkerpop.gremlin.OptionArgs.PredicateTraversal(
                            new hydra.tinkerpop.gremlin.PredicateAndTraversal(predFrom(a[0]), nestedRev(a[1]))));
                }
                if (a.length == 2) {
                    return new TraversalMethod.Option(new hydra.tinkerpop.gremlin.OptionArgs.ObjectTraversal(
                            new hydra.tinkerpop.gremlin.ObjectAndTraversal(litArg(a[0]), nestedRev(a[1]))));
                }
                throw notYetMapped("step", "option(merge forms) reverse");
            }

            // by(order | token | function | string | traversal)
            case "by": {
                if (a.length == 0) return new TraversalMethod.By(new hydra.tinkerpop.gremlin.ByArgs.Other(
                        new hydra.tinkerpop.gremlin.ByOtherArgs.Comparator(hydra.overlay.java.util.Optional.none())));
                Object x0 = a[0];
                if (x0 instanceof org.apache.tinkerpop.gremlin.process.traversal.Order)
                    return new TraversalMethod.By(new hydra.tinkerpop.gremlin.ByArgs.Order(
                            new hydra.tinkerpop.gremlin.TraversalOrderArgument.Value(orderRev((org.apache.tinkerpop.gremlin.process.traversal.Order) x0))));
                if (x0 instanceof org.apache.tinkerpop.gremlin.structure.T)
                    return new TraversalMethod.By(new hydra.tinkerpop.gremlin.ByArgs.Token(
                            new hydra.tinkerpop.gremlin.TraversalTokenArgument.Value(tokenRev((org.apache.tinkerpop.gremlin.structure.T) x0))));
                if (x0 instanceof Bytecode)
                    return new TraversalMethod.By(new hydra.tinkerpop.gremlin.ByArgs.Other(new hydra.tinkerpop.gremlin.ByOtherArgs.Traversal(nestedRev(x0))));
                if (x0 instanceof String)
                    return new TraversalMethod.By(new hydra.tinkerpop.gremlin.ByArgs.Other(new hydra.tinkerpop.gremlin.ByOtherArgs.String_(strArg(x0))));
                throw notYetMapped("step", "by(" + clsOf(x0) + ") reverse");
            }

            // property([cardinality,] objects...)
            case "property": {
                if (a.length >= 1 && a[0] instanceof org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality) {
                    List<GenericLiteralArgument> objs = new ArrayList<>();
                    for (int k = 1; k < a.length; k++) objs.add(litArg(a[k]));
                    return new TraversalMethod.Property(new hydra.tinkerpop.gremlin.PropertyArgs.CardinalityObjects(
                            new hydra.tinkerpop.gremlin.CardinalityAndObjects(
                                    new hydra.tinkerpop.gremlin.TraversalCardinalityArgument.Value(cardinalityRev((org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality) a[0])),
                                    objs)));
                }
                return new TraversalMethod.Property(new hydra.tinkerpop.gremlin.PropertyArgs.Objects(literalArgs(a)));
            }

            // call(service[, traversal]); map form not reversed
            case "call": {
                hydra.tinkerpop.gremlin.ServiceArguments sargs = (a.length >= 2 && a[1] instanceof Bytecode)
                        ? new hydra.tinkerpop.gremlin.ServiceArguments.Traversal(hydra.overlay.java.util.Optional.given(nestedRev(a[1])))
                        : new hydra.tinkerpop.gremlin.ServiceArguments.Traversal(hydra.overlay.java.util.Optional.none());
                return new TraversalMethod.Call(new hydra.tinkerpop.gremlin.ServiceCall(strArg(a[0]), sargs));
            }

            // with(key[, value]) — string-key form (WithOptions-key form not reversed)
            case "with": {
                hydra.tinkerpop.gremlin.WithArgsKeys keys = new hydra.tinkerpop.gremlin.WithArgsKeys.String_(strArg(a[0]));
                hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.WithArgsValues> vals =
                        (a.length >= 2) ? hydra.overlay.java.util.Optional.given(new hydra.tinkerpop.gremlin.WithArgsValues.Object_(litArg(a[1]))) : hydra.overlay.java.util.Optional.none();
                return new TraversalMethod.With(new hydra.tinkerpop.gremlin.WithArgs(keys, vals));
            }

            // has(key[, clause] | T, clause | label, key, clause)
            case "has":
                return new TraversalMethod.Has(hasArgsRev(a));

            default:
                throw notYetMapped("step", i.getOperator());
        }
    }

    // -- Reverse argument mappers --------------------------------------------------------------

    /** Maps a flat {@code Object[]} of boxed Java values to a list of generic-literal arguments. */
    static List<GenericLiteralArgument> literalArgs(Object[] args) {
        List<GenericLiteralArgument> out = new ArrayList<>(args.length);
        for (Object o : args) {
            out.add(new GenericLiteralArgument.Value(literal(o)));
        }
        return out;
    }

    /** Maps a boxed Java value to a Hydra {@link GenericLiteral} (inverse of the forward literal mapper). */
    static GenericLiteral literal(Object o) {
        if (o == null) {
            return new GenericLiteral.Null();
        }
        if (o instanceof String) {
            return new GenericLiteral.String_((String) o);
        }
        if (o instanceof Boolean) {
            return new GenericLiteral.Boolean_((Boolean) o);
        }
        if (isInteger(o)) {
            return new GenericLiteral.Numeric(new NumericLiteral.Integer_(integerLiteral(o)));
        }
        if (isFloat(o)) {
            return new GenericLiteral.Numeric(new NumericLiteral.Float_(floatLiteral(o)));
        }
        throw notYetMapped("literal", o.getClass().getName());
    }

    private static boolean isInteger(Object o) {
        return o instanceof Byte || o instanceof Short || o instanceof Integer
                || o instanceof Long || o instanceof BigInteger;
    }

    private static boolean isFloat(Object o) {
        return o instanceof Float || o instanceof Double || o instanceof java.math.BigDecimal;
    }

    /** A boxed Java integer → the precision-tagged Gremlin {@code IntegerLiteral} union variant. */
    static IntegerLiteral integerLiteral(Object o) {
        if (o instanceof Byte)       return new IntegerLiteral.Byte_((Byte) o);
        if (o instanceof Short)      return new IntegerLiteral.Short_((Short) o);
        if (o instanceof Integer)    return new IntegerLiteral.Int((Integer) o);
        if (o instanceof Long)       return new IntegerLiteral.Long_((Long) o);
        if (o instanceof BigInteger) return new IntegerLiteral.Big((BigInteger) o);
        throw notYetMapped("integer literal", o.getClass().getName());
    }

    /** A boxed Java float → the precision-tagged Gremlin {@code FloatLiteral} union variant. */
    static hydra.tinkerpop.gremlin.FloatLiteral floatLiteral(Object o) {
        if (o instanceof Float)               return new hydra.tinkerpop.gremlin.FloatLiteral.Float_((Float) o);
        if (o instanceof Double)              return new hydra.tinkerpop.gremlin.FloatLiteral.Double_((Double) o);
        if (o instanceof java.math.BigDecimal) return new hydra.tinkerpop.gremlin.FloatLiteral.Big((java.math.BigDecimal) o);
        throw notYetMapped("float literal", o.getClass().getName());
    }

    /** Maps a flat {@code Object[]} of (nullable) strings to a list of nullable-string arguments. */
    static List<StringNullableArgument> stringNullableArgs(Object[] args) {
        List<StringNullableArgument> out = new ArrayList<>(args.length);
        for (Object o : args) {
            hydra.overlay.java.util.Optional<String> opt = (o == null)
                    ? hydra.overlay.java.util.Optional.none()
                    : hydra.overlay.java.util.Optional.given((String) o);
            out.add(new StringNullableArgument.Value(opt));
        }
        return out;
    }

    /** Maps an optional trailing {@code Scope} argument to an optional Hydra {@code TraversalScopeArgument}. */
    static hydra.overlay.java.util.Optional<TraversalScopeArgument> scopeOpt(Object[] args) {
        if (args.length == 0) {
            return hydra.overlay.java.util.Optional.none();
        }
        Scope s = (Scope) args[0];
        TraversalScope scope = (s == Scope.local) ? new TraversalScope.Local() : new TraversalScope.Global();
        return hydra.overlay.java.util.Optional.given(new TraversalScopeArgument.Value(scope));
    }

    /** Reverse of as/cap/project: a flat {@code String[]} → {@code AsArgs(first, rest)}. */
    static hydra.tinkerpop.gremlin.AsArgs asArgs(Object[] args) {
        if (args.length == 0) {
            throw new IllegalArgumentException("as/cap/project expects at least 1 label");
        }
        hydra.tinkerpop.gremlin.StringArgument first =
                new hydra.tinkerpop.gremlin.StringArgument.Value((String) args[0]);
        List<StringNullableArgument> rest = new ArrayList<>(args.length - 1);
        for (int k = 1; k < args.length; k++) {
            Object o = args[k];
            hydra.overlay.java.util.Optional<String> opt = (o == null)
                    ? hydra.overlay.java.util.Optional.none()
                    : hydra.overlay.java.util.Optional.given((String) o);
            rest.add(new StringNullableArgument.Value(opt));
        }
        return new hydra.tinkerpop.gremlin.AsArgs(first, rest);
    }

    /** Reverse of limit/skip: {@code [Scope,] long} → {@code ScopeAndInteger(scope?, integer)}. */
    static hydra.tinkerpop.gremlin.ScopeAndInteger scopeAndInteger(Object[] args) {
        // Trailing count is a long (limit/skip coerce to long); an optional leading Scope precedes it.
        Object countObj = args[args.length - 1];
        hydra.tinkerpop.gremlin.IntegerArgument integer = intArg(countObj);
        hydra.overlay.java.util.Optional<TraversalScopeArgument> scope =
                (args.length >= 2 && args[0] instanceof Scope)
                        ? hydra.overlay.java.util.Optional.given(scopeArgRev((Scope) args[0]))
                        : hydra.overlay.java.util.Optional.none();
        return new hydra.tinkerpop.gremlin.ScopeAndInteger(scope, integer);
    }

    private static TraversalScopeArgument scopeArgRev(Scope s) {
        TraversalScope scope = (s == Scope.local) ? new TraversalScope.Local() : new TraversalScope.Global();
        return new TraversalScopeArgument.Value(scope);
    }

    private static boolean isPredicate(Object[] a) {
        return a.length == 1 && a[0] instanceof org.apache.tinkerpop.gremlin.process.traversal.P;
    }

    /** Reverse of is(...): a P → predicate, else a single value → Object_. */
    static hydra.tinkerpop.gremlin.PredicateOrObject predicateOrObjectRev(Object[] a) {
        if (isPredicate(a)) {
            return new hydra.tinkerpop.gremlin.PredicateOrObject.Predicate(Predicates.fromP((org.apache.tinkerpop.gremlin.process.traversal.P<?>) a[0]));
        }
        if (a.length != 1) {
            throw new IllegalArgumentException("is expects 1 argument, got " + a.length);
        }
        return new hydra.tinkerpop.gremlin.PredicateOrObject.Object_(
                new GenericLiteralArgument.Value(literal(a[0])));
    }

    /** Reverse of hasLabel/hasKey: a P → predicate, else string varargs → Strings. */
    static hydra.tinkerpop.gremlin.PredicateOrStrings predicateOrStringsRev(Object[] a) {
        if (isPredicate(a)) {
            return new hydra.tinkerpop.gremlin.PredicateOrStrings.Predicate(Predicates.fromP((org.apache.tinkerpop.gremlin.process.traversal.P<?>) a[0]));
        }
        return new hydra.tinkerpop.gremlin.PredicateOrStrings.Strings(stringNullableArgs(a));
    }

    /** Reverse of hasId/hasValue: a P → predicate, else value varargs → Objects. */
    static hydra.tinkerpop.gremlin.PredicateOrObjects predicateOrObjectsRev(Object[] a) {
        if (isPredicate(a)) {
            return new hydra.tinkerpop.gremlin.PredicateOrObjects.Predicate(Predicates.fromP((org.apache.tinkerpop.gremlin.process.traversal.P<?>) a[0]));
        }
        return new hydra.tinkerpop.gremlin.PredicateOrObjects.Objects(literalArgs(a));
    }

    // -- Reverse single-value helpers ----------------------------------------------------------

    private static GenericLiteralArgument litArg(Object o) {
        return new GenericLiteralArgument.Value(literal(o));
    }

    private static hydra.tinkerpop.gremlin.StringArgument strArg(Object o) {
        return new hydra.tinkerpop.gremlin.StringArgument.Value((String) o);
    }

    private static hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.StringArgument> strOpt(Object[] a) {
        return a.length == 0
                ? hydra.overlay.java.util.Optional.none()
                : hydra.overlay.java.util.Optional.given(strArg(a[0]));
    }

    private static StringNullableArgument stringNullableArg1(Object o) {
        hydra.overlay.java.util.Optional<String> opt = (o == null)
                ? hydra.overlay.java.util.Optional.none()
                : hydra.overlay.java.util.Optional.given((String) o);
        return new StringNullableArgument.Value(opt);
    }

    private static hydra.tinkerpop.gremlin.IntegerArgument intArg(Object o) {
        return new hydra.tinkerpop.gremlin.IntegerArgument.Value(integerLiteral(o));
    }

    private static hydra.tinkerpop.gremlin.NumericArgument numArg(Object o) {
        return new hydra.tinkerpop.gremlin.NumericArgument.Value(numeric0(o));
    }

    /** Boxed Java number → Hydra NumericLiteral (helper for numArg), preserving precision. */
    private static NumericLiteral numeric0(Object o) {
        if (isFloat(o)) {
            return new NumericLiteral.Float_(floatLiteral(o));
        }
        return new NumericLiteral.Integer_(integerLiteral(o));
    }

    // -- Reverse nested-traversal helpers ------------------------------------------------------

    /** A nested {@link Bytecode} (anonymous traversal arg) → Hydra {@code NestedTraversal}. */
    static hydra.tinkerpop.gremlin.NestedTraversal nestedRev(Object o) {
        if (!(o instanceof Bytecode)) {
            throw notYetMapped("argument", "expected nested Bytecode, got "
                    + (o == null ? "null" : o.getClass().getName()));
        }
        Bytecode child = (Bytecode) o;
        List<TraversalMethod> steps = new ArrayList<>();
        for (Bytecode.Instruction si : child.getStepInstructions()) {
            steps.add(stepFrom(si));
        }
        return new hydra.tinkerpop.gremlin.NestedTraversal.Anonymous(
                new hydra.tinkerpop.gremlin.ChainedTraversal(steps));
    }

    private static List<hydra.tinkerpop.gremlin.NestedTraversal> nestedListRev(Object[] a) {
        List<hydra.tinkerpop.gremlin.NestedTraversal> out = new ArrayList<>(a.length);
        for (Object o : a) {
            out.add(nestedRev(o));
        }
        return out;
    }

    private static String clsOf(Object o) {
        return o == null ? "null" : o.getClass().getSimpleName();
    }

    private static hydra.tinkerpop.gremlin.TraversalPredicate predFrom(Object o) {
        return Predicates.fromP((org.apache.tinkerpop.gremlin.process.traversal.P<?>) o);
    }

    private static hydra.tinkerpop.gremlin.PredicateOrTraversal predicateOrTraversalRev(Object o) {
        if (o instanceof Bytecode) {
            return new hydra.tinkerpop.gremlin.PredicateOrTraversal.Traversal(nestedRev(o));
        }
        return new hydra.tinkerpop.gremlin.PredicateOrTraversal.Predicate(predFrom(o));
    }

    private static hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal stringOrNestedRev(Object o) {
        if (o instanceof Bytecode) {
            return new hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal.Traversal(nestedRev(o));
        }
        return new hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal.String_(strArg(o));
    }

    private static hydra.tinkerpop.gremlin.MergeArgs mergeArgsRev(Object[] a) {
        if (a.length >= 1 && a[0] instanceof Bytecode) {
            return new hydra.tinkerpop.gremlin.MergeArgs.Traversal(nestedRev(a[0]));
        }
        throw notYetMapped("step", "merge*(map literal) reverse");
    }

    private static hydra.tinkerpop.gremlin.TraversalBiFunctionArgument biFunctionRev(Object o) {
        return new hydra.tinkerpop.gremlin.TraversalBiFunctionArgument.Value(Operators.fromGremlin(
                (org.apache.tinkerpop.gremlin.process.traversal.Operator) o));
    }

    private static hydra.tinkerpop.gremlin.TraversalDTArgument dtArgRev(Object o) {
        org.apache.tinkerpop.gremlin.process.traversal.DT dt = (org.apache.tinkerpop.gremlin.process.traversal.DT) o;
        hydra.tinkerpop.gremlin.TraversalDT t;
        switch (dt) {
            case second: t = new hydra.tinkerpop.gremlin.TraversalDT.Second(); break;
            case minute: t = new hydra.tinkerpop.gremlin.TraversalDT.Minute(); break;
            case hour:   t = new hydra.tinkerpop.gremlin.TraversalDT.Hour(); break;
            case day:    t = new hydra.tinkerpop.gremlin.TraversalDT.Day(); break;
            default: throw new IllegalArgumentException("Unknown DT: " + dt);
        }
        return new hydra.tinkerpop.gremlin.TraversalDTArgument.Value(t);
    }

    private static hydra.tinkerpop.gremlin.DirectionAndVarargs directionAndVarargsRev(Object[] a) {
        hydra.tinkerpop.gremlin.TraversalDirectionArgument dir = new hydra.tinkerpop.gremlin.TraversalDirectionArgument.Value(
                Directions.fromGremlin((org.apache.tinkerpop.gremlin.structure.Direction) a[0]));
        List<StringNullableArgument> labels = new ArrayList<>(Math.max(0, a.length - 1));
        for (int k = 1; k < a.length; k++) { labels.add(stringNullableArg1(a[k])); }
        return new hydra.tinkerpop.gremlin.DirectionAndVarargs(dir, labels);
    }

    // -- Reverse enum helpers ------------------------------------------------------------------

    private static hydra.tinkerpop.gremlin.TraversalColumn columnRev(org.apache.tinkerpop.gremlin.structure.Column c) {
        return c == org.apache.tinkerpop.gremlin.structure.Column.keys
                ? new hydra.tinkerpop.gremlin.TraversalColumn.Keys()
                : new hydra.tinkerpop.gremlin.TraversalColumn.Values();
    }

    private static hydra.tinkerpop.gremlin.TraversalPopArgument popRev(org.apache.tinkerpop.gremlin.process.traversal.Pop p) {
        hydra.tinkerpop.gremlin.TraversalPop t;
        switch (p) {
            case first: t = new hydra.tinkerpop.gremlin.TraversalPop.First(); break;
            case last:  t = new hydra.tinkerpop.gremlin.TraversalPop.Last(); break;
            case all:   t = new hydra.tinkerpop.gremlin.TraversalPop.All(); break;
            case mixed: t = new hydra.tinkerpop.gremlin.TraversalPop.Mixed(); break;
            default: throw new IllegalArgumentException("Unknown Pop: " + p);
        }
        return new hydra.tinkerpop.gremlin.TraversalPopArgument.Value(t);
    }

    private static hydra.tinkerpop.gremlin.TraversalOrder orderRev(org.apache.tinkerpop.gremlin.process.traversal.Order o) {
        switch (o) {
            case asc:     return new hydra.tinkerpop.gremlin.TraversalOrder.Asc();
            case desc:    return new hydra.tinkerpop.gremlin.TraversalOrder.Desc();
            case shuffle: return new hydra.tinkerpop.gremlin.TraversalOrder.Shuffle();
            default: throw new IllegalArgumentException("Unknown Order: " + o);
        }
    }

    private static hydra.tinkerpop.gremlin.TraversalToken tokenRev(org.apache.tinkerpop.gremlin.structure.T t) {
        switch (t) {
            case id:    return new hydra.tinkerpop.gremlin.TraversalToken.Id();
            case label: return new hydra.tinkerpop.gremlin.TraversalToken.Label();
            case key:   return new hydra.tinkerpop.gremlin.TraversalToken.Key();
            case value: return new hydra.tinkerpop.gremlin.TraversalToken.Value();
            default: throw new IllegalArgumentException("Unknown T: " + t);
        }
    }

    private static hydra.tinkerpop.gremlin.TraversalCardinality cardinalityRev(org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality c) {
        // The model's TraversalCardinality wraps a GenericLiteral payload; here there is no value, so use
        // a null literal placeholder (the bare-enum form). single/set/list select the variant.
        hydra.tinkerpop.gremlin.GenericLiteral none = new hydra.tinkerpop.gremlin.GenericLiteral.Null();
        switch (c) {
            case single: return new hydra.tinkerpop.gremlin.TraversalCardinality.Single(none);
            case set:    return new hydra.tinkerpop.gremlin.TraversalCardinality.Set(none);
            case list:   return new hydra.tinkerpop.gremlin.TraversalCardinality.List(none);
            default: throw new IllegalArgumentException("Unknown Cardinality: " + c);
        }
    }

    // -- Reverse has(...) ----------------------------------------------------------------------

    private static hydra.tinkerpop.gremlin.HasArgs hasArgsRev(Object[] a) {
        Object k0 = a[0];
        Object[] clauseArgs = java.util.Arrays.copyOfRange(a, 1, a.length);
        if (k0 instanceof org.apache.tinkerpop.gremlin.structure.T) {
            return new hydra.tinkerpop.gremlin.HasArgs.TraversalToken(
                    new hydra.tinkerpop.gremlin.HasArgsWithToken(
                            new hydra.tinkerpop.gremlin.TraversalTokenArgument.Value(tokenRev((org.apache.tinkerpop.gremlin.structure.T) k0)),
                            hasValueClauseRev(clauseArgs)));
        }
        StringNullableArgument key = stringNullableArg1(k0);
        hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.HasValueClause> clause =
                (clauseArgs.length == 0) ? hydra.overlay.java.util.Optional.none()
                        : hydra.overlay.java.util.Optional.given(hasValueClauseRev(clauseArgs));
        return new hydra.tinkerpop.gremlin.HasArgs.String_(new hydra.tinkerpop.gremlin.HasArgsWithKey(key, clause));
    }

    private static hydra.tinkerpop.gremlin.HasValueClause hasValueClauseRev(Object[] c) {
        if (c.length == 1) {
            Object o = c[0];
            if (o instanceof org.apache.tinkerpop.gremlin.process.traversal.P) {
                return new hydra.tinkerpop.gremlin.HasValueClause.Predicate(predFrom(o));
            }
            if (o instanceof Bytecode) {
                return new hydra.tinkerpop.gremlin.HasValueClause.Traversal(nestedRev(o));
            }
            return new hydra.tinkerpop.gremlin.HasValueClause.Object_(litArg(o));
        }
        if (c.length == 2) {
            // has(label, key, value) form: here c = [key, value-or-P]
            if (c[1] instanceof org.apache.tinkerpop.gremlin.process.traversal.P) {
                return new hydra.tinkerpop.gremlin.HasValueClause.KeyPredicate(
                        new hydra.tinkerpop.gremlin.StringKeyAndPredicate(stringNullableArg1(c[0]), predFrom(c[1])));
            }
            return new hydra.tinkerpop.gremlin.HasValueClause.KeyObject(
                    new hydra.tinkerpop.gremlin.StringKeyAndObject(stringNullableArg1(c[0]), litArg(c[1])));
        }
        throw notYetMapped("step", "has(...) value clause with " + c.length + " args");
    }

    private static UnsupportedOperationException notYetMapped(String kind, String what) {
        return new UnsupportedOperationException(
                "Bytecode→Hydra mapping not yet implemented for " + kind + ": " + what);
    }
}
