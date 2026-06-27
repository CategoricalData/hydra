package hydra.overlay.java.tinkerpop;

import hydra.tinkerpop.gremlin.GenericLiteral;
import hydra.tinkerpop.gremlin.GenericLiteralArgument;
import hydra.tinkerpop.gremlin.IntegerLiteral;
import hydra.tinkerpop.gremlin.NumericLiteral;
import hydra.tinkerpop.gremlin.RootTraversal;
import hydra.tinkerpop.gremlin.StringArgument;
import hydra.tinkerpop.gremlin.StringNullableArgument;
import hydra.tinkerpop.gremlin.TraversalMethod;
import hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod;

import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;

import java.util.ArrayList;
import java.util.List;

/**
 * Forward mapping: Hydra {@code hydra.tinkerpop.gremlin} model &rarr; TinkerPop {@link Bytecode}.
 *
 * <p>{@code Bytecode} is TinkerPop's language-agnostic traversal representation — an ordered list of
 * instructions, each a {@code (String operator, Object[] arguments)} pair, split into source
 * instructions (from {@code withX} config) and step instructions (the spawn step plus the chained
 * steps). It is what TinkerPop's own {@code Translator} instances consume, and it composes with
 * {@code JavaTranslator.of(g).translate(bytecode)} to produce a runnable {@code GraphTraversal}.
 *
 * <p>The Hydra model (after the #442 de-BNF streamlining) sits at the same semantic-step altitude as
 * Bytecode, so the mapping is shallow: most {@link TraversalMethod} variants map to a single
 * {@code addStep(name, args...)} call.
 *
 * <p><b>Coverage:</b> this is an incremental foundation. The structural backbone (source / spawn /
 * chained), the literal/argument mappers, and a representative set of common steps are implemented;
 * the long tail of mechanical steps throws {@link UnsupportedOperationException} via the partial
 * visitor's {@code otherwise} until filled in. Each unimplemented step is a small, isolated addition
 * (operator name + the same arg mappers).
 *
 * <p><b>Location note (#442 / #511):</b> this lives under {@code bindings/} for now; it is intended to
 * migrate into {@code overlay/java/hydra-pg} once #511 settles how a generated build config absorbs an
 * overlay-declared third-party dependency (gremlin-core). The code is written to move verbatim.
 */
public final class HydraToBytecode {

    private HydraToBytecode() {
    }

    /**
     * Maps a Hydra {@link RootTraversal} to a TinkerPop {@link Bytecode}.
     *
     * <p>Source/{@code withX} instructions are emitted first, then the spawn step, then each chained
     * step, mirroring {@code Bytecode}'s source-then-step ordering.
     */
    public static Bytecode toBytecode(RootTraversal rt) {
        Bytecode bc = new Bytecode();
        for (hydra.tinkerpop.gremlin.TraversalSourceSelfMethod self : rt.source.value) {
            addSource(bc, self);
        }
        addSpawn(bc, rt.spawnMethod);
        for (TraversalMethod m : rt.chained) {
            addStep(bc, m);
        }
        return bc;
    }

    // -- Source / withX instructions -----------------------------------------------------------

    private static void addSource(Bytecode bc, hydra.tinkerpop.gremlin.TraversalSourceSelfMethod self) {
        // withX config methods. Implemented incrementally; the common ones first.
        self.accept(new hydra.tinkerpop.gremlin.TraversalSourceSelfMethod.PartialVisitor<Void>() {
            @Override
            public Void otherwise(hydra.tinkerpop.gremlin.TraversalSourceSelfMethod instance) {
                throw notYetMapped("source/withX", instance.getClass().getSimpleName());
            }

            @Override
            public Void visit(hydra.tinkerpop.gremlin.TraversalSourceSelfMethod.WithBulk x) {
                bc.addSource("withBulk", x.value);
                return null;
            }

            @Override
            public Void visit(hydra.tinkerpop.gremlin.TraversalSourceSelfMethod.WithPath x) {
                bc.addSource("withPath");
                return null;
            }
        });
    }

    // -- Spawn step ----------------------------------------------------------------------------

    private static void addSpawn(Bytecode bc, TraversalSourceSpawnMethod spawn) {
        spawn.accept(new TraversalSourceSpawnMethod.PartialVisitor<Void>() {
            @Override
            public Void otherwise(TraversalSourceSpawnMethod instance) {
                throw notYetMapped("spawn", instance.getClass().getSimpleName());
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.V x) {
                bc.addStep("V", literalArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.E x) {
                bc.addStep("E", literalArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.Inject x) {
                bc.addStep("inject", literalArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.AddV x) {
                if (x.value.isNone()) { bc.addStep("addV"); }
                else { bc.addStep("addV", stringOrNested(x.value.fromGiven())); }
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.AddE x) {
                bc.addStep("addE", stringOrNested(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.MergeV x) {
                bc.addStep("mergeV", mergeArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.MergeE x) {
                bc.addStep("mergeE", mergeArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.Union x) {
                bc.addStep("union", nestedList(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.Io x) {
                bc.addStep("io", stringArg(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalSourceSpawnMethod.Call x) {
                if (x.value.isNone()) { bc.addStep("call"); }
                else { throw notYetMapped("spawn", "call(service) — service-call spawn"); }
                return null;
            }
        });
    }

    // -- Chained steps -------------------------------------------------------------------------

    private static void addStep(Bytecode bc, TraversalMethod m) {
        m.accept(new TraversalMethod.PartialVisitor<Void>() {
            @Override
            public Void otherwise(TraversalMethod instance) {
                throw notYetMapped("step", instance.getClass().getSimpleName());
            }

            // Element access
            @Override
            public Void visit(TraversalMethod.V x) {
                bc.addStep("V", literalArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.E x) {
                bc.addStep("E", literalArgs(x.value));
                return null;
            }

            // Adjacency: out/in/both take a varargs of (nullable) edge labels
            @Override
            public Void visit(TraversalMethod.Out x) {
                bc.addStep("out", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.In x) {
                bc.addStep("in", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Both x) {
                bc.addStep("both", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.OutE x) {
                bc.addStep("outE", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.InE x) {
                bc.addStep("inE", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.BothE x) {
                bc.addStep("bothE", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.OutV x) {
                bc.addStep("outV");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.InV x) {
                bc.addStep("inV");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.BothV x) {
                bc.addStep("bothV");
                return null;
            }

            // Value access
            @Override
            public Void visit(TraversalMethod.Values x) {
                bc.addStep("values", stringNullableArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Id x) {
                bc.addStep("id");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Label x) {
                bc.addStep("label");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Key x) {
                bc.addStep("key");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Value x) {
                bc.addStep("value");
                return null;
            }

            // Common termin+barrier-less utilities
            @Override
            public Void visit(TraversalMethod.Identity x) {
                bc.addStep("identity");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Drop x) {
                bc.addStep("drop");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Fold x) {
                // fold() with no seed/bifunction is the common case; the optional 2-arg form is a TODO.
                if (x.value.isNone()) {
                    bc.addStep("fold");
                } else {
                    throw notYetMapped("step", "fold(seed, biFunction)");
                }
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Unfold x) {
                bc.addStep("unfold");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Count x) {
                addScopeOptional(bc, "count", x.value);
                return null;
            }

            // No-argument steps: a single addStep(name) each.
            @Override
            public Void visit(TraversalMethod.AsBool x) {
                bc.addStep("asBool");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.AsDate x) {
                bc.addStep("asDate");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.ConnectedComponent x) {
                bc.addStep("connectedComponent");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.CyclicPath x) {
                bc.addStep("cyclicPath");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Discard x) {
                bc.addStep("discard");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Index x) {
                bc.addStep("index");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.OtherV x) {
                bc.addStep("otherV");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Path x) {
                bc.addStep("path");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.PeerPressure x) {
                bc.addStep("peerPressure");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Read x) {
                bc.addStep("read");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Reverse x) {
                bc.addStep("reverse");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.ShortestPath x) {
                bc.addStep("shortestPath");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.SimplePath x) {
                bc.addStep("simplePath");
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Write x) {
                bc.addStep("write");
                return null;
            }

            // Steps with an optional TraversalScope argument: addStep(name) or addStep(name, scope).
            @Override
            public Void visit(TraversalMethod.AsString x) {
                addScopeOptional(bc, "asString", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Length x) {
                addScopeOptional(bc, "length", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Max x) {
                addScopeOptional(bc, "max", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Mean x) {
                addScopeOptional(bc, "mean", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Min x) {
                addScopeOptional(bc, "min", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Order x) {
                addScopeOptional(bc, "order", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Sum x) {
                addScopeOptional(bc, "sum", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.ToLower x) {
                addScopeOptional(bc, "toLower", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.ToUpper x) {
                addScopeOptional(bc, "toUpper", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Trim x) {
                addScopeOptional(bc, "trim", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.LTrim x) {
                addScopeOptional(bc, "lTrim", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.RTrim x) {
                addScopeOptional(bc, "rTrim", x.value);
                return null;
            }

            // Label steps: as/cap/project take a first label + rest labels → flat string varargs.
            @Override
            public Void visit(TraversalMethod.As x) {
                bc.addStep("as", asArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Cap x) {
                bc.addStep("cap", asArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Project x) {
                bc.addStep("project", asArgs(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Constant x) {
                bc.addStep("constant", literalArg(x.value));
                return null;
            }

            // limit/skip take an optional scope + a long.
            @Override
            public Void visit(TraversalMethod.Limit x) {
                addScopeAndInteger(bc, "limit", x.value);
                return null;
            }

            @Override
            public Void visit(TraversalMethod.Skip x) {
                addScopeAndInteger(bc, "skip", x.value);
                return null;
            }

            // Predicate-bearing filters: a P/TextP predicate, or raw value(s).
            @Override
            public Void visit(TraversalMethod.Is x) {
                bc.addStep("is", predicateOrObject(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.HasLabel x) {
                bc.addStep("hasLabel", predicateOrStrings(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.HasKey x) {
                bc.addStep("hasKey", predicateOrStrings(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.HasId x) {
                bc.addStep("hasId", predicateOrObjects(x.value));
                return null;
            }

            @Override
            public Void visit(TraversalMethod.HasValue x) {
                bc.addStep("hasValue", predicateOrObjects(x.value));
                return null;
            }

            // Single generic-literal-argument steps.
            @Override public Void visit(TraversalMethod.Combine x)    { bc.addStep("combine", literalArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Difference x) { bc.addStep("difference", literalArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Disjunct x)   { bc.addStep("disjunct", literalArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Intersect x)  { bc.addStep("intersect", literalArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Merge x)      { bc.addStep("merge", literalArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Product x)    { bc.addStep("product", literalArg(x.value)); return null; }

            // Single nested-traversal steps.
            @Override public Void visit(TraversalMethod.Branch x)     { bc.addStep("branch", nested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.FlatMap x)    { bc.addStep("flatMap", nested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Local x)      { bc.addStep("local", nested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Map x)        { bc.addStep("map", nested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Not x)        { bc.addStep("not", nested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Optional x)   { bc.addStep("optional", nested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.SideEffect x) { bc.addStep("sideEffect", nested(x.value)); return null; }

            // List-of-nested-traversal steps (varargs).
            @Override public Void visit(TraversalMethod.And x)      { bc.addStep("and", nestedList(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Coalesce x) { bc.addStep("coalesce", nestedList(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Match x)    { bc.addStep("match", nestedList(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Or x)       { bc.addStep("or", nestedList(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Union x)    { bc.addStep("union", nestedList(x.value)); return null; }

            // Single string-argument steps.
            @Override public Void visit(TraversalMethod.Aggregate x) { bc.addStep("aggregate", stringArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Conjoin x)   { bc.addStep("conjoin", stringArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Format x)    { bc.addStep("format", stringArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Subgraph x)  { bc.addStep("subgraph", stringArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Math_ x)     { bc.addStep("math", stringArg(x.value)); return null; }

            // Optional-string-argument steps: addStep(name) or addStep(name, str).
            @Override public Void visit(TraversalMethod.Fail x)       { addStringOptional(bc, "fail", x.value); return null; }
            @Override public Void visit(TraversalMethod.Group x)      { addStringOptional(bc, "group", x.value); return null; }
            @Override public Void visit(TraversalMethod.GroupCount x) { addStringOptional(bc, "groupCount", x.value); return null; }
            @Override public Void visit(TraversalMethod.Loops x)      { addStringOptional(bc, "loops", x.value); return null; }
            @Override public Void visit(TraversalMethod.Profile x)    { addStringOptional(bc, "profile", x.value); return null; }
            @Override public Void visit(TraversalMethod.Tree x)       { addStringOptional(bc, "tree", x.value); return null; }

            // List-of-nullable-string steps (varargs).
            @Override public Void visit(TraversalMethod.Element x)     { bc.addStep("element", stringNullableArgs(x.value)); return null; }
            @Override public Void visit(TraversalMethod.ElementMap x)  { bc.addStep("elementMap", stringNullableArgs(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Properties x)  { bc.addStep("properties", stringNullableArgs(x.value)); return null; }
            @Override public Void visit(TraversalMethod.PropertyMap x) { bc.addStep("propertyMap", stringNullableArgs(x.value)); return null; }
            @Override public Void visit(TraversalMethod.HasNot x)      { bc.addStep("hasNot", stringNullableArg(x.value)); return null; }

            // Integer-argument steps.
            @Override public Void visit(TraversalMethod.TimeLimit x) { bc.addStep("timeLimit", integerArgLong(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Times x)     { bc.addStep("times", integerArg(x.value)); return null; }

            // inject (varargs of generic literals).
            @Override public Void visit(TraversalMethod.Inject x) { bc.addStep("inject", literalArgs(x.value)); return null; }

            // Predicate-argument steps (all/any/none).
            @Override public Void visit(TraversalMethod.All x)  { bc.addStep("all", Predicates.toP(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Any x)  { bc.addStep("any", Predicates.toP(x.value)); return null; }
            @Override public Void visit(TraversalMethod.None x) { bc.addStep("none", Predicates.toP(x.value)); return null; }

            // Predicate-or-traversal steps (filter/until/emit).
            @Override public Void visit(TraversalMethod.Filter x) { bc.addStep("filter", predicateOrTraversal(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Until x)  { bc.addStep("until", predicateOrTraversal(x.value)); return null; }
            @Override public Void visit(TraversalMethod.Emit x) {
                if (x.value.isNone()) { bc.addStep("emit"); }
                else { bc.addStep("emit", predicateOrTraversal(x.value.fromGiven())); }
                return null;
            }

            // coin (numeric); pageRank (optional numeric).
            @Override public Void visit(TraversalMethod.Coin x) { bc.addStep("coin", numericArg(x.value)); return null; }
            @Override public Void visit(TraversalMethod.PageRank x) {
                if (x.value.isNone()) { bc.addStep("pageRank"); }
                else { bc.addStep("pageRank", numericArg(x.value.fromGiven())); }
                return null;
            }

            // asNumber (optional GType).
            @Override public Void visit(TraversalMethod.AsNumber x) {
                if (x.value.isNone()) { bc.addStep("asNumber"); }
                else { bc.addStep("asNumber", gtypeArg(x.value.fromGiven())); }
                return null;
            }

            // addV/addE (string label or nested traversal).
            @Override public Void visit(TraversalMethod.AddE x) { bc.addStep("addE", stringOrNested(x.value)); return null; }
            @Override public Void visit(TraversalMethod.AddV x) {
                if (x.value.isNone()) { bc.addStep("addV"); }
                else { bc.addStep("addV", stringOrNested(x.value.fromGiven())); }
                return null;
            }

            // dateAdd(DT, int); dateDiff(traversal | date)
            @Override public Void visit(TraversalMethod.DateAdd x) {
                bc.addStep("dateAdd", dtArg(x.value.unit), integerArg(x.value.duration));
                return null;
            }
            @Override public Void visit(TraversalMethod.DateDiff x) {
                x.value.accept(new hydra.tinkerpop.gremlin.DateDiffArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.DateDiffArgs.Traversal y) { bc.addStep("dateDiff", nested(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.DateDiffArgs.Date y) { bc.addStep("dateDiff", dateArg(y.value)); return null; }
                });
                return null;
            }

            // range([scope,] min, max)
            @Override public Void visit(TraversalMethod.Range x) {
                hydra.tinkerpop.gremlin.RangeArgs r = x.value;
                long min = integerArgLong(r.min), max = integerArgLong(r.max);
                if (r.scope.isNone()) { bc.addStep("range", min, max); }
                else { bc.addStep("range", scopeArg(r.scope.fromGiven()), min, max); }
                return null;
            }

            // repeat([label,] traversal)
            @Override public Void visit(TraversalMethod.Repeat x) {
                hydra.tinkerpop.gremlin.RepeatArgs r = x.value;
                if (r.string.isNone()) { bc.addStep("repeat", nested(r.traversal)); }
                else { bc.addStep("repeat", stringArg(r.string.fromGiven()), nested(r.traversal)); }
                return null;
            }

            // replace([scope,] from, to)
            @Override public Void visit(TraversalMethod.Replace x) {
                hydra.tinkerpop.gremlin.ReplaceArgs r = x.value;
                if (r.scope.isNone()) { bc.addStep("replace", stringNullableArg(r.from), stringNullableArg(r.to)); }
                else { bc.addStep("replace", scopeArg(r.scope.fromGiven()), stringNullableArg(r.from), stringNullableArg(r.to)); }
                return null;
            }

            // split([scope,] delimiter)
            @Override public Void visit(TraversalMethod.Split x) {
                hydra.tinkerpop.gremlin.SplitArgs r = x.value;
                if (r.scope.isNone()) { bc.addStep("split", stringNullableArg(r.delimiter)); }
                else { bc.addStep("split", scopeArg(r.scope.fromGiven()), stringNullableArg(r.delimiter)); }
                return null;
            }

            // substring([scope,] start[, end])
            @Override public Void visit(TraversalMethod.Substring x) {
                hydra.tinkerpop.gremlin.SubstringArgs r = x.value;
                List<Object> args = new ArrayList<>();
                if (!r.scope.isNone()) { args.add(scopeArg(r.scope.fromGiven())); }
                args.add(integerArg(r.start));
                if (!r.end.isNone()) { args.add(integerArg(r.end.fromGiven())); }
                bc.addStep("substring", args.toArray());
                return null;
            }

            // sample([scope,] count)
            @Override public Void visit(TraversalMethod.Sample x) {
                hydra.tinkerpop.gremlin.SampleByScope r = x.value;
                Object n = integerArg(r.integer);
                if (r.scope.isNone()) { bc.addStep("sample", n); }
                else { bc.addStep("sample", scopeArg(r.scope.fromGiven()), n); }
                return null;
            }

            // tail([scope,] [count])
            @Override public Void visit(TraversalMethod.Tail x) {
                if (x.value.isNone()) { bc.addStep("tail"); return null; }
                hydra.tinkerpop.gremlin.TailArgs r = x.value.fromGiven();
                List<Object> args = new ArrayList<>();
                if (!r.scope.isNone()) { args.add(scopeArg(r.scope.fromGiven())); }
                if (!r.integer.isNone()) { args.add(integerArgLong(r.integer.fromGiven())); }
                bc.addStep("tail", args.toArray());
                return null;
            }

            // concat(traversal... | string...)
            @Override public Void visit(TraversalMethod.Concat x) {
                x.value.accept(new hydra.tinkerpop.gremlin.ConcatArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.ConcatArgs.Traversal y) { bc.addStep("concat", nestedList(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ConcatArgs.String_ y) { bc.addStep("concat", stringNullableArgs(y.value)); return null; }
                });
                return null;
            }

            // toE(direction, edgeLabels...); toV(direction)
            @Override public Void visit(TraversalMethod.ToE x) { bc.addStep("toE", directionAndVarargs(x.value)); return null; }
            @Override public Void visit(TraversalMethod.ToV x) { bc.addStep("toV", directionArg(x.value)); return null; }

            // has(...) — the most intricate filter. Key-based or token-based, with an optional value
            // clause that may itself be an object / predicate / nested traversal / (key,object) / (key,predicate).
            @Override public Void visit(TraversalMethod.Has x) {
                x.value.accept(new hydra.tinkerpop.gremlin.HasArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.HasArgs.String_ y) {
                        hydra.tinkerpop.gremlin.HasArgsWithKey k = y.value;
                        Object key = stringNullableArg(k.key);
                        if (k.value.isNone()) {
                            bc.addStep("has", key);
                        } else {
                            bc.addStep("has", prependKey(key, hasValueClause(k.value.fromGiven())));
                        }
                        return null;
                    }
                    @Override public Void visit(hydra.tinkerpop.gremlin.HasArgs.TraversalToken y) {
                        hydra.tinkerpop.gremlin.HasArgsWithToken k = y.value;
                        Object tok = tokenArg(k.token);
                        bc.addStep("has", prependKey(tok, hasValueClause(k.value)));
                        return null;
                    }
                });
                return null;
            }

            // where(predicate | string | traversal)
            @Override public Void visit(TraversalMethod.Where x) {
                x.value.accept(new hydra.tinkerpop.gremlin.WhereArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.WhereArgs.Predicate y) {
                        hydra.tinkerpop.gremlin.WhereWithPredicateArgs w = y.value;
                        if (w.leftArg.isNone()) { bc.addStep("where", Predicates.toP(w.predicate)); }
                        else { bc.addStep("where", stringArg(w.leftArg.fromGiven()), Predicates.toP(w.predicate)); }
                        return null;
                    }
                    @Override public Void visit(hydra.tinkerpop.gremlin.WhereArgs.String_ y) { bc.addStep("where", stringArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.WhereArgs.Traversal y) { bc.addStep("where", nested(y.value)); return null; }
                });
                return null;
            }

            // from(string | vertex | traversal)
            @Override public Void visit(TraversalMethod.From x) {
                x.value.accept(new hydra.tinkerpop.gremlin.FromArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.FromArgs.String_ y) { bc.addStep("from", stringArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.FromArgs.Traversal y) { bc.addStep("from", nested(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.FromArgs.Vertex y) { throw notYetMapped("step", "from(StructureVertex)"); }
                });
                return null;
            }

            // to(direction+labels | string | vertex | traversal)
            @Override public Void visit(TraversalMethod.To x) {
                x.value.accept(new hydra.tinkerpop.gremlin.ToArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.ToArgs.Direction y) { bc.addStep("to", directionAndVarargs(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ToArgs.String_ y) { bc.addStep("to", stringArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ToArgs.Traversal y) { bc.addStep("to", nested(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ToArgs.Vertex y) { throw notYetMapped("step", "to(StructureVertex)"); }
                });
                return null;
            }

            // mergeE/mergeV(map | traversal)
            @Override public Void visit(TraversalMethod.MergeE x) { bc.addStep("mergeE", mergeArgs(x.value)); return null; }
            @Override public Void visit(TraversalMethod.MergeV x) { bc.addStep("mergeV", mergeArgs(x.value)); return null; }

            // dedup(scope, strings... | strings...)
            @Override public Void visit(TraversalMethod.Dedup x) {
                x.value.accept(new hydra.tinkerpop.gremlin.DedupArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.DedupArgs.ScopeString y) {
                        hydra.tinkerpop.gremlin.ScopeStringArgs s = y.value;
                        List<Object> args = new ArrayList<>();
                        args.add(scopeArg(s.scope));
                        for (StringNullableArgument str : s.strings) { args.add(stringNullableArg(str)); }
                        bc.addStep("dedup", args.toArray());
                        return null;
                    }
                    @Override public Void visit(hydra.tinkerpop.gremlin.DedupArgs.String_ y) { bc.addStep("dedup", stringNullableArgs(y.value)); return null; }
                });
                return null;
            }

            // valueMap(strings... | booleanForm)
            @Override public Void visit(TraversalMethod.ValueMap x) {
                x.value.accept(new hydra.tinkerpop.gremlin.ValueMapArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.ValueMapArgs.String_ y) { bc.addStep("valueMap", stringNullableArgs(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ValueMapArgs.Boolean_ y) {
                        hydra.tinkerpop.gremlin.ValueMapBooleanArgs b = y.value;
                        List<Object> args = new ArrayList<>();
                        args.add(booleanArg(b.value));
                        if (!b.keys.isNone()) { for (StringNullableArgument str : b.keys.fromGiven()) { args.add(stringNullableArg(str)); } }
                        bc.addStep("valueMap", args.toArray());
                        return null;
                    }
                });
                return null;
            }

            // barrier(normSack | maxBarrierSize)
            @Override public Void visit(TraversalMethod.Barrier x) {
                if (x.value.isNone()) { bc.addStep("barrier"); return null; }
                x.value.fromGiven().accept(new hydra.tinkerpop.gremlin.BarrierArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.BarrierArgs.Int y) { bc.addStep("barrier", integerArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.BarrierArgs.Consumer y) { throw notYetMapped("step", "barrier(SackFunction consumer)"); }
                });
                return null;
            }

            // select(column | strings | [pop] traversal | [pop] keys...)
            @Override public Void visit(TraversalMethod.Select x) {
                x.value.accept(new hydra.tinkerpop.gremlin.SelectArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.SelectArgs.Column y) { bc.addStep("select", columnArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.SelectArgs.Strings y) {
                        List<Object> args = new ArrayList<>();
                        for (StringArgument s : y.value) { args.add(stringArg(s)); }
                        bc.addStep("select", args.toArray());
                        return null;
                    }
                    @Override public Void visit(hydra.tinkerpop.gremlin.SelectArgs.Traversal y) { bc.addStep("select", nested(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.SelectArgs.PopStrings y) {
                        hydra.tinkerpop.gremlin.SelectByKeys s = y.value;
                        List<Object> args = new ArrayList<>();
                        args.add(pop(s.pop));
                        for (StringArgument str : s.keys) { args.add(stringArg(str)); }
                        bc.addStep("select", args.toArray());
                        return null;
                    }
                    @Override public Void visit(hydra.tinkerpop.gremlin.SelectArgs.PopTraversal y) {
                        bc.addStep("select", pop(y.value.pop), nested(y.value.traversal));
                        return null;
                    }
                });
                return null;
            }

            // choose(function | predicate,true[,false] | traversal)
            @Override public Void visit(TraversalMethod.Choose x) {
                x.value.accept(new hydra.tinkerpop.gremlin.ChooseArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.ChooseArgs.Function y) { bc.addStep("choose", functionArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ChooseArgs.Traversal y) { bc.addStep("choose", nested(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ChooseArgs.PredicateTraversal y) {
                        hydra.tinkerpop.gremlin.PredicateOrTraversalChoice c = y.value;
                        if (c.false_.isNone()) { bc.addStep("choose", Predicates.toP(c.predicate), nested(c.true_)); }
                        else { bc.addStep("choose", Predicates.toP(c.predicate), nested(c.true_), nested(c.false_.fromGiven())); }
                        return null;
                    }
                });
                return null;
            }

            // option(predicate,traversal | object,traversal | merge forms | traversal)
            @Override public Void visit(TraversalMethod.Option x) {
                x.value.accept(new hydra.tinkerpop.gremlin.OptionArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.OptionArgs.Traversal y) { bc.addStep("option", nested(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.OptionArgs.PredicateTraversal y) { bc.addStep("option", Predicates.toP(y.value.predicate), nested(y.value.traversal)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.OptionArgs.ObjectTraversal y) { bc.addStep("option", literalArg(y.value.object), nested(y.value.traversal)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.OptionArgs.MergeMap y) { throw notYetMapped("step", "option(Merge, map)"); }
                    @Override public Void visit(hydra.tinkerpop.gremlin.OptionArgs.MergeTraversal y) { throw notYetMapped("step", "option(Merge, traversal)"); }
                });
                return null;
            }

            // call(service[, args])
            @Override public Void visit(TraversalMethod.Call x) {
                hydra.tinkerpop.gremlin.ServiceCall sc = x.value;
                List<Object> args = new ArrayList<>();
                args.add(stringArg(sc.service));
                sc.arguments.accept(new hydra.tinkerpop.gremlin.ServiceArguments.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.ServiceArguments.Map y) { if (!y.value.isNone()) { throw notYetMapped("step", "call(service, map)"); } return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ServiceArguments.Traversal y) { if (!y.value.isNone()) { args.add(nested(y.value.fromGiven())); } return null; }
                });
                bc.addStep("call", args.toArray());
                return null;
            }

            // sack([biFunction])
            @Override public Void visit(TraversalMethod.Sack x) {
                if (x.value.isNone()) { bc.addStep("sack"); }
                else { bc.addStep("sack", biFunctionArg(x.value.fromGiven())); }
                return null;
            }

            // by(order | token | comparator/function/string/traversal)
            @Override public Void visit(TraversalMethod.By x) {
                x.value.accept(new hydra.tinkerpop.gremlin.ByArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.ByArgs.Order y) { bc.addStep("by", orderArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ByArgs.Token y) { bc.addStep("by", tokenArg(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.ByArgs.Other y) {
                        y.value.accept(new hydra.tinkerpop.gremlin.ByOtherArgs.Visitor<Void>() {
                            @Override public Void visit(hydra.tinkerpop.gremlin.ByOtherArgs.Comparator z) {
                                if (z.value.isNone()) { bc.addStep("by"); }
                                else { bc.addStep("by", orderArg(z.value.fromGiven())); }
                                return null;
                            }
                            @Override public Void visit(hydra.tinkerpop.gremlin.ByOtherArgs.Function z) { bc.addStep("by", functionArg(z.value)); return null; }
                            @Override public Void visit(hydra.tinkerpop.gremlin.ByOtherArgs.String_ z) { bc.addStep("by", stringArg(z.value)); return null; }
                            @Override public Void visit(hydra.tinkerpop.gremlin.ByOtherArgs.Traversal z) { bc.addStep("by", nested(z.value)); return null; }
                        });
                        return null;
                    }
                });
                return null;
            }

            // property([cardinality,] objects... | map)
            @Override public Void visit(TraversalMethod.Property x) {
                x.value.accept(new hydra.tinkerpop.gremlin.PropertyArgs.Visitor<Void>() {
                    @Override public Void visit(hydra.tinkerpop.gremlin.PropertyArgs.Objects y) { bc.addStep("property", literalArgs(y.value)); return null; }
                    @Override public Void visit(hydra.tinkerpop.gremlin.PropertyArgs.CardinalityObjects y) {
                        List<Object> args = new ArrayList<>();
                        args.add(cardinalityArg(y.value.cardinality));
                        for (GenericLiteralArgument g : y.value.objects) { args.add(literalArg(g)); }
                        bc.addStep("property", args.toArray());
                        return null;
                    }
                    @Override public Void visit(hydra.tinkerpop.gremlin.PropertyArgs.Object_ y) { throw notYetMapped("step", "property(map)"); }
                    @Override public Void visit(hydra.tinkerpop.gremlin.PropertyArgs.CardinalityObject y) { throw notYetMapped("step", "property(cardinality, map)"); }
                });
                return null;
            }

            // with(key[, value]) — provider options. Common forms: with("k", v) and with(WithOptions.x).
            @Override public Void visit(TraversalMethod.With x) {
                hydra.tinkerpop.gremlin.WithArgs w = x.value;
                Object key = withKey(w.keys);
                if (w.values.isNone()) { bc.addStep("with", key); }
                else { bc.addStep("with", key, withValue(w.values.fromGiven())); }
                return null;
            }
        });
    }

    private static Object withKey(hydra.tinkerpop.gremlin.WithArgsKeys k) {
        return k.accept(new hydra.tinkerpop.gremlin.WithArgsKeys.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.WithArgsKeys.String_ y) { return stringArg(y.value); }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithArgsKeys.WithOption y) {
                return y.value.accept(new hydra.tinkerpop.gremlin.WithOptionKeys.PartialVisitor<Object>() {
                    @Override public Object otherwise(hydra.tinkerpop.gremlin.WithOptionKeys instance) {
                        throw notYetMapped("argument", "WithOptionKeys." + instance.getClass().getSimpleName());
                    }
                    @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionKeys.WithOptionsTokens y) { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.tokens; }
                    @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionKeys.WithOptionsIndexer y) { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.indexer; }
                });
            }
        });
    }

    private static Object withValue(hydra.tinkerpop.gremlin.WithArgsValues v) {
        return v.accept(new hydra.tinkerpop.gremlin.WithArgsValues.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.WithArgsValues.Object_ y) { return literalArg(y.value); }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithArgsValues.WithOptions y) { return withOptionsValue(y.value); }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithArgsValues.Io y) { throw notYetMapped("argument", "with(... , IO option)"); }
        });
    }

    private static Object withOptionsValue(hydra.tinkerpop.gremlin.WithOptionsValues v) {
        return v.accept(new hydra.tinkerpop.gremlin.WithOptionsValues.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.Tokens y) { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.tokens; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.None y)   { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.none; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.Ids y)    { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.ids; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.Labels y) { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.labels; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.Keys y)   { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.keys; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.Values y) { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.values; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.All y)    { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.all; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.List y)   { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.list; }
            @Override public Object visit(hydra.tinkerpop.gremlin.WithOptionsValues.Map y)    { return org.apache.tinkerpop.gremlin.process.traversal.step.util.WithOptions.map; }
        });
    }

    private static org.apache.tinkerpop.gremlin.process.traversal.Order orderArg(hydra.tinkerpop.gremlin.TraversalOrderArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalOrderArgument.Visitor<org.apache.tinkerpop.gremlin.process.traversal.Order>() {
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalOrderArgument.Value x) { return order(x.value); }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalOrderArgument.Variable x) { throw notYetMapped("argument", "TraversalOrderArgument.Variable"); }
        });
    }

    private static org.apache.tinkerpop.gremlin.process.traversal.Order orderArg(hydra.tinkerpop.gremlin.TraversalComparatorArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalComparatorArgument.Visitor<org.apache.tinkerpop.gremlin.process.traversal.Order>() {
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalComparatorArgument.Value x) { return order(x.value); }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalComparatorArgument.Variable x) { throw notYetMapped("argument", "TraversalComparatorArgument.Variable"); }
        });
    }

    static org.apache.tinkerpop.gremlin.process.traversal.Order order(hydra.tinkerpop.gremlin.TraversalOrder o) {
        return o.accept(new hydra.tinkerpop.gremlin.TraversalOrder.Visitor<org.apache.tinkerpop.gremlin.process.traversal.Order>() {
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalOrder.Asc x)     { return org.apache.tinkerpop.gremlin.process.traversal.Order.asc; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalOrder.Desc x)    { return org.apache.tinkerpop.gremlin.process.traversal.Order.desc; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Order visit(hydra.tinkerpop.gremlin.TraversalOrder.Shuffle x) { return org.apache.tinkerpop.gremlin.process.traversal.Order.shuffle; }
        });
    }

    /** A {@code TraversalCardinalityArgument} → TinkerPop {@code VertexProperty.Cardinality}. */
    private static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality cardinalityArg(
            hydra.tinkerpop.gremlin.TraversalCardinalityArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalCardinalityArgument.Visitor<org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality>() {
            @Override public org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality visit(hydra.tinkerpop.gremlin.TraversalCardinalityArgument.Value x) {
                return x.value.accept(new hydra.tinkerpop.gremlin.TraversalCardinality.Visitor<org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality>() {
                    @Override public org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality visit(hydra.tinkerpop.gremlin.TraversalCardinality.Single x) { return org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single; }
                    @Override public org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality visit(hydra.tinkerpop.gremlin.TraversalCardinality.Set x)    { return org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.set; }
                    @Override public org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality visit(hydra.tinkerpop.gremlin.TraversalCardinality.List x)   { return org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.list; }
                });
            }
            @Override public org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality visit(hydra.tinkerpop.gremlin.TraversalCardinalityArgument.Variable x) { throw notYetMapped("argument", "TraversalCardinalityArgument.Variable"); }
        });
    }

    /** A {@code TraversalBiFunctionArgument} → a TinkerPop {@code Operator} (used by sack/fold). */
    static Object biFunctionArg(hydra.tinkerpop.gremlin.TraversalBiFunctionArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalBiFunctionArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalBiFunctionArgument.Value x) { return Operators.toGremlin(x.value); }
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalBiFunctionArgument.Variable x) { throw notYetMapped("argument", "TraversalBiFunctionArgument.Variable"); }
        });
    }

    static org.apache.tinkerpop.gremlin.process.traversal.Pop pop(hydra.tinkerpop.gremlin.TraversalPopArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalPopArgument.Visitor<org.apache.tinkerpop.gremlin.process.traversal.Pop>() {
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Pop visit(hydra.tinkerpop.gremlin.TraversalPopArgument.Value x) {
                return x.value.accept(new hydra.tinkerpop.gremlin.TraversalPop.Visitor<org.apache.tinkerpop.gremlin.process.traversal.Pop>() {
                    @Override public org.apache.tinkerpop.gremlin.process.traversal.Pop visit(hydra.tinkerpop.gremlin.TraversalPop.First x) { return org.apache.tinkerpop.gremlin.process.traversal.Pop.first; }
                    @Override public org.apache.tinkerpop.gremlin.process.traversal.Pop visit(hydra.tinkerpop.gremlin.TraversalPop.Last x)  { return org.apache.tinkerpop.gremlin.process.traversal.Pop.last; }
                    @Override public org.apache.tinkerpop.gremlin.process.traversal.Pop visit(hydra.tinkerpop.gremlin.TraversalPop.All x)   { return org.apache.tinkerpop.gremlin.process.traversal.Pop.all; }
                    @Override public org.apache.tinkerpop.gremlin.process.traversal.Pop visit(hydra.tinkerpop.gremlin.TraversalPop.Mixed x) { return org.apache.tinkerpop.gremlin.process.traversal.Pop.mixed; }
                });
            }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Pop visit(hydra.tinkerpop.gremlin.TraversalPopArgument.Variable x) { throw notYetMapped("argument", "TraversalPopArgument.Variable"); }
        });
    }

    static org.apache.tinkerpop.gremlin.structure.Column column(hydra.tinkerpop.gremlin.TraversalColumn c) {
        return c.accept(new hydra.tinkerpop.gremlin.TraversalColumn.Visitor<org.apache.tinkerpop.gremlin.structure.Column>() {
            @Override public org.apache.tinkerpop.gremlin.structure.Column visit(hydra.tinkerpop.gremlin.TraversalColumn.Keys x)   { return org.apache.tinkerpop.gremlin.structure.Column.keys; }
            @Override public org.apache.tinkerpop.gremlin.structure.Column visit(hydra.tinkerpop.gremlin.TraversalColumn.Values x) { return org.apache.tinkerpop.gremlin.structure.Column.values; }
        });
    }

    private static Object columnArg(hydra.tinkerpop.gremlin.TraversalColumnArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalColumnArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalColumnArgument.Value x) { return column(x.value); }
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalColumnArgument.Variable x) { throw notYetMapped("argument", "TraversalColumnArgument.Variable"); }
        });
    }

    /** A {@code TraversalFunction} → a TinkerPop {@code T} or {@code Column} function token. */
    private static Object functionArg(hydra.tinkerpop.gremlin.TraversalFunctionArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalFunctionArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalFunctionArgument.Value x) {
                return x.value.accept(new hydra.tinkerpop.gremlin.TraversalFunction.Visitor<Object>() {
                    @Override public Object visit(hydra.tinkerpop.gremlin.TraversalFunction.Token y) { return token(y.value); }
                    @Override public Object visit(hydra.tinkerpop.gremlin.TraversalFunction.Column y) { return column(y.value); }
                });
            }
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalFunctionArgument.Variable x) { throw notYetMapped("argument", "TraversalFunctionArgument.Variable"); }
        });
    }

    private static Object[] mergeArgs(hydra.tinkerpop.gremlin.MergeArgs m) {
        return m.accept(new hydra.tinkerpop.gremlin.MergeArgs.Visitor<Object[]>() {
            @Override public Object[] visit(hydra.tinkerpop.gremlin.MergeArgs.Map y) {
                // GenericLiteralMapNullableArgument → a Java Map argument (or empty for an absent map).
                return new Object[]{literalArg0(y.value)};
            }
            @Override public Object[] visit(hydra.tinkerpop.gremlin.MergeArgs.Traversal y) {
                return new Object[]{nested(y.value)};
            }
        });
    }

    private static Object booleanArg(hydra.tinkerpop.gremlin.BooleanArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.BooleanArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.BooleanArgument.Value x) { return x.value; }
            @Override public Object visit(hydra.tinkerpop.gremlin.BooleanArgument.Variable x) { throw notYetMapped("argument", "BooleanArgument.Variable"); }
        });
    }

    private static Object literalArg0(hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument.Value x) {
                // The map literal is optional; map unpacking into a java.util.Map is a TODO — pass through.
                throw notYetMapped("argument", "GenericLiteralMapNullableArgument value (map literal)");
            }
            @Override public Object visit(hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument.Variable x) { throw notYetMapped("argument", "GenericLiteralMapNullableArgument.Variable"); }
        });
    }

    /** Builds {@code [key, clause...]} for a has(...) step. */
    private static Object[] prependKey(Object key, Object[] clause) {
        Object[] out = new Object[1 + clause.length];
        out[0] = key;
        System.arraycopy(clause, 0, out, 1, clause.length);
        return out;
    }

    /** A {@code HasValueClause} → the trailing argument(s) after the key/token of a has(...) step. */
    private static Object[] hasValueClause(hydra.tinkerpop.gremlin.HasValueClause c) {
        return c.accept(new hydra.tinkerpop.gremlin.HasValueClause.Visitor<Object[]>() {
            @Override public Object[] visit(hydra.tinkerpop.gremlin.HasValueClause.Object_ y) {
                return new Object[]{literalArg(y.value)};
            }
            @Override public Object[] visit(hydra.tinkerpop.gremlin.HasValueClause.Predicate y) {
                return new Object[]{Predicates.toP(y.value)};
            }
            @Override public Object[] visit(hydra.tinkerpop.gremlin.HasValueClause.Traversal y) {
                return new Object[]{nested(y.value)};
            }
            @Override public Object[] visit(hydra.tinkerpop.gremlin.HasValueClause.KeyObject y) {
                // has(label, key, object): the "key" already emitted is the label; here key+object follow.
                return new Object[]{stringNullableArg(y.value.key), literalArg(y.value.object)};
            }
            @Override public Object[] visit(hydra.tinkerpop.gremlin.HasValueClause.KeyPredicate y) {
                return new Object[]{stringNullableArg(y.value.key), Predicates.toP(y.value.predicate)};
            }
        });
    }

    /** A {@code TraversalTokenArgument} → TinkerPop {@code T}. */
    static org.apache.tinkerpop.gremlin.structure.T tokenArg(hydra.tinkerpop.gremlin.TraversalTokenArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalTokenArgument.Visitor<org.apache.tinkerpop.gremlin.structure.T>() {
            @Override public org.apache.tinkerpop.gremlin.structure.T visit(hydra.tinkerpop.gremlin.TraversalTokenArgument.Value x) { return token(x.value); }
            @Override public org.apache.tinkerpop.gremlin.structure.T visit(hydra.tinkerpop.gremlin.TraversalTokenArgument.Variable x) { throw notYetMapped("argument", "TraversalTokenArgument.Variable"); }
        });
    }

    static org.apache.tinkerpop.gremlin.structure.T token(hydra.tinkerpop.gremlin.TraversalToken t) {
        return t.accept(new hydra.tinkerpop.gremlin.TraversalToken.Visitor<org.apache.tinkerpop.gremlin.structure.T>() {
            @Override public org.apache.tinkerpop.gremlin.structure.T visit(hydra.tinkerpop.gremlin.TraversalToken.Id x)    { return org.apache.tinkerpop.gremlin.structure.T.id; }
            @Override public org.apache.tinkerpop.gremlin.structure.T visit(hydra.tinkerpop.gremlin.TraversalToken.Label x) { return org.apache.tinkerpop.gremlin.structure.T.label; }
            @Override public org.apache.tinkerpop.gremlin.structure.T visit(hydra.tinkerpop.gremlin.TraversalToken.Key x)   { return org.apache.tinkerpop.gremlin.structure.T.key; }
            @Override public org.apache.tinkerpop.gremlin.structure.T visit(hydra.tinkerpop.gremlin.TraversalToken.Value x) { return org.apache.tinkerpop.gremlin.structure.T.value; }
        });
    }

    // -- Helpers for the *Args batch -----------------------------------------------------------

    private static Object dtArg(hydra.tinkerpop.gremlin.TraversalDTArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalDTArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalDTArgument.Value x) { return Dts.toGremlin(x.value); }
            @Override public Object visit(hydra.tinkerpop.gremlin.TraversalDTArgument.Variable x) { throw notYetMapped("argument", "TraversalDTArgument.Variable"); }
        });
    }

    private static Object dateArg(hydra.tinkerpop.gremlin.DateArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.DateArgument.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.DateArgument.Value x) {
                // DateLiteral wraps an optional string; pass the string through (date parsing is a TODO).
                return x.value.value.isNone() ? null : stringArg(x.value.value.fromGiven());
            }
            @Override public Object visit(hydra.tinkerpop.gremlin.DateArgument.Variable x) { throw notYetMapped("argument", "DateArgument.Variable"); }
        });
    }

    private static org.apache.tinkerpop.gremlin.structure.Direction directionArg(hydra.tinkerpop.gremlin.TraversalDirectionArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalDirectionArgument.Visitor<org.apache.tinkerpop.gremlin.structure.Direction>() {
            @Override public org.apache.tinkerpop.gremlin.structure.Direction visit(hydra.tinkerpop.gremlin.TraversalDirectionArgument.Value x) { return Directions.toGremlin(x.value); }
            @Override public org.apache.tinkerpop.gremlin.structure.Direction visit(hydra.tinkerpop.gremlin.TraversalDirectionArgument.Variable x) { throw notYetMapped("argument", "TraversalDirectionArgument.Variable"); }
        });
    }

    private static Object[] directionAndVarargs(hydra.tinkerpop.gremlin.DirectionAndVarargs d) {
        List<Object> out = new ArrayList<>(1 + d.varargs.size());
        out.add(directionArg(d.direction));
        for (StringNullableArgument s : d.varargs) { out.add(stringNullableArg(s)); }
        return out.toArray();
    }

    // -- Helpers for the batch above -----------------------------------------------------------

    /** A {@code NestedTraversal} → a child {@link Bytecode} (the TinkerPop nested-traversal arg form). */
    static Bytecode nested(hydra.tinkerpop.gremlin.NestedTraversal nt) {
        return nt.accept(new hydra.tinkerpop.gremlin.NestedTraversal.Visitor<Bytecode>() {
            @Override
            public Bytecode visit(hydra.tinkerpop.gremlin.NestedTraversal.Root x) {
                return toBytecode(x.value);
            }

            @Override
            public Bytecode visit(hydra.tinkerpop.gremlin.NestedTraversal.Chained x) {
                return chainedBytecode(x.value);
            }

            @Override
            public Bytecode visit(hydra.tinkerpop.gremlin.NestedTraversal.Anonymous x) {
                return chainedBytecode(x.value);
            }
        });
    }

    private static Bytecode chainedBytecode(hydra.tinkerpop.gremlin.ChainedTraversal ct) {
        Bytecode child = new Bytecode();
        for (TraversalMethod m : ct.value) {
            addStep(child, m);
        }
        return child;
    }

    private static Object[] nestedList(List<hydra.tinkerpop.gremlin.NestedTraversal> nts) {
        List<Object> out = new ArrayList<>(nts.size());
        for (hydra.tinkerpop.gremlin.NestedTraversal nt : nts) {
            out.add(nested(nt));
        }
        return out.toArray();
    }

    private static void addStringOptional(
            Bytecode bc, String name, hydra.overlay.java.util.Optional<StringArgument> s) {
        if (s.isNone()) {
            bc.addStep(name);
        } else {
            bc.addStep(name, stringArg(s.fromGiven()));
        }
    }

    private static Object[] predicateOrTraversal(hydra.tinkerpop.gremlin.PredicateOrTraversal p) {
        return p.accept(new hydra.tinkerpop.gremlin.PredicateOrTraversal.Visitor<Object[]>() {
            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrTraversal.Predicate x) {
                return new Object[]{Predicates.toP(x.value)};
            }

            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrTraversal.Traversal x) {
                return new Object[]{nested(x.value)};
            }
        });
    }

    private static Object numericArg(hydra.tinkerpop.gremlin.NumericArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.NumericArgument.Visitor<Object>() {
            @Override
            public Object visit(hydra.tinkerpop.gremlin.NumericArgument.Value x) {
                return numeric(x.value);
            }

            @Override
            public Object visit(hydra.tinkerpop.gremlin.NumericArgument.Variable x) {
                throw notYetMapped("argument", "NumericArgument.Variable (" + x.value.value + ")");
            }
        });
    }

    private static Object gtypeArg(hydra.tinkerpop.gremlin.TraversalGTypeArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalGTypeArgument.Visitor<Object>() {
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGTypeArgument.Value x) {
                return Gtypes.toGremlin(x.value);
            }

            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGTypeArgument.Variable x) {
                throw notYetMapped("argument", "TraversalGTypeArgument.Variable (" + x.value.value + ")");
            }
        });
    }

    private static Object stringOrNested(hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal s) {
        return s.accept(new hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal.Visitor<Object>() {
            @Override
            public Object visit(hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal.String_ x) {
                return stringArg(x.value);
            }

            @Override
            public Object visit(hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal.Traversal x) {
                return nested(x.value);
            }
        });
    }

    /** A {@code PredicateOrObject} (is) → one arg: a {@code P}, or the boxed value. */
    private static Object[] predicateOrObject(hydra.tinkerpop.gremlin.PredicateOrObject p) {
        return p.accept(new hydra.tinkerpop.gremlin.PredicateOrObject.Visitor<Object[]>() {
            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrObject.Predicate x) {
                return new Object[]{Predicates.toP(x.value)};
            }

            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrObject.Object_ x) {
                return new Object[]{literalArg(x.value)};
            }
        });
    }

    /** A {@code PredicateOrStrings} (hasLabel/hasKey) → a {@code P}, or string varargs. */
    private static Object[] predicateOrStrings(hydra.tinkerpop.gremlin.PredicateOrStrings p) {
        return p.accept(new hydra.tinkerpop.gremlin.PredicateOrStrings.Visitor<Object[]>() {
            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrStrings.Predicate x) {
                return new Object[]{Predicates.toP(x.value)};
            }

            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrStrings.Strings x) {
                return stringNullableArgs(x.value);
            }
        });
    }

    /** A {@code PredicateOrObjects} (hasId/hasValue) → a {@code P}, or value varargs. */
    private static Object[] predicateOrObjects(hydra.tinkerpop.gremlin.PredicateOrObjects p) {
        return p.accept(new hydra.tinkerpop.gremlin.PredicateOrObjects.Visitor<Object[]>() {
            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrObjects.Predicate x) {
                return new Object[]{Predicates.toP(x.value)};
            }

            @Override
            public Object[] visit(hydra.tinkerpop.gremlin.PredicateOrObjects.Objects x) {
                return literalArgs(x.value);
            }
        });
    }

    /** Flattens an {@code AsArgs} (first label + rest labels) to a {@code String[]} for as/cap/project. */
    private static Object[] asArgs(hydra.tinkerpop.gremlin.AsArgs args) {
        List<Object> out = new ArrayList<>(1 + args.rest.size());
        out.add(stringArg(args.first));
        for (StringNullableArgument s : args.rest) {
            out.add(stringNullableArg(s));
        }
        return out.toArray();
    }

    /** Emits a step with an optional scope and a required integer: addStep(name, [scope,] long). */
    private static void addScopeAndInteger(Bytecode bc, String name, hydra.tinkerpop.gremlin.ScopeAndInteger sai) {
        long n = integerArgLong(sai.integer);
        if (sai.scope.isNone()) {
            bc.addStep(name, n);
        } else {
            bc.addStep(name, scopeArg(sai.scope.fromGiven()), n);
        }
    }

    /** Maps an {@code IntegerArgument} to a {@code long} (the value form; variable is a TODO). */
    /**
     * Maps an {@code IntegerArgument} to a boxed Java integer, narrowed to {@code Integer} when it fits
     * (else {@code Long}). The narrowing matches what TinkerPop's parser produces for small integer
     * literals (e.g. {@code times(2)} yields an {@code int}), so forward/reverse round-trips compare
     * equal under {@code Bytecode.equals} (where {@code Integer(2) != Long(2)}).
     */
    static Object integerArg(hydra.tinkerpop.gremlin.IntegerArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.IntegerArgument.Visitor<Object>() {
            @Override
            public Object visit(hydra.tinkerpop.gremlin.IntegerArgument.Value x) {
                return integerLiteral(x.value);  // boxed by the literal's precision tag
            }

            @Override
            public Object visit(hydra.tinkerpop.gremlin.IntegerArgument.Variable x) {
                throw notYetMapped("argument", "IntegerArgument.Variable");
            }
        });
    }

    /**
     * Long-coerced integer argument, for steps whose TinkerPop signature takes {@code long}
     * (limit/range/tail/skip/timeLimit) — the parser coerces the literal to Long regardless of its
     * written width, so the forward mapper must too for byte-exact round-trips.
     */
    static long integerArgLong(hydra.tinkerpop.gremlin.IntegerArgument a) {
        return ((Number) integerArg(a)).longValue();
    }

    /**
     * Emits a step that takes an optional {@code TraversalScope}: {@code addStep(name)} when absent, or
     * {@code addStep(name, scope)} when present. The scope maps to the TinkerPop {@code Scope} enum.
     */
    private static void addScopeOptional(
            Bytecode bc, String name,
            hydra.overlay.java.util.Optional<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope) {
        if (scope.isNone()) {
            bc.addStep(name);
        } else {
            bc.addStep(name, scopeArg(scope.fromGiven()));
        }
    }

    /** Maps a Hydra {@code TraversalScopeArgument} to the TinkerPop {@code Scope} enum constant. */
    static org.apache.tinkerpop.gremlin.process.traversal.Scope scopeArg(
            hydra.tinkerpop.gremlin.TraversalScopeArgument a) {
        return a.accept(new hydra.tinkerpop.gremlin.TraversalScopeArgument.Visitor<
                org.apache.tinkerpop.gremlin.process.traversal.Scope>() {
            @Override
            public org.apache.tinkerpop.gremlin.process.traversal.Scope visit(
                    hydra.tinkerpop.gremlin.TraversalScopeArgument.Value x) {
                return x.value.accept(new hydra.tinkerpop.gremlin.TraversalScope.Visitor<
                        org.apache.tinkerpop.gremlin.process.traversal.Scope>() {
                    @Override
                    public org.apache.tinkerpop.gremlin.process.traversal.Scope visit(
                            hydra.tinkerpop.gremlin.TraversalScope.Local x) {
                        return org.apache.tinkerpop.gremlin.process.traversal.Scope.local;
                    }

                    @Override
                    public org.apache.tinkerpop.gremlin.process.traversal.Scope visit(
                            hydra.tinkerpop.gremlin.TraversalScope.Global x) {
                        return org.apache.tinkerpop.gremlin.process.traversal.Scope.global;
                    }
                });
            }

            @Override
            public org.apache.tinkerpop.gremlin.process.traversal.Scope visit(
                    hydra.tinkerpop.gremlin.TraversalScopeArgument.Variable x) {
                throw notYetMapped("argument", "TraversalScopeArgument.Variable (" + x.value.value + ")");
            }
        });
    }

    // -- Argument mappers ----------------------------------------------------------------------

    /** Maps a list of {@link GenericLiteralArgument} to a flat {@code Object[]} of boxed Java values. */
    static Object[] literalArgs(List<GenericLiteralArgument> args) {
        List<Object> out = new ArrayList<>(args.size());
        for (GenericLiteralArgument a : args) {
            out.add(literalArg(a));
        }
        return out.toArray();
    }

    /** Maps a single value-or-variable generic-literal argument to a boxed Java value. */
    static Object literalArg(GenericLiteralArgument a) {
        return a.accept(new GenericLiteralArgument.Visitor<Object>() {
            @Override
            public Object visit(GenericLiteralArgument.Value x) {
                return literal(x.value);
            }

            @Override
            public Object visit(GenericLiteralArgument.Variable x) {
                // Bound variables map to traversal parameters. The vast majority of generated args are
                // literals; variable support (binding a Var) is a v2 addition. Fail loudly rather than
                // silently producing a wrong literal.
                throw notYetMapped("argument", "GenericLiteralArgument.Variable (" + x.value.value + ")");
            }
        });
    }

    /** Maps a Hydra {@link GenericLiteral} to a boxed Java value (common scalar cases). */
    static Object literal(GenericLiteral lit) {
        return lit.accept(new GenericLiteral.PartialVisitor<Object>() {
            @Override
            public Object otherwise(GenericLiteral instance) {
                throw notYetMapped("literal", instance.getClass().getSimpleName());
            }

            @Override
            public Object visit(GenericLiteral.String_ x) {
                return x.value;
            }

            @Override
            public Object visit(GenericLiteral.Boolean_ x) {
                return x.value;
            }

            @Override
            public Object visit(GenericLiteral.Numeric x) {
                return numeric(x.value);
            }

            @Override
            public Object visit(GenericLiteral.Null x) {
                return null;
            }
        });
    }

    /** Maps a {@link NumericLiteral} to a boxed Java number. */
    static Object numeric(NumericLiteral n) {
        return n.accept(new NumericLiteral.Visitor<Object>() {
            @Override
            public Object visit(NumericLiteral.Integer_ x) {
                return integerLiteral(x.value);
            }

            @Override
            public Object visit(NumericLiteral.Float_ x) {
                return floatLiteral(x.value);
            }
        });
    }

    /** A Gremlin {@code IntegerLiteral} union → the boxed Java type matching its written precision tag. */
    static Object integerLiteral(hydra.tinkerpop.gremlin.IntegerLiteral lit) {
        return lit.accept(new hydra.tinkerpop.gremlin.IntegerLiteral.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.IntegerLiteral.Byte_ x)  { return x.value; }  // Byte
            @Override public Object visit(hydra.tinkerpop.gremlin.IntegerLiteral.Short_ x) { return x.value; }  // Short
            @Override public Object visit(hydra.tinkerpop.gremlin.IntegerLiteral.Int x)    { return x.value; }  // Integer
            @Override public Object visit(hydra.tinkerpop.gremlin.IntegerLiteral.Long_ x)  { return x.value; }  // Long
            @Override public Object visit(hydra.tinkerpop.gremlin.IntegerLiteral.Big x)    { return x.value; }  // BigInteger
        });
    }

    /** A Gremlin {@code FloatLiteral} union → the boxed Java type matching its written precision tag. */
    static Object floatLiteral(hydra.tinkerpop.gremlin.FloatLiteral lit) {
        return lit.accept(new hydra.tinkerpop.gremlin.FloatLiteral.Visitor<Object>() {
            @Override public Object visit(hydra.tinkerpop.gremlin.FloatLiteral.Float_ x)  { return x.value; }  // Float
            @Override public Object visit(hydra.tinkerpop.gremlin.FloatLiteral.Double_ x) { return x.value; }  // Double
            @Override public Object visit(hydra.tinkerpop.gremlin.FloatLiteral.Big x)     { return x.value; }  // BigDecimal
        });
    }

    /** Maps a list of nullable-string arguments (e.g. edge labels for out/in/both) to {@code String[]}. */
    static Object[] stringNullableArgs(List<StringNullableArgument> args) {
        List<Object> out = new ArrayList<>(args.size());
        for (StringNullableArgument a : args) {
            out.add(stringNullableArg(a));
        }
        return out.toArray();
    }

    static Object stringNullableArg(StringNullableArgument a) {
        return a.accept(new StringNullableArgument.Visitor<Object>() {
            @Override
            public Object visit(StringNullableArgument.Value x) {
                // Optional<String>: present → the string; absent → null (Gremlin label varargs allow it).
                return x.value.orElse(null);
            }

            @Override
            public Object visit(StringNullableArgument.Variable x) {
                throw notYetMapped("argument", "StringNullableArgument.Variable (" + x.value.value + ")");
            }
        });
    }

    static String stringArg(StringArgument a) {
        return a.accept(new StringArgument.Visitor<String>() {
            @Override
            public String visit(StringArgument.Value x) {
                return x.value;
            }

            @Override
            public String visit(StringArgument.Variable x) {
                throw notYetMapped("argument", "StringArgument.Variable (" + x.value.value + ")");
            }
        });
    }

    private static UnsupportedOperationException notYetMapped(String kind, String what) {
        return new UnsupportedOperationException(
                "Hydra→Bytecode mapping not yet implemented for " + kind + ": " + what);
    }
}
