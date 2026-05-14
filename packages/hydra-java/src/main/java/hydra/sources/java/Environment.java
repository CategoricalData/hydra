package hydra.sources.java;

import hydra.dsl.Types;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.Namespace;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;

import static hydra.dsl.java.Helpers.doc;
import static hydra.dsl.java.Helpers.typeDef;
import static hydra.dsl.java.Helpers.typeref;

/**
 * Environment types for Java code generation.
 *
 * <p>Mirror of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Environment.hs}.</p>
 */
public class Environment {
    public static final Namespace NS = new Namespace("hydra.java.environment");

    // Reference helpers, mirroring Haskell {@code environment}, {@code syntax}, etc.
    private static final Namespace SYNTAX_NS = new Namespace("hydra.java.syntax");
    private static final Namespace CORE_NS = new Namespace("hydra.core");
    private static final Namespace GRAPH_NS = new Namespace("hydra.graph");
    private static final Namespace MODULE_NS = new Namespace("hydra.packaging");
    private static final Namespace TYPING_NS = new Namespace("hydra.typing");

    private static hydra.core.Type env(String local) { return typeref(NS, local); }
    private static hydra.core.Type syntax(String local) { return typeref(SYNTAX_NS, local); }
    private static hydra.core.Type core(String local) { return typeref(CORE_NS, local); }
    private static hydra.core.Type graph(String local) { return typeref(GRAPH_NS, local); }
    private static hydra.core.Type modul(String local) { return typeref(MODULE_NS, local); }
    private static hydra.core.Type typing(String local) { return typeref(TYPING_NS, local); }

    /** Classification of a Java symbol for code generation. */
    private static Definition javaSymbolClass() {
        return typeDef(NS, "JavaSymbolClass",
            doc("Classification of a Java symbol for code generation",
                Types.union(
                    Types.field("constant", doc("A constant value", Types.unit())),
                    Types.field("nullaryFunction", doc("A nullary function (no arguments)", Types.unit())),
                    Types.field("hoistedLambda", doc("A hoisted lambda wrapped in type lambdas. The Int is the number of curried lambda parameters.", Types.int32())),
                    Types.field("unaryFunction", doc("A unary function (single argument)", Types.unit())),
                    Types.field("localVariable", doc("A local variable", Types.unit())))));
    }

    /** Feature flags for the target Java version. */
    private static Definition javaFeatures() {
        return typeDef(NS, "JavaFeatures",
            doc("Feature flags for the target Java version",
                Types.record(
                    Types.field("supportsDiamondOperator",
                        doc("Whether the diamond operator (<>) is supported (Java 7+)", Types.boolean_())))));
    }

    /** Aliases and context for Java code generation. */
    private static Definition aliases() {
        return typeDef(NS, "Aliases",
            doc("Aliases and context for Java code generation",
                Types.record(
                    Types.field("currentNamespace", doc("Current module namespace context", modul("Namespace"))),
                    Types.field("packages", doc("Maps namespaces to Java package names",
                        Types.map(modul("Namespace"), syntax("PackageName")))),
                    Types.field("branchVars", doc("Variables bound in pattern matching branches",
                        Types.set(core("Name")))),
                    Types.field("recursiveVars", doc("Variables that are self-recursive",
                        Types.set(core("Name")))),
                    Types.field("inScopeTypeParams", doc("Type parameters that are in scope (from method-level type parameters)",
                        Types.set(core("Name")))),
                    Types.field("polymorphicLocals", doc("Local variables that have polymorphic types (declared with raw types)",
                        Types.set(core("Name")))),
                    Types.field("inScopeJavaVars", doc("All in-scope Java variable names (for avoiding lambda parameter shadowing)",
                        Types.set(core("Name")))),
                    Types.field("varRenames", doc("Variable renames for avoiding shadowing (maps Hydra name to Java name)",
                        Types.map(core("Name"), core("Name")))),
                    Types.field("lambdaVars", doc("Lambda-bound variables (including hoisted captures with qualified names)",
                        Types.set(core("Name")))),
                    Types.field("typeVarSubst", doc("Type variable substitution: maps fresh inference variable names to canonical scheme variable names",
                        Types.map(core("Name"), core("Name")))),
                    Types.field("trustedTypeVars", doc("Type variables that actually appear in the method's formal parameter types",
                        Types.set(core("Name")))),
                    Types.field("methodCodomain", doc("The enclosing method's codomain (return type), used for casting pair expressions",
                        Types.optional(core("Type")))),
                    Types.field("thunkedVars", doc("Variables that have been thunked (wrapped in Supplier) for lazy evaluation",
                        Types.set(core("Name")))))));
    }

    /** Environment for Java code generation. */
    private static Definition javaEnvironment() {
        return typeDef(NS, "JavaEnvironment",
            doc("Environment for Java code generation",
                Types.record(
                    Types.field("aliases", doc("Aliases and context state", env("Aliases"))),
                    Types.field("graph", doc("Graph context for type inference", graph("Graph"))))));
    }

    private static final List<Definition> DEFINITIONS = Arrays.asList(
        javaSymbolClass(),
        javaFeatures(),
        aliases(),
        javaEnvironment());

    private static final List<Namespace> DEPENDENCIES = Arrays.asList(
        SYNTAX_NS, CORE_NS, GRAPH_NS, MODULE_NS, TYPING_NS);

    public static final Module module_ = new Module(
        Maybe.just("Environment types for Java code generation"),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
