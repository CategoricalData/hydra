package hydra;

import hydra.Annotations;
import hydra.Codegen;
import hydra.Sorting;
import hydra.core.Binding;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;

import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.json.model.Value;
import hydra.lib.Libraries;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.TermDefinition;
import hydra.packaging.ModuleName;
import hydra.Rewriting;
import hydra.tools.PrimitiveFunction;
import hydra.util.Either;
import hydra.util.Maybe;
import hydra.util.Pair;
import java.util.HashSet;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;


/**
 * I/O wrapper for Hydra code generation in Java.
 * Provides file I/O around the pure Either-based functions in Codegen.
 */
public class Generation {

    /**
     * Create an empty graph with standard primitives (the bootstrap graph).
     */
    public static Graph bootstrapGraph() {
        Map<Name, Term> boundTerms = new HashMap<>();
        Map<Name, TypeScheme> boundTypes = new HashMap<>();
        Map<Name, hydra.core.TypeVariableMetadata> classConstraints = new HashMap<>();
        java.util.Set<Name> lambdaVariables = new HashSet<>();
        Map<Name, Term> metadata = new HashMap<>();

        Map<Name, Primitive> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        Map<Name, TypeScheme> schemaTypes = new HashMap<>();
        java.util.Set<Name> typeVariables = new HashSet<>();

        return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
    }

    /**
     * Parse a JSON file into a Hydra JSON value.
     * Uses a simple hand-written recursive descent parser (no external dependencies).
     */
    public static Value parseJsonFile(String path) throws IOException {
        String jsonStr = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
        return new SimpleJsonParser(jsonStr).parseValue();
    }

    /**
     * A simple recursive descent JSON parser that produces hydra.json.model.Value objects.
     */
    private static class SimpleJsonParser {
        private final String input;
        private int pos;

        SimpleJsonParser(String input) {
            this.input = input;
            this.pos = 0;
        }

        Value parseValue() {
            skipWhitespace();
            if (pos >= input.length()) throw new RuntimeException("Unexpected end of JSON");
            char c = input.charAt(pos);
            if (c == '{') return parseObject();
            if (c == '[') return parseArray();
            if (c == '"') return parseString();
            if (c == 't' || c == 'f') return parseBoolean();
            if (c == 'n') return parseNull();
            if (c == '-' || (c >= '0' && c <= '9')) return parseNumber();
            throw new RuntimeException("Unexpected character '" + c + "' at position " + pos);
        }

        private Value parseObject() {
            expect('{');
            skipWhitespace();
            Map<String, Value> map = new java.util.TreeMap<>();
            if (pos < input.length() && input.charAt(pos) != '}') {
                parseKeyValue(map);
                while (pos < input.length() && input.charAt(pos) == ',') {
                    pos++;
                    parseKeyValue(map);
                }
            }
            expect('}');
            return new Value.Object_(map);
        }

        private void parseKeyValue(Map<String, Value> map) {
            skipWhitespace();
            String key = parseRawString();
            skipWhitespace();
            expect(':');
            Value val = parseValue();
            map.put(key, val);
        }

        private Value parseArray() {
            expect('[');
            skipWhitespace();
            List<Value> list = new ArrayList<>();
            if (pos < input.length() && input.charAt(pos) != ']') {
                list.add(parseValue());
                while (pos < input.length() && input.charAt(pos) == ',') {
                    pos++;
                    list.add(parseValue());
                }
            }
            expect(']');
            return new Value.Array(list);
        }

        private Value parseString() {
            return new Value.String_(parseRawString());
        }

        private String parseRawString() {
            expect('"');
            StringBuilder sb = new StringBuilder();
            while (pos < input.length() && input.charAt(pos) != '"') {
                char c = input.charAt(pos);
                if (c == '\\') {
                    pos++;
                    if (pos >= input.length()) throw new RuntimeException("Unexpected end of string escape");
                    char escaped = input.charAt(pos);
                    switch (escaped) {
                        case '"': sb.append('"'); break;
                        case '\\': sb.append('\\'); break;
                        case '/': sb.append('/'); break;
                        case 'b': sb.append('\b'); break;
                        case 'f': sb.append('\f'); break;
                        case 'n': sb.append('\n'); break;
                        case 'r': sb.append('\r'); break;
                        case 't': sb.append('\t'); break;
                        case 'u':
                            pos++;
                            if (pos + 4 > input.length()) throw new RuntimeException("Invalid unicode escape");
                            int codePoint = Integer.parseInt(input.substring(pos, pos + 4), 16);
                            sb.append((char) codePoint);
                            pos += 3; // will be incremented below
                            break;
                        default: sb.append(escaped);
                    }
                } else {
                    sb.append(c);
                }
                pos++;
            }
            expect('"');
            return sb.toString();
        }

        private Value parseNumber() {
            int start = pos;
            if (pos < input.length() && input.charAt(pos) == '-') pos++;
            while (pos < input.length() && input.charAt(pos) >= '0' && input.charAt(pos) <= '9') pos++;
            if (pos < input.length() && input.charAt(pos) == '.') {
                pos++;
                while (pos < input.length() && input.charAt(pos) >= '0' && input.charAt(pos) <= '9') pos++;
            }
            if (pos < input.length() && (input.charAt(pos) == 'e' || input.charAt(pos) == 'E')) {
                pos++;
                if (pos < input.length() && (input.charAt(pos) == '+' || input.charAt(pos) == '-')) pos++;
                while (pos < input.length() && input.charAt(pos) >= '0' && input.charAt(pos) <= '9') pos++;
            }
            String numStr = input.substring(start, pos);
            return new Value.Number_(new java.math.BigDecimal(numStr));
        }

        private Value parseBoolean() {
            if (input.startsWith("true", pos)) {
                pos += 4;
                return new Value.Boolean_(true);
            }
            if (input.startsWith("false", pos)) {
                pos += 5;
                return new Value.Boolean_(false);
            }
            throw new RuntimeException("Expected boolean at position " + pos);
        }

        private Value parseNull() {
            if (input.startsWith("null", pos)) {
                pos += 4;
                return new Value.Null();
            }
            throw new RuntimeException("Expected null at position " + pos);
        }

        private void skipWhitespace() {
            while (pos < input.length()) {
                char c = input.charAt(pos);
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                    pos++;
                } else {
                    break;
                }
            }
        }

        private void expect(char c) {
            skipWhitespace();
            if (pos >= input.length() || input.charAt(pos) != c) {
                throw new RuntimeException("Expected '" + c + "' at position " + pos);
            }
            pos++;
        }
    }

    /**
     * Decode a single module from a JSON value using the generated schema-based decoder.
     * Requires a non-empty universe of modules for type resolution.
     */
    public static Module decodeModuleFromJson(Graph bsGraph, List<Module> universeModules,
            Value jsonVal) {
        Either<hydra.errors.Error_, Module> result = Codegen.decodeModuleFromJson(
                bsGraph, universeModules, jsonVal);
        return result.accept(new Either.Visitor<hydra.errors.Error_, Module, Module>() {
            @Override
            public Module visit(Either.Left<hydra.errors.Error_, Module> instance) {
                throw new RuntimeException("Module decode error: " + hydra.show.Errors.error(instance.value));
            }

            @Override
            public Module visit(Either.Right<hydra.errors.Error_, Module> instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode a single module from a JSON value using a pre-built schema map (Name → Type).
     * This avoids the Module → Graph → SchemaMap roundtrip, using the bootstrap type map directly.
     */
    public static Module decodeModuleFromJson(Graph bsGraph, Map<Name, hydra.core.Type> schemaMap,
            Value jsonVal) {
        Name modName = new Name("hydra.packaging.Module");
        hydra.core.Type modType = new hydra.core.Type.Variable(modName);
        Either<String, hydra.core.Term> jsonResult = hydra.json.Decode.fromJson(schemaMap, modName, modType, jsonVal);
        return jsonResult.accept(new Either.Visitor<String, hydra.core.Term, Module>() {
            @Override
            public Module visit(Either.Left<String, hydra.core.Term> instance) {
                throw new RuntimeException("JSON decode error: " + instance.value);
            }

            @Override
            public Module visit(Either.Right<String, hydra.core.Term> instance) {
                hydra.core.Term term = instance.value;
                Either<hydra.errors.DecodingError, Module> modResult =
                    hydra.decode.Packaging.module(bsGraph, term);
                return modResult.accept(new Either.Visitor<hydra.errors.DecodingError, Module, Module>() {
                    @Override
                    public Module visit(Either.Left<hydra.errors.DecodingError, Module> left) {
                        throw new RuntimeException("Module decode error: " + left.value.value);
                    }

                    @Override
                    public Module visit(Either.Right<hydra.errors.DecodingError, Module> right) {
                        return right.value;
                    }
                });
            }
        });
    }

    /**
     * Build a schema map suitable for the JSON decoder from the bootstrap type map.
     * Converts each System F type (with foralls and annotations) to a TypeScheme,
     * then extracts and recursively deannotates the body type for JSON decoding.
     */
    public static Map<Name, hydra.core.Type> bootstrapSchemaMap() {
        Map<Name, hydra.core.Type> raw = hydra.json.Bootstrap.typesByName();
        Map<Name, hydra.core.Type> result = new HashMap<>();
        for (Map.Entry<Name, hydra.core.Type> entry : raw.entrySet()) {
            hydra.core.TypeScheme ts = hydra.Scoping.fTypeToTypeScheme(entry.getValue());
            result.put(entry.getKey(), Strip.deannotateTypeRecursive(ts.body));
        }
        return result;
    }

    /**
     * Build a TypeScheme map from the bootstrap type map.
     * Converts each System F type (with foralls) to a TypeScheme.
     */
    public static Map<Name, hydra.core.TypeScheme> bootstrapTypeSchemes() {
        Map<Name, hydra.core.Type> raw = hydra.json.Bootstrap.typesByName();
        Map<Name, hydra.core.TypeScheme> result = new HashMap<>();
        for (Map.Entry<Name, hydra.core.Type> entry : raw.entrySet()) {
            result.put(entry.getKey(), hydra.Scoping.fTypeToTypeScheme(entry.getValue()));
        }
        return result;
    }

    /**
     * Load modules from JSON files using a pre-built schema map (from Bootstrap.typesByName()).
     */
    public static List<Module> loadModulesFromJson(String basePath,
            Map<Name, hydra.core.Type> schemaMap, List<ModuleName> namespaces) throws IOException {
        Graph bsGraph = bootstrapGraph();
        List<Module> modules = new ArrayList<>();
        for (ModuleName ns : namespaces) {
            String filePath = basePath + File.separator
                    + Codegen.namespaceToPath(ns) + ".json";
            Value jsonVal = parseJsonFile(filePath);
            Module mod = decodeModuleFromJson(bsGraph, schemaMap, jsonVal);
            System.out.println("  Loaded: " + ns.value);
            modules.add(mod);
        }
        return modules;
    }


    private static Map<String, Value> expectObject(Value val, String context) {
        if (val instanceof Value.Object_) {
            return ((Value.Object_) val).value;
        }
        throw new RuntimeException("Expected JSON object for " + context + ", got " + val.getClass().getSimpleName());
    }

    private static List<Value> expectArray(Value val, String context) {
        if (val instanceof Value.Array) {
            return ((Value.Array) val).value;
        }
        throw new RuntimeException("Expected JSON array for " + context + ", got " + val.getClass().getSimpleName());
    }

    private static String expectString(Value val, String context) {
        if (val instanceof Value.String_) {
            return ((Value.String_) val).value;
        }
        throw new RuntimeException("Expected JSON string for " + context + ", got " + val.getClass().getSimpleName());
    }

    /**
     * Read the manifest.json file from a JSON base directory and extract a named
     * field (e.g. "kernelModules", "mainModules", "testModules") as a list of Namespaces.
     */
    public static List<ModuleName> readManifestField(String basePath, String fieldName) throws IOException {
        String manifestPath = basePath + File.separator + "manifest.json";
        Value manifestVal = parseJsonFile(manifestPath);
        Map<String, Value> obj = expectObject(manifestVal, "manifest.json");
        List<Value> arr = expectArray(obj.get(fieldName), "manifest." + fieldName);
        List<ModuleName> result = new ArrayList<>(arr.size());
        for (Value v : arr) {
            result.add(new ModuleName(expectString(v, fieldName + " entry")));
        }
        return result;
    }

    /**
     * Generate source files and write them to disk.
     */
    public static void generateSources(
            Function<Module, Function<List<Definition>, Function<hydra.context.Context, Function<Graph, Either<hydra.errors.Error_, Map<String, String>>>>>> coder,
            hydra.coders.Language language,
            boolean doInfer,
            boolean doExpand,
            boolean doHoistCase,
            boolean doHoistPoly,
            String basePath,
            List<Module> universe,
            List<Module> modulesToGenerate) {
        Graph bsGraph = bootstrapGraph();
        hydra.context.Context cx = new hydra.context.Context(
                new ArrayList<>(), new ArrayList<>(), new HashMap<>());
        Either<hydra.errors.Error_, List<Pair<String, String>>> result =
                Codegen.generateSourceFiles(coder, language,
                        doInfer, doExpand, doHoistCase, doHoistPoly,
                        bsGraph, universe, modulesToGenerate, cx);
        List<Pair<String, String>> files;
        if (result.isLeft()) {
            hydra.errors.Error_ err = ((Either.Left<hydra.errors.Error_, List<Pair<String, String>>>) result).value;
            throw new RuntimeException("Code generation failed: " + hydra.show.Errors.error(err));
        }
        files = ((Either.Right<hydra.errors.Error_, List<Pair<String, String>>>) result).value;
        for (Pair<String, String> pair : files) {
            String filePath = basePath + File.separator + pair.first;
            String content = pair.second;
            if (!content.endsWith("\n")) {
                content = content + "\n";
            }
            try {
                Path p = Paths.get(filePath);
                Files.createDirectories(p.getParent());
                Files.write(p, content.getBytes(StandardCharsets.UTF_8));
            } catch (IOException e) {
                throw new RuntimeException("Failed to write " + filePath, e);
            }
        }
    }

    /**
     * Generate Java source files from modules.
     */
    public static void writeJava(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.java.Coder.moduleToJava(mod, defs, cx, g),
                hydra.java.Language.javaLanguage(),
                false, true, false, true,
                basePath, universe, mods);
    }

    /**
     * Generate Python source files from modules.
     */
    public static void writePython(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.python.Coder.moduleToPython(mod, defs, cx, g),
                hydra.python.Language.pythonLanguage(),
                false, true, true, false,
                basePath, universe, mods);
    }

    /**
     * Generate TypeScript source files from modules.
     *
     * doHoistCaseStatements=true mirrors Python's TS-host setting and the
     * per-target dispatch in heads/typescript/.../bootstrap.ts. Hoisting
     * pulls cases out of inline IIFEs into top-level helpers, saving
     * stack frames when the TS runtime walks deeply-nested terms.
     */
    public static void writeTypeScript(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.typeScript.Coder.moduleToTypeScript(mod, defs, cx, g),
                hydra.typeScript.Language.typeScriptLanguage(),
                false, true, true, false,
                basePath, universe, mods);
    }

    /**
     * Generate Haskell source files from modules.
     */
    public static void writeHaskell(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.haskell.Coder.moduleToHaskell(mod, defs, cx, g),
                hydra.haskell.Language.haskellLanguage(),
                false, false, false, false,
                basePath, universe, mods);
    }

    /**
     * Generate source files for a Lisp dialect (Clojure, Scheme, Common Lisp, or Emacs Lisp).
     */
    public static void writeLispDialect(String basePath, String dialectName, String fileExt,
                                         List<Module> universe, List<Module> mods) {
        hydra.lisp.syntax.Dialect dialect;
        hydra.util.CaseConvention caseConv;
        switch (dialectName) {
            case "clojure":
                dialect = new hydra.lisp.syntax.Dialect.Clojure();
                caseConv = new hydra.util.CaseConvention.Camel();
                break;
            case "scheme":
                dialect = new hydra.lisp.syntax.Dialect.Scheme();
                caseConv = new hydra.util.CaseConvention.LowerSnake();
                break;
            case "commonLisp":
                dialect = new hydra.lisp.syntax.Dialect.CommonLisp();
                caseConv = new hydra.util.CaseConvention.LowerSnake();
                break;
            case "emacsLisp":
                dialect = new hydra.lisp.syntax.Dialect.EmacsLisp();
                caseConv = new hydra.util.CaseConvention.LowerSnake();
                break;
            default:
                throw new IllegalArgumentException("Unknown Lisp dialect: " + dialectName);
        }

        final hydra.lisp.syntax.Dialect d = dialect;
        final hydra.util.CaseConvention cc = caseConv;
        generateSources(
                mod -> defs -> cx -> g -> {
                    // TODO: lisp.Coder excluded due to stale visitor patterns; needs regeneration
                    hydra.util.Either result = new hydra.util.Either.Left("Lisp code generation temporarily disabled");
                    if (result instanceof hydra.util.Either.Left) {
                        return result;
                    }
                    hydra.lisp.syntax.Program program = (hydra.lisp.syntax.Program) ((hydra.util.Either.Right) result).value;
                    String code = hydra.Serialization.printExpr(
                            hydra.Serialization.parenthesize(
                                    hydra.lisp.Serde.programToExpr(program)));
                    String filePath = hydra.Names.namespaceToFilePath(
                            cc, new hydra.packaging.FileExtension(fileExt), mod.name);
                    Map<String, String> fileMap = new java.util.TreeMap<>();
                    fileMap.put(filePath, code);
                    return new hydra.util.Either.Right(fileMap);
                },
                hydra.lisp.Language.lispLanguage(),
                false, false, false, false,
                basePath, universe, mods);
    }

    /**
     * Convert a namespace to a file path.
     */
    public static String namespaceToPath(ModuleName ns) {
        return Codegen.namespaceToPath(ns);
    }

    /**
     * Strip System F type annotations from all term bodies in a module.
     * Uses removeTypesFromTerm which strips TypeApplication, TypeLambda,
     * lambda domain types, and let-binding TypeSchemes from terms.
     * Module-level TypeSchemes are stripped from term bindings (to avoid
     * type conflicts) but preserved on type-defining bindings
     * (needed by isNativeType for schema graph construction).
     */
    public static Module stripTermTypes(Module m) {
        List<Definition> stripped = new ArrayList<>();
        for (Definition d : m.definitions) {
            d.accept(new Definition.Visitor<Void>() {
                @Override public Void visit(Definition.Term td) {
                    TermDefinition t = td.value;
                    Term newTerm = Strip.removeTypesFromTerm(t.term);
                    Maybe<TypeScheme> newType = Maybe.nothing();
                    stripped.add(new Definition.Term(new TermDefinition(t.name, newTerm, newType)));
                    return null;
                }
                @Override public Void visit(Definition.Type td) {
                    stripped.add(d);
                    return null;
                }
            });
        }
        return new Module(m.description, m.name, m.dependencies, stripped);
    }

    /**
     * Strip System F type annotations from all modules.
     */
    public static List<Module> stripAllTermTypes(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            result.add(stripTermTypes(m));
        }
        return result;
    }

    /**
     * Filter modules to only kernel modules (exclude hydra.* namespaces).
     */
    public static List<Module> filterKernelModules(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            if (!m.name.value.startsWith("hydra.") && !m.name.value.startsWith("hydra.json.yaml.")) {
                result.add(m);
            }
        }
        return result;
    }

    /**
     * Filter modules to only those containing type-defining bindings.
     */
    public static List<Module> filterTypeModules(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            boolean hasTypes = false;
            for (Definition d : m.definitions) {
                if (d instanceof Definition.Type) {
                    hasTypes = true;
                    break;
                }
            }
            if (hasTypes) {
                result.add(m);
            }
        }
        return result;
    }

    /**
     * Prefix-to-package table. Order matters: more-specific prefixes first.
     * Any namespace not matching any prefix falls through to "hydra-kernel".
     * Mirrors Hydra.PackageRouting.packagePrefixes on the Haskell side.
     */
    private static final List<Pair<String, String>> PACKAGE_PREFIXES = java.util.Arrays.asList(
        // Coder packages (main runtime modules)
        new Pair<>("hydra.haskell.",              "hydra-haskell"),
        new Pair<>("hydra.java.",                 "hydra-java"),
        new Pair<>("hydra.python.",               "hydra-python"),
        new Pair<>("hydra.scala.",                "hydra-scala"),
        new Pair<>("hydra.lisp.",                 "hydra-lisp"),
        new Pair<>("hydra.coq.",                  "hydra-coq"),
        new Pair<>("hydra.typeScript.",           "hydra-typescript"),
        new Pair<>("hydra.go.",                   "hydra-go"),
        // DSL wrapper modules for coder packages
        new Pair<>("hydra.dsl.haskell.",          "hydra-haskell"),
        new Pair<>("hydra.dsl.java.",             "hydra-java"),
        new Pair<>("hydra.dsl.python.",           "hydra-python"),
        new Pair<>("hydra.dsl.scala.",            "hydra-scala"),
        new Pair<>("hydra.dsl.lisp.",             "hydra-lisp"),
        new Pair<>("hydra.dsl.coq.",              "hydra-coq"),
        new Pair<>("hydra.dsl.typeScript.",       "hydra-typescript"),
        new Pair<>("hydra.dsl.go.",               "hydra-go"),
        // Synthesized decoder source modules for coder packages
        new Pair<>("hydra.sources.decode.haskell.",    "hydra-haskell"),
        new Pair<>("hydra.sources.decode.java.",       "hydra-java"),
        new Pair<>("hydra.sources.decode.python.",     "hydra-python"),
        new Pair<>("hydra.sources.decode.scala.",      "hydra-scala"),
        new Pair<>("hydra.sources.decode.lisp.",       "hydra-lisp"),
        new Pair<>("hydra.sources.decode.coq.",        "hydra-coq"),
        new Pair<>("hydra.sources.decode.typeScript.", "hydra-typescript"),
        // Synthesized encoder source modules for coder packages
        new Pair<>("hydra.sources.encode.haskell.",    "hydra-haskell"),
        new Pair<>("hydra.sources.encode.java.",       "hydra-java"),
        new Pair<>("hydra.sources.encode.python.",     "hydra-python"),
        new Pair<>("hydra.sources.encode.scala.",      "hydra-scala"),
        new Pair<>("hydra.sources.encode.lisp.",       "hydra-lisp"),
        new Pair<>("hydra.sources.encode.coq.",        "hydra-coq"),
        new Pair<>("hydra.sources.encode.typeScript.", "hydra-typescript"),
        // Property graph package
        new Pair<>("hydra.pg.",                   "hydra-pg"),
        new Pair<>("hydra.cypher.",               "hydra-pg"),
        new Pair<>("hydra.graphviz.",             "hydra-pg"),
        new Pair<>("hydra.tinkerpop.",            "hydra-pg"),
        new Pair<>("hydra.error.pg",              "hydra-pg"),
        new Pair<>("hydra.show.error.pg",         "hydra-pg"),
        new Pair<>("hydra.validate.pg",           "hydra-pg"),
        new Pair<>("hydra.decode.pg.",            "hydra-pg"),
        new Pair<>("hydra.encode.pg.",            "hydra-pg"),
        new Pair<>("hydra.sources.decode.pg.",    "hydra-pg"),
        new Pair<>("hydra.sources.encode.pg.",    "hydra-pg"),
        new Pair<>("hydra.demos.genpg.",          "hydra-pg"),
        new Pair<>("openGql.grammar",             "hydra-pg"),
        new Pair<>("com.gdblab.pathAlgebra.",     "hydra-pg"),
        new Pair<>("hydra.dsl.pg.",               "hydra-pg"),
        new Pair<>("hydra.dsl.cypher.",           "hydra-pg"),
        new Pair<>("hydra.dsl.graphviz.",         "hydra-pg"),
        new Pair<>("hydra.dsl.tinkerpop.",        "hydra-pg"),
        new Pair<>("hydra.dsl.error.pg",          "hydra-pg"),
        new Pair<>("hydra.dsl.openGql.",          "hydra-pg"),
        new Pair<>("hydra.dsl.com.gdblab.pathAlgebra.", "hydra-pg"),
        // RDF / OWL / SHACL / ShEx / XML schema package
        new Pair<>("hydra.rdf.",                  "hydra-rdf"),
        new Pair<>("hydra.owl.",                  "hydra-rdf"),
        new Pair<>("hydra.shacl.",                "hydra-rdf"),
        new Pair<>("hydra.shex.",                 "hydra-rdf"),
        new Pair<>("hydra.xml.schema",            "hydra-rdf"),
        new Pair<>("hydra.dsl.rdf.",              "hydra-rdf"),
        new Pair<>("hydra.dsl.owl.",              "hydra-rdf"),
        new Pair<>("hydra.dsl.shacl.",            "hydra-rdf"),
        new Pair<>("hydra.dsl.shex.",             "hydra-rdf"),
        new Pair<>("hydra.dsl.xml.schema",        "hydra-rdf"),
        // WebAssembly package
        new Pair<>("hydra.wasm.",                 "hydra-wasm"),
        new Pair<>("hydra.dsl.wasm.",             "hydra-wasm"),
        // Benchmark package
        new Pair<>("hydra.bench.",                "hydra-bench"),
        // Extension package (truly-ext coders: Avro, Protobuf, GraphQL, etc.)
        new Pair<>("hydra.atlas",                 "hydra-ext"),
        new Pair<>("hydra.avro.",                 "hydra-ext"),
        new Pair<>("hydra.azure.",                "hydra-ext"),
        new Pair<>("hydra.cpp.",                  "hydra-ext"),
        new Pair<>("hydra.csharp.",               "hydra-ext"),
        new Pair<>("hydra.datalog.",              "hydra-ext"),
        new Pair<>("hydra.delta.",                "hydra-ext"),
        new Pair<>("hydra.geojson.",              "hydra-ext"),
        new Pair<>("hydra.graphql.",              "hydra-ext"),
        new Pair<>("hydra.iana.",                 "hydra-ext"),
        new Pair<>("hydra.json.schema",           "hydra-ext"),
        new Pair<>("hydra.kusto.",                "hydra-ext"),
        new Pair<>("hydra.osv.",                  "hydra-ext"),
        new Pair<>("hydra.parquet.",              "hydra-ext"),
        new Pair<>("hydra.pegasus.",              "hydra-ext"),
        new Pair<>("hydra.protobuf.",             "hydra-ext"),
        new Pair<>("hydra.rust.",                 "hydra-ext"),
        new Pair<>("hydra.sql.",                  "hydra-ext"),
        new Pair<>("hydra.stac.",                 "hydra-ext"),
        new Pair<>("hydra.typeScript.",           "hydra-ext"),
        new Pair<>("hydra.workflow",              "hydra-ext"),
        new Pair<>("hydra.dsl.atlas",             "hydra-ext"),
        new Pair<>("hydra.dsl.avro.",             "hydra-ext"),
        new Pair<>("hydra.dsl.azure.",            "hydra-ext"),
        new Pair<>("hydra.dsl.cpp.",              "hydra-ext"),
        new Pair<>("hydra.dsl.csharp.",           "hydra-ext"),
        new Pair<>("hydra.dsl.datalog.",          "hydra-ext"),
        new Pair<>("hydra.dsl.delta.",            "hydra-ext"),
        new Pair<>("hydra.dsl.geojson.",          "hydra-ext"),
        new Pair<>("hydra.dsl.graphql.",          "hydra-ext"),
        new Pair<>("hydra.dsl.iana.",             "hydra-ext"),
        new Pair<>("hydra.dsl.json.schema",       "hydra-ext"),
        new Pair<>("hydra.dsl.kusto.",            "hydra-ext"),
        new Pair<>("hydra.dsl.osv.",              "hydra-ext"),
        new Pair<>("hydra.dsl.parquet.",          "hydra-ext"),
        new Pair<>("hydra.dsl.pegasus.",          "hydra-ext"),
        new Pair<>("hydra.dsl.protobuf.",         "hydra-ext"),
        new Pair<>("hydra.dsl.rust.",             "hydra-ext"),
        new Pair<>("hydra.dsl.sql.",              "hydra-ext"),
        new Pair<>("hydra.dsl.stac.",             "hydra-ext"),
        new Pair<>("hydra.dsl.typeScript.",       "hydra-ext"),
        new Pair<>("hydra.dsl.workflow",          "hydra-ext"),
        // hydra.yaml.model lives in hydra-kernel; route ext-owned yaml modules explicitly.
        new Pair<>("hydra.yaml.coder",            "hydra-ext"),
        new Pair<>("hydra.yaml.language",         "hydra-ext"),
        new Pair<>("hydra.yaml.serde",            "hydra-ext")
    );

    /**
     * Map a ModuleName to its owning package name. Mirrors
     * {@code Hydra.PackageRouting.namespaceToPackage}. Falls back to
     * {@code "hydra-kernel"} if no prefix matches.
     */
    public static String namespaceToPackage(ModuleName ns) {
        String s = ns.value;
        for (Pair<String, String> p : PACKAGE_PREFIXES) {
            if (s.startsWith(p.first)) {
                return p.second;
            }
        }
        return "hydra-kernel";
    }

    /**
     * Partition a list of modules by owning package. Returns groups sorted
     * by package name for deterministic ordering. Mirrors
     * {@code Hydra.PackageRouting.groupByPackage}.
     */
    public static List<Pair<String, List<Module>>> groupByPackage(List<Module> mods) {
        Map<String, List<Module>> byPkg = new java.util.TreeMap<>();
        for (Module m : mods) {
            byPkg.computeIfAbsent(namespaceToPackage(m.name), k -> new ArrayList<>()).add(m);
        }
        List<Pair<String, List<Module>>> out = new ArrayList<>();
        for (Map.Entry<String, List<Module>> e : byPkg.entrySet()) {
            out.add(new Pair<>(e.getKey(), e.getValue()));
        }
        return out;
    }

    /**
     * Read a package's declared dependencies from
     * {@code packages/<pkg>/package.json}. Returns the values of the
     * top-level {@code "dependencies"} array, or empty if absent /
     * unreadable. Mirrors {@code Hydra.Generation.loadPackageDeps}.
     */
    public static List<String> loadPackageDeps(String hydraRoot, String pkg) {
        Path path = Paths.get(hydraRoot, "packages", pkg, "package.json");
        if (!Files.isRegularFile(path)) return java.util.Collections.emptyList();
        try {
            Value v = parseJsonFile(path.toString());
            if (!(v instanceof Value.Object_)) return java.util.Collections.emptyList();
            Value deps = null;
            for (Map.Entry<String, Value> e : ((Value.Object_) v).value.entrySet()) {
                if ("dependencies".equals(e.getKey())) { deps = e.getValue(); break; }
            }
            if (!(deps instanceof Value.Array)) return java.util.Collections.emptyList();
            List<String> out = new ArrayList<>();
            for (Value e : ((Value.Array) deps).value) {
                if (e instanceof Value.String_) {
                    out.add(((Value.String_) e).value);
                }
            }
            return out;
        } catch (IOException ex) {
            return java.util.Collections.emptyList();
        }
    }

    /**
     * Per-package iterative inference + JSON write driver. Mirrors
     * {@code Hydra.Generation.inferAndWriteByPackage} on the Haskell side.
     *
     * <p>Processes packages in dependency order (topo sort over each
     * {@code package.json}'s {@code "dependencies"} field) and runs
     * {@link Codegen#inferModulesGiven} once per package, threading the
     * typed-so-far output of upstream packages through as the universe.
     * Each iteration writes its package's JSON to disk immediately.</p>
     *
     * @param hydraRoot     Worktree root; used to locate
     *                      {@code packages/<pkg>/package.json}.
     * @param distJsonRoot  Output JSON root (e.g. {@code <root>/dist/json}).
     *                      Per-package outputs are written to
     *                      {@code <root>/dist/json/<pkg>/src/main/json/}.
     * @param universeMods  All modules participating in type resolution
     *                      (kernel + sources). Grouped + iterated.
     * @param mods          Subset that should actually be re-inferred + written.
     * @param seedAcc       Pre-typed modules to seed the accumulator with
     *                      (e.g. kernel modules already loaded from JSON).
     *                      Excluded from grouping/iteration.
     * @return The full set of inferred target modules, concatenated across
     *         packages in topo order.
     */
    public static List<Module> inferAndWriteByPackage(
            String hydraRoot,
            String distJsonRoot,
            List<Module> universeMods,
            List<Module> mods,
            List<Module> seedAcc) throws IOException {
        java.util.Set<String> seedNs = new HashSet<>();
        for (Module m : seedAcc) seedNs.add(m.name.value);

        List<Module> groupingUniverse = new ArrayList<>();
        for (Module m : universeMods) {
            if (!seedNs.contains(m.name.value)) groupingUniverse.add(m);
        }
        List<Module> groupingTargets = new ArrayList<>();
        for (Module m : mods) {
            if (!seedNs.contains(m.name.value)) groupingTargets.add(m);
        }

        List<Pair<String, List<Module>>> universeGroups = groupByPackage(groupingUniverse);
        List<Pair<String, List<Module>>> targetGroups   = groupByPackage(groupingTargets);
        Map<String, List<Module>> pkgToUniverse = new HashMap<>();
        for (Pair<String, List<Module>> p : universeGroups) pkgToUniverse.put(p.first, p.second);
        Map<String, List<Module>> pkgToMods = new HashMap<>();
        for (Pair<String, List<Module>> p : targetGroups) pkgToMods.put(p.first, p.second);

        List<String> pkgsInScope = new ArrayList<>();
        HashSet<String> seen = new HashSet<>();
        for (Pair<String, List<Module>> p : universeGroups) {
            if (seen.add(p.first)) pkgsInScope.add(p.first);
        }
        for (Pair<String, List<Module>> p : targetGroups) {
            if (seen.add(p.first)) pkgsInScope.add(p.first);
        }

        List<Pair<String, List<String>>> pkgDeps = new ArrayList<>();
        for (String p : pkgsInScope) {
            List<String> deps = loadPackageDeps(hydraRoot, p);
            List<String> inScope = new ArrayList<>();
            for (String d : deps) {
                if (pkgsInScope.contains(d)) inScope.add(d);
            }
            pkgDeps.add(new Pair<>(p, inScope));
        }

        Either<List<List<String>>, List<String>> topoResult = Sorting.topologicalSort(pkgDeps);
        List<String> ordered;
        if (topoResult instanceof Either.Right) {
            ordered = ((Either.Right<List<List<String>>, List<String>>) topoResult).value;
        } else {
            List<List<String>> cycles =
                ((Either.Left<List<List<String>>, List<String>>) topoResult).value;
            throw new RuntimeException(
                "inferAndWriteByPackage: package dep graph has cycles: " + cycles);
        }

        System.err.println("  Per-package inference: " + ordered.size()
            + " packages in dep order: " + String.join(" -> ", ordered));

        hydra.context.Context ctx = new hydra.context.Context(
            java.util.Collections.emptyList(),
            java.util.Collections.emptyList(),
            java.util.Collections.emptyMap());
        Graph bsGraph = bootstrapGraph();
        List<Module> acc = new ArrayList<>(seedAcc);
        List<Module> inferredAll = new ArrayList<>();
        for (String pkg : ordered) {
            List<Module> pkgTargets  = pkgToMods.getOrDefault(pkg, java.util.Collections.emptyList());
            List<Module> pkgUniverse = pkgToUniverse.getOrDefault(pkg, java.util.Collections.emptyList());
            java.util.Set<String> targetNs = new HashSet<>();
            for (Module m : pkgTargets) targetNs.add(m.name.value);
            List<Module> inferTargets = pkgTargets.isEmpty() ? pkgUniverse : pkgTargets;
            List<Module> typedUniverse = new ArrayList<>(acc);
            typedUniverse.addAll(pkgUniverse);
            System.err.println("  [" + pkg + "] " + pkgTargets.size() + " write / "
                + inferTargets.size() + " infer / " + acc.size() + " typed-so-far");
            if (inferTargets.isEmpty()) continue;
            Either<hydra.errors.Error_, List<Module>> result =
                Codegen.inferModulesGiven(ctx, bsGraph, typedUniverse, inferTargets);
            List<Module> inferred;
            if (result instanceof Either.Right) {
                inferred = ((Either.Right<hydra.errors.Error_, List<Module>>) result).value;
            } else {
                hydra.errors.Error_ err =
                    ((Either.Left<hydra.errors.Error_, List<Module>>) result).value;
                throw new RuntimeException(
                    "inferAndWriteByPackage: inference failed for " + pkg + ": " + err);
            }
            List<Module> toWrite = new ArrayList<>();
            for (Module m : inferred) {
                if (targetNs.contains(m.name.value)) toWrite.add(m);
            }
            if (!toWrite.isEmpty()) {
                writePackageSplitJson(distJsonRoot, typedUniverse, inferred, toWrite);
            }
            acc.addAll(inferred);
            inferredAll.addAll(inferred);
        }
        return inferredAll;
    }

    /**
     * Encode a set of inferred modules to JSON and write them under
     * {@code distJsonRoot/<pkg>/src/main/json/}. Mirrors
     * {@code Hydra.Generation.writePackageSplitJson}.
     */
    private static void writePackageSplitJson(
            String distJsonRoot,
            List<Module> universeMods,
            List<Module> universeForSchema,
            List<Module> toWrite) throws IOException {
        List<Module> combined = new ArrayList<>(universeMods);
        combined.addAll(universeForSchema);
        Graph graph = Codegen.modulesToGraph(bootstrapGraph(), combined, universeMods);
        Map<Name, hydra.core.Type> schemaMap = Codegen.buildSchemaMap(graph);
        for (Pair<String, List<Module>> grp : groupByPackage(toWrite)) {
            String pkg = grp.first;
            List<Module> pkgMods = grp.second;
            Path pkgDir = Paths.get(distJsonRoot, pkg, "src", "main", "json");
            System.err.println("  " + pkg + ": " + pkgMods.size() + " modules -> " + pkgDir);
            for (Module m : pkgMods) {
                Either<hydra.errors.Error_, String> encoded = Codegen.moduleToJson(schemaMap, m);
                if (encoded instanceof Either.Left) {
                    hydra.errors.Error_ err =
                        ((Either.Left<hydra.errors.Error_, String>) encoded).value;
                    throw new RuntimeException(
                        "writePackageSplitJson: encode failed for "
                        + m.name.value + ": " + err);
                }
                String jsonStr = ((Either.Right<hydra.errors.Error_, String>) encoded).value;
                Path filePath = pkgDir.resolve(namespaceToPath(m.name) + ".json");
                Files.createDirectories(filePath.getParent());
                String newContent = jsonStr + "\n";
                if (Files.exists(filePath)) {
                    String old = new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8);
                    if (old.equals(newContent)) continue;
                }
                Files.write(filePath, newContent.getBytes(StandardCharsets.UTF_8));
            }
        }
    }
}
