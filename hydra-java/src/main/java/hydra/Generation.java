package hydra;

import hydra.Annotations;
import hydra.CodeGeneration;
import hydra.core.Binding;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;

import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.json.model.Value;
import hydra.lib.Libraries;
import hydra.module.Definition;
import hydra.module.Module;
import hydra.module.TermDefinition;
import hydra.module.Namespace;
import hydra.Rewriting;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Either;
import hydra.util.Maybe;
import hydra.util.Pair;
import hydra.util.PersistentMap;
import hydra.util.PersistentSet;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;


/**
 * I/O wrapper for Hydra code generation in Java.
 * Provides file I/O around the pure Either-based functions in CodeGeneration.
 */
public class Generation {

    /**
     * Create an empty graph with standard primitives (the bootstrap graph).
     */
    public static Graph bootstrapGraph() {
        PersistentMap<Name, Term> boundTerms = PersistentMap.empty();
        PersistentMap<Name, TypeScheme> boundTypes = PersistentMap.empty();
        PersistentMap<Name, hydra.core.TypeVariableMetadata> classConstraints = PersistentMap.empty();
        PersistentSet<Name> lambdaVariables = PersistentSet.empty();
        PersistentMap<Name, Term> metadata = PersistentMap.empty();

        PersistentMap<Name, Primitive> primitives = PersistentMap.empty();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives = primitives.insert(prim.name(), prim.toNative());
        }

        PersistentMap<Name, TypeScheme> schemaTypes = PersistentMap.empty();
        PersistentSet<Name> typeVariables = PersistentSet.empty();

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
            PersistentMap<String, Value> map = PersistentMap.empty();
            if (pos < input.length() && input.charAt(pos) != '}') {
                map = parseKeyValue(map);
                while (pos < input.length() && input.charAt(pos) == ',') {
                    pos++;
                    map = parseKeyValue(map);
                }
            }
            expect('}');
            return new Value.Object_(map);
        }

        private PersistentMap<String, Value> parseKeyValue(PersistentMap<String, Value> map) {
            skipWhitespace();
            String key = parseRawString();
            skipWhitespace();
            expect(':');
            Value val = parseValue();
            return map.insert(key, val);
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
            return new Value.Array(ConsList.fromList(list));
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
            boolean stripTypeSchemes, Value jsonVal) {
        Either<String, Module> result = CodeGeneration.decodeModuleFromJson(
                bsGraph, ConsList.fromList(universeModules), stripTypeSchemes, jsonVal);
        return result.accept(new Either.Visitor<String, Module, Module>() {
            @Override
            public Module visit(Either.Left<String, Module> instance) {
                throw new RuntimeException("Module decode error: " + instance.value);
            }

            @Override
            public Module visit(Either.Right<String, Module> instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode a single module from a JSON value using a pre-built schema map (Name → Type).
     * This avoids the Module → Graph → SchemaMap roundtrip, using the bootstrap type map directly.
     */
    public static Module decodeModuleFromJson(Graph bsGraph, Map<Name, hydra.core.Type> schemaMap,
            boolean stripTypeSchemes, Value jsonVal) {
        Name modName = new Name("hydra.module.Module");
        hydra.core.Type modType = new hydra.core.Type.Variable(modName);
        Either<String, hydra.core.Term> jsonResult = hydra.json.Decode.fromJson(PersistentMap.fromMap(schemaMap), modName, modType, jsonVal);
        return jsonResult.accept(new Either.Visitor<String, hydra.core.Term, Module>() {
            @Override
            public Module visit(Either.Left<String, hydra.core.Term> instance) {
                throw new RuntimeException("JSON decode error: " + instance.value);
            }

            @Override
            public Module visit(Either.Right<String, hydra.core.Term> instance) {
                hydra.core.Term term = instance.value;
                Either<hydra.errors.DecodingError, Module> modResult =
                    hydra.decode.Module.module(bsGraph, term);
                return modResult.accept(new Either.Visitor<hydra.errors.DecodingError, Module, Module>() {
                    @Override
                    public Module visit(Either.Left<hydra.errors.DecodingError, Module> left) {
                        throw new RuntimeException("Module decode error: " + left.value.value);
                    }

                    @Override
                    public Module visit(Either.Right<hydra.errors.DecodingError, Module> right) {
                        Module mod = right.value;
                        return stripTypeSchemes
                            ? CodeGeneration.stripModuleTypeSchemes(mod)
                            : mod;
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
            hydra.core.TypeScheme ts = hydra.Rewriting.fTypeToTypeScheme(entry.getValue());
            result.put(entry.getKey(), Rewriting.deannotateTypeRecursive(ts.type));
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
            result.put(entry.getKey(), hydra.Rewriting.fTypeToTypeScheme(entry.getValue()));
        }
        return result;
    }

    /**
     * Load modules from JSON files using a pre-built schema map (from Bootstrap.typesByName()).
     */
    public static List<Module> loadModulesFromJson(boolean stripTypeSchemes, String basePath,
            Map<Name, hydra.core.Type> schemaMap, List<Namespace> namespaces) throws IOException {
        Graph bsGraph = bootstrapGraph();
        List<Module> modules = new ArrayList<>();
        for (Namespace ns : namespaces) {
            String filePath = basePath + File.separator
                    + CodeGeneration.namespaceToPath(ns) + ".json";
            Value jsonVal = parseJsonFile(filePath);
            Module mod = decodeModuleFromJson(bsGraph, schemaMap, stripTypeSchemes, jsonVal);
            System.out.println("  Loaded: " + ns.value);
            modules.add(mod);
        }
        return modules;
    }


    private static PersistentMap<String, Value> expectObject(Value val, String context) {
        if (val instanceof Value.Object_) {
            return ((Value.Object_) val).value;
        }
        throw new RuntimeException("Expected JSON object for " + context + ", got " + val.getClass().getSimpleName());
    }

    private static ConsList<Value> expectArray(Value val, String context) {
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
    public static List<Namespace> readManifestField(String basePath, String fieldName) throws IOException {
        String manifestPath = basePath + File.separator + "manifest.json";
        Value manifestVal = parseJsonFile(manifestPath);
        PersistentMap<String, Value> obj = expectObject(manifestVal, "manifest.json");
        ConsList<Value> arr = expectArray(obj.get(fieldName), "manifest." + fieldName);
        List<Namespace> result = new ArrayList<>(arr.size());
        for (Value v : arr) {
            result.add(new Namespace(expectString(v, fieldName + " entry")));
        }
        return result;
    }

    /**
     * Generate source files and write them to disk.
     */
    public static void generateSources(
            Function<Module, Function<ConsList<Definition>, Function<hydra.context.Context, Function<Graph, Either<hydra.context.InContext<hydra.errors.Error_>, PersistentMap<String, String>>>>>> coder,
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
                ConsList.empty(), ConsList.empty(), PersistentMap.empty());
        Either<hydra.context.InContext<hydra.errors.Error_>, ConsList<Pair<String, String>>> result =
                CodeGeneration.generateSourceFiles(coder, language,
                        doInfer, doExpand, doHoistCase, doHoistPoly,
                        bsGraph, ConsList.fromList(universe), ConsList.fromList(modulesToGenerate), cx);
        ConsList<Pair<String, String>> files;
        if (result.isLeft()) {
            hydra.context.InContext<hydra.errors.Error_> err = ((Either.Left<hydra.context.InContext<hydra.errors.Error_>, ConsList<Pair<String, String>>>) result).value;
            throw new RuntimeException("Code generation failed: " + hydra.show.Errors.error(err.object));
        }
        files = ((Either.Right<hydra.context.InContext<hydra.errors.Error_>, ConsList<Pair<String, String>>>) result).value;
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
                mod -> defs -> cx -> g -> hydra.ext.java.Coder.moduleToJava(mod, defs, cx, g),
                hydra.ext.java.Language.javaLanguage(),
                false, true, false, true,
                basePath, universe, mods);
    }

    /**
     * Generate Python source files from modules.
     */
    public static void writePython(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.ext.python.Coder.moduleToPython(mod, defs, cx, g),
                hydra.ext.python.Language.pythonLanguage(),
                false, true, true, false,
                basePath, universe, mods);
    }

    /**
     * Generate Haskell source files from modules.
     */
    public static void writeHaskell(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.ext.haskell.Coder.moduleToHaskell(mod, defs, cx, g),
                hydra.ext.haskell.Language.haskellLanguage(),
                false, false, false, false,
                basePath, universe, mods);
    }

    /**
     * Generate source files for a Lisp dialect (Clojure, Scheme, Common Lisp, or Emacs Lisp).
     */
    public static void writeLispDialect(String basePath, String dialectName, String fileExt,
                                         List<Module> universe, List<Module> mods) {
        hydra.ext.lisp.syntax.Dialect dialect;
        hydra.util.CaseConvention caseConv;
        switch (dialectName) {
            case "clojure":
                dialect = new hydra.ext.lisp.syntax.Dialect.Clojure();
                caseConv = new hydra.util.CaseConvention.Camel();
                break;
            case "scheme":
                dialect = new hydra.ext.lisp.syntax.Dialect.Scheme();
                caseConv = new hydra.util.CaseConvention.LowerSnake();
                break;
            case "commonLisp":
                dialect = new hydra.ext.lisp.syntax.Dialect.CommonLisp();
                caseConv = new hydra.util.CaseConvention.LowerSnake();
                break;
            case "emacsLisp":
                dialect = new hydra.ext.lisp.syntax.Dialect.EmacsLisp();
                caseConv = new hydra.util.CaseConvention.LowerSnake();
                break;
            default:
                throw new IllegalArgumentException("Unknown Lisp dialect: " + dialectName);
        }

        final hydra.ext.lisp.syntax.Dialect d = dialect;
        final hydra.util.CaseConvention cc = caseConv;
        generateSources(
                mod -> defs -> cx -> g -> {
                    // TODO: lisp.Coder excluded due to stale visitor patterns; needs regeneration
                    hydra.util.Either result = new hydra.util.Either.Left("Lisp code generation temporarily disabled");
                    if (result instanceof hydra.util.Either.Left) {
                        return result;
                    }
                    hydra.ext.lisp.syntax.Program program = (hydra.ext.lisp.syntax.Program) ((hydra.util.Either.Right) result).value;
                    String code = hydra.Serialization.printExpr(
                            hydra.Serialization.parenthesize(
                                    hydra.ext.lisp.Serde.programToExpr(program)));
                    String filePath = hydra.Names.namespaceToFilePath(
                            cc, new hydra.module.FileExtension(fileExt), mod.namespace);
                    return new hydra.util.Either.Right(PersistentMap.<String, String>empty().insert(filePath, code));
                },
                hydra.ext.lisp.Language.lispLanguage(),
                false, false, false, false,
                basePath, universe, mods);
    }

    /**
     * Convert a namespace to a file path.
     */
    public static String namespaceToPath(Namespace ns) {
        return CodeGeneration.namespaceToPath(ns);
    }

    /**
     * Strip System F type annotations from all term bodies in a module.
     * Uses removeTypesFromTerm which strips TypeApplication, TypeLambda,
     * lambda domain types, and let-binding TypeSchemes from terms.
     * Module-level TypeSchemes are stripped from term bindings (to avoid
     * bigfloat/float64 conflicts) but preserved on type-defining bindings
     * (needed by isNativeType for schema graph construction).
     */
    public static Module stripTermTypes(Module m) {
        List<Definition> stripped = new ArrayList<>();
        for (Definition d : m.definitions) {
            d.accept(new Definition.Visitor<Void>() {
                @Override public Void visit(Definition.Term td) {
                    TermDefinition t = td.value;
                    Term newTerm = Rewriting.removeTypesFromTerm(t.term);
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
        return new Module(m.namespace, ConsList.fromList(stripped), m.typeDependencies, m.termDependencies, m.description);
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
     * Filter modules to only kernel modules (exclude hydra.ext.* namespaces).
     */
    public static List<Module> filterKernelModules(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            if (!m.namespace.value.startsWith("hydra.ext.") && !m.namespace.value.startsWith("hydra.json.yaml.")) {
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
}
