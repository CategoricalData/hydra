package hydra;

import hydra.coders.Language;
import hydra.errors.Error_;
import hydra.graph.Graph;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.util.Either;
import hydra.util.Pair;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * Cross-target source-code writers for the Java host: emit Haskell, Java, Python,
 * Scala, TypeScript and the four Lisp dialects from kernel modules.
 *
 * <p>These methods reference every target language's coder
 * ({@code hydra.{haskell,python,scala,typeScript,lisp}.Coder}), so they only
 * compile against the full local {@code dist/java} tree — NOT against a
 * Java-only published-host classpath ({@code net.fortytwo.hydra:hydra-java}).
 * They were split out of {@link Generation} (#370) precisely so the DSL→JSON
 * driver in {@code Generation}/{@link UpdateJavaJson} can compile and run against
 * the published Java host alone, with no local cross-target build.</p>
 *
 * <p>The lone caller of these writers is the cross-host bootstrap demo
 * ({@link Bootstrap}), which legitimately needs all target coders present.</p>
 */
public class GenerationTargets {

    /**
     * Generate source files and write them to disk.
     *
     * Emission flags (eta-expansion, case-hoisting, polymorphic-let-hoisting)
     * are now read from {@code language.supportedFeatures} inside
     * {@code generateSourceFiles}; the caller only supplies {@code doInfer}.
     */
    public static void generateSources(
            Function<Module, Function<List<Definition>, Function<hydra.typing.InferenceContext, Function<Graph, Either<Error_, Map<String, String>>>>>> coder,
            Language language,
            boolean doInfer,
            String basePath,
            List<Module> universe,
            List<Module> modulesToGenerate) {
        Graph bsGraph = Generation.bootstrapGraph();
        hydra.typing.InferenceContext cx = new hydra.typing.InferenceContext(0, new java.util.ArrayList<>());
        Either<Error_, List<Pair<String, String>>> result =
                Codegen.generateSourceFiles(coder, language,
                        doInfer,
                        bsGraph, universe, modulesToGenerate, cx);
        List<Pair<String, String>> files;
        if (result.isLeft()) {
            Error_ err = ((Either.Left<Error_, List<Pair<String, String>>>) result).value;
            throw new RuntimeException("Code generation failed: " + hydra.show.Errors.error(err));
        }
        files = ((Either.Right<Error_, List<Pair<String, String>>>) result).value;
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
                false,
                basePath, universe, mods);
    }

    /**
     * Generate Python source files from modules.
     */
    public static void writePython(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.python.Coder.moduleToPython(mod, defs, cx, g),
                hydra.python.Language.pythonLanguage(),
                false,
                basePath, universe, mods);
    }

    /**
     * Generate Scala source files from modules.
     */
    public static void writeScala(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.scala.Coder.moduleToScala(mod, defs, cx, g),
                hydra.scala.Language.scalaLanguage(),
                false,
                basePath, universe, mods);
    }

    /**
     * Generate TypeScript source files from modules.
     */
    public static void writeTypeScript(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.typeScript.Coder.moduleToTypeScript(mod, defs, cx, g),
                hydra.typeScript.Language.typeScriptLanguage(),
                false,
                basePath, universe, mods);
    }

    /**
     * Generate Haskell source files from modules.
     */
    public static void writeHaskell(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> cx -> g -> hydra.haskell.Coder.moduleToHaskell(mod, defs, cx, g),
                hydra.haskell.Language.haskellLanguage(),
                false,
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
                    hydra.util.Either result = hydra.lisp.Coder.moduleToLisp(d, mod, defs, cx, g);
                    if (result instanceof hydra.util.Either.Left) {
                        return result;
                    }
                    hydra.lisp.syntax.Program program = (hydra.lisp.syntax.Program) ((hydra.util.Either.Right) result).value;
                    String code = hydra.Serialization.printExpr(
                            hydra.Serialization.parenthesize(
                                    hydra.lisp.Serde.programToExpr(program)));
                    String filePath = hydra.Names.moduleNameToFilePath(
                            cc, new hydra.util.FileExtension(fileExt), mod.name);
                    Map<String, String> fileMap = new java.util.TreeMap<>();
                    fileMap.put(filePath, code);
                    return new hydra.util.Either.Right(fileMap);
                },
                hydra.lisp.Language.lispLanguage(),
                false,
                basePath, universe, mods);
    }
}
