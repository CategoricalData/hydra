package hydra.core.dsl;

import org.junit.jupiter.api.Test;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Conformance tests for the #731 D2 fix: {@link hydra.core.overlay.java.dsl.Phantoms}'s
 * {@code apply}/{@code project}/{@code proj}/{@code match}/{@code cases} signatures thread the
 * phantom's arrow structure, so the mistake classes below (M1-M3, from the #410 phantom-fidelity
 * battery) are now Java compile errors rather than silent runtime/inference failures.
 *
 * <p>The M1-M3 cases are verified via the Java Compiler API rather than as commented-out source,
 * so a regression (signature loosened back to a wildcard) fails this test suite instead of
 * silently rotting as an unverified comment.</p>
 */
public class PhantomsTest {

    /** Compiles {@code source} (a single top-level class body) against the current test classpath,
     *  returning true if compilation succeeds. */
    private static boolean compiles(String className, String classBody) {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        String classpath = System.getProperty("java.class.path");
        int result = compiler.run(null, null, null,
                "-classpath", classpath, "-d", System.getProperty("java.io.tmpdir"),
                "-proc:none",
                sourceToTempFile(className, classBody));
        return result == 0;
    }

    /** javac's compiler.run(...) API needs a real file for a top-level public class; write one
     *  under a per-test scratch directory. */
    private static String sourceToTempFile(String className, String classBody) {
        try {
            java.io.File dir = java.nio.file.Files.createTempDirectory("phantoms-conformance").toFile();
            java.io.File file = new java.io.File(dir, className + ".java");
            try (java.io.FileWriter w = new java.io.FileWriter(file)) {
                w.write(classBody);
            }
            dir.deleteOnExit();
            file.deleteOnExit();
            return file.getAbsolutePath();
        } catch (java.io.IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static final String IMPORTS =
            "import hydra.core.typed.TypedTerm;\n" +
            "import static hydra.core.overlay.java.dsl.Phantoms.*;\n" +
            "import hydra.core.model.Binding;\n";

    /** Positive control: well-typed projection + application infers with no extra annotation. */
    @Test
    public void positiveControlCompiles() {
        String src = IMPORTS +
                "public class PositiveControl {\n" +
                "    public static TypedTerm<?> f() {\n" +
                "        return proj(Binding.TYPE_, Binding.NAME, \"b\");\n" +
                "    }\n" +
                "}\n";
        assertTrue(compiles("PositiveControl", src),
                "well-typed proj(...) composition should compile with no extra annotation");
    }

    /** M1 — ill-typed application: applying a non-function (a string literal) as a function. */
    @Test
    public void m1IllTypedApplicationFailsToCompile() {
        String src = IMPORTS +
                "public class M1IllTypedApply {\n" +
                "    public static TypedTerm<?> f() {\n" +
                // string("x") is TypedTerm<String>, not a TypedTerm<? extends Function<A,B>>
                "        return apply(string(\"not a function\"), string(\"arg\"));\n" +
                "    }\n" +
                "}\n";
        assertFalse(compiles("M1IllTypedApply", src),
                "applying a non-function TypedTerm should fail to compile");
    }

    /** M2 — domain mismatch: a projection's declared domain doesn't match the applied argument.
     *  The domain must be pinned independently of the {@code apply} call site (an explicit
     *  {@code TypedTerm<Binding>} local), or {@code apply}'s own type inference would otherwise
     *  happily infer the projection's free domain type variable from whatever argument is passed
     *  — which is correct behavior for an honestly-untyped {@code var}/{@code ref} reference, but
     *  masks the mismatch this fixture is meant to exercise. */
    @Test
    public void m2DomainMismatchFailsToCompile() {
        String src = IMPORTS +
                "public class M2DomainMismatch {\n" +
                "    public static TypedTerm<?> f() {\n" +
                // getName : TypedTerm<Function<Binding, Name>>, pinned via an explicit declared type.
                "        TypedTerm<java.util.function.Function<Binding, hydra.core.model.Name>> getName =\n" +
                "            project(Binding.TYPE_, Binding.NAME);\n" +
                // Applying it to a TypedTerm<String> (not TypedTerm<Binding>) is a domain mismatch.
                "        return apply(getName, string(\"wrong domain\"));\n" +
                "    }\n" +
                "}\n";
        assertFalse(compiles("M2DomainMismatch", src),
                "applying a projection to an argument of the wrong domain type should fail to compile");
    }

    /** M3 — value/function confusion (#615): a {@code cases} (function phantom) used where a
     *  {@code match} (value phantom) result is expected, and vice versa, are distinct types. */
    @Test
    public void m3ValueFunctionConfusionFailsToCompile() {
        String src = IMPORTS +
                "import hydra.core.model.Field;\n" +
                "public class M3ValueFunctionConfusion {\n" +
                "    public static TypedTerm<java.lang.String> f() {\n" +
                // cases(...) yields TypedTerm<Function<A,B>> (unapplied eliminator) -- assigning
                // it directly to a TypedTerm<String> (expecting a VALUE) is a type mismatch,
                // exactly the #615 cases/match confusion this fix makes a compile error.
                "        TypedTerm<java.lang.String> notAValue = cases(Binding.TYPE_, new Field[0]);\n" +
                "        return notAValue;\n" +
                "    }\n" +
                "}\n";
        assertFalse(compiles("M3ValueFunctionConfusion", src),
                "assigning an unapplied `cases` eliminator (a function phantom) where a value "
                        + "phantom is expected should fail to compile");
    }
}
