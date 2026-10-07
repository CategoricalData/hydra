package hydra;

// Regression test for #780: sanitizeJavaComment (packages/hydra-java/.../Serde.java) escaped
// only &, <, > (added for #493), leaving backslash-u escapes and "*/" to break javac.

import hydra.build.overlay.java.Generation;
import hydra.core.Annotations;
import hydra.core.graph.Graph;
import hydra.core.model.FieldType;
import hydra.core.model.LiteralType;
import hydra.core.model.Name;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.Optional;
import hydra.core.packaging.Definition;
import hydra.core.packaging.Module;
import hydra.core.packaging.ModuleDependency;
import hydra.core.packaging.ModuleName;
import hydra.core.packaging.TypeDefinition;
import hydra.core.typing.InferenceContext;
import hydra.java.Coder;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SanitizeJavaCommentTest {

    private static String generateWithDescription(String desc) {
        Type body = new Type.Record(List.of(new FieldType(new Name("value"),
            new Type.Literal(new LiteralType.String_()))));
        body = Annotations.setTypeDescription(Optional.given(desc), body);
        TypeDefinition td = new TypeDefinition(new Name("com.example.repro.Widget"), Optional.none(),
            new TypeScheme(new ArrayList<>(), body, new java.util.HashMap<>()));
        List<Definition> defs = List.of(new Definition.Type(td));
        Module mod = new Module(new ModuleName("com.example.repro"), Optional.none(),
            new ArrayList<ModuleDependency>(), defs);
        Graph g = Generation.bootstrapGraph();
        Either<hydra.core.errors.Error_, Map<String, String>> result = Coder.moduleToJava(
            Collections.emptySet(), mod, defs, new InferenceContext(0, new ArrayList<>()), g);
        Map<String, String> files = result.getOrThrow(err -> new RuntimeException("moduleToJava failed: " + err));
        return String.join("\n", files.values());
    }

    @Test
    void escapesBackslash() {
        String generated = generateWithDescription("\\uXXXX escapes are not supported.");
        assertTrue(generated.contains("&#92;uXXXX"), "expected an escaped backslash, got:\n" + generated);
        assertFalse(generated.contains("\\uXXXX"), "raw backslash-u leaked into generated source:\n" + generated);
    }

    @Test
    void escapesWellFormedUnicodeEscape() {
        String generated = generateWithDescription("\\u0041 is a well-formed escape.");
        assertTrue(generated.contains("&#92;u0041"), "expected an escaped backslash, got:\n" + generated);
    }

    @Test
    void escapesCommentTerminator() {
        String generated = generateWithDescription("ends here */ oops.");
        assertTrue(generated.contains("*&#47;"), "expected an escaped comment terminator, got:\n" + generated);
        assertFalse(generated.contains("*/ oops"), "raw comment terminator leaked into generated source:\n" + generated);
    }
}
