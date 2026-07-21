package hydra;

// Driver-level fail-loud tests for Generation's routing wrapper (#560).
//
// hydra.test.build.routing (packages/hydra-build) already exercises the
// GENERATED module (hydra.build.Routing) directly for unrouted-namespace
// failures. This test closes the remaining gap: proving Generation.java's
// own Either-to-RuntimeException conversion (namespaceToPackage,
// groupByPackage) actually propagates that fail-loud behavior rather than
// swallowing it.

import hydra.overlay.java.util.Optional;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class GenerationRoutingTest {

    // A minimal dist/json-shaped tree with one package declaring one module,
    // so buildRoutingMap has a non-empty (but incomplete) routing map to
    // fail against.
    private static Path writeFixtureDistJsonRoot(Path tmp) throws IOException {
        Path manifestDir = tmp.resolve("hydra-kernel/src/main/json");
        Files.createDirectories(manifestDir);
        Files.writeString(manifestDir.resolve("manifest.json"),
            "{\"package\": \"hydra-kernel\", \"mainModules\": [\"hydra.only.module\"]}");
        return tmp;
    }

    @Test
    void namespaceToPackageThrowsOnUnroutedNamespace(@TempDir Path tmp) throws IOException {
        Path distJsonRoot = writeFixtureDistJsonRoot(tmp);
        RuntimeException ex = assertThrows(RuntimeException.class, () ->
            Generation.namespaceToPackage(distJsonRoot.toString(), new ModuleName("hydra.totally.unrouted")));
        assertTrue(ex.getMessage().startsWith("namespaceToPackage:"),
            "expected a namespaceToPackage-prefixed failure, got: " + ex.getMessage());
    }

    @Test
    void groupByPackageThrowsIfAnyModuleIsUnrouted(@TempDir Path tmp) throws IOException {
        Path distJsonRoot = writeFixtureDistJsonRoot(tmp);
        Module unrouted = new Module(
            new ModuleName("hydra.totally.unrouted"),
            Optional.none(),
            Collections.emptyList(),
            Collections.emptyList());
        List<Module> mods = List.of(unrouted);
        RuntimeException ex = assertThrows(RuntimeException.class, () ->
            Generation.groupByPackage(distJsonRoot.toString(), mods));
        assertTrue(ex.getMessage().startsWith("groupByPackage:"),
            "expected a groupByPackage-prefixed failure, got: " + ex.getMessage());
    }
}
