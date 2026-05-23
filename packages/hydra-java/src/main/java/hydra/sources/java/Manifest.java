package hydra.sources.java;
import hydra.packaging.Module;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Package manifest for hydra-java.
 *
 * Mirror of packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Manifest.hs.
 * Owns the Java coder DSL sources.
 *
 * <p>Exposes {@link #mainModules} and {@link #testModules}, used by
 * build tooling to enumerate which Hydra modules belong to the
 * hydra-java package.</p>
 */
public class Manifest {
    public static final List<Module> mainModules = Arrays.asList(
        Coder.module_,
        Environment.module_,
        Language.module_,
        Names.module_,
        Serde.module_,
        Syntax.module_,
        Testing.module_,
        Utils.module_);

    public static final List<Module> testModules = Collections.emptyList();
}
