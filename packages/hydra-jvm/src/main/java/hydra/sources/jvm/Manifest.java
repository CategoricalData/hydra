package hydra.sources.jvm;
import hydra.packaging.Module;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Package manifest for hydra-jvm.
 *
 * <p>Exposes {@link #mainModules} and {@link #testModules}, used by
 * build tooling to enumerate which Hydra modules belong to the
 * hydra-jvm package.</p>
 */
public class Manifest {
    public static final List<Module> mainModules = Arrays.asList(
        Serde.module_);

    public static final List<Module> testModules = Collections.emptyList();
}
