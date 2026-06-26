# Hydra-JVM

This package contains shared JVM utilities used across Hydra's JVM-targeting coders:
`hydra-java`, `hydra-scala`, and (in future) `hydra-clojure`.
It factors out cross-coder logic so each JVM-language package depends on `hydra-jvm`
rather than duplicating the shared code or importing across coder boundaries.

## Contents

The package is authored in Java (host-native DSL, same style as `hydra-java`):

- `packages/hydra-jvm/src/main/java/hydra/sources/jvm/Serde.java` —
  string/character escaping helpers for JVM language string literals
  (`escapeJavaChar`, `escapeJavaString`, `hexDigit`, `javaUnicodeEscape`, `padHex4`).
  These functions are shared by at least the Java and Scala coders.

DSL sources here are the sole source of truth for the `hydra.jvm.*` modules.
`bin/generate-hydra-java-from-java.sh` (Phase 5 of the main sync) generates
`dist/json/hydra-jvm/` from them via the Java driver.

## Code organization

```
packages/hydra-jvm/
  package.json                          — dependencies: [hydra-kernel]
  src/main/java/hydra/sources/jvm/
    Serde.java                          — NS: hydra.jvm.serde; string/char helpers
    Manifest.java                       — mainModules = [Serde.module_]
```

Generated artifacts land under `dist/json/hydra-jvm/` (JSON) and are then
assembled into target-language packages by the host-specific sync steps.
There is no `dist/java/hydra-jvm/` runtime tree; the Java head imports the
package sources directly from `packages/hydra-jvm/src/main/java/`.

## Publishing

`hydra-jvm` is listed in `PUBLISHED_HOSTS` in `bin/lib/hydra-packages.py`.
When the package is ready to publish, add it to the Java Maven build
(`dist/java/hydra-jvm/build.gradle`) and declare it as a dependency in
`hydra-java`'s published `build.gradle`.
