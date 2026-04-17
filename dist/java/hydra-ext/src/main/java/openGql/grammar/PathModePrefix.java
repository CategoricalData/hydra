// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PathModePrefix implements Serializable, Comparable<PathModePrefix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathModePrefix");

  public static final hydra.core.Name MODE = new hydra.core.Name("mode");

  public static final hydra.core.Name OR_PATHS = new hydra.core.Name("orPaths");

  public final openGql.grammar.PathMode mode;

  public final hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths;

  public PathModePrefix (openGql.grammar.PathMode mode, hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    this.mode = mode;
    this.orPaths = orPaths;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathModePrefix)) {
      return false;
    }
    PathModePrefix o = (PathModePrefix) other;
    return java.util.Objects.equals(
      this.mode,
      o.mode) && java.util.Objects.equals(
      this.orPaths,
      o.orPaths);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mode) + 3 * java.util.Objects.hashCode(orPaths);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathModePrefix other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      mode,
      other.mode);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      orPaths,
      other.orPaths);
  }

  public PathModePrefix withMode(openGql.grammar.PathMode mode) {
    return new PathModePrefix(mode, orPaths);
  }

  public PathModePrefix withOrPaths(hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    return new PathModePrefix(mode, orPaths);
  }
}
