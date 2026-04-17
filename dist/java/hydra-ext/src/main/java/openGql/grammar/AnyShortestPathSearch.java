// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AnyShortestPathSearch implements Serializable, Comparable<AnyShortestPathSearch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AnyShortestPathSearch");

  public static final hydra.core.Name MODE = new hydra.core.Name("mode");

  public static final hydra.core.Name OR_PATHS = new hydra.core.Name("orPaths");

  public final hydra.util.Maybe<openGql.grammar.PathMode> mode;

  public final hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths;

  public AnyShortestPathSearch (hydra.util.Maybe<openGql.grammar.PathMode> mode, hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    this.mode = mode;
    this.orPaths = orPaths;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnyShortestPathSearch)) {
      return false;
    }
    AnyShortestPathSearch o = (AnyShortestPathSearch) other;
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
  public int compareTo(AnyShortestPathSearch other) {
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

  public AnyShortestPathSearch withMode(hydra.util.Maybe<openGql.grammar.PathMode> mode) {
    return new AnyShortestPathSearch(mode, orPaths);
  }

  public AnyShortestPathSearch withOrPaths(hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    return new AnyShortestPathSearch(mode, orPaths);
  }
}
