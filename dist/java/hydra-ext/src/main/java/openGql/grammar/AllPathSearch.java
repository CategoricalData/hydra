// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AllPathSearch implements Serializable, Comparable<AllPathSearch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AllPathSearch");

  public static final hydra.core.Name MODE = new hydra.core.Name("mode");

  public static final hydra.core.Name OR_PATHS = new hydra.core.Name("orPaths");

  public final hydra.util.Maybe<openGql.grammar.PathMode> mode;

  public final hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths;

  public AllPathSearch (hydra.util.Maybe<openGql.grammar.PathMode> mode, hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    this.mode = mode;
    this.orPaths = orPaths;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AllPathSearch)) {
      return false;
    }
    AllPathSearch o = (AllPathSearch) other;
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
  public int compareTo(AllPathSearch other) {
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

  public AllPathSearch withMode(hydra.util.Maybe<openGql.grammar.PathMode> mode) {
    return new AllPathSearch(mode, orPaths);
  }

  public AllPathSearch withOrPaths(hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    return new AllPathSearch(mode, orPaths);
  }
}
