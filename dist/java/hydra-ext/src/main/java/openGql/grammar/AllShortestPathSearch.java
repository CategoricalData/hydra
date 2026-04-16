// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AllShortestPathSearch implements Serializable, Comparable<AllShortestPathSearch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AllShortestPathSearch");

  public static final hydra.core.Name MODE = new hydra.core.Name("mode");

  public static final hydra.core.Name OR_PATHS = new hydra.core.Name("orPaths");

  public final hydra.util.Maybe<openGql.grammar.PathMode> mode;

  public final hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths;

  public AllShortestPathSearch (hydra.util.Maybe<openGql.grammar.PathMode> mode, hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    this.mode = mode;
    this.orPaths = orPaths;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AllShortestPathSearch)) {
      return false;
    }
    AllShortestPathSearch o = (AllShortestPathSearch) other;
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
  public int compareTo(AllShortestPathSearch other) {
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

  public AllShortestPathSearch withMode(hydra.util.Maybe<openGql.grammar.PathMode> mode) {
    return new AllShortestPathSearch(mode, orPaths);
  }

  public AllShortestPathSearch withOrPaths(hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    return new AllShortestPathSearch(mode, orPaths);
  }
}
