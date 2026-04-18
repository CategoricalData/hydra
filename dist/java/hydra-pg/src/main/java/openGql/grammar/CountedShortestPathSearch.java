// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CountedShortestPathSearch implements Serializable, Comparable<CountedShortestPathSearch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CountedShortestPathSearch");

  public static final hydra.core.Name NUMBER_OF_PATHS = new hydra.core.Name("numberOfPaths");

  public static final hydra.core.Name MODE = new hydra.core.Name("mode");

  public static final hydra.core.Name OR_PATHS = new hydra.core.Name("orPaths");

  public final openGql.grammar.NonNegativeIntegerSpecification numberOfPaths;

  public final hydra.util.Maybe<openGql.grammar.PathMode> mode;

  public final hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths;

  public CountedShortestPathSearch (openGql.grammar.NonNegativeIntegerSpecification numberOfPaths, hydra.util.Maybe<openGql.grammar.PathMode> mode, hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    this.numberOfPaths = numberOfPaths;
    this.mode = mode;
    this.orPaths = orPaths;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CountedShortestPathSearch)) {
      return false;
    }
    CountedShortestPathSearch o = (CountedShortestPathSearch) other;
    return java.util.Objects.equals(
      this.numberOfPaths,
      o.numberOfPaths) && java.util.Objects.equals(
      this.mode,
      o.mode) && java.util.Objects.equals(
      this.orPaths,
      o.orPaths);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(numberOfPaths) + 3 * java.util.Objects.hashCode(mode) + 5 * java.util.Objects.hashCode(orPaths);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CountedShortestPathSearch other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      numberOfPaths,
      other.numberOfPaths);
    if (cmp != 0) {
      return cmp;
    }
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

  public CountedShortestPathSearch withNumberOfPaths(openGql.grammar.NonNegativeIntegerSpecification numberOfPaths) {
    return new CountedShortestPathSearch(numberOfPaths, mode, orPaths);
  }

  public CountedShortestPathSearch withMode(hydra.util.Maybe<openGql.grammar.PathMode> mode) {
    return new CountedShortestPathSearch(numberOfPaths, mode, orPaths);
  }

  public CountedShortestPathSearch withOrPaths(hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    return new CountedShortestPathSearch(numberOfPaths, mode, orPaths);
  }
}
