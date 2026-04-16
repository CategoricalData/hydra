// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CountedShortestGroupSearch implements Serializable, Comparable<CountedShortestGroupSearch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CountedShortestGroupSearch");

  public static final hydra.core.Name NUMBER_OF_GROUPS = new hydra.core.Name("numberOfGroups");

  public static final hydra.core.Name MODE = new hydra.core.Name("mode");

  public static final hydra.core.Name OR_PATHS = new hydra.core.Name("orPaths");

  public static final hydra.core.Name GROUPS = new hydra.core.Name("groups");

  public final hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> numberOfGroups;

  public final hydra.util.Maybe<openGql.grammar.PathMode> mode;

  public final hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths;

  public final Boolean groups;

  public CountedShortestGroupSearch (hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> numberOfGroups, hydra.util.Maybe<openGql.grammar.PathMode> mode, hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths, Boolean groups) {
    this.numberOfGroups = numberOfGroups;
    this.mode = mode;
    this.orPaths = orPaths;
    this.groups = groups;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CountedShortestGroupSearch)) {
      return false;
    }
    CountedShortestGroupSearch o = (CountedShortestGroupSearch) other;
    return java.util.Objects.equals(
      this.numberOfGroups,
      o.numberOfGroups) && java.util.Objects.equals(
      this.mode,
      o.mode) && java.util.Objects.equals(
      this.orPaths,
      o.orPaths) && java.util.Objects.equals(
      this.groups,
      o.groups);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(numberOfGroups) + 3 * java.util.Objects.hashCode(mode) + 5 * java.util.Objects.hashCode(orPaths) + 7 * java.util.Objects.hashCode(groups);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CountedShortestGroupSearch other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      numberOfGroups,
      other.numberOfGroups);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      mode,
      other.mode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      orPaths,
      other.orPaths);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      groups,
      other.groups);
  }

  public CountedShortestGroupSearch withNumberOfGroups(hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> numberOfGroups) {
    return new CountedShortestGroupSearch(numberOfGroups, mode, orPaths, groups);
  }

  public CountedShortestGroupSearch withMode(hydra.util.Maybe<openGql.grammar.PathMode> mode) {
    return new CountedShortestGroupSearch(numberOfGroups, mode, orPaths, groups);
  }

  public CountedShortestGroupSearch withOrPaths(hydra.util.Maybe<openGql.grammar.PathOrPaths> orPaths) {
    return new CountedShortestGroupSearch(numberOfGroups, mode, orPaths, groups);
  }

  public CountedShortestGroupSearch withGroups(Boolean groups) {
    return new CountedShortestGroupSearch(numberOfGroups, mode, orPaths, groups);
  }
}
