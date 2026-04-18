// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Match queries
 */
public class MatchFeatures implements Serializable, Comparable<MatchFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.MatchFeatures");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name OPTIONAL_MATCH = new hydra.core.Name("optionalMatch");

  /**
   * The basic (non-optional) MATCH clause
   */
  public final Boolean match;

  /**
   * OPTIONAL MATCH
   */
  public final Boolean optionalMatch;

  public MatchFeatures (Boolean match, Boolean optionalMatch) {
    this.match = match;
    this.optionalMatch = optionalMatch;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MatchFeatures)) {
      return false;
    }
    MatchFeatures o = (MatchFeatures) other;
    return java.util.Objects.equals(
      this.match,
      o.match) && java.util.Objects.equals(
      this.optionalMatch,
      o.optionalMatch);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(match) + 3 * java.util.Objects.hashCode(optionalMatch);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MatchFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      match,
      other.match);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      optionalMatch,
      other.optionalMatch);
  }

  public MatchFeatures withMatch(Boolean match) {
    return new MatchFeatures(match, optionalMatch);
  }

  public MatchFeatures withOptionalMatch(Boolean optionalMatch) {
    return new MatchFeatures(match, optionalMatch);
  }
}
