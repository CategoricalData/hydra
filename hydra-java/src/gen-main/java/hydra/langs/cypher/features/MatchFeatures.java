// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for match queries.
 */
public class MatchFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.MatchFeatures");
  
  /**
   * Whether to expect the basic (non-optional) MATCH clause.
   */
  public final Boolean match;
  
  /**
   * Whether to expect OPTIONAL MATCH.
   */
  public final Boolean optionalMatch;
  
  public MatchFeatures (Boolean match, Boolean optionalMatch) {
    java.util.Objects.requireNonNull((match));
    java.util.Objects.requireNonNull((optionalMatch));
    this.match = match;
    this.optionalMatch = optionalMatch;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MatchFeatures)) {
      return false;
    }
    MatchFeatures o = (MatchFeatures) (other);
    return match.equals(o.match) && optionalMatch.equals(o.optionalMatch);
  }
  
  @Override
  public int hashCode() {
    return 2 * match.hashCode() + 3 * optionalMatch.hashCode();
  }
  
  public MatchFeatures withMatch(Boolean match) {
    java.util.Objects.requireNonNull((match));
    return new MatchFeatures(match, optionalMatch);
  }
  
  public MatchFeatures withOptionalMatch(Boolean optionalMatch) {
    java.util.Objects.requireNonNull((optionalMatch));
    return new MatchFeatures(match, optionalMatch);
  }
}