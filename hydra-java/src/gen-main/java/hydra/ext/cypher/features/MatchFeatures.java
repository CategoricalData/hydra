// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Match queries
 */
public class MatchFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.MatchFeatures");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONAL_MATCH = new hydra.core.Name("optionalMatch");
  
  /**
   * The basic (non-optional) MATCH clause
   */
  public final Boolean match;
  
  /**
   * OPTIONAL MATCH
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