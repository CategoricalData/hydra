// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Comparison operators and functions
 */
public class ComparisonFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.ComparisonFeatures");
  
  public static final hydra.core.Name FIELD_NAME_EQUAL = new hydra.core.Name("equal");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN = new hydra.core.Name("greaterThan");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN_OR_EQUAL = new hydra.core.Name("greaterThanOrEqual");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN_OR_EQUAL = new hydra.core.Name("lessThanOrEqual");
  
  public static final hydra.core.Name FIELD_NAME_NOT_EQUAL = new hydra.core.Name("notEqual");
  
  /**
   * The = comparison operator
   */
  public final Boolean equal;
  
  /**
   * The &gt; comparison operator
   */
  public final Boolean greaterThan;
  
  /**
   * The &gt;= comparison operator
   */
  public final Boolean greaterThanOrEqual;
  
  /**
   * The &lt; comparison operator
   */
  public final Boolean lessThan;
  
  /**
   * The &lt;= comparison operator
   */
  public final Boolean lessThanOrEqual;
  
  /**
   * The &lt;&gt; comparison operator
   */
  public final Boolean notEqual;
  
  public ComparisonFeatures (Boolean equal, Boolean greaterThan, Boolean greaterThanOrEqual, Boolean lessThan, Boolean lessThanOrEqual, Boolean notEqual) {
    java.util.Objects.requireNonNull((equal));
    java.util.Objects.requireNonNull((greaterThan));
    java.util.Objects.requireNonNull((greaterThanOrEqual));
    java.util.Objects.requireNonNull((lessThan));
    java.util.Objects.requireNonNull((lessThanOrEqual));
    java.util.Objects.requireNonNull((notEqual));
    this.equal = equal;
    this.greaterThan = greaterThan;
    this.greaterThanOrEqual = greaterThanOrEqual;
    this.lessThan = lessThan;
    this.lessThanOrEqual = lessThanOrEqual;
    this.notEqual = notEqual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonFeatures)) {
      return false;
    }
    ComparisonFeatures o = (ComparisonFeatures) (other);
    return equal.equals(o.equal) && greaterThan.equals(o.greaterThan) && greaterThanOrEqual.equals(o.greaterThanOrEqual) && lessThan.equals(o.lessThan) && lessThanOrEqual.equals(o.lessThanOrEqual) && notEqual.equals(o.notEqual);
  }
  
  @Override
  public int hashCode() {
    return 2 * equal.hashCode() + 3 * greaterThan.hashCode() + 5 * greaterThanOrEqual.hashCode() + 7 * lessThan.hashCode() + 11 * lessThanOrEqual.hashCode() + 13 * notEqual.hashCode();
  }
  
  public ComparisonFeatures withEqual(Boolean equal) {
    java.util.Objects.requireNonNull((equal));
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withGreaterThan(Boolean greaterThan) {
    java.util.Objects.requireNonNull((greaterThan));
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withGreaterThanOrEqual(Boolean greaterThanOrEqual) {
    java.util.Objects.requireNonNull((greaterThanOrEqual));
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withLessThan(Boolean lessThan) {
    java.util.Objects.requireNonNull((lessThan));
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withLessThanOrEqual(Boolean lessThanOrEqual) {
    java.util.Objects.requireNonNull((lessThanOrEqual));
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withNotEqual(Boolean notEqual) {
    java.util.Objects.requireNonNull((notEqual));
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
}