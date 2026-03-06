// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Comparison operators and functions
 */
public class ComparisonFeatures implements Serializable, Comparable<ComparisonFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.ComparisonFeatures");
  
  public static final hydra.core.Name EQUAL = new hydra.core.Name("equal");
  
  public static final hydra.core.Name GREATER_THAN = new hydra.core.Name("greaterThan");
  
  public static final hydra.core.Name GREATER_THAN_OR_EQUAL = new hydra.core.Name("greaterThanOrEqual");
  
  public static final hydra.core.Name LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name LESS_THAN_OR_EQUAL = new hydra.core.Name("lessThanOrEqual");
  
  public static final hydra.core.Name NOT_EQUAL = new hydra.core.Name("notEqual");
  
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
    ComparisonFeatures o = (ComparisonFeatures) other;
    return java.util.Objects.equals(
      this.equal,
      o.equal) && java.util.Objects.equals(
      this.greaterThan,
      o.greaterThan) && java.util.Objects.equals(
      this.greaterThanOrEqual,
      o.greaterThanOrEqual) && java.util.Objects.equals(
      this.lessThan,
      o.lessThan) && java.util.Objects.equals(
      this.lessThanOrEqual,
      o.lessThanOrEqual) && java.util.Objects.equals(
      this.notEqual,
      o.notEqual);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(equal) + 3 * java.util.Objects.hashCode(greaterThan) + 5 * java.util.Objects.hashCode(greaterThanOrEqual) + 7 * java.util.Objects.hashCode(lessThan) + 11 * java.util.Objects.hashCode(lessThanOrEqual) + 13 * java.util.Objects.hashCode(notEqual);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ComparisonFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) equal).compareTo(other.equal);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) greaterThan).compareTo(other.greaterThan);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) greaterThanOrEqual).compareTo(other.greaterThanOrEqual);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) lessThan).compareTo(other.lessThan);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) lessThanOrEqual).compareTo(other.lessThanOrEqual);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) notEqual).compareTo(other.notEqual);
  }
  
  public ComparisonFeatures withEqual(Boolean equal) {
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withGreaterThan(Boolean greaterThan) {
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withGreaterThanOrEqual(Boolean greaterThanOrEqual) {
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withLessThan(Boolean lessThan) {
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withLessThanOrEqual(Boolean lessThanOrEqual) {
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
  
  public ComparisonFeatures withNotEqual(Boolean notEqual) {
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual);
  }
}
