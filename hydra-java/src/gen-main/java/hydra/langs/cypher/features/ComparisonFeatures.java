// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for comparison operators and functions.
 */
public class ComparisonFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.ComparisonFeatures");
  
  /**
   * Whether to expect the = comparison operator.
   */
  public final Boolean equal;
  
  /**
   * Whether to expect the &gt; comparison operator.
   */
  public final Boolean greaterThan;
  
  /**
   * Whether to expect the &gt;= comparison operator.
   */
  public final Boolean greaterThanOrEqual;
  
  /**
   * Whether to expect the &lt; comparison operator.
   */
  public final Boolean lessThan;
  
  /**
   * Whether to expect the &lt;= comparison operator.
   */
  public final Boolean lessThanOrEqual;
  
  /**
   * Whether to expect the &lt;&gt; comparison operator.
   */
  public final Boolean notEqual;
  
  /**
   * Whether to expect the nullIf() function.
   */
  public final Boolean nullIf;
  
  public ComparisonFeatures (Boolean equal, Boolean greaterThan, Boolean greaterThanOrEqual, Boolean lessThan, Boolean lessThanOrEqual, Boolean notEqual, Boolean nullIf) {
    if (equal == null) {
      throw new IllegalArgumentException("null value for 'equal' argument");
    }
    if (greaterThan == null) {
      throw new IllegalArgumentException("null value for 'greaterThan' argument");
    }
    if (greaterThanOrEqual == null) {
      throw new IllegalArgumentException("null value for 'greaterThanOrEqual' argument");
    }
    if (lessThan == null) {
      throw new IllegalArgumentException("null value for 'lessThan' argument");
    }
    if (lessThanOrEqual == null) {
      throw new IllegalArgumentException("null value for 'lessThanOrEqual' argument");
    }
    if (notEqual == null) {
      throw new IllegalArgumentException("null value for 'notEqual' argument");
    }
    if (nullIf == null) {
      throw new IllegalArgumentException("null value for 'nullIf' argument");
    }
    this.equal = equal;
    this.greaterThan = greaterThan;
    this.greaterThanOrEqual = greaterThanOrEqual;
    this.lessThan = lessThan;
    this.lessThanOrEqual = lessThanOrEqual;
    this.notEqual = notEqual;
    this.nullIf = nullIf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonFeatures)) {
      return false;
    }
    ComparisonFeatures o = (ComparisonFeatures) (other);
    return equal.equals(o.equal) && greaterThan.equals(o.greaterThan) && greaterThanOrEqual.equals(o.greaterThanOrEqual) && lessThan.equals(o.lessThan) && lessThanOrEqual.equals(o.lessThanOrEqual) && notEqual.equals(o.notEqual) && nullIf.equals(o.nullIf);
  }
  
  @Override
  public int hashCode() {
    return 2 * equal.hashCode() + 3 * greaterThan.hashCode() + 5 * greaterThanOrEqual.hashCode() + 7 * lessThan.hashCode() + 11 * lessThanOrEqual.hashCode() + 13 * notEqual.hashCode() + 17 * nullIf.hashCode();
  }
  
  public ComparisonFeatures withEqual(Boolean equal) {
    if (equal == null) {
      throw new IllegalArgumentException("null value for 'equal' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
  
  public ComparisonFeatures withGreaterThan(Boolean greaterThan) {
    if (greaterThan == null) {
      throw new IllegalArgumentException("null value for 'greaterThan' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
  
  public ComparisonFeatures withGreaterThanOrEqual(Boolean greaterThanOrEqual) {
    if (greaterThanOrEqual == null) {
      throw new IllegalArgumentException("null value for 'greaterThanOrEqual' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
  
  public ComparisonFeatures withLessThan(Boolean lessThan) {
    if (lessThan == null) {
      throw new IllegalArgumentException("null value for 'lessThan' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
  
  public ComparisonFeatures withLessThanOrEqual(Boolean lessThanOrEqual) {
    if (lessThanOrEqual == null) {
      throw new IllegalArgumentException("null value for 'lessThanOrEqual' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
  
  public ComparisonFeatures withNotEqual(Boolean notEqual) {
    if (notEqual == null) {
      throw new IllegalArgumentException("null value for 'notEqual' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
  
  public ComparisonFeatures withNullIf(Boolean nullIf) {
    if (nullIf == null) {
      throw new IllegalArgumentException("null value for 'nullIf' argument");
    }
    return new ComparisonFeatures(equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, notEqual, nullIf);
  }
}