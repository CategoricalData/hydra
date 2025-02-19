// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Range literals within relationship patterns
 */
public class RangeLiteralFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.RangeLiteralFeatures");
  
  public static final hydra.core.Name FIELD_NAME_BOUNDS = new hydra.core.Name("bounds");
  
  public static final hydra.core.Name FIELD_NAME_EXACT_RANGE = new hydra.core.Name("exactRange");
  
  public static final hydra.core.Name FIELD_NAME_LOWER_BOUND = new hydra.core.Name("lowerBound");
  
  public static final hydra.core.Name FIELD_NAME_STAR_RANGE = new hydra.core.Name("starRange");
  
  public static final hydra.core.Name FIELD_NAME_UPPER_BOUND = new hydra.core.Name("upperBound");
  
  /**
   * Range literals with both lower and upper bounds
   */
  public final Boolean bounds;
  
  /**
   * Range literals providing an exact number of repetitions
   */
  public final Boolean exactRange;
  
  /**
   * Range literals with a lower bound (only)
   */
  public final Boolean lowerBound;
  
  /**
   * The * range literal
   */
  public final Boolean starRange;
  
  /**
   * Range literals with an upper bound (only)
   */
  public final Boolean upperBound;
  
  public RangeLiteralFeatures (Boolean bounds, Boolean exactRange, Boolean lowerBound, Boolean starRange, Boolean upperBound) {
    java.util.Objects.requireNonNull((bounds));
    java.util.Objects.requireNonNull((exactRange));
    java.util.Objects.requireNonNull((lowerBound));
    java.util.Objects.requireNonNull((starRange));
    java.util.Objects.requireNonNull((upperBound));
    this.bounds = bounds;
    this.exactRange = exactRange;
    this.lowerBound = lowerBound;
    this.starRange = starRange;
    this.upperBound = upperBound;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeLiteralFeatures)) {
      return false;
    }
    RangeLiteralFeatures o = (RangeLiteralFeatures) (other);
    return bounds.equals(o.bounds) && exactRange.equals(o.exactRange) && lowerBound.equals(o.lowerBound) && starRange.equals(o.starRange) && upperBound.equals(o.upperBound);
  }
  
  @Override
  public int hashCode() {
    return 2 * bounds.hashCode() + 3 * exactRange.hashCode() + 5 * lowerBound.hashCode() + 7 * starRange.hashCode() + 11 * upperBound.hashCode();
  }
  
  public RangeLiteralFeatures withBounds(Boolean bounds) {
    java.util.Objects.requireNonNull((bounds));
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withExactRange(Boolean exactRange) {
    java.util.Objects.requireNonNull((exactRange));
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withLowerBound(Boolean lowerBound) {
    java.util.Objects.requireNonNull((lowerBound));
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withStarRange(Boolean starRange) {
    java.util.Objects.requireNonNull((starRange));
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withUpperBound(Boolean upperBound) {
    java.util.Objects.requireNonNull((upperBound));
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
}