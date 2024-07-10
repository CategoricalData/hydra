// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for range literals within relationship patterns.
 */
public class RangeLiteralFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.RangeLiteralFeatures");
  
  /**
   * Whether to expect range literals with both lower and upper bounds.
   */
  public final Boolean bounds;
  
  /**
   * Whether to expect range literals providing an exact number of repetitions.
   */
  public final Boolean exactRange;
  
  /**
   * Whether to expect range literals with a lower bound (only).
   */
  public final Boolean lowerBound;
  
  /**
   * Whether to expect the * range literal.
   */
  public final Boolean starRange;
  
  /**
   * Whether to expect range literals with an upper bound (only).
   */
  public final Boolean upperBound;
  
  public RangeLiteralFeatures (Boolean bounds, Boolean exactRange, Boolean lowerBound, Boolean starRange, Boolean upperBound) {
    if (bounds == null) {
      throw new IllegalArgumentException("null value for 'bounds' argument");
    }
    if (exactRange == null) {
      throw new IllegalArgumentException("null value for 'exactRange' argument");
    }
    if (lowerBound == null) {
      throw new IllegalArgumentException("null value for 'lowerBound' argument");
    }
    if (starRange == null) {
      throw new IllegalArgumentException("null value for 'starRange' argument");
    }
    if (upperBound == null) {
      throw new IllegalArgumentException("null value for 'upperBound' argument");
    }
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
    if (bounds == null) {
      throw new IllegalArgumentException("null value for 'bounds' argument");
    }
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withExactRange(Boolean exactRange) {
    if (exactRange == null) {
      throw new IllegalArgumentException("null value for 'exactRange' argument");
    }
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withLowerBound(Boolean lowerBound) {
    if (lowerBound == null) {
      throw new IllegalArgumentException("null value for 'lowerBound' argument");
    }
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withStarRange(Boolean starRange) {
    if (starRange == null) {
      throw new IllegalArgumentException("null value for 'starRange' argument");
    }
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
  
  public RangeLiteralFeatures withUpperBound(Boolean upperBound) {
    if (upperBound == null) {
      throw new IllegalArgumentException("null value for 'upperBound' argument");
    }
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
}