// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Range literals within relationship patterns
 */
public class RangeLiteralFeatures implements Serializable, Comparable<RangeLiteralFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.RangeLiteralFeatures");

  public static final hydra.core.Name BOUNDS = new hydra.core.Name("bounds");

  public static final hydra.core.Name EXACT_RANGE = new hydra.core.Name("exactRange");

  public static final hydra.core.Name LOWER_BOUND = new hydra.core.Name("lowerBound");

  public static final hydra.core.Name STAR_RANGE = new hydra.core.Name("starRange");

  public static final hydra.core.Name UPPER_BOUND = new hydra.core.Name("upperBound");

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
    RangeLiteralFeatures o = (RangeLiteralFeatures) other;
    return java.util.Objects.equals(
      this.bounds,
      o.bounds) && java.util.Objects.equals(
      this.exactRange,
      o.exactRange) && java.util.Objects.equals(
      this.lowerBound,
      o.lowerBound) && java.util.Objects.equals(
      this.starRange,
      o.starRange) && java.util.Objects.equals(
      this.upperBound,
      o.upperBound);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bounds) + 3 * java.util.Objects.hashCode(exactRange) + 5 * java.util.Objects.hashCode(lowerBound) + 7 * java.util.Objects.hashCode(starRange) + 11 * java.util.Objects.hashCode(upperBound);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RangeLiteralFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      bounds,
      other.bounds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      exactRange,
      other.exactRange);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      lowerBound,
      other.lowerBound);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      starRange,
      other.starRange);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      upperBound,
      other.upperBound);
  }

  public RangeLiteralFeatures withBounds(Boolean bounds) {
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }

  public RangeLiteralFeatures withExactRange(Boolean exactRange) {
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }

  public RangeLiteralFeatures withLowerBound(Boolean lowerBound) {
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }

  public RangeLiteralFeatures withStarRange(Boolean starRange) {
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }

  public RangeLiteralFeatures withUpperBound(Boolean upperBound) {
    return new RangeLiteralFeatures(bounds, exactRange, lowerBound, starRange, upperBound);
  }
}
