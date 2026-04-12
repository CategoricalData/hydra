// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Temporal duration functions
 */
public class TemporalDurationFunctionFeatures implements Serializable, Comparable<TemporalDurationFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.TemporalDurationFunctionFeatures");

  public static final hydra.core.Name DURATION = new hydra.core.Name("duration");

  public static final hydra.core.Name DURATION_BETWEEN = new hydra.core.Name("durationBetween");

  public static final hydra.core.Name DURATION_IN_DAYS = new hydra.core.Name("durationInDays");

  public static final hydra.core.Name DURATION_IN_MONTHS = new hydra.core.Name("durationInMonths");

  public static final hydra.core.Name DURATION_IN_SECONDS = new hydra.core.Name("durationInSeconds");

  /**
   * The duration() function. Constructs a DURATION value.
   */
  public final Boolean duration;

  /**
   * The duration.between() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in logical units.
   */
  public final Boolean durationBetween;

  /**
   * The duration.inDays() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in days.
   */
  public final Boolean durationInDays;

  /**
   * The duration.inMonths() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in months.
   */
  public final Boolean durationInMonths;

  /**
   * The duration.inSeconds() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in seconds.
   */
  public final Boolean durationInSeconds;

  public TemporalDurationFunctionFeatures (Boolean duration, Boolean durationBetween, Boolean durationInDays, Boolean durationInMonths, Boolean durationInSeconds) {
    this.duration = duration;
    this.durationBetween = durationBetween;
    this.durationInDays = durationInDays;
    this.durationInMonths = durationInMonths;
    this.durationInSeconds = durationInSeconds;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TemporalDurationFunctionFeatures)) {
      return false;
    }
    TemporalDurationFunctionFeatures o = (TemporalDurationFunctionFeatures) other;
    return java.util.Objects.equals(
      this.duration,
      o.duration) && java.util.Objects.equals(
      this.durationBetween,
      o.durationBetween) && java.util.Objects.equals(
      this.durationInDays,
      o.durationInDays) && java.util.Objects.equals(
      this.durationInMonths,
      o.durationInMonths) && java.util.Objects.equals(
      this.durationInSeconds,
      o.durationInSeconds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(duration) + 3 * java.util.Objects.hashCode(durationBetween) + 5 * java.util.Objects.hashCode(durationInDays) + 7 * java.util.Objects.hashCode(durationInMonths) + 11 * java.util.Objects.hashCode(durationInSeconds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemporalDurationFunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      duration,
      other.duration);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      durationBetween,
      other.durationBetween);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      durationInDays,
      other.durationInDays);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      durationInMonths,
      other.durationInMonths);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      durationInSeconds,
      other.durationInSeconds);
  }

  public TemporalDurationFunctionFeatures withDuration(Boolean duration) {
    return new TemporalDurationFunctionFeatures(duration, durationBetween, durationInDays, durationInMonths, durationInSeconds);
  }

  public TemporalDurationFunctionFeatures withDurationBetween(Boolean durationBetween) {
    return new TemporalDurationFunctionFeatures(duration, durationBetween, durationInDays, durationInMonths, durationInSeconds);
  }

  public TemporalDurationFunctionFeatures withDurationInDays(Boolean durationInDays) {
    return new TemporalDurationFunctionFeatures(duration, durationBetween, durationInDays, durationInMonths, durationInSeconds);
  }

  public TemporalDurationFunctionFeatures withDurationInMonths(Boolean durationInMonths) {
    return new TemporalDurationFunctionFeatures(duration, durationBetween, durationInDays, durationInMonths, durationInSeconds);
  }

  public TemporalDurationFunctionFeatures withDurationInSeconds(Boolean durationInSeconds) {
    return new TemporalDurationFunctionFeatures(duration, durationBetween, durationInDays, durationInMonths, durationInSeconds);
  }
}
