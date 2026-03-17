// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Temporal duration functions
 */
public class TemporalDurationFunctionFeatures implements Serializable, Comparable<TemporalDurationFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.TemporalDurationFunctionFeatures");

  public static final hydra.core.Name DURATION = new hydra.core.Name("duration");

  public static final hydra.core.Name DURATION_BETWEEN = new hydra.core.Name("duration.between");

  public static final hydra.core.Name DURATION_IN_DAYS = new hydra.core.Name("duration.inDays");

  public static final hydra.core.Name DURATION_IN_MONTHS = new hydra.core.Name("duration.inMonths");

  public static final hydra.core.Name DURATION_IN_SECONDS = new hydra.core.Name("duration.inSeconds");

  /**
   * The duration() function. Constructs a DURATION value.
   */
  public final Boolean duration;

  /**
   * The duration.between() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in logical units.
   */
  public final Boolean duration_between;

  /**
   * The duration.inDays() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in days.
   */
  public final Boolean duration_inDays;

  /**
   * The duration.inMonths() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in months.
   */
  public final Boolean duration_inMonths;

  /**
   * The duration.inSeconds() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in seconds.
   */
  public final Boolean duration_inSeconds;

  public TemporalDurationFunctionFeatures (Boolean duration, Boolean duration_between, Boolean duration_inDays, Boolean duration_inMonths, Boolean duration_inSeconds) {
    this.duration = duration;
    this.duration_between = duration_between;
    this.duration_inDays = duration_inDays;
    this.duration_inMonths = duration_inMonths;
    this.duration_inSeconds = duration_inSeconds;
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
      this.duration_between,
      o.duration_between) && java.util.Objects.equals(
      this.duration_inDays,
      o.duration_inDays) && java.util.Objects.equals(
      this.duration_inMonths,
      o.duration_inMonths) && java.util.Objects.equals(
      this.duration_inSeconds,
      o.duration_inSeconds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(duration) + 3 * java.util.Objects.hashCode(duration_between) + 5 * java.util.Objects.hashCode(duration_inDays) + 7 * java.util.Objects.hashCode(duration_inMonths) + 11 * java.util.Objects.hashCode(duration_inSeconds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemporalDurationFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) duration).compareTo(other.duration);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) duration_between).compareTo(other.duration_between);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) duration_inDays).compareTo(other.duration_inDays);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) duration_inMonths).compareTo(other.duration_inMonths);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) duration_inSeconds).compareTo(other.duration_inSeconds);
  }

  public TemporalDurationFunctionFeatures withDuration(Boolean duration) {
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }

  public TemporalDurationFunctionFeatures withDuration_between(Boolean duration_between) {
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }

  public TemporalDurationFunctionFeatures withDuration_inDays(Boolean duration_inDays) {
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }

  public TemporalDurationFunctionFeatures withDuration_inMonths(Boolean duration_inMonths) {
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }

  public TemporalDurationFunctionFeatures withDuration_inSeconds(Boolean duration_inSeconds) {
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }
}
