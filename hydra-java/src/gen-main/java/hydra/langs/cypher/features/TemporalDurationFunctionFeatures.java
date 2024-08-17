// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Temporal duration functions
 */
public class TemporalDurationFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.TemporalDurationFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DURATION = new hydra.core.Name("duration");
  
  public static final hydra.core.Name FIELD_NAME_DURATION.BETWEEN = new hydra.core.Name("duration.between");
  
  public static final hydra.core.Name FIELD_NAME_DURATION.IN_DAYS = new hydra.core.Name("duration.inDays");
  
  public static final hydra.core.Name FIELD_NAME_DURATION.IN_MONTHS = new hydra.core.Name("duration.inMonths");
  
  public static final hydra.core.Name FIELD_NAME_DURATION.IN_SECONDS = new hydra.core.Name("duration.inSeconds");
  
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
    java.util.Objects.requireNonNull((duration));
    java.util.Objects.requireNonNull((duration_between));
    java.util.Objects.requireNonNull((duration_inDays));
    java.util.Objects.requireNonNull((duration_inMonths));
    java.util.Objects.requireNonNull((duration_inSeconds));
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
    TemporalDurationFunctionFeatures o = (TemporalDurationFunctionFeatures) (other);
    return duration.equals(o.duration) && duration_between.equals(o.duration_between) && duration_inDays.equals(o.duration_inDays) && duration_inMonths.equals(o.duration_inMonths) && duration_inSeconds.equals(o.duration_inSeconds);
  }
  
  @Override
  public int hashCode() {
    return 2 * duration.hashCode() + 3 * duration_between.hashCode() + 5 * duration_inDays.hashCode() + 7 * duration_inMonths.hashCode() + 11 * duration_inSeconds.hashCode();
  }
  
  public TemporalDurationFunctionFeatures withDuration(Boolean duration) {
    java.util.Objects.requireNonNull((duration));
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }
  
  public TemporalDurationFunctionFeatures withDuration.between(Boolean duration_between) {
    java.util.Objects.requireNonNull((duration_between));
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }
  
  public TemporalDurationFunctionFeatures withDuration.inDays(Boolean duration_inDays) {
    java.util.Objects.requireNonNull((duration_inDays));
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }
  
  public TemporalDurationFunctionFeatures withDuration.inMonths(Boolean duration_inMonths) {
    java.util.Objects.requireNonNull((duration_inMonths));
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }
  
  public TemporalDurationFunctionFeatures withDuration.inSeconds(Boolean duration_inSeconds) {
    java.util.Objects.requireNonNull((duration_inSeconds));
    return new TemporalDurationFunctionFeatures(duration, duration_between, duration_inDays, duration_inMonths, duration_inSeconds);
  }
}