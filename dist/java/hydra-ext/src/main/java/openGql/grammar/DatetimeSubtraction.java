// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DatetimeSubtraction implements Serializable, Comparable<DatetimeSubtraction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DatetimeSubtraction");

  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");

  public static final hydra.core.Name TEMPORAL_DURATION_QUALIFIER = new hydra.core.Name("temporalDurationQualifier");

  public final openGql.grammar.DatetimeSubtractionParameters parameters;

  public final hydra.util.Maybe<openGql.grammar.TemporalDurationQualifier> temporalDurationQualifier;

  public DatetimeSubtraction (openGql.grammar.DatetimeSubtractionParameters parameters, hydra.util.Maybe<openGql.grammar.TemporalDurationQualifier> temporalDurationQualifier) {
    this.parameters = parameters;
    this.temporalDurationQualifier = temporalDurationQualifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatetimeSubtraction)) {
      return false;
    }
    DatetimeSubtraction o = (DatetimeSubtraction) other;
    return java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.temporalDurationQualifier,
      o.temporalDurationQualifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameters) + 3 * java.util.Objects.hashCode(temporalDurationQualifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatetimeSubtraction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parameters,
      other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      temporalDurationQualifier,
      other.temporalDurationQualifier);
  }

  public DatetimeSubtraction withParameters(openGql.grammar.DatetimeSubtractionParameters parameters) {
    return new DatetimeSubtraction(parameters, temporalDurationQualifier);
  }

  public DatetimeSubtraction withTemporalDurationQualifier(hydra.util.Maybe<openGql.grammar.TemporalDurationQualifier> temporalDurationQualifier) {
    return new DatetimeSubtraction(parameters, temporalDurationQualifier);
  }
}
