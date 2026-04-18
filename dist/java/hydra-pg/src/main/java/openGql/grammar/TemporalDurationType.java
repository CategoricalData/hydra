// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TemporalDurationType implements Serializable, Comparable<TemporalDurationType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TemporalDurationType");

  public static final hydra.core.Name QUALIFIER = new hydra.core.Name("qualifier");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final openGql.grammar.TemporalDurationQualifier qualifier;

  public final Boolean notNull;

  public TemporalDurationType (openGql.grammar.TemporalDurationQualifier qualifier, Boolean notNull) {
    this.qualifier = qualifier;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TemporalDurationType)) {
      return false;
    }
    TemporalDurationType o = (TemporalDurationType) other;
    return java.util.Objects.equals(
      this.qualifier,
      o.qualifier) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualifier) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemporalDurationType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qualifier,
      other.qualifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public TemporalDurationType withQualifier(openGql.grammar.TemporalDurationQualifier qualifier) {
    return new TemporalDurationType(qualifier, notNull);
  }

  public TemporalDurationType withNotNull(Boolean notNull) {
    return new TemporalDurationType(qualifier, notNull);
  }
}
