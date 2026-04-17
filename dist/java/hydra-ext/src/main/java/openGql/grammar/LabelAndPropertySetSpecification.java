// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class LabelAndPropertySetSpecification implements Serializable, Comparable<LabelAndPropertySetSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LabelAndPropertySetSpecification");

  public static final hydra.core.Name IS_OR_COLON = new hydra.core.Name("isOrColon");

  public static final hydra.core.Name LABEL_SET = new hydra.core.Name("labelSet");

  public static final hydra.core.Name PROPERTY_SPECIFICATION = new hydra.core.Name("propertySpecification");

  public final hydra.util.Maybe<openGql.grammar.IsOrColon> isOrColon;

  public final hydra.util.Maybe<java.util.List<String>> labelSet;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.PropertyKeyValuePair>> propertySpecification;

  public LabelAndPropertySetSpecification (hydra.util.Maybe<openGql.grammar.IsOrColon> isOrColon, hydra.util.Maybe<java.util.List<String>> labelSet, hydra.util.Maybe<java.util.List<openGql.grammar.PropertyKeyValuePair>> propertySpecification) {
    this.isOrColon = isOrColon;
    this.labelSet = labelSet;
    this.propertySpecification = propertySpecification;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabelAndPropertySetSpecification)) {
      return false;
    }
    LabelAndPropertySetSpecification o = (LabelAndPropertySetSpecification) other;
    return java.util.Objects.equals(
      this.isOrColon,
      o.isOrColon) && java.util.Objects.equals(
      this.labelSet,
      o.labelSet) && java.util.Objects.equals(
      this.propertySpecification,
      o.propertySpecification);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isOrColon) + 3 * java.util.Objects.hashCode(labelSet) + 5 * java.util.Objects.hashCode(propertySpecification);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabelAndPropertySetSpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      isOrColon,
      other.isOrColon);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      labelSet,
      other.labelSet);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      propertySpecification,
      other.propertySpecification);
  }

  public LabelAndPropertySetSpecification withIsOrColon(hydra.util.Maybe<openGql.grammar.IsOrColon> isOrColon) {
    return new LabelAndPropertySetSpecification(isOrColon, labelSet, propertySpecification);
  }

  public LabelAndPropertySetSpecification withLabelSet(hydra.util.Maybe<java.util.List<String>> labelSet) {
    return new LabelAndPropertySetSpecification(isOrColon, labelSet, propertySpecification);
  }

  public LabelAndPropertySetSpecification withPropertySpecification(hydra.util.Maybe<java.util.List<openGql.grammar.PropertyKeyValuePair>> propertySpecification) {
    return new LabelAndPropertySetSpecification(isOrColon, labelSet, propertySpecification);
  }
}
