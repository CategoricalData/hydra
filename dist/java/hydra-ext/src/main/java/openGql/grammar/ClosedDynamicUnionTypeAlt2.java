// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ClosedDynamicUnionTypeAlt2 implements Serializable, Comparable<ClosedDynamicUnionTypeAlt2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ClosedDynamicUnionTypeAlt2");

  public static final hydra.core.Name VALUE_TYPES = new hydra.core.Name("valueTypes");

  public final java.util.List<openGql.grammar.ValueType> valueTypes;

  public ClosedDynamicUnionTypeAlt2 (java.util.List<openGql.grammar.ValueType> valueTypes) {
    this.valueTypes = valueTypes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClosedDynamicUnionTypeAlt2)) {
      return false;
    }
    ClosedDynamicUnionTypeAlt2 o = (ClosedDynamicUnionTypeAlt2) other;
    return java.util.Objects.equals(
      this.valueTypes,
      o.valueTypes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueTypes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClosedDynamicUnionTypeAlt2 other) {
    return hydra.util.Comparing.compare(
      valueTypes,
      other.valueTypes);
  }
}
