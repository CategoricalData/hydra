// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ValueTypePredicatePart2 implements Serializable, Comparable<ValueTypePredicatePart2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ValueTypePredicatePart2");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");

  public final Boolean not;

  public final java.lang.Void typed;

  public final openGql.grammar.ValueType valueType;

  public ValueTypePredicatePart2 (Boolean not, java.lang.Void typed, openGql.grammar.ValueType valueType) {
    this.not = not;
    this.typed = typed;
    this.valueType = valueType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueTypePredicatePart2)) {
      return false;
    }
    ValueTypePredicatePart2 o = (ValueTypePredicatePart2) other;
    return java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.valueType,
      o.valueType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(not) + 3 * java.util.Objects.hashCode(typed) + 5 * java.util.Objects.hashCode(valueType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ValueTypePredicatePart2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      not,
      other.not);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typed,
      other.typed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueType,
      other.valueType);
  }

  public ValueTypePredicatePart2 withNot(Boolean not) {
    return new ValueTypePredicatePart2(not, typed, valueType);
  }

  public ValueTypePredicatePart2 withTyped(java.lang.Void typed) {
    return new ValueTypePredicatePart2(not, typed, valueType);
  }

  public ValueTypePredicatePart2 withValueType(openGql.grammar.ValueType valueType) {
    return new ValueTypePredicatePart2(not, typed, valueType);
  }
}
