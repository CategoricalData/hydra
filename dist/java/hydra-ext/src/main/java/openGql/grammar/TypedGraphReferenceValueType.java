// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TypedGraphReferenceValueType implements Serializable, Comparable<TypedGraphReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TypedGraphReferenceValueType");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");

  public final hydra.util.Maybe<java.lang.Void> typed;

  public final openGql.grammar.GraphReferenceValueType valueType;

  public TypedGraphReferenceValueType (hydra.util.Maybe<java.lang.Void> typed, openGql.grammar.GraphReferenceValueType valueType) {
    this.typed = typed;
    this.valueType = valueType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedGraphReferenceValueType)) {
      return false;
    }
    TypedGraphReferenceValueType o = (TypedGraphReferenceValueType) other;
    return java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.valueType,
      o.valueType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typed) + 3 * java.util.Objects.hashCode(valueType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypedGraphReferenceValueType other) {
    int cmp = 0;
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

  public TypedGraphReferenceValueType withTyped(hydra.util.Maybe<java.lang.Void> typed) {
    return new TypedGraphReferenceValueType(typed, valueType);
  }

  public TypedGraphReferenceValueType withValueType(openGql.grammar.GraphReferenceValueType valueType) {
    return new TypedGraphReferenceValueType(typed, valueType);
  }
}
