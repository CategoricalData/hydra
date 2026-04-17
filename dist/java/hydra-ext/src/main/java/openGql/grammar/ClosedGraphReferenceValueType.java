// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ClosedGraphReferenceValueType implements Serializable, Comparable<ClosedGraphReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ClosedGraphReferenceValueType");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name NESTED_SPEC = new hydra.core.Name("nestedSpec");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean property;

  public final java.util.List<openGql.grammar.ElementTypeSpecification> nestedSpec;

  public final Boolean notNull;

  public ClosedGraphReferenceValueType (Boolean property, java.util.List<openGql.grammar.ElementTypeSpecification> nestedSpec, Boolean notNull) {
    this.property = property;
    this.nestedSpec = nestedSpec;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClosedGraphReferenceValueType)) {
      return false;
    }
    ClosedGraphReferenceValueType o = (ClosedGraphReferenceValueType) other;
    return java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.nestedSpec,
      o.nestedSpec) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(property) + 3 * java.util.Objects.hashCode(nestedSpec) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClosedGraphReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      nestedSpec,
      other.nestedSpec);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public ClosedGraphReferenceValueType withProperty(Boolean property) {
    return new ClosedGraphReferenceValueType(property, nestedSpec, notNull);
  }

  public ClosedGraphReferenceValueType withNestedSpec(java.util.List<openGql.grammar.ElementTypeSpecification> nestedSpec) {
    return new ClosedGraphReferenceValueType(property, nestedSpec, notNull);
  }

  public ClosedGraphReferenceValueType withNotNull(Boolean notNull) {
    return new ClosedGraphReferenceValueType(property, nestedSpec, notNull);
  }
}
