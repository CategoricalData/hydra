// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OpenGraphReferenceValueType implements Serializable, Comparable<OpenGraphReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OpenGraphReferenceValueType");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<Boolean> any;

  public final Boolean property;

  public final Boolean notNull;

  public OpenGraphReferenceValueType (hydra.util.Maybe<Boolean> any, Boolean property, Boolean notNull) {
    this.any = any;
    this.property = property;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenGraphReferenceValueType)) {
      return false;
    }
    OpenGraphReferenceValueType o = (OpenGraphReferenceValueType) other;
    return java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(any) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OpenGraphReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      any,
      other.any);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public OpenGraphReferenceValueType withAny(hydra.util.Maybe<Boolean> any) {
    return new OpenGraphReferenceValueType(any, property, notNull);
  }

  public OpenGraphReferenceValueType withProperty(Boolean property) {
    return new OpenGraphReferenceValueType(any, property, notNull);
  }

  public OpenGraphReferenceValueType withNotNull(Boolean notNull) {
    return new OpenGraphReferenceValueType(any, property, notNull);
  }
}
