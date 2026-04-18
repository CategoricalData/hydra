// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AnyRecordType implements Serializable, Comparable<AnyRecordType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AnyRecordType");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean any;

  public final Boolean notNull;

  public AnyRecordType (Boolean any, Boolean notNull) {
    this.any = any;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnyRecordType)) {
      return false;
    }
    AnyRecordType o = (AnyRecordType) other;
    return java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(any) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnyRecordType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      any,
      other.any);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public AnyRecordType withAny(Boolean any) {
    return new AnyRecordType(any, notNull);
  }

  public AnyRecordType withNotNull(Boolean notNull) {
    return new AnyRecordType(any, notNull);
  }
}
