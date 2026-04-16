// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SpecifiedRecordType implements Serializable, Comparable<SpecifiedRecordType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SpecifiedRecordType");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  public static final hydra.core.Name FIELD_TYPES = new hydra.core.Name("fieldTypes");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean record;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.FieldType>> fieldTypes;

  public final Boolean notNull;

  public SpecifiedRecordType (Boolean record, hydra.util.Maybe<java.util.List<openGql.grammar.FieldType>> fieldTypes, Boolean notNull) {
    this.record = record;
    this.fieldTypes = fieldTypes;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpecifiedRecordType)) {
      return false;
    }
    SpecifiedRecordType o = (SpecifiedRecordType) other;
    return java.util.Objects.equals(
      this.record,
      o.record) && java.util.Objects.equals(
      this.fieldTypes,
      o.fieldTypes) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(record) + 3 * java.util.Objects.hashCode(fieldTypes) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SpecifiedRecordType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      record,
      other.record);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      fieldTypes,
      other.fieldTypes);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public SpecifiedRecordType withRecord(Boolean record) {
    return new SpecifiedRecordType(record, fieldTypes, notNull);
  }

  public SpecifiedRecordType withFieldTypes(hydra.util.Maybe<java.util.List<openGql.grammar.FieldType>> fieldTypes) {
    return new SpecifiedRecordType(record, fieldTypes, notNull);
  }

  public SpecifiedRecordType withNotNull(Boolean notNull) {
    return new SpecifiedRecordType(record, fieldTypes, notNull);
  }
}
