// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A record, or labeled tuple; a map of field names to terms
 */
public class Record implements Serializable, Comparable<Record> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Record");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * The name of the record type
   */
  public final hydra.core.Name typeName;
  
  /**
   * The fields of the record, as a list of name/term pairs
   */
  public final java.util.List<hydra.core.Field> fields;
  
  public Record (hydra.core.Name typeName, java.util.List<hydra.core.Field> fields) {
    this.typeName = typeName;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Record)) {
      return false;
    }
    Record o = (Record) (other);
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(fields);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Record other) {
    int cmp = 0;
    cmp = ((Comparable) (typeName)).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      fields.hashCode(),
      other.fields.hashCode());
  }
  
  public Record withTypeName(hydra.core.Name typeName) {
    return new Record(typeName, fields);
  }
  
  public Record withFields(java.util.List<hydra.core.Field> fields) {
    return new Record(typeName, fields);
  }
}
