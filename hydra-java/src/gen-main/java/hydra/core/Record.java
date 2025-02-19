// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A record, or labeled tuple; a map of field names to terms
 */
public class Record implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Record");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.core.Name typeName;
  
  public final java.util.List<hydra.core.Field> fields;
  
  public Record (hydra.core.Name typeName, java.util.List<hydra.core.Field> fields) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((fields));
    this.typeName = typeName;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Record)) {
      return false;
    }
    Record o = (Record) (other);
    return typeName.equals(o.typeName) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * fields.hashCode();
  }
  
  public Record withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new Record(typeName, fields);
  }
  
  public Record withFields(java.util.List<hydra.core.Field> fields) {
    java.util.Objects.requireNonNull((fields));
    return new Record(typeName, fields);
  }
}