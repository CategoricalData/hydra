// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A labeled record or union type
 */
public class RowType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.RowType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * The name of the row type, which must correspond to the name of a Type element
   */
  public final hydra.core.Name typeName;
  
  /**
   * The fields of this row type, excluding any inherited fields
   */
  public final java.util.List<hydra.core.FieldType> fields;
  
  public RowType (hydra.core.Name typeName, java.util.List<hydra.core.FieldType> fields) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((fields));
    this.typeName = typeName;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RowType)) {
      return false;
    }
    RowType o = (RowType) (other);
    return typeName.equals(o.typeName) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * fields.hashCode();
  }
  
  public RowType withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new RowType(typeName, fields);
  }
  
  public RowType withFields(java.util.List<hydra.core.FieldType> fields) {
    java.util.Objects.requireNonNull((fields));
    return new RowType(typeName, fields);
  }
}
