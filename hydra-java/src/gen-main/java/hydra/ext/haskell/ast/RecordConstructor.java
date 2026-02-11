// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A record-style data constructor
 */
public class RecordConstructor implements Serializable, Comparable<RecordConstructor> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.RecordConstructor");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * The name of the constructor
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The named fields of the record
   */
  public final java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields;
  
  public RecordConstructor (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields) {
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordConstructor)) {
      return false;
    }
    RecordConstructor o = (RecordConstructor) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(fields);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RecordConstructor other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      fields.hashCode(),
      other.fields.hashCode());
  }
  
  public RecordConstructor withName(hydra.ext.haskell.ast.Name name) {
    return new RecordConstructor(name, fields);
  }
  
  public RecordConstructor withFields(java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields) {
    return new RecordConstructor(name, fields);
  }
}
