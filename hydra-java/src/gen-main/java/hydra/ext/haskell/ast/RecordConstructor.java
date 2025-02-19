// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A record-style data constructor
 */
public class RecordConstructor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.RecordConstructor");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields;
  
  public RecordConstructor (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((fields));
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordConstructor)) {
      return false;
    }
    RecordConstructor o = (RecordConstructor) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public RecordConstructor withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new RecordConstructor(name, fields);
  }
  
  public RecordConstructor withFields(java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields) {
    java.util.Objects.requireNonNull((fields));
    return new RecordConstructor(name, fields);
  }
}