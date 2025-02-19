// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A record constructor expression
 */
public class ConstructRecordExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ConstructRecordExpression");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields;
  
  public ConstructRecordExpression (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((fields));
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructRecordExpression)) {
      return false;
    }
    ConstructRecordExpression o = (ConstructRecordExpression) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public ConstructRecordExpression withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ConstructRecordExpression(name, fields);
  }
  
  public ConstructRecordExpression withFields(java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    java.util.Objects.requireNonNull((fields));
    return new ConstructRecordExpression(name, fields);
  }
}