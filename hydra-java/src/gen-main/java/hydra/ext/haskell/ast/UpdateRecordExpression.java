// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An update record expression
 */
public class UpdateRecordExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.UpdateRecordExpression");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.ext.haskell.ast.Expression inner;
  
  public final java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields;
  
  public UpdateRecordExpression (hydra.ext.haskell.ast.Expression inner, java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    java.util.Objects.requireNonNull((inner));
    java.util.Objects.requireNonNull((fields));
    this.inner = inner;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UpdateRecordExpression)) {
      return false;
    }
    UpdateRecordExpression o = (UpdateRecordExpression) (other);
    return inner.equals(o.inner) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * fields.hashCode();
  }
  
  public UpdateRecordExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    java.util.Objects.requireNonNull((inner));
    return new UpdateRecordExpression(inner, fields);
  }
  
  public UpdateRecordExpression withFields(java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    java.util.Objects.requireNonNull((fields));
    return new UpdateRecordExpression(inner, fields);
  }
}