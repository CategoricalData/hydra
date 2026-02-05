// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An update record expression
 */
public class UpdateRecordExpression implements Serializable, Comparable<UpdateRecordExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.UpdateRecordExpression");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * The record being updated
   */
  public final hydra.ext.haskell.ast.Expression inner;
  
  /**
   * The field updates
   */
  public final java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields;
  
  public UpdateRecordExpression (hydra.ext.haskell.ast.Expression inner, java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    this.inner = inner;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UpdateRecordExpression)) {
      return false;
    }
    UpdateRecordExpression o = (UpdateRecordExpression) (other);
    return java.util.Objects.equals(
      this.inner,
      o.inner) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(inner) + 3 * java.util.Objects.hashCode(fields);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UpdateRecordExpression other) {
    int cmp = 0;
    cmp = ((Comparable) (inner)).compareTo(other.inner);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      fields.hashCode(),
      other.fields.hashCode());
  }
  
  public UpdateRecordExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    return new UpdateRecordExpression(inner, fields);
  }
  
  public UpdateRecordExpression withFields(java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    return new UpdateRecordExpression(inner, fields);
  }
}
