package hydra.ext.haskell.ast;

/**
 * An update record expression
 */
public class Expression_UpdateRecord {
  public final hydra.ext.haskell.ast.Expression inner;
  
  public final java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields;
  
  public Expression_UpdateRecord (hydra.ext.haskell.ast.Expression inner, java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    this.inner = inner;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_UpdateRecord)) {
      return false;
    }
    Expression_UpdateRecord o = (Expression_UpdateRecord) (other);
    return inner.equals(o.inner) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * fields.hashCode();
  }
  
  public Expression_UpdateRecord withInner(hydra.ext.haskell.ast.Expression inner) {
    return new Expression_UpdateRecord(inner, fields);
  }
  
  public Expression_UpdateRecord withFields(java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    return new Expression_UpdateRecord(inner, fields);
  }
}