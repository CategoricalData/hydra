package hydra.ext.haskell.ast;

/**
 * An update record expression
 */
public class Expression_UpdateRecord {
  public final Expression inner;
  
  public final java.util.List<FieldUpdate> fields;
  
  public Expression_UpdateRecord (Expression inner, java.util.List<FieldUpdate> fields) {
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
  
  public Expression_UpdateRecord withInner(Expression inner) {
    return new Expression_UpdateRecord(inner, fields);
  }
  
  public Expression_UpdateRecord withFields(java.util.List<FieldUpdate> fields) {
    return new Expression_UpdateRecord(inner, fields);
  }
}