package hydra.ext.haskell.ast;

/**
 * A record constructor expression
 */
public class Expression_ConstructRecord {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.ConstructRecord");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields;
  
  public Expression_ConstructRecord (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_ConstructRecord)) {
      return false;
    }
    Expression_ConstructRecord o = (Expression_ConstructRecord) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public Expression_ConstructRecord withName(hydra.ext.haskell.ast.Name name) {
    return new Expression_ConstructRecord(name, fields);
  }
  
  public Expression_ConstructRecord withFields(java.util.List<hydra.ext.haskell.ast.FieldUpdate> fields) {
    return new Expression_ConstructRecord(name, fields);
  }
}