package hydra.ext.haskell.ast;

/**
 * A record constructor expression
 */
public class Expression_ConstructRecord {
  public final Name name;
  
  public final java.util.List<FieldUpdate> fields;
  
  public Expression_ConstructRecord (Name name, java.util.List<FieldUpdate> fields) {
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
  
  public Expression_ConstructRecord withName(Name name) {
    return new Expression_ConstructRecord(name, fields);
  }
  
  public Expression_ConstructRecord withFields(java.util.List<FieldUpdate> fields) {
    return new Expression_ConstructRecord(name, fields);
  }
}