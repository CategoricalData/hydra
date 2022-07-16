package hydra.ext.java.syntax;

public class FieldAccess {
  public final FieldAccess_Qualifier qualifier;
  
  public final Identifier identifier;
  
  public FieldAccess (FieldAccess_Qualifier qualifier, Identifier identifier) {
    this.qualifier = qualifier;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldAccess)) {
      return false;
    }
    FieldAccess o = (FieldAccess) (other);
    return qualifier.equals(o.qualifier) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifier.hashCode() + 3 * identifier.hashCode();
  }
  
  public FieldAccess withQualifier(FieldAccess_Qualifier qualifier) {
    return new FieldAccess(qualifier, identifier);
  }
  
  public FieldAccess withIdentifier(Identifier identifier) {
    return new FieldAccess(qualifier, identifier);
  }
}