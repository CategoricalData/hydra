package hydra.ext.java.syntax;

public class ExpressionName {
  public final java.util.Optional<AmbiguousName> qualifier;
  
  public final Identifier identifier;
  
  public ExpressionName (java.util.Optional<AmbiguousName> qualifier, Identifier identifier) {
    this.qualifier = qualifier;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExpressionName)) {
      return false;
    }
    ExpressionName o = (ExpressionName) (other);
    return qualifier.equals(o.qualifier) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifier.hashCode() + 3 * identifier.hashCode();
  }
  
  public ExpressionName withQualifier(java.util.Optional<AmbiguousName> qualifier) {
    return new ExpressionName(qualifier, identifier);
  }
  
  public ExpressionName withIdentifier(Identifier identifier) {
    return new ExpressionName(qualifier, identifier);
  }
}