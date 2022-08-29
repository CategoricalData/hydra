package hydra.ext.java.syntax;

public class MethodReference_Primary {
  public final hydra.ext.java.syntax.Primary primary;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodReference_Primary (hydra.ext.java.syntax.Primary primary, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    this.primary = primary;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Primary)) {
      return false;
    }
    MethodReference_Primary o = (MethodReference_Primary) (other);
    return primary.equals(o.primary) && typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * typeArguments.hashCode() + 5 * identifier.hashCode();
  }
  
  public MethodReference_Primary withPrimary(hydra.ext.java.syntax.Primary primary) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
  
  public MethodReference_Primary withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
  
  public MethodReference_Primary withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
}