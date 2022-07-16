package hydra.ext.java.syntax;

public class MethodReference_Primary {
  public final Primary primary;
  
  public final java.util.List<TypeArgument> typeArguments;
  
  public final Identifier identifier;
  
  public MethodReference_Primary (Primary primary, java.util.List<TypeArgument> typeArguments, Identifier identifier) {
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
  
  public MethodReference_Primary withPrimary(Primary primary) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
  
  public MethodReference_Primary withTypeArguments(java.util.List<TypeArgument> typeArguments) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
  
  public MethodReference_Primary withIdentifier(Identifier identifier) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
}