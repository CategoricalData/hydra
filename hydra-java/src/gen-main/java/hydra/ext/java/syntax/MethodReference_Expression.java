package hydra.ext.java.syntax;

public class MethodReference_Expression {
  public final ExpressionName name;
  
  public final java.util.List<TypeArgument> typeArguments;
  
  public final Identifier identifier;
  
  public MethodReference_Expression (ExpressionName name, java.util.List<TypeArgument> typeArguments, Identifier identifier) {
    this.name = name;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Expression)) {
      return false;
    }
    MethodReference_Expression o = (MethodReference_Expression) (other);
    return name.equals(o.name) && typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typeArguments.hashCode() + 5 * identifier.hashCode();
  }
  
  public MethodReference_Expression withName(ExpressionName name) {
    return new MethodReference_Expression(name, typeArguments, identifier);
  }
  
  public MethodReference_Expression withTypeArguments(java.util.List<TypeArgument> typeArguments) {
    return new MethodReference_Expression(name, typeArguments, identifier);
  }
  
  public MethodReference_Expression withIdentifier(Identifier identifier) {
    return new MethodReference_Expression(name, typeArguments, identifier);
  }
}