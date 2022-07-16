package hydra.ext.java.syntax;

public class Resource_Local {
  public final java.util.List<VariableModifier> modifiers;
  
  public final LocalVariableType type;
  
  public final Identifier identifier;
  
  public final Expression expression;
  
  public Resource_Local (java.util.List<VariableModifier> modifiers, LocalVariableType type, Identifier identifier, Expression expression) {
    this.modifiers = modifiers;
    this.type = type;
    this.identifier = identifier;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Resource_Local)) {
      return false;
    }
    Resource_Local o = (Resource_Local) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && identifier.equals(o.identifier) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * identifier.hashCode() + 7 * expression.hashCode();
  }
  
  public Resource_Local withModifiers(java.util.List<VariableModifier> modifiers) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }
  
  public Resource_Local withType(LocalVariableType type) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }
  
  public Resource_Local withIdentifier(Identifier identifier) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }
  
  public Resource_Local withExpression(Expression expression) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }
}