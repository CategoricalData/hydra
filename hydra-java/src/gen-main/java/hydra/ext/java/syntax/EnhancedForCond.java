package hydra.ext.java.syntax;

public class EnhancedForCond {
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.LocalVariableType type;
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public EnhancedForCond (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.LocalVariableType type, hydra.ext.java.syntax.VariableDeclaratorId id, hydra.ext.java.syntax.Expression expression) {
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnhancedForCond)) {
      return false;
    }
    EnhancedForCond o = (EnhancedForCond) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && id.equals(o.id) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * id.hashCode() + 7 * expression.hashCode();
  }
  
  public EnhancedForCond withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }
  
  public EnhancedForCond withType(hydra.ext.java.syntax.LocalVariableType type) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }
  
  public EnhancedForCond withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }
  
  public EnhancedForCond withExpression(hydra.ext.java.syntax.Expression expression) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }
}