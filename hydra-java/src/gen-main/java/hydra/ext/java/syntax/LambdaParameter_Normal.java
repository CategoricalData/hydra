package hydra.ext.java.syntax;

public class LambdaParameter_Normal {
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.LambdaParameterType type;
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public LambdaParameter_Normal (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.LambdaParameterType type, hydra.ext.java.syntax.VariableDeclaratorId id) {
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaParameter_Normal)) {
      return false;
    }
    LambdaParameter_Normal o = (LambdaParameter_Normal) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && id.equals(o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * id.hashCode();
  }
  
  public LambdaParameter_Normal withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    return new LambdaParameter_Normal(modifiers, type, id);
  }
  
  public LambdaParameter_Normal withType(hydra.ext.java.syntax.LambdaParameterType type) {
    return new LambdaParameter_Normal(modifiers, type, id);
  }
  
  public LambdaParameter_Normal withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    return new LambdaParameter_Normal(modifiers, type, id);
  }
}