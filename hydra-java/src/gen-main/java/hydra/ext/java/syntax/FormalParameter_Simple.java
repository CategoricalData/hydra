package hydra.ext.java.syntax;

public class FormalParameter_Simple {
  public final java.util.List<VariableModifier> modifiers;
  
  public final UnannType type;
  
  public final VariableDeclaratorId id;
  
  public FormalParameter_Simple (java.util.List<VariableModifier> modifiers, UnannType type, VariableDeclaratorId id) {
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FormalParameter_Simple)) {
      return false;
    }
    FormalParameter_Simple o = (FormalParameter_Simple) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && id.equals(o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * id.hashCode();
  }
  
  public FormalParameter_Simple withModifiers(java.util.List<VariableModifier> modifiers) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
  
  public FormalParameter_Simple withType(UnannType type) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
  
  public FormalParameter_Simple withId(VariableDeclaratorId id) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
}