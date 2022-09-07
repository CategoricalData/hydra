package hydra.ext.java.syntax;

public class FormalParameter_Simple {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.FormalParameter.Simple");
  
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.UnannType type;
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public FormalParameter_Simple (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.UnannType type, hydra.ext.java.syntax.VariableDeclaratorId id) {
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
  
  public FormalParameter_Simple withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
  
  public FormalParameter_Simple withType(hydra.ext.java.syntax.UnannType type) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
  
  public FormalParameter_Simple withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
}