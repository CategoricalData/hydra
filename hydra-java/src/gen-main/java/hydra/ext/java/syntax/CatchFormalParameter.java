package hydra.ext.java.syntax;

public class CatchFormalParameter {
  public final java.util.List<VariableModifier> modifiers;
  
  public final CatchType type;
  
  public final VariableDeclaratorId id;
  
  public CatchFormalParameter (java.util.List<VariableModifier> modifiers, CatchType type, VariableDeclaratorId id) {
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchFormalParameter)) {
      return false;
    }
    CatchFormalParameter o = (CatchFormalParameter) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && id.equals(o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * id.hashCode();
  }
  
  public CatchFormalParameter withModifiers(java.util.List<VariableModifier> modifiers) {
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withType(CatchType type) {
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withId(VariableDeclaratorId id) {
    return new CatchFormalParameter(modifiers, type, id);
  }
}