// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class CatchFormalParameter implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.CatchFormalParameter");
  
  public final java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers;
  
  public final hydra.langs.java.syntax.CatchType type;
  
  public final hydra.langs.java.syntax.VariableDeclaratorId id;
  
  public CatchFormalParameter (java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers, hydra.langs.java.syntax.CatchType type, hydra.langs.java.syntax.VariableDeclaratorId id) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (id == null) {
      throw new IllegalArgumentException("null value for 'id' argument");
    }
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
  
  public CatchFormalParameter withModifiers(java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withType(hydra.langs.java.syntax.CatchType type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withId(hydra.langs.java.syntax.VariableDeclaratorId id) {
    if (id == null) {
      throw new IllegalArgumentException("null value for 'id' argument");
    }
    return new CatchFormalParameter(modifiers, type, id);
  }
}