// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CatchFormalParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.CatchFormalParameter");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.CatchType type;
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public CatchFormalParameter (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.CatchType type, hydra.ext.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((id));
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
  
  public CatchFormalParameter withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withType(hydra.ext.java.syntax.CatchType type) {
    java.util.Objects.requireNonNull((type));
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((id));
    return new CatchFormalParameter(modifiers, type, id);
  }
}
