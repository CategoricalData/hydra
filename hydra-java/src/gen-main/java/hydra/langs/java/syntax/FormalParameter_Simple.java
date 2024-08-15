// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class FormalParameter_Simple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.FormalParameter.Simple");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public final java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers;
  
  public final hydra.langs.java.syntax.UnannType type;
  
  public final hydra.langs.java.syntax.VariableDeclaratorId id;
  
  public FormalParameter_Simple (java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers, hydra.langs.java.syntax.UnannType type, hydra.langs.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((id));
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
  
  public FormalParameter_Simple withModifiers(java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new FormalParameter_Simple(modifiers, type, id);
  }
  
  public FormalParameter_Simple withType(hydra.langs.java.syntax.UnannType type) {
    java.util.Objects.requireNonNull((type));
    return new FormalParameter_Simple(modifiers, type, id);
  }
  
  public FormalParameter_Simple withId(hydra.langs.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((id));
    return new FormalParameter_Simple(modifiers, type, id);
  }
}