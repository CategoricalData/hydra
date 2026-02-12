// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class FormalParameter_Simple implements Serializable, Comparable<FormalParameter_Simple> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.FormalParameter_Simple");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
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
    FormalParameter_Simple o = (FormalParameter_Simple) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.id,
      o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(id);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FormalParameter_Simple other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) id).compareTo(other.id);
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
