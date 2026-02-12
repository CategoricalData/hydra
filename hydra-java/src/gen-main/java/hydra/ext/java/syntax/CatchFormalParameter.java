// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CatchFormalParameter implements Serializable, Comparable<CatchFormalParameter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CatchFormalParameter");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.CatchType type;
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public CatchFormalParameter (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.CatchType type, hydra.ext.java.syntax.VariableDeclaratorId id) {
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchFormalParameter)) {
      return false;
    }
    CatchFormalParameter o = (CatchFormalParameter) other;
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
  public int compareTo(CatchFormalParameter other) {
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
  
  public CatchFormalParameter withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withType(hydra.ext.java.syntax.CatchType type) {
    return new CatchFormalParameter(modifiers, type, id);
  }
  
  public CatchFormalParameter withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    return new CatchFormalParameter(modifiers, type, id);
  }
}
