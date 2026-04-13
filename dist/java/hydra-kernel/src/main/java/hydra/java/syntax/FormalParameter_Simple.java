// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class FormalParameter_Simple implements Serializable, Comparable<FormalParameter_Simple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.FormalParameter_Simple");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public final java.util.List<hydra.java.syntax.VariableModifier> modifiers;

  public final hydra.java.syntax.UnannType type;

  public final hydra.java.syntax.VariableDeclaratorId id;

  public FormalParameter_Simple (java.util.List<hydra.java.syntax.VariableModifier> modifiers, hydra.java.syntax.UnannType type, hydra.java.syntax.VariableDeclaratorId id) {
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
    cmp = hydra.util.Comparing.compare(
      modifiers,
      other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      id,
      other.id);
  }

  public FormalParameter_Simple withModifiers(java.util.List<hydra.java.syntax.VariableModifier> modifiers) {
    return new FormalParameter_Simple(modifiers, type, id);
  }

  public FormalParameter_Simple withType(hydra.java.syntax.UnannType type) {
    return new FormalParameter_Simple(modifiers, type, id);
  }

  public FormalParameter_Simple withId(hydra.java.syntax.VariableDeclaratorId id) {
    return new FormalParameter_Simple(modifiers, type, id);
  }
}
