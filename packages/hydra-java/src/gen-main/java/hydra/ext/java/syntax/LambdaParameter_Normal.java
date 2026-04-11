// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LambdaParameter_Normal implements Serializable, Comparable<LambdaParameter_Normal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.LambdaParameter_Normal");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

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
    LambdaParameter_Normal o = (LambdaParameter_Normal) other;
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
  public int compareTo(LambdaParameter_Normal other) {
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
