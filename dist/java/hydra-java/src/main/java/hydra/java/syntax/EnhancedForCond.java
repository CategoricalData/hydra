// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class EnhancedForCond implements Serializable, Comparable<EnhancedForCond> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.EnhancedForCond");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final java.util.List<hydra.java.syntax.VariableModifier> modifiers;

  public final hydra.java.syntax.LocalVariableType type;

  public final hydra.java.syntax.VariableDeclaratorId id;

  public final hydra.java.syntax.Expression expression;

  public EnhancedForCond (java.util.List<hydra.java.syntax.VariableModifier> modifiers, hydra.java.syntax.LocalVariableType type, hydra.java.syntax.VariableDeclaratorId id, hydra.java.syntax.Expression expression) {
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnhancedForCond)) {
      return false;
    }
    EnhancedForCond o = (EnhancedForCond) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(id) + 7 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EnhancedForCond other) {
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
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public EnhancedForCond withModifiers(java.util.List<hydra.java.syntax.VariableModifier> modifiers) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }

  public EnhancedForCond withType(hydra.java.syntax.LocalVariableType type) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }

  public EnhancedForCond withId(hydra.java.syntax.VariableDeclaratorId id) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }

  public EnhancedForCond withExpression(hydra.java.syntax.Expression expression) {
    return new EnhancedForCond(modifiers, type, id, expression);
  }
}
