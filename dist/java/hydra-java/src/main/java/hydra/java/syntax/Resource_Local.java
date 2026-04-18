// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class Resource_Local implements Serializable, Comparable<Resource_Local> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.Resource_Local");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final java.util.List<hydra.java.syntax.VariableModifier> modifiers;

  public final hydra.java.syntax.LocalVariableType type;

  public final hydra.java.syntax.Identifier identifier;

  public final hydra.java.syntax.Expression expression;

  public Resource_Local (java.util.List<hydra.java.syntax.VariableModifier> modifiers, hydra.java.syntax.LocalVariableType type, hydra.java.syntax.Identifier identifier, hydra.java.syntax.Expression expression) {
    this.modifiers = modifiers;
    this.type = type;
    this.identifier = identifier;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Resource_Local)) {
      return false;
    }
    Resource_Local o = (Resource_Local) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(identifier) + 7 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Resource_Local other) {
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
      identifier,
      other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public Resource_Local withModifiers(java.util.List<hydra.java.syntax.VariableModifier> modifiers) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }

  public Resource_Local withType(hydra.java.syntax.LocalVariableType type) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }

  public Resource_Local withIdentifier(hydra.java.syntax.Identifier identifier) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }

  public Resource_Local withExpression(hydra.java.syntax.Expression expression) {
    return new Resource_Local(modifiers, type, identifier, expression);
  }
}
