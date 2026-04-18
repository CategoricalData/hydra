// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class VariableArityParameter implements Serializable, Comparable<VariableArityParameter> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.VariableArityParameter");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public final hydra.java.syntax.VariableModifier modifiers;

  public final hydra.java.syntax.UnannType type;

  public final java.util.List<hydra.java.syntax.Annotation> annotations;

  public final hydra.java.syntax.Identifier identifier;

  public VariableArityParameter (hydra.java.syntax.VariableModifier modifiers, hydra.java.syntax.UnannType type, java.util.List<hydra.java.syntax.Annotation> annotations, hydra.java.syntax.Identifier identifier) {
    this.modifiers = modifiers;
    this.type = type;
    this.annotations = annotations;
    this.identifier = identifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableArityParameter)) {
      return false;
    }
    VariableArityParameter o = (VariableArityParameter) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(annotations) + 7 * java.util.Objects.hashCode(identifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableArityParameter other) {
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
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      identifier,
      other.identifier);
  }

  public VariableArityParameter withModifiers(hydra.java.syntax.VariableModifier modifiers) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }

  public VariableArityParameter withType(hydra.java.syntax.UnannType type) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }

  public VariableArityParameter withAnnotations(java.util.List<hydra.java.syntax.Annotation> annotations) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }

  public VariableArityParameter withIdentifier(hydra.java.syntax.Identifier identifier) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
}
