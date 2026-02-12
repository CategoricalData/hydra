// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class VariableArityParameter implements Serializable, Comparable<VariableArityParameter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.VariableArityParameter");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.VariableModifier modifiers;
  
  public final hydra.ext.java.syntax.UnannType type;
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public VariableArityParameter (hydra.ext.java.syntax.VariableModifier modifiers, hydra.ext.java.syntax.UnannType type, java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.Identifier identifier) {
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
    cmp = ((Comparable) modifiers).compareTo(other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) identifier).compareTo(other.identifier);
  }
  
  public VariableArityParameter withModifiers(hydra.ext.java.syntax.VariableModifier modifiers) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withType(hydra.ext.java.syntax.UnannType type) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
}
