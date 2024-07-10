// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class VariableArityParameter implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.VariableArityParameter");
  
  public final hydra.langs.java.syntax.VariableModifier modifiers;
  
  public final hydra.langs.java.syntax.UnannType type;
  
  public final java.util.List<hydra.langs.java.syntax.Annotation> annotations;
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public VariableArityParameter (hydra.langs.java.syntax.VariableModifier modifiers, hydra.langs.java.syntax.UnannType type, java.util.List<hydra.langs.java.syntax.Annotation> annotations, hydra.langs.java.syntax.Identifier identifier) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
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
    VariableArityParameter o = (VariableArityParameter) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && annotations.equals(o.annotations) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * annotations.hashCode() + 7 * identifier.hashCode();
  }
  
  public VariableArityParameter withModifiers(hydra.langs.java.syntax.VariableModifier modifiers) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withType(hydra.langs.java.syntax.UnannType type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withAnnotations(java.util.List<hydra.langs.java.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withIdentifier(hydra.langs.java.syntax.Identifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
}