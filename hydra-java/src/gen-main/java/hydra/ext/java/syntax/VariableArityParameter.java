// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class VariableArityParameter implements Serializable {
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
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((identifier));
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
  
  public VariableArityParameter withModifiers(hydra.ext.java.syntax.VariableModifier modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withType(hydra.ext.java.syntax.UnannType type) {
    java.util.Objects.requireNonNull((type));
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
}