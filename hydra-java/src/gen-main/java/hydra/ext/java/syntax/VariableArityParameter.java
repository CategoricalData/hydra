package hydra.ext.java.syntax;

public class VariableArityParameter {
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
    VariableArityParameter o = (VariableArityParameter) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && annotations.equals(o.annotations) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * annotations.hashCode() + 7 * identifier.hashCode();
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