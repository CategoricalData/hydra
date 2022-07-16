package hydra.ext.java.syntax;

public class VariableArityParameter {
  public final VariableModifier modifiers;
  
  public final UnannType type;
  
  public final java.util.List<Annotation> annotations;
  
  public final Identifier identifier;
  
  public VariableArityParameter (VariableModifier modifiers, UnannType type, java.util.List<Annotation> annotations, Identifier identifier) {
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
  
  public VariableArityParameter withModifiers(VariableModifier modifiers) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withType(UnannType type) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withAnnotations(java.util.List<Annotation> annotations) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
  
  public VariableArityParameter withIdentifier(Identifier identifier) {
    return new VariableArityParameter(modifiers, type, annotations, identifier);
  }
}