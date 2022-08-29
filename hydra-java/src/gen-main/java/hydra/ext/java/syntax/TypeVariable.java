package hydra.ext.java.syntax;

public class TypeVariable {
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public TypeVariable (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.TypeIdentifier identifier) {
    this.annotations = annotations;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeVariable)) {
      return false;
    }
    TypeVariable o = (TypeVariable) (other);
    return annotations.equals(o.annotations) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * identifier.hashCode();
  }
  
  public TypeVariable withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new TypeVariable(annotations, identifier);
  }
  
  public TypeVariable withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new TypeVariable(annotations, identifier);
  }
}