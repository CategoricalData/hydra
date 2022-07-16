package hydra.ext.java.syntax;

public class AnnotatedIdentifier {
  public final java.util.List<Annotation> annotations;
  
  public final Identifier identifier;
  
  public AnnotatedIdentifier (java.util.List<Annotation> annotations, Identifier identifier) {
    this.annotations = annotations;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedIdentifier)) {
      return false;
    }
    AnnotatedIdentifier o = (AnnotatedIdentifier) (other);
    return annotations.equals(o.annotations) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * identifier.hashCode();
  }
  
  public AnnotatedIdentifier withAnnotations(java.util.List<Annotation> annotations) {
    return new AnnotatedIdentifier(annotations, identifier);
  }
  
  public AnnotatedIdentifier withIdentifier(Identifier identifier) {
    return new AnnotatedIdentifier(annotations, identifier);
  }
}