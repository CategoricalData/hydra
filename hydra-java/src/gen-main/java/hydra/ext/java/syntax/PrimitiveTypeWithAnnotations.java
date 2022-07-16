package hydra.ext.java.syntax;

public class PrimitiveTypeWithAnnotations {
  public final PrimitiveType type;
  
  public final java.util.List<Annotation> annotations;
  
  public PrimitiveTypeWithAnnotations (PrimitiveType type, java.util.List<Annotation> annotations) {
    this.type = type;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimitiveTypeWithAnnotations)) {
      return false;
    }
    PrimitiveTypeWithAnnotations o = (PrimitiveTypeWithAnnotations) (other);
    return type.equals(o.type) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * annotations.hashCode();
  }
  
  public PrimitiveTypeWithAnnotations withType(PrimitiveType type) {
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
  
  public PrimitiveTypeWithAnnotations withAnnotations(java.util.List<Annotation> annotations) {
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
}