package hydra.ext.java.syntax;

public class PrimitiveTypeWithAnnotations {
  public final hydra.ext.java.syntax.PrimitiveType type;
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public PrimitiveTypeWithAnnotations (hydra.ext.java.syntax.PrimitiveType type, java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
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
  
  public PrimitiveTypeWithAnnotations withType(hydra.ext.java.syntax.PrimitiveType type) {
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
  
  public PrimitiveTypeWithAnnotations withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
}