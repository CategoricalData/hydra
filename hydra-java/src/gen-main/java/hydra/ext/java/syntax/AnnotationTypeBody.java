package hydra.ext.java.syntax;

public class AnnotationTypeBody {
  public final java.util.List<java.util.List<hydra.ext.java.syntax.AnnotationTypeMemberDeclaration>> value;
  
  public AnnotationTypeBody (java.util.List<java.util.List<hydra.ext.java.syntax.AnnotationTypeMemberDeclaration>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationTypeBody)) {
      return false;
    }
    AnnotationTypeBody o = (AnnotationTypeBody) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}