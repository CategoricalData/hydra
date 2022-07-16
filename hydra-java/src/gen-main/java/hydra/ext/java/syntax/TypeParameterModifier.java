package hydra.ext.java.syntax;

public class TypeParameterModifier {
  public final Annotation value;
  
  public TypeParameterModifier (Annotation value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameterModifier)) {
      return false;
    }
    TypeParameterModifier o = (TypeParameterModifier) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}