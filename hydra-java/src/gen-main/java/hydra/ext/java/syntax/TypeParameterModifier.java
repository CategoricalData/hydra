package hydra.ext.java.syntax;

public class TypeParameterModifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.TypeParameterModifier");
  
  public final hydra.ext.java.syntax.Annotation value;
  
  public TypeParameterModifier (hydra.ext.java.syntax.Annotation value) {
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