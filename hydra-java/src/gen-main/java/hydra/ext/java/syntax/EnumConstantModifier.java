package hydra.ext.java.syntax;

public class EnumConstantModifier {
  public final Annotation value;
  
  public EnumConstantModifier (Annotation value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumConstantModifier)) {
      return false;
    }
    EnumConstantModifier o = (EnumConstantModifier) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}