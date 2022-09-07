package hydra.ext.java.syntax;

public class EnumConstantModifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.EnumConstantModifier");
  
  public final hydra.ext.java.syntax.Annotation value;
  
  public EnumConstantModifier (hydra.ext.java.syntax.Annotation value) {
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