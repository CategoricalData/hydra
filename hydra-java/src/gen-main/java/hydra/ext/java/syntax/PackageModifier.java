package hydra.ext.java.syntax;

public class PackageModifier {
  public final hydra.ext.java.syntax.Annotation value;
  
  public PackageModifier (hydra.ext.java.syntax.Annotation value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PackageModifier)) {
      return false;
    }
    PackageModifier o = (PackageModifier) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}