package hydra.langs.java.syntax;

public class PackageModifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.PackageModifier");
  
  public final hydra.langs.java.syntax.Annotation value;
  
  public PackageModifier (hydra.langs.java.syntax.Annotation value) {
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