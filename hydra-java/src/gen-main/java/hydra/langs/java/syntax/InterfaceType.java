package hydra.langs.java.syntax;

public class InterfaceType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.InterfaceType");
  
  public final hydra.langs.java.syntax.ClassType value;
  
  public InterfaceType (hydra.langs.java.syntax.ClassType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceType)) {
      return false;
    }
    InterfaceType o = (InterfaceType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}