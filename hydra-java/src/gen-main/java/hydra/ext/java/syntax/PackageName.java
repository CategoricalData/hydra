package hydra.ext.java.syntax;

public class PackageName {
  public final java.util.List<Identifier> value;
  
  public PackageName (java.util.List<Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PackageName)) {
      return false;
    }
    PackageName o = (PackageName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}