package hydra.ext.java.syntax;

public class PackageOrTypeName {
  public final java.util.List<Identifier> value;
  
  public PackageOrTypeName (java.util.List<Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PackageOrTypeName)) {
      return false;
    }
    PackageOrTypeName o = (PackageOrTypeName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}