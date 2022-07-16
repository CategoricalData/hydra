package hydra.ext.scala.meta;

public class Type_Placeholder {
  public final Type_Bounds bounds;
  
  public Type_Placeholder (Type_Bounds bounds) {
    this.bounds = bounds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Placeholder)) {
      return false;
    }
    Type_Placeholder o = (Type_Placeholder) (other);
    return bounds.equals(o.bounds);
  }
  
  @Override
  public int hashCode() {
    return 2 * bounds.hashCode();
  }
}