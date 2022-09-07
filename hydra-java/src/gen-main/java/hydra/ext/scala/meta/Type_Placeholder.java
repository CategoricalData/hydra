package hydra.ext.scala.meta;

public class Type_Placeholder {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Placeholder");
  
  public final hydra.ext.scala.meta.Type_Bounds bounds;
  
  public Type_Placeholder (hydra.ext.scala.meta.Type_Bounds bounds) {
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