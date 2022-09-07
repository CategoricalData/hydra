package hydra.ext.scala.meta;

public class Type_Singleton {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Singleton");
  
  public final hydra.ext.scala.meta.Data_Ref ref;
  
  public Type_Singleton (hydra.ext.scala.meta.Data_Ref ref) {
    this.ref = ref;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Singleton)) {
      return false;
    }
    Type_Singleton o = (Type_Singleton) (other);
    return ref.equals(o.ref);
  }
  
  @Override
  public int hashCode() {
    return 2 * ref.hashCode();
  }
}