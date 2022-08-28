package hydra.ext.scala.meta;

public class Defn_Object {
  public final hydra.ext.scala.meta.Data_Name name;
  
  public Defn_Object (hydra.ext.scala.meta.Data_Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Object)) {
      return false;
    }
    Defn_Object o = (Defn_Object) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}