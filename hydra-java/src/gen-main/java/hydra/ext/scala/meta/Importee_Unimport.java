package hydra.ext.scala.meta;

public class Importee_Unimport {
  public final hydra.ext.scala.meta.Name name;
  
  public Importee_Unimport (hydra.ext.scala.meta.Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Unimport)) {
      return false;
    }
    Importee_Unimport o = (Importee_Unimport) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}