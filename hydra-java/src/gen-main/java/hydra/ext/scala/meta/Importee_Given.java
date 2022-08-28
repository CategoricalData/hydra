package hydra.ext.scala.meta;

public class Importee_Given {
  public final hydra.ext.scala.meta.Type tpe;
  
  public Importee_Given (hydra.ext.scala.meta.Type tpe) {
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Given)) {
      return false;
    }
    Importee_Given o = (Importee_Given) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}