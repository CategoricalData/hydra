package hydra.ext.scala.meta;

public class Importee_Name {
  public final Name name;
  
  public Importee_Name (Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Name)) {
      return false;
    }
    Importee_Name o = (Importee_Name) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}