package hydra.langs.scala.meta;

public class Importee_Name {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Importee.Name");
  
  public final hydra.langs.scala.meta.Name name;
  
  public Importee_Name (hydra.langs.scala.meta.Name name) {
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