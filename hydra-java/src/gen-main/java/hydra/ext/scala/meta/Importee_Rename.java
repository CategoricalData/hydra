package hydra.ext.scala.meta;

public class Importee_Rename {
  public final Name name;
  
  public final Name rename;
  
  public Importee_Rename (Name name, Name rename) {
    this.name = name;
    this.rename = rename;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Rename)) {
      return false;
    }
    Importee_Rename o = (Importee_Rename) (other);
    return name.equals(o.name) && rename.equals(o.rename);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * rename.hashCode();
  }
  
  public Importee_Rename withName(Name name) {
    return new Importee_Rename(name, rename);
  }
  
  public Importee_Rename withRename(Name rename) {
    return new Importee_Rename(name, rename);
  }
}