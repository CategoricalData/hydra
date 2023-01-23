package hydra.ext.sql.ansi;

public class MultisetValueConstructor {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.MultisetValueConstructor");
  
  public MultisetValueConstructor () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultisetValueConstructor)) {
      return false;
    }
    MultisetValueConstructor o = (MultisetValueConstructor) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}