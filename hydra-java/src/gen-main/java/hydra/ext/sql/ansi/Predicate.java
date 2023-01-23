package hydra.ext.sql.ansi;

public class Predicate {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.Predicate");
  
  public Predicate () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Predicate)) {
      return false;
    }
    Predicate o = (Predicate) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}