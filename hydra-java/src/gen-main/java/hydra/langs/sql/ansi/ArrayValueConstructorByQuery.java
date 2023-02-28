package hydra.langs.sql.ansi;

public class ArrayValueConstructorByQuery {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.ArrayValueConstructorByQuery");
  
  public ArrayValueConstructorByQuery () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayValueConstructorByQuery)) {
      return false;
    }
    ArrayValueConstructorByQuery o = (ArrayValueConstructorByQuery) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}