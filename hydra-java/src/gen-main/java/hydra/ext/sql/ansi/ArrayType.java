package hydra.ext.sql.ansi;

public class ArrayType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ArrayType");
  
  public ArrayType () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayType)) {
      return false;
    }
    ArrayType o = (ArrayType) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}