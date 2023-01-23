package hydra.ext.sql.ansi;

public class BinaryStringLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.BinaryStringLiteral");
  
  public BinaryStringLiteral () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryStringLiteral)) {
      return false;
    }
    BinaryStringLiteral o = (BinaryStringLiteral) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}