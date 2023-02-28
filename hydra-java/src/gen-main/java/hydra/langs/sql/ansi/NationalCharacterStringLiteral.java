package hydra.langs.sql.ansi;

public class NationalCharacterStringLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.NationalCharacterStringLiteral");
  
  public NationalCharacterStringLiteral () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NationalCharacterStringLiteral)) {
      return false;
    }
    NationalCharacterStringLiteral o = (NationalCharacterStringLiteral) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}