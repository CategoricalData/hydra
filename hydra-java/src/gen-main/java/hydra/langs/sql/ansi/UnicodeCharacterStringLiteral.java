package hydra.langs.sql.ansi;

public class UnicodeCharacterStringLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.UnicodeCharacterStringLiteral");
  
  public UnicodeCharacterStringLiteral () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnicodeCharacterStringLiteral)) {
      return false;
    }
    UnicodeCharacterStringLiteral o = (UnicodeCharacterStringLiteral) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}