package hydra.ext.sql.ansi;

public class NationalCharacterStringType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.NationalCharacterStringType");
  
  public NationalCharacterStringType () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NationalCharacterStringType)) {
      return false;
    }
    NationalCharacterStringType o = (NationalCharacterStringType) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}