package hydra.ext.sql.ansi;

public class CharacterSetSpecification {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.CharacterSetSpecification");
  
  public CharacterSetSpecification () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CharacterSetSpecification)) {
      return false;
    }
    CharacterSetSpecification o = (CharacterSetSpecification) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}