package hydra.ext.sql.ansi;

public class SubtypeTreatment {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.SubtypeTreatment");
  
  public SubtypeTreatment () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtypeTreatment)) {
      return false;
    }
    SubtypeTreatment o = (SubtypeTreatment) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}