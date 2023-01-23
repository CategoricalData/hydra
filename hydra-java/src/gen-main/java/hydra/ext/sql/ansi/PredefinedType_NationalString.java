package hydra.ext.sql.ansi;

public class PredefinedType_NationalString {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.PredefinedType.NationalString");
  
  public final hydra.ext.sql.ansi.NationalCharacterStringType type;
  
  public final java.util.Optional<hydra.ext.sql.ansi.CollateClause> collate;
  
  public PredefinedType_NationalString (hydra.ext.sql.ansi.NationalCharacterStringType type, java.util.Optional<hydra.ext.sql.ansi.CollateClause> collate) {
    this.type = type;
    this.collate = collate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PredefinedType_NationalString)) {
      return false;
    }
    PredefinedType_NationalString o = (PredefinedType_NationalString) (other);
    return type.equals(o.type) && collate.equals(o.collate);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * collate.hashCode();
  }
  
  public PredefinedType_NationalString withType(hydra.ext.sql.ansi.NationalCharacterStringType type) {
    return new PredefinedType_NationalString(type, collate);
  }
  
  public PredefinedType_NationalString withCollate(java.util.Optional<hydra.ext.sql.ansi.CollateClause> collate) {
    return new PredefinedType_NationalString(type, collate);
  }
}