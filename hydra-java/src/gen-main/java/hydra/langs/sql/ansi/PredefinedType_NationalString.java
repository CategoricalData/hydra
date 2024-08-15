// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class PredefinedType_NationalString implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/sql/ansi.PredefinedType.NationalString");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_COLLATE = new hydra.core.Name("collate");
  
  public final hydra.langs.sql.ansi.NationalCharacterStringType type;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate;
  
  public PredefinedType_NationalString (hydra.langs.sql.ansi.NationalCharacterStringType type, hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((collate));
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
  
  public PredefinedType_NationalString withType(hydra.langs.sql.ansi.NationalCharacterStringType type) {
    java.util.Objects.requireNonNull((type));
    return new PredefinedType_NationalString(type, collate);
  }
  
  public PredefinedType_NationalString withCollate(hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    java.util.Objects.requireNonNull((collate));
    return new PredefinedType_NationalString(type, collate);
  }
}