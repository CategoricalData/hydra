// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class PredefinedType_String implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/sql/ansi.PredefinedType.String");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_CHARACTERS = new hydra.core.Name("characters");
  
  public static final hydra.core.Name FIELD_NAME_COLLATE = new hydra.core.Name("collate");
  
  public final hydra.langs.sql.ansi.CharacterStringType type;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.CharacterSetSpecification> characters;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate;
  
  public PredefinedType_String (hydra.langs.sql.ansi.CharacterStringType type, hydra.util.Opt<hydra.langs.sql.ansi.CharacterSetSpecification> characters, hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((characters));
    java.util.Objects.requireNonNull((collate));
    this.type = type;
    this.characters = characters;
    this.collate = collate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PredefinedType_String)) {
      return false;
    }
    PredefinedType_String o = (PredefinedType_String) (other);
    return type.equals(o.type) && characters.equals(o.characters) && collate.equals(o.collate);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * characters.hashCode() + 5 * collate.hashCode();
  }
  
  public PredefinedType_String withType(hydra.langs.sql.ansi.CharacterStringType type) {
    java.util.Objects.requireNonNull((type));
    return new PredefinedType_String(type, characters, collate);
  }
  
  public PredefinedType_String withCharacters(hydra.util.Opt<hydra.langs.sql.ansi.CharacterSetSpecification> characters) {
    java.util.Objects.requireNonNull((characters));
    return new PredefinedType_String(type, characters, collate);
  }
  
  public PredefinedType_String withCollate(hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    java.util.Objects.requireNonNull((collate));
    return new PredefinedType_String(type, characters, collate);
  }
}