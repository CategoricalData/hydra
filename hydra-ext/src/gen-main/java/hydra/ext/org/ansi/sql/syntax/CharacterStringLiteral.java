// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class CharacterStringLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.CharacterStringLiteral");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public CharacterStringLiteral (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CharacterStringLiteral)) {
      return false;
    }
    CharacterStringLiteral o = (CharacterStringLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}