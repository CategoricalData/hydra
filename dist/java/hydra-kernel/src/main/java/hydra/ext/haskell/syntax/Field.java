// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A field (name/type pair)
 */
public class Field implements Serializable, Comparable<Field> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.Field");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The field name
   */
  public final hydra.ext.haskell.syntax.Name name;

  /**
   * The field type
   */
  public final hydra.ext.haskell.syntax.Type type;

  public Field (hydra.ext.haskell.syntax.Name name, hydra.ext.haskell.syntax.Type type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Field other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public Field withName(hydra.ext.haskell.syntax.Name name) {
    return new Field(name, type);
  }

  public Field withType(hydra.ext.haskell.syntax.Type type) {
    return new Field(name, type);
  }
}
