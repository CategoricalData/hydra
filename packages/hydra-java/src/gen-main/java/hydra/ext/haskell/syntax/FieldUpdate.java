// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A field name and value
 */
public class FieldUpdate implements Serializable, Comparable<FieldUpdate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.FieldUpdate");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The field name
   */
  public final hydra.ext.haskell.syntax.Name name;

  /**
   * The field value
   */
  public final hydra.ext.haskell.syntax.Expression value;

  public FieldUpdate (hydra.ext.haskell.syntax.Name name, hydra.ext.haskell.syntax.Expression value) {
    this.name = name;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldUpdate)) {
      return false;
    }
    FieldUpdate o = (FieldUpdate) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldUpdate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public FieldUpdate withName(hydra.ext.haskell.syntax.Name name) {
    return new FieldUpdate(name, value);
  }

  public FieldUpdate withValue(hydra.ext.haskell.syntax.Expression value) {
    return new FieldUpdate(name, value);
  }
}
