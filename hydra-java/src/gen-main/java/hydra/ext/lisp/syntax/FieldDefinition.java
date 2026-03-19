// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A field in a record type definition
 */
public class FieldDefinition implements Serializable, Comparable<FieldDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.FieldDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DEFAULT_VALUE = new hydra.core.Name("defaultValue");

  /**
   * The field name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * Optional default value
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Expression> defaultValue;

  public FieldDefinition (hydra.ext.lisp.syntax.Symbol name, hydra.util.Maybe<hydra.ext.lisp.syntax.Expression> defaultValue) {
    this.name = name;
    this.defaultValue = defaultValue;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldDefinition)) {
      return false;
    }
    FieldDefinition o = (FieldDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.defaultValue,
      o.defaultValue);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(defaultValue);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldDefinition other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) defaultValue).compareTo(other.defaultValue);
  }

  public FieldDefinition withName(hydra.ext.lisp.syntax.Symbol name) {
    return new FieldDefinition(name, defaultValue);
  }

  public FieldDefinition withDefaultValue(hydra.util.Maybe<hydra.ext.lisp.syntax.Expression> defaultValue) {
    return new FieldDefinition(name, defaultValue);
  }
}
