// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Field implements Serializable, Comparable<Field> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Field");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String name;

  public final openGql.grammar.ValueExpression value;

  public Field (String name, openGql.grammar.ValueExpression value) {
    this.name = name;
    this.value = value;
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
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
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
      value,
      other.value);
  }

  public Field withName(String name) {
    return new Field(name, value);
  }

  public Field withValue(openGql.grammar.ValueExpression value) {
    return new Field(name, value);
  }
}
