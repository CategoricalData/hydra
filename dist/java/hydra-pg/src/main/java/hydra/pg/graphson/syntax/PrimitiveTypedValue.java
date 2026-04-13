// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class PrimitiveTypedValue implements Serializable, Comparable<PrimitiveTypedValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.PrimitiveTypedValue");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.pg.graphson.syntax.TypeName type;

  public final String value;

  public PrimitiveTypedValue (hydra.pg.graphson.syntax.TypeName type, String value) {
    this.type = type;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimitiveTypedValue)) {
      return false;
    }
    PrimitiveTypedValue o = (PrimitiveTypedValue) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PrimitiveTypedValue other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public PrimitiveTypedValue withType(hydra.pg.graphson.syntax.TypeName type) {
    return new PrimitiveTypedValue(type, value);
  }

  public PrimitiveTypedValue withValue(String value) {
    return new PrimitiveTypedValue(type, value);
  }
}
