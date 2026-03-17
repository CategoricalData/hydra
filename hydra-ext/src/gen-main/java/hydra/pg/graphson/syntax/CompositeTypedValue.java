// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class CompositeTypedValue implements Serializable, Comparable<CompositeTypedValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.CompositeTypedValue");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  public final hydra.pg.graphson.syntax.TypeName type;

  public final hydra.pg.graphson.syntax.Map fields;

  public CompositeTypedValue (hydra.pg.graphson.syntax.TypeName type, hydra.pg.graphson.syntax.Map fields) {
    this.type = type;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompositeTypedValue)) {
      return false;
    }
    CompositeTypedValue o = (CompositeTypedValue) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(fields);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CompositeTypedValue other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) fields).compareTo(other.fields);
  }

  public CompositeTypedValue withType(hydra.pg.graphson.syntax.TypeName type) {
    return new CompositeTypedValue(type, fields);
  }

  public CompositeTypedValue withFields(hydra.pg.graphson.syntax.Map fields) {
    return new CompositeTypedValue(type, fields);
  }
}
