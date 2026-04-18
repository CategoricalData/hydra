// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class ValueMapBooleanArgs implements Serializable, Comparable<ValueMapBooleanArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.ValueMapBooleanArgs");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name KEYS = new hydra.core.Name("keys");

  public final hydra.tinkerpop.gremlin.BooleanArgument value;

  public final hydra.util.Maybe<java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument>> keys;

  public ValueMapBooleanArgs (hydra.tinkerpop.gremlin.BooleanArgument value, hydra.util.Maybe<java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument>> keys) {
    this.value = value;
    this.keys = keys;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueMapBooleanArgs)) {
      return false;
    }
    ValueMapBooleanArgs o = (ValueMapBooleanArgs) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.keys,
      o.keys);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(keys);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ValueMapBooleanArgs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      keys,
      other.keys);
  }

  public ValueMapBooleanArgs withValue(hydra.tinkerpop.gremlin.BooleanArgument value) {
    return new ValueMapBooleanArgs(value, keys);
  }

  public ValueMapBooleanArgs withKeys(hydra.util.Maybe<java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument>> keys) {
    return new ValueMapBooleanArgs(value, keys);
  }
}
