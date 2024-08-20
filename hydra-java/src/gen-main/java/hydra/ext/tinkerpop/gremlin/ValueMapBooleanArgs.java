// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class ValueMapBooleanArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.ValueMapBooleanArgs");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_KEYS = new hydra.core.Name("keys");
  
  public final hydra.ext.tinkerpop.gremlin.BooleanArgument value;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument>> keys;
  
  public ValueMapBooleanArgs (hydra.ext.tinkerpop.gremlin.BooleanArgument value, hydra.util.Opt<java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument>> keys) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((keys));
    this.value = value;
    this.keys = keys;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueMapBooleanArgs)) {
      return false;
    }
    ValueMapBooleanArgs o = (ValueMapBooleanArgs) (other);
    return value.equals(o.value) && keys.equals(o.keys);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * keys.hashCode();
  }
  
  public ValueMapBooleanArgs withValue(hydra.ext.tinkerpop.gremlin.BooleanArgument value) {
    java.util.Objects.requireNonNull((value));
    return new ValueMapBooleanArgs(value, keys);
  }
  
  public ValueMapBooleanArgs withKeys(hydra.util.Opt<java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument>> keys) {
    java.util.Objects.requireNonNull((keys));
    return new ValueMapBooleanArgs(value, keys);
  }
}
