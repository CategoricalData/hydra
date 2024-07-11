// Note: this is an automatically generated file. Do not edit.

package hydra.langs.avro.schema;

import java.io.Serializable;

public class Union implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/avro/schema.Union");
  
  public final java.util.List<hydra.langs.avro.schema.Schema> value;
  
  public Union (java.util.List<hydra.langs.avro.schema.Schema> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Union)) {
      return false;
    }
    Union o = (Union) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}