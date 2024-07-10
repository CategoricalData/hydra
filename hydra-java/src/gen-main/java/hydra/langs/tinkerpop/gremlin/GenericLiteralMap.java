// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralMap implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralMap");
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.MapEntry> value;
  
  public GenericLiteralMap (java.util.List<hydra.langs.tinkerpop.gremlin.MapEntry> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralMap)) {
      return false;
    }
    GenericLiteralMap o = (GenericLiteralMap) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}