// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralList implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralList");
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteral> value;
  
  public GenericLiteralList (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteral> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralList)) {
      return false;
    }
    GenericLiteralList o = (GenericLiteralList) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}