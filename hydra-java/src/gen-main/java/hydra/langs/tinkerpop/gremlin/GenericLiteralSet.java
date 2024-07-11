// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralSet implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralSet");
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteral> value;
  
  public GenericLiteralSet (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteral> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralSet)) {
      return false;
    }
    GenericLiteralSet o = (GenericLiteralSet) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}