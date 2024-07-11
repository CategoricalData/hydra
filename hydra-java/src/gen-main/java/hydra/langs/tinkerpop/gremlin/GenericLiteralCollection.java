// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralCollection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralCollection");
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteral> value;
  
  public GenericLiteralCollection (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteral> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralCollection)) {
      return false;
    }
    GenericLiteralCollection o = (GenericLiteralCollection) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}