// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class Include implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.Include");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.shex.syntax.TripleExprLabel value;
  
  public Include (hydra.ext.shex.syntax.TripleExprLabel value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Include)) {
      return false;
    }
    Include o = (Include) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
