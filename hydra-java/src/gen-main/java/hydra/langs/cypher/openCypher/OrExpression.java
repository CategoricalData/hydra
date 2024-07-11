// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class OrExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.OrExpression");
  
  public final java.util.List<hydra.langs.cypher.openCypher.XorExpression> value;
  
  public OrExpression (java.util.List<hydra.langs.cypher.openCypher.XorExpression> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrExpression)) {
      return false;
    }
    OrExpression o = (OrExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}