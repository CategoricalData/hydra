// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class InclusiveOrExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.InclusiveOrExpression");
  
  public final java.util.List<hydra.langs.java.syntax.ExclusiveOrExpression> value;
  
  public InclusiveOrExpression (java.util.List<hydra.langs.java.syntax.ExclusiveOrExpression> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InclusiveOrExpression)) {
      return false;
    }
    InclusiveOrExpression o = (InclusiveOrExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}