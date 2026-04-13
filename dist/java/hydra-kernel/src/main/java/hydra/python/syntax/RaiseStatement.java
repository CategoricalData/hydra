// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class RaiseStatement implements Serializable, Comparable<RaiseStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.RaiseStatement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.Maybe<hydra.python.syntax.RaiseExpression> value;

  public RaiseStatement (hydra.util.Maybe<hydra.python.syntax.RaiseExpression> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RaiseStatement)) {
      return false;
    }
    RaiseStatement o = (RaiseStatement) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RaiseStatement other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
