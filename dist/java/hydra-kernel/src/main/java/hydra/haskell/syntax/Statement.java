// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A do-notation statement
 */
public class Statement implements Serializable, Comparable<Statement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.Statement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.haskell.syntax.Expression value;

  public Statement (hydra.haskell.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Statement)) {
      return false;
    }
    Statement o = (Statement) other;
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
  public int compareTo(Statement other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
