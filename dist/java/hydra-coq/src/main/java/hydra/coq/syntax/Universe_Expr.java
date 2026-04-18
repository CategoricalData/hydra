// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Universe_Expr implements Serializable, Comparable<Universe_Expr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Universe_Expr");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name NUMBER = new hydra.core.Name("number");

  public final hydra.coq.syntax.UniverseName name;

  public final hydra.util.Maybe<hydra.coq.syntax.Natural> number;

  public Universe_Expr (hydra.coq.syntax.UniverseName name, hydra.util.Maybe<hydra.coq.syntax.Natural> number) {
    this.name = name;
    this.number = number;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Universe_Expr)) {
      return false;
    }
    Universe_Expr o = (Universe_Expr) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.number,
      o.number);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(number);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Universe_Expr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      number,
      other.number);
  }

  public Universe_Expr withName(hydra.coq.syntax.UniverseName name) {
    return new Universe_Expr(name, number);
  }

  public Universe_Expr withNumber(hydra.util.Maybe<hydra.coq.syntax.Natural> number) {
    return new Universe_Expr(name, number);
  }
}
