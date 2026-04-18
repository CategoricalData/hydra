// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class ReturnAs implements Serializable, Comparable<ReturnAs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ReturnAs");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public final hydra.util.Maybe<hydra.coq.syntax.Name> as;

  public final hydra.coq.syntax.Term100 return_;

  public ReturnAs (hydra.util.Maybe<hydra.coq.syntax.Name> as, hydra.coq.syntax.Term100 return_) {
    this.as = as;
    this.return_ = return_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnAs)) {
      return false;
    }
    ReturnAs o = (ReturnAs) other;
    return java.util.Objects.equals(
      this.as,
      o.as) && java.util.Objects.equals(
      this.return_,
      o.return_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(as) + 3 * java.util.Objects.hashCode(return_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ReturnAs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      as,
      other.as);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      return_,
      other.return_);
  }

  public ReturnAs withAs(hydra.util.Maybe<hydra.coq.syntax.Name> as) {
    return new ReturnAs(as, return_);
  }

  public ReturnAs withReturn(hydra.coq.syntax.Term100 return_) {
    return new ReturnAs(as, return_);
  }
}
