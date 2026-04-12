// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A dotted pair literal: '(a . b). Not available in Clojure.
 */
public class DottedPair implements Serializable, Comparable<DottedPair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.DottedPair");

  public static final hydra.core.Name CAR = new hydra.core.Name("car");

  public static final hydra.core.Name CDR = new hydra.core.Name("cdr");

  /**
   * The first element
   */
  public final hydra.ext.lisp.syntax.Expression car;

  /**
   * The second element
   */
  public final hydra.ext.lisp.syntax.Expression cdr;

  public DottedPair (hydra.ext.lisp.syntax.Expression car, hydra.ext.lisp.syntax.Expression cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DottedPair)) {
      return false;
    }
    DottedPair o = (DottedPair) other;
    return java.util.Objects.equals(
      this.car,
      o.car) && java.util.Objects.equals(
      this.cdr,
      o.cdr);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(car) + 3 * java.util.Objects.hashCode(cdr);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DottedPair other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      car,
      other.car);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      cdr,
      other.cdr);
  }

  public DottedPair withCar(hydra.ext.lisp.syntax.Expression car) {
    return new DottedPair(car, cdr);
  }

  public DottedPair withCdr(hydra.ext.lisp.syntax.Expression cdr) {
    return new DottedPair(car, cdr);
  }
}
