// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class LetDestructuring_Variant1 implements Serializable, Comparable<LetDestructuring_Variant1> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetDestructuring_Variant1");

  public static final hydra.core.Name NAMES = new hydra.core.Name("names");

  public static final hydra.core.Name RETURN_AS = new hydra.core.Name("returnAs");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final java.util.List<hydra.coq.syntax.Name> names;

  public final hydra.util.Maybe<hydra.coq.syntax.ReturnAs> returnAs;

  public final hydra.coq.syntax.Term term;

  public LetDestructuring_Variant1 (java.util.List<hydra.coq.syntax.Name> names, hydra.util.Maybe<hydra.coq.syntax.ReturnAs> returnAs, hydra.coq.syntax.Term term) {
    this.names = names;
    this.returnAs = returnAs;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant1)) {
      return false;
    }
    LetDestructuring_Variant1 o = (LetDestructuring_Variant1) other;
    return java.util.Objects.equals(
      this.names,
      o.names) && java.util.Objects.equals(
      this.returnAs,
      o.returnAs) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(names) + 3 * java.util.Objects.hashCode(returnAs) + 5 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetDestructuring_Variant1 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      names,
      other.names);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      returnAs,
      other.returnAs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term,
      other.term);
  }

  public LetDestructuring_Variant1 withNames(java.util.List<hydra.coq.syntax.Name> names) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }

  public LetDestructuring_Variant1 withReturnAs(hydra.util.Maybe<hydra.coq.syntax.ReturnAs> returnAs) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }

  public LetDestructuring_Variant1 withTerm(hydra.coq.syntax.Term term) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
}
