// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class LetDestructuring_Variant2 implements Serializable, Comparable<LetDestructuring_Variant2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetDestructuring_Variant2");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public final hydra.coq.syntax.Pattern pattern;

  public final hydra.coq.syntax.Term term;

  public final hydra.util.Maybe<hydra.coq.syntax.Term100> return_;

  public LetDestructuring_Variant2 (hydra.coq.syntax.Pattern pattern, hydra.coq.syntax.Term term, hydra.util.Maybe<hydra.coq.syntax.Term100> return_) {
    this.pattern = pattern;
    this.term = term;
    this.return_ = return_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant2)) {
      return false;
    }
    LetDestructuring_Variant2 o = (LetDestructuring_Variant2) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.return_,
      o.return_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(term) + 5 * java.util.Objects.hashCode(return_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetDestructuring_Variant2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      term,
      other.term);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      return_,
      other.return_);
  }

  public LetDestructuring_Variant2 withPattern(hydra.coq.syntax.Pattern pattern) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }

  public LetDestructuring_Variant2 withTerm(hydra.coq.syntax.Term term) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }

  public LetDestructuring_Variant2 withReturn(hydra.util.Maybe<hydra.coq.syntax.Term100> return_) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
}
