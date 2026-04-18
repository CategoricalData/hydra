// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class LetDestructuring_Variant3 implements Serializable, Comparable<LetDestructuring_Variant3> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetDestructuring_Variant3");

  public static final hydra.core.Name PATTERN1 = new hydra.core.Name("pattern1");

  public static final hydra.core.Name PATTERN2 = new hydra.core.Name("pattern2");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public final hydra.coq.syntax.Pattern pattern1;

  public final hydra.coq.syntax.Pattern pattern2;

  public final hydra.coq.syntax.Term term;

  public final hydra.coq.syntax.Term100 return_;

  public LetDestructuring_Variant3 (hydra.coq.syntax.Pattern pattern1, hydra.coq.syntax.Pattern pattern2, hydra.coq.syntax.Term term, hydra.coq.syntax.Term100 return_) {
    this.pattern1 = pattern1;
    this.pattern2 = pattern2;
    this.term = term;
    this.return_ = return_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant3)) {
      return false;
    }
    LetDestructuring_Variant3 o = (LetDestructuring_Variant3) other;
    return java.util.Objects.equals(
      this.pattern1,
      o.pattern1) && java.util.Objects.equals(
      this.pattern2,
      o.pattern2) && java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.return_,
      o.return_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern1) + 3 * java.util.Objects.hashCode(pattern2) + 5 * java.util.Objects.hashCode(term) + 7 * java.util.Objects.hashCode(return_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetDestructuring_Variant3 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern1,
      other.pattern1);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pattern2,
      other.pattern2);
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

  public LetDestructuring_Variant3 withPattern1(hydra.coq.syntax.Pattern pattern1) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }

  public LetDestructuring_Variant3 withPattern2(hydra.coq.syntax.Pattern pattern2) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }

  public LetDestructuring_Variant3 withTerm(hydra.coq.syntax.Term term) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }

  public LetDestructuring_Variant3 withReturn(hydra.coq.syntax.Term100 return_) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
}
