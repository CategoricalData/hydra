// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Equation implements Serializable, Comparable<Equation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Equation");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final java.util.List<java.util.List<hydra.coq.syntax.Pattern>> pattern;

  public final hydra.coq.syntax.Term term;

  public Equation (java.util.List<java.util.List<hydra.coq.syntax.Pattern>> pattern, hydra.coq.syntax.Term term) {
    this.pattern = pattern;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Equation)) {
      return false;
    }
    Equation o = (Equation) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Equation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term,
      other.term);
  }

  public Equation withPattern(java.util.List<java.util.List<hydra.coq.syntax.Pattern>> pattern) {
    return new Equation(pattern, term);
  }

  public Equation withTerm(hydra.coq.syntax.Term term) {
    return new Equation(pattern, term);
  }
}
