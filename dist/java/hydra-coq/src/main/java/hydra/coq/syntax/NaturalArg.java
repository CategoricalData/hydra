// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class NaturalArg implements Serializable, Comparable<NaturalArg> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.NaturalArg");

  public static final hydra.core.Name NATURAL = new hydra.core.Name("natural");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final hydra.coq.syntax.Natural natural;

  public final hydra.coq.syntax.Term term;

  public NaturalArg (hydra.coq.syntax.Natural natural, hydra.coq.syntax.Term term) {
    this.natural = natural;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NaturalArg)) {
      return false;
    }
    NaturalArg o = (NaturalArg) other;
    return java.util.Objects.equals(
      this.natural,
      o.natural) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(natural) + 3 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NaturalArg other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      natural,
      other.natural);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term,
      other.term);
  }

  public NaturalArg withNatural(hydra.coq.syntax.Natural natural) {
    return new NaturalArg(natural, term);
  }

  public NaturalArg withTerm(hydra.coq.syntax.Term term) {
    return new NaturalArg(natural, term);
  }
}
