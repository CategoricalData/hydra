// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Match implements Serializable, Comparable<Match> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Match");

  public static final hydra.core.Name CASE_ITEMS = new hydra.core.Name("caseItems");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public static final hydra.core.Name PIPE = new hydra.core.Name("pipe");

  public static final hydra.core.Name EQUATIONS = new hydra.core.Name("equations");

  public final java.util.List<hydra.coq.syntax.CaseItem> caseItems;

  public final hydra.util.Maybe<hydra.coq.syntax.Term100> return_;

  public final Boolean pipe;

  public final java.util.List<hydra.coq.syntax.Equation> equations;

  public Match (java.util.List<hydra.coq.syntax.CaseItem> caseItems, hydra.util.Maybe<hydra.coq.syntax.Term100> return_, Boolean pipe, java.util.List<hydra.coq.syntax.Equation> equations) {
    this.caseItems = caseItems;
    this.return_ = return_;
    this.pipe = pipe;
    this.equations = equations;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Match)) {
      return false;
    }
    Match o = (Match) other;
    return java.util.Objects.equals(
      this.caseItems,
      o.caseItems) && java.util.Objects.equals(
      this.return_,
      o.return_) && java.util.Objects.equals(
      this.pipe,
      o.pipe) && java.util.Objects.equals(
      this.equations,
      o.equations);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(caseItems) + 3 * java.util.Objects.hashCode(return_) + 5 * java.util.Objects.hashCode(pipe) + 7 * java.util.Objects.hashCode(equations);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Match other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      caseItems,
      other.caseItems);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      return_,
      other.return_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pipe,
      other.pipe);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      equations,
      other.equations);
  }

  public Match withCaseItems(java.util.List<hydra.coq.syntax.CaseItem> caseItems) {
    return new Match(caseItems, return_, pipe, equations);
  }

  public Match withReturn(hydra.util.Maybe<hydra.coq.syntax.Term100> return_) {
    return new Match(caseItems, return_, pipe, equations);
  }

  public Match withPipe(Boolean pipe) {
    return new Match(caseItems, return_, pipe, equations);
  }

  public Match withEquations(java.util.List<hydra.coq.syntax.Equation> equations) {
    return new Match(caseItems, return_, pipe, equations);
  }
}
