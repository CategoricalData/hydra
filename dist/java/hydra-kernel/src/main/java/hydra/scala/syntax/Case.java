// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Case implements Serializable, Comparable<Case> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Case");

  public static final hydra.core.Name PAT = new hydra.core.Name("pat");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.scala.syntax.Pat pat;

  public final hydra.util.Maybe<hydra.scala.syntax.Data> cond;

  public final hydra.scala.syntax.Data body;

  public Case (hydra.scala.syntax.Pat pat, hydra.util.Maybe<hydra.scala.syntax.Data> cond, hydra.scala.syntax.Data body) {
    this.pat = pat;
    this.cond = cond;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Case)) {
      return false;
    }
    Case o = (Case) other;
    return java.util.Objects.equals(
      this.pat,
      o.pat) && java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pat) + 3 * java.util.Objects.hashCode(cond) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Case other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pat,
      other.pat);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      cond,
      other.cond);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Case withPat(hydra.scala.syntax.Pat pat) {
    return new Case(pat, cond, body);
  }

  public Case withCond(hydra.util.Maybe<hydra.scala.syntax.Data> cond) {
    return new Case(pat, cond, body);
  }

  public Case withBody(hydra.scala.syntax.Data body) {
    return new Case(pat, cond, body);
  }
}
