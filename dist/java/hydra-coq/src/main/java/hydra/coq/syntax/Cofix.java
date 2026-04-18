// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Cofix implements Serializable, Comparable<Cofix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Cofix");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name QUAL = new hydra.core.Name("qual");

  public final hydra.coq.syntax.CofixBody body;

  public final hydra.util.Maybe<hydra.coq.syntax.CofixQual> qual;

  public Cofix (hydra.coq.syntax.CofixBody body, hydra.util.Maybe<hydra.coq.syntax.CofixQual> qual) {
    this.body = body;
    this.qual = qual;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Cofix)) {
      return false;
    }
    Cofix o = (Cofix) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.qual,
      o.qual);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(qual);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Cofix other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      qual,
      other.qual);
  }

  public Cofix withBody(hydra.coq.syntax.CofixBody body) {
    return new Cofix(body, qual);
  }

  public Cofix withQual(hydra.util.Maybe<hydra.coq.syntax.CofixQual> qual) {
    return new Cofix(body, qual);
  }
}
