// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Dictcomp implements Serializable, Comparable<Dictcomp> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Dictcomp");

  public static final hydra.core.Name KVPAIR = new hydra.core.Name("kvpair");

  public static final hydra.core.Name FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");

  public final hydra.python.syntax.Kvpair kvpair;

  public final hydra.python.syntax.ForIfClauses forIfClauses;

  public Dictcomp (hydra.python.syntax.Kvpair kvpair, hydra.python.syntax.ForIfClauses forIfClauses) {
    this.kvpair = kvpair;
    this.forIfClauses = forIfClauses;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Dictcomp)) {
      return false;
    }
    Dictcomp o = (Dictcomp) other;
    return java.util.Objects.equals(
      this.kvpair,
      o.kvpair) && java.util.Objects.equals(
      this.forIfClauses,
      o.forIfClauses);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kvpair) + 3 * java.util.Objects.hashCode(forIfClauses);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Dictcomp other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      kvpair,
      other.kvpair);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      forIfClauses,
      other.forIfClauses);
  }

  public Dictcomp withKvpair(hydra.python.syntax.Kvpair kvpair) {
    return new Dictcomp(kvpair, forIfClauses);
  }

  public Dictcomp withForIfClauses(hydra.python.syntax.ForIfClauses forIfClauses) {
    return new Dictcomp(kvpair, forIfClauses);
  }
}
