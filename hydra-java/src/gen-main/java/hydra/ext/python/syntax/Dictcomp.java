// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Dictcomp implements Serializable, Comparable<Dictcomp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Dictcomp");
  
  public static final hydra.core.Name FIELD_NAME_KVPAIR = new hydra.core.Name("kvpair");
  
  public static final hydra.core.Name FIELD_NAME_FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");
  
  public final hydra.ext.python.syntax.Kvpair kvpair;
  
  public final hydra.ext.python.syntax.ForIfClauses forIfClauses;
  
  public Dictcomp (hydra.ext.python.syntax.Kvpair kvpair, hydra.ext.python.syntax.ForIfClauses forIfClauses) {
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
    cmp = ((Comparable) kvpair).compareTo(other.kvpair);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) forIfClauses).compareTo(other.forIfClauses);
  }
  
  public Dictcomp withKvpair(hydra.ext.python.syntax.Kvpair kvpair) {
    return new Dictcomp(kvpair, forIfClauses);
  }
  
  public Dictcomp withForIfClauses(hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    return new Dictcomp(kvpair, forIfClauses);
  }
}
