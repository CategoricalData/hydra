// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Dictcomp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Dictcomp");
  
  public static final hydra.core.Name FIELD_NAME_KVPAIR = new hydra.core.Name("kvpair");
  
  public static final hydra.core.Name FIELD_NAME_FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");
  
  public final hydra.ext.python.syntax.Kvpair kvpair;
  
  public final hydra.ext.python.syntax.ForIfClauses forIfClauses;
  
  public Dictcomp (hydra.ext.python.syntax.Kvpair kvpair, hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    java.util.Objects.requireNonNull((kvpair));
    java.util.Objects.requireNonNull((forIfClauses));
    this.kvpair = kvpair;
    this.forIfClauses = forIfClauses;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Dictcomp)) {
      return false;
    }
    Dictcomp o = (Dictcomp) (other);
    return kvpair.equals(o.kvpair) && forIfClauses.equals(o.forIfClauses);
  }
  
  @Override
  public int hashCode() {
    return 2 * kvpair.hashCode() + 3 * forIfClauses.hashCode();
  }
  
  public Dictcomp withKvpair(hydra.ext.python.syntax.Kvpair kvpair) {
    java.util.Objects.requireNonNull((kvpair));
    return new Dictcomp(kvpair, forIfClauses);
  }
  
  public Dictcomp withForIfClauses(hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    java.util.Objects.requireNonNull((forIfClauses));
    return new Dictcomp(kvpair, forIfClauses);
  }
}