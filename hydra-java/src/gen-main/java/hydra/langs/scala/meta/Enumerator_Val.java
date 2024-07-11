// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Enumerator_Val implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Enumerator.Val");
  
  public final hydra.langs.scala.meta.Pat pat;
  
  public final hydra.langs.scala.meta.Data rhs;
  
  public Enumerator_Val (hydra.langs.scala.meta.Pat pat, hydra.langs.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((pat));
    java.util.Objects.requireNonNull((rhs));
    this.pat = pat;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Val)) {
      return false;
    }
    Enumerator_Val o = (Enumerator_Val) (other);
    return pat.equals(o.pat) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * rhs.hashCode();
  }
  
  public Enumerator_Val withPat(hydra.langs.scala.meta.Pat pat) {
    java.util.Objects.requireNonNull((pat));
    return new Enumerator_Val(pat, rhs);
  }
  
  public Enumerator_Val withRhs(hydra.langs.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Enumerator_Val(pat, rhs);
  }
}