// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Enumerator_Generator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Enumerator.Generator");
  
  public static final hydra.core.Name FIELD_NAME_PAT = new hydra.core.Name("pat");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.langs.scala.meta.Pat pat;
  
  public final hydra.langs.scala.meta.Data rhs;
  
  public Enumerator_Generator (hydra.langs.scala.meta.Pat pat, hydra.langs.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((pat));
    java.util.Objects.requireNonNull((rhs));
    this.pat = pat;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Generator)) {
      return false;
    }
    Enumerator_Generator o = (Enumerator_Generator) (other);
    return pat.equals(o.pat) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * rhs.hashCode();
  }
  
  public Enumerator_Generator withPat(hydra.langs.scala.meta.Pat pat) {
    java.util.Objects.requireNonNull((pat));
    return new Enumerator_Generator(pat, rhs);
  }
  
  public Enumerator_Generator withRhs(hydra.langs.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Enumerator_Generator(pat, rhs);
  }
}