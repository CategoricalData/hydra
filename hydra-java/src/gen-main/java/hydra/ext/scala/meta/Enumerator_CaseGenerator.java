// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Enumerator_CaseGenerator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Enumerator.CaseGenerator");
  
  public static final hydra.core.Name FIELD_NAME_PAT = new hydra.core.Name("pat");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Pat pat;
  
  public final hydra.ext.scala.meta.Data rhs;
  
  public Enumerator_CaseGenerator (hydra.ext.scala.meta.Pat pat, hydra.ext.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((pat));
    java.util.Objects.requireNonNull((rhs));
    this.pat = pat;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_CaseGenerator)) {
      return false;
    }
    Enumerator_CaseGenerator o = (Enumerator_CaseGenerator) (other);
    return pat.equals(o.pat) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * pat.hashCode() + 3 * rhs.hashCode();
  }
  
  public Enumerator_CaseGenerator withPat(hydra.ext.scala.meta.Pat pat) {
    java.util.Objects.requireNonNull((pat));
    return new Enumerator_CaseGenerator(pat, rhs);
  }
  
  public Enumerator_CaseGenerator withRhs(hydra.ext.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Enumerator_CaseGenerator(pat, rhs);
  }
}