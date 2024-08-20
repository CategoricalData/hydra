// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class NormalApplication implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.NormalApplication");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.fr.inria.coq.syntax.Term1 lhs;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Arg> rhs;
  
  public NormalApplication (hydra.ext.fr.inria.coq.syntax.Term1 lhs, java.util.List<hydra.ext.fr.inria.coq.syntax.Arg> rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalApplication)) {
      return false;
    }
    NormalApplication o = (NormalApplication) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public NormalApplication withLhs(hydra.ext.fr.inria.coq.syntax.Term1 lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new NormalApplication(lhs, rhs);
  }
  
  public NormalApplication withRhs(java.util.List<hydra.ext.fr.inria.coq.syntax.Arg> rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new NormalApplication(lhs, rhs);
  }
}