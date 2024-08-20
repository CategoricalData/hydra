// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class ReturnAs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.ReturnAs");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> as;
  
  public final hydra.ext.fr.inria.coq.syntax.Term100 return_;
  
  public ReturnAs (hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> as, hydra.ext.fr.inria.coq.syntax.Term100 return_) {
    java.util.Objects.requireNonNull((as));
    java.util.Objects.requireNonNull((return_));
    this.as = as;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnAs)) {
      return false;
    }
    ReturnAs o = (ReturnAs) (other);
    return as.equals(o.as) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * as.hashCode() + 3 * return_.hashCode();
  }
  
  public ReturnAs withAs(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> as) {
    java.util.Objects.requireNonNull((as));
    return new ReturnAs(as, return_);
  }
  
  public ReturnAs withReturn(hydra.ext.fr.inria.coq.syntax.Term100 return_) {
    java.util.Objects.requireNonNull((return_));
    return new ReturnAs(as, return_);
  }
}