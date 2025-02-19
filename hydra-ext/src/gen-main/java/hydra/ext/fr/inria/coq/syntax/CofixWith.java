// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class CofixWith implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.CofixWith");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  public static final hydra.core.Name FIELD_NAME_FOR = new hydra.core.Name("for");
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.CofixBody> with;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> for_;
  
  public CofixWith (java.util.List<hydra.ext.fr.inria.coq.syntax.CofixBody> with, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> for_) {
    java.util.Objects.requireNonNull((with));
    java.util.Objects.requireNonNull((for_));
    this.with = with;
    this.for_ = for_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CofixWith)) {
      return false;
    }
    CofixWith o = (CofixWith) (other);
    return with.equals(o.with) && for_.equals(o.for_);
  }
  
  @Override
  public int hashCode() {
    return 2 * with.hashCode() + 3 * for_.hashCode();
  }
  
  public CofixWith withWith(java.util.List<hydra.ext.fr.inria.coq.syntax.CofixBody> with) {
    java.util.Objects.requireNonNull((with));
    return new CofixWith(with, for_);
  }
  
  public CofixWith withFor(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> for_) {
    java.util.Objects.requireNonNull((for_));
    return new CofixWith(with, for_);
  }
}