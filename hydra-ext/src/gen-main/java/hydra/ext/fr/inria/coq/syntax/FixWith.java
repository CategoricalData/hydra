// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class FixWith implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.FixWith");
  
  public static final hydra.core.Name FIELD_NAME_DECLS = new hydra.core.Name("decls");
  
  public static final hydra.core.Name FIELD_NAME_FOR = new hydra.core.Name("for");
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Fix_Decl> decls;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> for_;
  
  public FixWith (java.util.List<hydra.ext.fr.inria.coq.syntax.Fix_Decl> decls, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> for_) {
    java.util.Objects.requireNonNull((decls));
    java.util.Objects.requireNonNull((for_));
    this.decls = decls;
    this.for_ = for_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixWith)) {
      return false;
    }
    FixWith o = (FixWith) (other);
    return decls.equals(o.decls) && for_.equals(o.for_);
  }
  
  @Override
  public int hashCode() {
    return 2 * decls.hashCode() + 3 * for_.hashCode();
  }
  
  public FixWith withDecls(java.util.List<hydra.ext.fr.inria.coq.syntax.Fix_Decl> decls) {
    java.util.Objects.requireNonNull((decls));
    return new FixWith(decls, for_);
  }
  
  public FixWith withFor(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> for_) {
    java.util.Objects.requireNonNull((for_));
    return new FixWith(decls, for_);
  }
}