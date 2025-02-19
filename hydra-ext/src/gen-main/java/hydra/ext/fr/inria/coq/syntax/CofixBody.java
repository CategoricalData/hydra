// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class CofixBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.CofixBody");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public static final hydra.core.Name FIELD_NAME_BINDERS = new hydra.core.Name("binders");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.ext.fr.inria.coq.syntax.Ident ident;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public CofixBody (hydra.ext.fr.inria.coq.syntax.Ident ident, java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((ident));
    java.util.Objects.requireNonNull((binders));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((term));
    this.ident = ident;
    this.binders = binders;
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CofixBody)) {
      return false;
    }
    CofixBody o = (CofixBody) (other);
    return ident.equals(o.ident) && binders.equals(o.binders) && type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * binders.hashCode() + 5 * type.hashCode() + 7 * term.hashCode();
  }
  
  public CofixBody withIdent(hydra.ext.fr.inria.coq.syntax.Ident ident) {
    java.util.Objects.requireNonNull((ident));
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withBinders(java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders) {
    java.util.Objects.requireNonNull((binders));
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withType(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type) {
    java.util.Objects.requireNonNull((type));
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new CofixBody(ident, binders, type, term);
  }
}