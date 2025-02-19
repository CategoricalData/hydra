// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class IdentArg implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.IdentArg");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.ext.fr.inria.coq.syntax.Ident ident;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public IdentArg (hydra.ext.fr.inria.coq.syntax.Ident ident, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((ident));
    java.util.Objects.requireNonNull((term));
    this.ident = ident;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdentArg)) {
      return false;
    }
    IdentArg o = (IdentArg) (other);
    return ident.equals(o.ident) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * term.hashCode();
  }
  
  public IdentArg withIdent(hydra.ext.fr.inria.coq.syntax.Ident ident) {
    java.util.Objects.requireNonNull((ident));
    return new IdentArg(ident, term);
  }
  
  public IdentArg withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new IdentArg(ident, term);
  }
}