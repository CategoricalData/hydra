// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class FixAnnot_Wf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public final hydra.ext.fr.inria.coq.syntax.OneTerm term;
  
  public final hydra.ext.fr.inria.coq.syntax.Ident ident;
  
  public FixAnnot_Wf (hydra.ext.fr.inria.coq.syntax.OneTerm term, hydra.ext.fr.inria.coq.syntax.Ident ident) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((ident));
    this.term = term;
    this.ident = ident;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixAnnot_Wf)) {
      return false;
    }
    FixAnnot_Wf o = (FixAnnot_Wf) (other);
    return term.equals(o.term) && ident.equals(o.ident);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * ident.hashCode();
  }
  
  public FixAnnot_Wf withTerm(hydra.ext.fr.inria.coq.syntax.OneTerm term) {
    java.util.Objects.requireNonNull((term));
    return new FixAnnot_Wf(term, ident);
  }
  
  public FixAnnot_Wf withIdent(hydra.ext.fr.inria.coq.syntax.Ident ident) {
    java.util.Objects.requireNonNull((ident));
    return new FixAnnot_Wf(term, ident);
  }
}