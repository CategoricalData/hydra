// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class FixAnnot_Measure implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.FixAnnot.Measure");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public static final hydra.core.Name FIELD_NAME_TERM2 = new hydra.core.Name("term2");
  
  public final hydra.ext.fr.inria.coq.syntax.OneTerm term;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> ident;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.OneTerm> term2;
  
  public FixAnnot_Measure (hydra.ext.fr.inria.coq.syntax.OneTerm term, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> ident, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.OneTerm> term2) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((ident));
    java.util.Objects.requireNonNull((term2));
    this.term = term;
    this.ident = ident;
    this.term2 = term2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixAnnot_Measure)) {
      return false;
    }
    FixAnnot_Measure o = (FixAnnot_Measure) (other);
    return term.equals(o.term) && ident.equals(o.ident) && term2.equals(o.term2);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * ident.hashCode() + 5 * term2.hashCode();
  }
  
  public FixAnnot_Measure withTerm(hydra.ext.fr.inria.coq.syntax.OneTerm term) {
    java.util.Objects.requireNonNull((term));
    return new FixAnnot_Measure(term, ident, term2);
  }
  
  public FixAnnot_Measure withIdent(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Ident> ident) {
    java.util.Objects.requireNonNull((ident));
    return new FixAnnot_Measure(term, ident, term2);
  }
  
  public FixAnnot_Measure withTerm2(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.OneTerm> term2) {
    java.util.Objects.requireNonNull((term2));
    return new FixAnnot_Measure(term, ident, term2);
  }
}