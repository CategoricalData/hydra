// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class CaseItem implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.CaseItem");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public final hydra.ext.fr.inria.coq.syntax.Term100 term;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> as;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Pattern> in;
  
  public CaseItem (hydra.ext.fr.inria.coq.syntax.Term100 term, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> as, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Pattern> in) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((as));
    java.util.Objects.requireNonNull((in));
    this.term = term;
    this.as = as;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseItem)) {
      return false;
    }
    CaseItem o = (CaseItem) (other);
    return term.equals(o.term) && as.equals(o.as) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * as.hashCode() + 5 * in.hashCode();
  }
  
  public CaseItem withTerm(hydra.ext.fr.inria.coq.syntax.Term100 term) {
    java.util.Objects.requireNonNull((term));
    return new CaseItem(term, as, in);
  }
  
  public CaseItem withAs(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> as) {
    java.util.Objects.requireNonNull((as));
    return new CaseItem(term, as, in);
  }
  
  public CaseItem withIn(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Pattern> in) {
    java.util.Objects.requireNonNull((in));
    return new CaseItem(term, as, in);
  }
}