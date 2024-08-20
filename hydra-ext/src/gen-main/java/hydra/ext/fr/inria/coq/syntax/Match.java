// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Match implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Match");
  
  public static final hydra.core.Name FIELD_NAME_CASE_ITEMS = new hydra.core.Name("caseItems");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public static final hydra.core.Name FIELD_NAME_PIPE = new hydra.core.Name("pipe");
  
  public static final hydra.core.Name FIELD_NAME_EQUATIONS = new hydra.core.Name("equations");
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.CaseItem> caseItems;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term100> return_;
  
  public final Boolean pipe;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Equation> equations;
  
  public Match (java.util.List<hydra.ext.fr.inria.coq.syntax.CaseItem> caseItems, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term100> return_, Boolean pipe, java.util.List<hydra.ext.fr.inria.coq.syntax.Equation> equations) {
    java.util.Objects.requireNonNull((caseItems));
    java.util.Objects.requireNonNull((return_));
    java.util.Objects.requireNonNull((pipe));
    java.util.Objects.requireNonNull((equations));
    this.caseItems = caseItems;
    this.return_ = return_;
    this.pipe = pipe;
    this.equations = equations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Match)) {
      return false;
    }
    Match o = (Match) (other);
    return caseItems.equals(o.caseItems) && return_.equals(o.return_) && pipe.equals(o.pipe) && equations.equals(o.equations);
  }
  
  @Override
  public int hashCode() {
    return 2 * caseItems.hashCode() + 3 * return_.hashCode() + 5 * pipe.hashCode() + 7 * equations.hashCode();
  }
  
  public Match withCaseItems(java.util.List<hydra.ext.fr.inria.coq.syntax.CaseItem> caseItems) {
    java.util.Objects.requireNonNull((caseItems));
    return new Match(caseItems, return_, pipe, equations);
  }
  
  public Match withReturn(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term100> return_) {
    java.util.Objects.requireNonNull((return_));
    return new Match(caseItems, return_, pipe, equations);
  }
  
  public Match withPipe(Boolean pipe) {
    java.util.Objects.requireNonNull((pipe));
    return new Match(caseItems, return_, pipe, equations);
  }
  
  public Match withEquations(java.util.List<hydra.ext.fr.inria.coq.syntax.Equation> equations) {
    java.util.Objects.requireNonNull((equations));
    return new Match(caseItems, return_, pipe, equations);
  }
}