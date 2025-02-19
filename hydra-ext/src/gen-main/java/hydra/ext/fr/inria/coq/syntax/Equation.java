// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Equation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Equation");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final java.util.List<java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern>> pattern;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public Equation (java.util.List<java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern>> pattern, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((term));
    this.pattern = pattern;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Equation)) {
      return false;
    }
    Equation o = (Equation) (other);
    return pattern.equals(o.pattern) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * term.hashCode();
  }
  
  public Equation withPattern(java.util.List<java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern>> pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new Equation(pattern, term);
  }
  
  public Equation withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new Equation(pattern, term);
  }
}