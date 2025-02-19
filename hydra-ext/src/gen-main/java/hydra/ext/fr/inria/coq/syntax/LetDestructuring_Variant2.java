// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class LetDestructuring_Variant2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern pattern;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term100> return_;
  
  public LetDestructuring_Variant2 (hydra.ext.fr.inria.coq.syntax.Pattern pattern, hydra.ext.fr.inria.coq.syntax.Term term, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term100> return_) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((return_));
    this.pattern = pattern;
    this.term = term;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant2)) {
      return false;
    }
    LetDestructuring_Variant2 o = (LetDestructuring_Variant2) (other);
    return pattern.equals(o.pattern) && term.equals(o.term) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * term.hashCode() + 5 * return_.hashCode();
  }
  
  public LetDestructuring_Variant2 withPattern(hydra.ext.fr.inria.coq.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
  
  public LetDestructuring_Variant2 withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
  
  public LetDestructuring_Variant2 withReturn(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term100> return_) {
    java.util.Objects.requireNonNull((return_));
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
}