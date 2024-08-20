// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class LetDestructuring_Variant3 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.LetDestructuring.Variant3");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN1 = new hydra.core.Name("pattern1");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN2 = new hydra.core.Name("pattern2");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern pattern1;
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern pattern2;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public final hydra.ext.fr.inria.coq.syntax.Term100 return_;
  
  public LetDestructuring_Variant3 (hydra.ext.fr.inria.coq.syntax.Pattern pattern1, hydra.ext.fr.inria.coq.syntax.Pattern pattern2, hydra.ext.fr.inria.coq.syntax.Term term, hydra.ext.fr.inria.coq.syntax.Term100 return_) {
    java.util.Objects.requireNonNull((pattern1));
    java.util.Objects.requireNonNull((pattern2));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((return_));
    this.pattern1 = pattern1;
    this.pattern2 = pattern2;
    this.term = term;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant3)) {
      return false;
    }
    LetDestructuring_Variant3 o = (LetDestructuring_Variant3) (other);
    return pattern1.equals(o.pattern1) && pattern2.equals(o.pattern2) && term.equals(o.term) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern1.hashCode() + 3 * pattern2.hashCode() + 5 * term.hashCode() + 7 * return_.hashCode();
  }
  
  public LetDestructuring_Variant3 withPattern1(hydra.ext.fr.inria.coq.syntax.Pattern pattern1) {
    java.util.Objects.requireNonNull((pattern1));
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
  
  public LetDestructuring_Variant3 withPattern2(hydra.ext.fr.inria.coq.syntax.Pattern pattern2) {
    java.util.Objects.requireNonNull((pattern2));
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
  
  public LetDestructuring_Variant3 withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
  
  public LetDestructuring_Variant3 withReturn(hydra.ext.fr.inria.coq.syntax.Term100 return_) {
    java.util.Objects.requireNonNull((return_));
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
}