package hydra.ext.coq.syntax;

public class LetDestructuring_Variant2 {
  public final hydra.ext.coq.syntax.Pattern pattern;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Term100> return_;
  
  public LetDestructuring_Variant2 (hydra.ext.coq.syntax.Pattern pattern, hydra.ext.coq.syntax.Term term, java.util.Optional<hydra.ext.coq.syntax.Term100> return_) {
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
  
  public LetDestructuring_Variant2 withPattern(hydra.ext.coq.syntax.Pattern pattern) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
  
  public LetDestructuring_Variant2 withTerm(hydra.ext.coq.syntax.Term term) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
  
  public LetDestructuring_Variant2 withReturn(java.util.Optional<hydra.ext.coq.syntax.Term100> return_) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
}