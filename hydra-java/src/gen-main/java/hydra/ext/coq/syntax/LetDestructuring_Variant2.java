package hydra.ext.coq.syntax;

public class LetDestructuring_Variant2 {
  public final Pattern pattern;
  
  public final Term term;
  
  public final java.util.Optional<Term100> return_;
  
  public LetDestructuring_Variant2 (Pattern pattern, Term term, java.util.Optional<Term100> return_) {
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
  
  public LetDestructuring_Variant2 withPattern(Pattern pattern) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
  
  public LetDestructuring_Variant2 withTerm(Term term) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
  
  public LetDestructuring_Variant2 withReturn(java.util.Optional<Term100> return_) {
    return new LetDestructuring_Variant2(pattern, term, return_);
  }
}