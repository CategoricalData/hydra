package hydra.ext.coq.syntax;

public class LetDestructuring_Variant3 {
  public final Pattern pattern1;
  
  public final Pattern pattern2;
  
  public final Term term;
  
  public final Term100 return_;
  
  public LetDestructuring_Variant3 (Pattern pattern1, Pattern pattern2, Term term, Term100 return_) {
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
  
  public LetDestructuring_Variant3 withPattern1(Pattern pattern1) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
  
  public LetDestructuring_Variant3 withPattern2(Pattern pattern2) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
  
  public LetDestructuring_Variant3 withTerm(Term term) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
  
  public LetDestructuring_Variant3 withReturn(Term100 return_) {
    return new LetDestructuring_Variant3(pattern1, pattern2, term, return_);
  }
}