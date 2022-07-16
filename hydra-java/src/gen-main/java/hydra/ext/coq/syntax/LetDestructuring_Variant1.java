package hydra.ext.coq.syntax;

public class LetDestructuring_Variant1 {
  public final java.util.List<Name> names;
  
  public final java.util.Optional<ReturnAs> returnAs;
  
  public final Term term;
  
  public LetDestructuring_Variant1 (java.util.List<Name> names, java.util.Optional<ReturnAs> returnAs, Term term) {
    this.names = names;
    this.returnAs = returnAs;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant1)) {
      return false;
    }
    LetDestructuring_Variant1 o = (LetDestructuring_Variant1) (other);
    return names.equals(o.names) && returnAs.equals(o.returnAs) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * names.hashCode() + 3 * returnAs.hashCode() + 5 * term.hashCode();
  }
  
  public LetDestructuring_Variant1 withNames(java.util.List<Name> names) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
  
  public LetDestructuring_Variant1 withReturnAs(java.util.Optional<ReturnAs> returnAs) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
  
  public LetDestructuring_Variant1 withTerm(Term term) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
}