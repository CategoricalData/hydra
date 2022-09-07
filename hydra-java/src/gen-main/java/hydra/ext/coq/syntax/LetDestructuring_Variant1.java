package hydra.ext.coq.syntax;

public class LetDestructuring_Variant1 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.LetDestructuring.Variant1");
  
  public final java.util.List<hydra.ext.coq.syntax.Name> names;
  
  public final java.util.Optional<hydra.ext.coq.syntax.ReturnAs> returnAs;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public LetDestructuring_Variant1 (java.util.List<hydra.ext.coq.syntax.Name> names, java.util.Optional<hydra.ext.coq.syntax.ReturnAs> returnAs, hydra.ext.coq.syntax.Term term) {
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
  
  public LetDestructuring_Variant1 withNames(java.util.List<hydra.ext.coq.syntax.Name> names) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
  
  public LetDestructuring_Variant1 withReturnAs(java.util.Optional<hydra.ext.coq.syntax.ReturnAs> returnAs) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
  
  public LetDestructuring_Variant1 withTerm(hydra.ext.coq.syntax.Term term) {
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
}