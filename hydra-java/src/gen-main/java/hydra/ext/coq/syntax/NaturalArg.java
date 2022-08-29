package hydra.ext.coq.syntax;

public class NaturalArg {
  public final hydra.ext.coq.syntax.Natural natural;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public NaturalArg (hydra.ext.coq.syntax.Natural natural, hydra.ext.coq.syntax.Term term) {
    this.natural = natural;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NaturalArg)) {
      return false;
    }
    NaturalArg o = (NaturalArg) (other);
    return natural.equals(o.natural) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * natural.hashCode() + 3 * term.hashCode();
  }
  
  public NaturalArg withNatural(hydra.ext.coq.syntax.Natural natural) {
    return new NaturalArg(natural, term);
  }
  
  public NaturalArg withTerm(hydra.ext.coq.syntax.Term term) {
    return new NaturalArg(natural, term);
  }
}