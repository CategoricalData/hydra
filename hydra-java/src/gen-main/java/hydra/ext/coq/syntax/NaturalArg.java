package hydra.ext.coq.syntax;

public class NaturalArg {
  public final Natural natural;
  
  public final Term term;
  
  public NaturalArg (Natural natural, Term term) {
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
  
  public NaturalArg withNatural(Natural natural) {
    return new NaturalArg(natural, term);
  }
  
  public NaturalArg withTerm(Term term) {
    return new NaturalArg(natural, term);
  }
}