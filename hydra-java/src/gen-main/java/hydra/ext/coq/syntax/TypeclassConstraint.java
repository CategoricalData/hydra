package hydra.ext.coq.syntax;

public class TypeclassConstraint {
  public final java.util.Optional<Name> name;
  
  public final Boolean generalizing;
  
  public final Term term;
  
  public TypeclassConstraint (java.util.Optional<Name> name, Boolean generalizing, Term term) {
    this.name = name;
    this.generalizing = generalizing;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeclassConstraint)) {
      return false;
    }
    TypeclassConstraint o = (TypeclassConstraint) (other);
    return name.equals(o.name) && generalizing.equals(o.generalizing) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * generalizing.hashCode() + 5 * term.hashCode();
  }
  
  public TypeclassConstraint withName(java.util.Optional<Name> name) {
    return new TypeclassConstraint(name, generalizing, term);
  }
  
  public TypeclassConstraint withGeneralizing(Boolean generalizing) {
    return new TypeclassConstraint(name, generalizing, term);
  }
  
  public TypeclassConstraint withTerm(Term term) {
    return new TypeclassConstraint(name, generalizing, term);
  }
}