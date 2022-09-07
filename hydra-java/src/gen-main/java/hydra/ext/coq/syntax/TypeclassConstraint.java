package hydra.ext.coq.syntax;

public class TypeclassConstraint {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.TypeclassConstraint");
  
  public final java.util.Optional<hydra.ext.coq.syntax.Name> name;
  
  public final Boolean generalizing;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public TypeclassConstraint (java.util.Optional<hydra.ext.coq.syntax.Name> name, Boolean generalizing, hydra.ext.coq.syntax.Term term) {
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
  
  public TypeclassConstraint withName(java.util.Optional<hydra.ext.coq.syntax.Name> name) {
    return new TypeclassConstraint(name, generalizing, term);
  }
  
  public TypeclassConstraint withGeneralizing(Boolean generalizing) {
    return new TypeclassConstraint(name, generalizing, term);
  }
  
  public TypeclassConstraint withTerm(hydra.ext.coq.syntax.Term term) {
    return new TypeclassConstraint(name, generalizing, term);
  }
}