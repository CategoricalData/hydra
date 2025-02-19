// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class TypeclassConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.TypeclassConstraint");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_GENERALIZING = new hydra.core.Name("generalizing");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> name;
  
  public final Boolean generalizing;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public TypeclassConstraint (hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> name, Boolean generalizing, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((generalizing));
    java.util.Objects.requireNonNull((term));
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
  
  public TypeclassConstraint withName(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Name> name) {
    java.util.Objects.requireNonNull((name));
    return new TypeclassConstraint(name, generalizing, term);
  }
  
  public TypeclassConstraint withGeneralizing(Boolean generalizing) {
    java.util.Objects.requireNonNull((generalizing));
    return new TypeclassConstraint(name, generalizing, term);
  }
  
  public TypeclassConstraint withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new TypeclassConstraint(name, generalizing, term);
  }
}