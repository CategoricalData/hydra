// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class TypeclassConstraint implements Serializable, Comparable<TypeclassConstraint> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.TypeclassConstraint");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name GENERALIZING = new hydra.core.Name("generalizing");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final hydra.util.Maybe<hydra.coq.syntax.Name> name;

  public final Boolean generalizing;

  public final hydra.coq.syntax.Term term;

  public TypeclassConstraint (hydra.util.Maybe<hydra.coq.syntax.Name> name, Boolean generalizing, hydra.coq.syntax.Term term) {
    this.name = name;
    this.generalizing = generalizing;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeclassConstraint)) {
      return false;
    }
    TypeclassConstraint o = (TypeclassConstraint) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.generalizing,
      o.generalizing) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(generalizing) + 5 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeclassConstraint other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      generalizing,
      other.generalizing);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term,
      other.term);
  }

  public TypeclassConstraint withName(hydra.util.Maybe<hydra.coq.syntax.Name> name) {
    return new TypeclassConstraint(name, generalizing, term);
  }

  public TypeclassConstraint withGeneralizing(Boolean generalizing) {
    return new TypeclassConstraint(name, generalizing, term);
  }

  public TypeclassConstraint withTerm(hydra.coq.syntax.Term term) {
    return new TypeclassConstraint(name, generalizing, term);
  }
}
