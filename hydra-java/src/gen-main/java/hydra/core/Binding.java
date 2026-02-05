// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A field with an optional type scheme, used to bind variables to terms in a 'let' expression
 */
public class Binding implements Serializable, Comparable<Binding> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Binding");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The name of the bound variable
   */
  public final hydra.core.Name name;
  
  /**
   * The term to which the variable is bound
   */
  public final hydra.core.Term term;
  
  /**
   * The optional type of the bound term
   */
  public final hydra.util.Maybe<hydra.core.TypeScheme> type;
  
  public Binding (hydra.core.Name name, hydra.core.Term term, hydra.util.Maybe<hydra.core.TypeScheme> type) {
    this.name = name;
    this.term = term;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Binding)) {
      return false;
    }
    Binding o = (Binding) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(term) + 5 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Binding other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (term)).compareTo(other.term);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      type.hashCode(),
      other.type.hashCode());
  }
  
  public Binding withName(hydra.core.Name name) {
    return new Binding(name, term, type);
  }
  
  public Binding withTerm(hydra.core.Term term) {
    return new Binding(name, term, type);
  }
  
  public Binding withType(hydra.util.Maybe<hydra.core.TypeScheme> type) {
    return new Binding(name, term, type);
  }
}
