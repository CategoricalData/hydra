// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A field with an optional type scheme, used to bind variables to terms in a 'let' expression
 */
public class Binding implements Serializable {
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
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
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
    return name.equals(o.name) && term.equals(o.term) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode() + 5 * type.hashCode();
  }
  
  public Binding withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Binding(name, term, type);
  }
  
  public Binding withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new Binding(name, term, type);
  }
  
  public Binding withType(hydra.util.Maybe<hydra.core.TypeScheme> type) {
    java.util.Objects.requireNonNull((type));
    return new Binding(name, term, type);
  }
}
