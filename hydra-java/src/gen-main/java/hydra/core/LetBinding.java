// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A field with an optional type scheme, used to bind variables to terms in a 'let' expression
 */
public class LetBinding implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.LetBinding");
  
  public final hydra.core.Name name;
  
  public final hydra.core.Term term;
  
  public final hydra.util.Opt<hydra.core.TypeScheme> type;
  
  public LetBinding (hydra.core.Name name, hydra.core.Term term, hydra.util.Opt<hydra.core.TypeScheme> type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    this.name = name;
    this.term = term;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetBinding)) {
      return false;
    }
    LetBinding o = (LetBinding) (other);
    return name.equals(o.name) && term.equals(o.term) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode() + 5 * type.hashCode();
  }
  
  public LetBinding withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new LetBinding(name, term, type);
  }
  
  public LetBinding withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new LetBinding(name, term, type);
  }
  
  public LetBinding withType(hydra.util.Opt<hydra.core.TypeScheme> type) {
    java.util.Objects.requireNonNull((type));
    return new LetBinding(name, term, type);
  }
}