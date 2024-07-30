// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term annotated with its type
 */
public class TermWithType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.TermWithType");
  
  public final hydra.core.Term term;
  
  public final hydra.core.Type type;
  
  public TermWithType (hydra.core.Term term, hydra.core.Type type) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    this.term = term;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermWithType)) {
      return false;
    }
    TermWithType o = (TermWithType) (other);
    return term.equals(o.term) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * type.hashCode();
  }
  
  public TermWithType withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new TermWithType(term, type);
  }
  
  public TermWithType withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TermWithType(term, type);
  }
}