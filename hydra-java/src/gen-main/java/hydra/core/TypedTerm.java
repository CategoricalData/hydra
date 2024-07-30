// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term together with its type
 */
public class TypedTerm implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.TypedTerm");
  
  public final hydra.core.Term term;
  
  public final hydra.core.Type type;
  
  public TypedTerm (hydra.core.Term term, hydra.core.Type type) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    this.term = term;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedTerm)) {
      return false;
    }
    TypedTerm o = (TypedTerm) (other);
    return term.equals(o.term) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * type.hashCode();
  }
  
  public TypedTerm withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new TypedTerm(term, type);
  }
  
  public TypedTerm withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypedTerm(term, type);
  }
}