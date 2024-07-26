// Note: this is an automatically generated file. Do not edit.

package hydra.mantle;

import java.io.Serializable;

/**
 * A type together with an instance of the type
 */
public class TypedTerm implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/mantle.TypedTerm");
  
  public final hydra.core.Type type;
  
  public final hydra.core.Term term;
  
  public TypedTerm (hydra.core.Type type, hydra.core.Term term) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((term));
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedTerm)) {
      return false;
    }
    TypedTerm o = (TypedTerm) (other);
    return type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * term.hashCode();
  }
  
  public TypedTerm withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypedTerm(type, term);
  }
  
  public TypedTerm withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new TypedTerm(type, term);
  }
}