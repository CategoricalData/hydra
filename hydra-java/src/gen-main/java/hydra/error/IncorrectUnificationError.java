// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A post-unification consistency check failure
 */
public class IncorrectUnificationError implements Serializable, Comparable<IncorrectUnificationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.IncorrectUnificationError");
  
  public static final hydra.core.Name SUBSTITUTION = new hydra.core.Name("substitution");
  
  /**
   * The substitution that failed the consistency check
   */
  public final hydra.typing.TypeSubst substitution;
  
  public IncorrectUnificationError (hydra.typing.TypeSubst substitution) {
    this.substitution = substitution;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IncorrectUnificationError)) {
      return false;
    }
    IncorrectUnificationError o = (IncorrectUnificationError) other;
    return java.util.Objects.equals(
      this.substitution,
      o.substitution);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(substitution);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IncorrectUnificationError other) {
    return ((Comparable) substitution).compareTo(other.substitution);
  }
}
