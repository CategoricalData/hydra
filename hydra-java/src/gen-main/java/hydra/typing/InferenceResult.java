// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * The result of applying inference rules to a term.
 */
public class InferenceResult implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.InferenceResult");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_SUBST = new hydra.core.Name("subst");
  
  /**
   * The term which was inferred
   */
  public final hydra.core.Term term;
  
  /**
   * The inferred type of the term
   */
  public final hydra.core.Type type;
  
  /**
   * The type substitution resulting from unification
   */
  public final hydra.typing.TypeSubst subst;
  
  public InferenceResult (hydra.core.Term term, hydra.core.Type type, hydra.typing.TypeSubst subst) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((subst));
    this.term = term;
    this.type = type;
    this.subst = subst;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceResult)) {
      return false;
    }
    InferenceResult o = (InferenceResult) (other);
    return term.equals(o.term) && type.equals(o.type) && subst.equals(o.subst);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * type.hashCode() + 5 * subst.hashCode();
  }
  
  public InferenceResult withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new InferenceResult(term, type, subst);
  }
  
  public InferenceResult withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new InferenceResult(term, type, subst);
  }
  
  public InferenceResult withSubst(hydra.typing.TypeSubst subst) {
    java.util.Objects.requireNonNull((subst));
    return new InferenceResult(term, type, subst);
  }
}
