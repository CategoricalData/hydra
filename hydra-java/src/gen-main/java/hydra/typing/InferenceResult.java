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
  
  public static final hydra.core.Name FIELD_NAME_CLASS_CONSTRAINTS = new hydra.core.Name("classConstraints");
  
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
  
  /**
   * Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints;
  
  public InferenceResult (hydra.core.Term term, hydra.core.Type type, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((subst));
    java.util.Objects.requireNonNull((classConstraints));
    this.term = term;
    this.type = type;
    this.subst = subst;
    this.classConstraints = classConstraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceResult)) {
      return false;
    }
    InferenceResult o = (InferenceResult) (other);
    return term.equals(o.term) && type.equals(o.type) && subst.equals(o.subst) && classConstraints.equals(o.classConstraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * type.hashCode() + 5 * subst.hashCode() + 7 * classConstraints.hashCode();
  }
  
  public InferenceResult withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new InferenceResult(term, type, subst, classConstraints);
  }
  
  public InferenceResult withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new InferenceResult(term, type, subst, classConstraints);
  }
  
  public InferenceResult withSubst(hydra.typing.TypeSubst subst) {
    java.util.Objects.requireNonNull((subst));
    return new InferenceResult(term, type, subst, classConstraints);
  }
  
  public InferenceResult withClassConstraints(java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    java.util.Objects.requireNonNull((classConstraints));
    return new InferenceResult(term, type, subst, classConstraints);
  }
}
