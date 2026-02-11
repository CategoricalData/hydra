// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * The result of applying inference rules to a term.
 */
public class InferenceResult implements Serializable, Comparable<InferenceResult> {
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
    InferenceResult o = (InferenceResult) other;
    return java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.subst,
      o.subst) && java.util.Objects.equals(
      this.classConstraints,
      o.classConstraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(subst) + 7 * java.util.Objects.hashCode(classConstraints);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InferenceResult other) {
    int cmp = 0;
    cmp = ((Comparable) term).compareTo(other.term);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) subst).compareTo(other.subst);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      classConstraints.hashCode(),
      other.classConstraints.hashCode());
  }
  
  public InferenceResult withTerm(hydra.core.Term term) {
    return new InferenceResult(term, type, subst, classConstraints);
  }
  
  public InferenceResult withType(hydra.core.Type type) {
    return new InferenceResult(term, type, subst, classConstraints);
  }
  
  public InferenceResult withSubst(hydra.typing.TypeSubst subst) {
    return new InferenceResult(term, type, subst, classConstraints);
  }
  
  public InferenceResult withClassConstraints(java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    return new InferenceResult(term, type, subst, classConstraints);
  }
}
