// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * The result of applying inference rules to a term.
 */
public class InferenceResult implements Serializable, Comparable<InferenceResult> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.typing.InferenceResult");
  
  public static final hydra.core.Name TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name SUBST = new hydra.core.Name("subst");
  
  public static final hydra.core.Name CLASS_CONSTRAINTS = new hydra.core.Name("classConstraints");
  
  public static final hydra.core.Name CONTEXT = new hydra.core.Name("context");
  
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
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints;
  
  /**
   * The updated context after inference (carries fresh variable state)
   */
  public final hydra.context.Context context;
  
  public InferenceResult (hydra.core.Term term, hydra.core.Type type, hydra.typing.TypeSubst subst, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints, hydra.context.Context context) {
    this.term = term;
    this.type = type;
    this.subst = subst;
    this.classConstraints = classConstraints;
    this.context = context;
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
      o.classConstraints) && java.util.Objects.equals(
      this.context,
      o.context);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(subst) + 7 * java.util.Objects.hashCode(classConstraints) + 11 * java.util.Objects.hashCode(context);
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
    cmp = ((Comparable) classConstraints).compareTo(other.classConstraints);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) context).compareTo(other.context);
  }
  
  public InferenceResult withTerm(hydra.core.Term term) {
    return new InferenceResult(term, type, subst, classConstraints, context);
  }
  
  public InferenceResult withType(hydra.core.Type type) {
    return new InferenceResult(term, type, subst, classConstraints, context);
  }
  
  public InferenceResult withSubst(hydra.typing.TypeSubst subst) {
    return new InferenceResult(term, type, subst, classConstraints, context);
  }
  
  public InferenceResult withClassConstraints(hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    return new InferenceResult(term, type, subst, classConstraints, context);
  }
  
  public InferenceResult withContext(hydra.context.Context context) {
    return new InferenceResult(term, type, subst, classConstraints, context);
  }
}
