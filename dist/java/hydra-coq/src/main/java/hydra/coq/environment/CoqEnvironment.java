// Note: this is an automatically generated file. Do not edit.

package hydra.coq.environment;

import java.io.Serializable;

/**
 * Cross-module state threaded through the Coq encoder
 */
public class CoqEnvironment implements Serializable, Comparable<CoqEnvironment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.environment.CoqEnvironment");

  public static final hydra.core.Name CURRENT_NAMESPACE = new hydra.core.Name("currentNamespace");

  public static final hydra.core.Name CONSTRUCTOR_COUNTS = new hydra.core.Name("constructorCounts");

  public static final hydra.core.Name AMBIGUOUS_NAMES = new hydra.core.Name("ambiguousNames");

  public static final hydra.core.Name SANITIZED_ACCESSORS = new hydra.core.Name("sanitizedAccessors");

  /**
   * The Hydra namespace of the module currently being encoded (e.g. "hydra.core"). Used by the name resolver to decide whether a cross-namespace reference needs to stay qualified.
   */
  public final String currentNamespace;

  /**
   * Number of constructors in each union type, keyed by local type name (e.g. "Term" -&gt; 14). Used to decide whether a match is exhaustive.
   */
  public final java.util.Map<String, Integer> constructorCounts;

  /**
   * Local names (without namespace prefix) that are defined in more than one module. References to these must be kept fully qualified.
   */
  public final java.util.Set<String> ambiguousNames;

  /**
   * Accessor names for record fields that were sanitized to unit due to Coq's strict positivity requirement. Applications of these accessors are replaced with hydra_unreachable at emission time.
   */
  public final java.util.Set<String> sanitizedAccessors;

  public CoqEnvironment (String currentNamespace, java.util.Map<String, Integer> constructorCounts, java.util.Set<String> ambiguousNames, java.util.Set<String> sanitizedAccessors) {
    this.currentNamespace = currentNamespace;
    this.constructorCounts = constructorCounts;
    this.ambiguousNames = ambiguousNames;
    this.sanitizedAccessors = sanitizedAccessors;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CoqEnvironment)) {
      return false;
    }
    CoqEnvironment o = (CoqEnvironment) other;
    return java.util.Objects.equals(
      this.currentNamespace,
      o.currentNamespace) && java.util.Objects.equals(
      this.constructorCounts,
      o.constructorCounts) && java.util.Objects.equals(
      this.ambiguousNames,
      o.ambiguousNames) && java.util.Objects.equals(
      this.sanitizedAccessors,
      o.sanitizedAccessors);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(currentNamespace) + 3 * java.util.Objects.hashCode(constructorCounts) + 5 * java.util.Objects.hashCode(ambiguousNames) + 7 * java.util.Objects.hashCode(sanitizedAccessors);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CoqEnvironment other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      currentNamespace,
      other.currentNamespace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      constructorCounts,
      other.constructorCounts);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      ambiguousNames,
      other.ambiguousNames);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sanitizedAccessors,
      other.sanitizedAccessors);
  }

  public CoqEnvironment withCurrentNamespace(String currentNamespace) {
    return new CoqEnvironment(currentNamespace, constructorCounts, ambiguousNames, sanitizedAccessors);
  }

  public CoqEnvironment withConstructorCounts(java.util.Map<String, Integer> constructorCounts) {
    return new CoqEnvironment(currentNamespace, constructorCounts, ambiguousNames, sanitizedAccessors);
  }

  public CoqEnvironment withAmbiguousNames(java.util.Set<String> ambiguousNames) {
    return new CoqEnvironment(currentNamespace, constructorCounts, ambiguousNames, sanitizedAccessors);
  }

  public CoqEnvironment withSanitizedAccessors(java.util.Set<String> sanitizedAccessors) {
    return new CoqEnvironment(currentNamespace, constructorCounts, ambiguousNames, sanitizedAccessors);
  }
}
