// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * A unification failure at a specific subterm locus during inference
 */
public class UnificationInferenceError implements Serializable, Comparable<UnificationInferenceError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.UnificationInferenceError");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name CAUSE = new hydra.core.Name("cause");

  /**
   * The subterm path at which the unification failure was observed
   */
  public final hydra.paths.SubtermPath path;

  /**
   * The underlying unification error
   */
  public final hydra.errors.UnificationError cause;

  public UnificationInferenceError (hydra.paths.SubtermPath path, hydra.errors.UnificationError cause) {
    this.path = path;
    this.cause = cause;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnificationInferenceError)) {
      return false;
    }
    UnificationInferenceError o = (UnificationInferenceError) other;
    return java.util.Objects.equals(
      this.path,
      o.path) && java.util.Objects.equals(
      this.cause,
      o.cause);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(path) + 3 * java.util.Objects.hashCode(cause);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnificationInferenceError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      path,
      other.path);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      cause,
      other.cause);
  }

  public UnificationInferenceError withPath(hydra.paths.SubtermPath path) {
    return new UnificationInferenceError(path, cause);
  }

  public UnificationInferenceError withCause(hydra.errors.UnificationError cause) {
    return new UnificationInferenceError(path, cause);
  }
}
