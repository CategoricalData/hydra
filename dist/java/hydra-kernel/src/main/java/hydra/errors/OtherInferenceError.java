// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * A generic inference error: message + subterm path
 */
public class OtherInferenceError implements Serializable, Comparable<OtherInferenceError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.OtherInferenceError");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name MESSAGE = new hydra.core.Name("message");

  /**
   * The subterm path at which the error was observed
   */
  public final hydra.paths.SubtermPath path;

  /**
   * A human-readable error message
   */
  public final String message;

  public OtherInferenceError (hydra.paths.SubtermPath path, String message) {
    this.path = path;
    this.message = message;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OtherInferenceError)) {
      return false;
    }
    OtherInferenceError o = (OtherInferenceError) other;
    return java.util.Objects.equals(
      this.path,
      o.path) && java.util.Objects.equals(
      this.message,
      o.message);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(path) + 3 * java.util.Objects.hashCode(message);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OtherInferenceError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      path,
      other.path);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      message,
      other.message);
  }

  public OtherInferenceError withPath(hydra.paths.SubtermPath path) {
    return new OtherInferenceError(path, message);
  }

  public OtherInferenceError withMessage(String message) {
    return new OtherInferenceError(path, message);
  }
}
