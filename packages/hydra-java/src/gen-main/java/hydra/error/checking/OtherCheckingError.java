// Note: this is an automatically generated file. Do not edit.

package hydra.error.checking;

import java.io.Serializable;

/**
 * A generic checking error: message + subterm path
 */
public class OtherCheckingError implements Serializable, Comparable<OtherCheckingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.checking.OtherCheckingError");

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

  public OtherCheckingError (hydra.paths.SubtermPath path, String message) {
    this.path = path;
    this.message = message;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OtherCheckingError)) {
      return false;
    }
    OtherCheckingError o = (OtherCheckingError) other;
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
  public int compareTo(OtherCheckingError other) {
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

  public OtherCheckingError withPath(hydra.paths.SubtermPath path) {
    return new OtherCheckingError(path, message);
  }

  public OtherCheckingError withMessage(String message) {
    return new OtherCheckingError(path, message);
  }
}
