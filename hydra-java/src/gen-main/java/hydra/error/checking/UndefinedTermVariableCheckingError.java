// Note: this is an automatically generated file. Do not edit.

package hydra.error.checking;

import java.io.Serializable;

/**
 * A reference to a term variable that is not bound in scope, encountered during checking
 */
public class UndefinedTermVariableCheckingError implements Serializable, Comparable<UndefinedTermVariableCheckingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The subterm path at which the variable was referenced
   */
  public final hydra.paths.SubtermPath path;

  /**
   * The name of the undefined variable
   */
  public final hydra.core.Name name;

  public UndefinedTermVariableCheckingError (hydra.paths.SubtermPath path, hydra.core.Name name) {
    this.path = path;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTermVariableCheckingError)) {
      return false;
    }
    UndefinedTermVariableCheckingError o = (UndefinedTermVariableCheckingError) other;
    return java.util.Objects.equals(
      this.path,
      o.path) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(path) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UndefinedTermVariableCheckingError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      path,
      other.path);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public UndefinedTermVariableCheckingError withPath(hydra.paths.SubtermPath path) {
    return new UndefinedTermVariableCheckingError(path, name);
  }

  public UndefinedTermVariableCheckingError withName(hydra.core.Name name) {
    return new UndefinedTermVariableCheckingError(path, name);
  }
}
