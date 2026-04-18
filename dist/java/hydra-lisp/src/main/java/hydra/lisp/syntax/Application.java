// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * Function application: (function arg1 arg2 ...)
 */
public class Application implements Serializable, Comparable<Application> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.Application");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  /**
   * The function being applied
   */
  public final hydra.lisp.syntax.Expression function;

  /**
   * The arguments
   */
  public final java.util.List<hydra.lisp.syntax.Expression> arguments;

  public Application (hydra.lisp.syntax.Expression function, java.util.List<hydra.lisp.syntax.Expression> arguments) {
    this.function = function;
    this.arguments = arguments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Application)) {
      return false;
    }
    Application o = (Application) other;
    return java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(function) + 3 * java.util.Objects.hashCode(arguments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Application other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      function,
      other.function);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      arguments,
      other.arguments);
  }

  public Application withFunction(hydra.lisp.syntax.Expression function) {
    return new Application(function, arguments);
  }

  public Application withArguments(java.util.List<hydra.lisp.syntax.Expression> arguments) {
    return new Application(function, arguments);
  }
}
