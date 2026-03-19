// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * Function application: (function arg1 arg2 ...)
 */
public class Application implements Serializable, Comparable<Application> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Application");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  /**
   * The function being applied
   */
  public final hydra.ext.lisp.syntax.Expression function;

  /**
   * The arguments
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> arguments;

  public Application (hydra.ext.lisp.syntax.Expression function, hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> arguments) {
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
    cmp = ((Comparable) function).compareTo(other.function);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) arguments).compareTo(other.arguments);
  }

  public Application withFunction(hydra.ext.lisp.syntax.Expression function) {
    return new Application(function, arguments);
  }

  public Application withArguments(hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> arguments) {
    return new Application(function, arguments);
  }
}
