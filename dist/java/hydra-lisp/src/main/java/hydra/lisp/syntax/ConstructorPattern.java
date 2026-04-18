// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A constructor pattern matching a tagged value
 */
public class ConstructorPattern implements Serializable, Comparable<ConstructorPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.ConstructorPattern");

  public static final hydra.core.Name CONSTRUCTOR = new hydra.core.Name("constructor");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  /**
   * The constructor/tag name
   */
  public final hydra.lisp.syntax.Symbol constructor;

  /**
   * The sub-patterns for constructor arguments
   */
  public final java.util.List<hydra.lisp.syntax.Pattern> arguments;

  public ConstructorPattern (hydra.lisp.syntax.Symbol constructor, java.util.List<hydra.lisp.syntax.Pattern> arguments) {
    this.constructor = constructor;
    this.arguments = arguments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorPattern)) {
      return false;
    }
    ConstructorPattern o = (ConstructorPattern) other;
    return java.util.Objects.equals(
      this.constructor,
      o.constructor) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(constructor) + 3 * java.util.Objects.hashCode(arguments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstructorPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      constructor,
      other.constructor);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      arguments,
      other.arguments);
  }

  public ConstructorPattern withConstructor(hydra.lisp.syntax.Symbol constructor) {
    return new ConstructorPattern(constructor, arguments);
  }

  public ConstructorPattern withArguments(java.util.List<hydra.lisp.syntax.Pattern> arguments) {
    return new ConstructorPattern(constructor, arguments);
  }
}
