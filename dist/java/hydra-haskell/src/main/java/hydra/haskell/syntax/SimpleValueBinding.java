// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A simple value binding
 */
public class SimpleValueBinding implements Serializable, Comparable<SimpleValueBinding> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public static final hydra.core.Name LOCAL_BINDINGS = new hydra.core.Name("localBindings");

  /**
   * The pattern being bound
   */
  public final hydra.haskell.syntax.Pattern pattern;

  /**
   * The right-hand side
   */
  public final hydra.haskell.syntax.RightHandSide rhs;

  /**
   * Optional local bindings (where clause)
   */
  public final hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> localBindings;

  public SimpleValueBinding (hydra.haskell.syntax.Pattern pattern, hydra.haskell.syntax.RightHandSide rhs, hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> localBindings) {
    this.pattern = pattern;
    this.rhs = rhs;
    this.localBindings = localBindings;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleValueBinding)) {
      return false;
    }
    SimpleValueBinding o = (SimpleValueBinding) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.rhs,
      o.rhs) && java.util.Objects.equals(
      this.localBindings,
      o.localBindings);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(rhs) + 5 * java.util.Objects.hashCode(localBindings);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimpleValueBinding other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      rhs,
      other.rhs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      localBindings,
      other.localBindings);
  }

  public SimpleValueBinding withPattern(hydra.haskell.syntax.Pattern pattern) {
    return new SimpleValueBinding(pattern, rhs, localBindings);
  }

  public SimpleValueBinding withRhs(hydra.haskell.syntax.RightHandSide rhs) {
    return new SimpleValueBinding(pattern, rhs, localBindings);
  }

  public SimpleValueBinding withLocalBindings(hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> localBindings) {
    return new SimpleValueBinding(pattern, rhs, localBindings);
  }
}
