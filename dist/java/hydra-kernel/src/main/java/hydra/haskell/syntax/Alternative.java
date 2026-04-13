// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A pattern-matching alternative
 */
public class Alternative implements Serializable, Comparable<Alternative> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.Alternative");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public static final hydra.core.Name BINDS = new hydra.core.Name("binds");

  /**
   * The pattern to match
   */
  public final hydra.haskell.syntax.Pattern pattern;

  /**
   * The right-hand side of the alternative
   */
  public final hydra.haskell.syntax.CaseRhs rhs;

  /**
   * Optional local bindings
   */
  public final hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> binds;

  public Alternative (hydra.haskell.syntax.Pattern pattern, hydra.haskell.syntax.CaseRhs rhs, hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> binds) {
    this.pattern = pattern;
    this.rhs = rhs;
    this.binds = binds;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Alternative)) {
      return false;
    }
    Alternative o = (Alternative) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.rhs,
      o.rhs) && java.util.Objects.equals(
      this.binds,
      o.binds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(rhs) + 5 * java.util.Objects.hashCode(binds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Alternative other) {
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
      binds,
      other.binds);
  }

  public Alternative withPattern(hydra.haskell.syntax.Pattern pattern) {
    return new Alternative(pattern, rhs, binds);
  }

  public Alternative withRhs(hydra.haskell.syntax.CaseRhs rhs) {
    return new Alternative(pattern, rhs, binds);
  }

  public Alternative withBinds(hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> binds) {
    return new Alternative(pattern, rhs, binds);
  }
}
