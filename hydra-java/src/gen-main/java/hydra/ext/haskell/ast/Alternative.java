// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A pattern-matching alternative
 */
public class Alternative implements Serializable, Comparable<Alternative> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Alternative");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public static final hydra.core.Name FIELD_NAME_BINDS = new hydra.core.Name("binds");
  
  /**
   * The pattern to match
   */
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  /**
   * The right-hand side of the alternative
   */
  public final hydra.ext.haskell.ast.CaseRhs rhs;
  
  /**
   * Optional local bindings
   */
  public final hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> binds;
  
  public Alternative (hydra.ext.haskell.ast.Pattern pattern, hydra.ext.haskell.ast.CaseRhs rhs, hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> binds) {
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
    cmp = ((Comparable) pattern).compareTo(other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rhs).compareTo(other.rhs);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      binds.hashCode(),
      other.binds.hashCode());
  }
  
  public Alternative withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withRhs(hydra.ext.haskell.ast.CaseRhs rhs) {
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withBinds(hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> binds) {
    return new Alternative(pattern, rhs, binds);
  }
}
