// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * A pattern-matching alternative
 */
public class Alternative implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.Alternative");
  
  public final hydra.langs.haskell.ast.Pattern pattern;
  
  public final hydra.langs.haskell.ast.CaseRhs rhs;
  
  public final hydra.util.Opt<hydra.langs.haskell.ast.LocalBindings> binds;
  
  public Alternative (hydra.langs.haskell.ast.Pattern pattern, hydra.langs.haskell.ast.CaseRhs rhs, hydra.util.Opt<hydra.langs.haskell.ast.LocalBindings> binds) {
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    if (binds == null) {
      throw new IllegalArgumentException("null value for 'binds' argument");
    }
    this.pattern = pattern;
    this.rhs = rhs;
    this.binds = binds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Alternative)) {
      return false;
    }
    Alternative o = (Alternative) (other);
    return pattern.equals(o.pattern) && rhs.equals(o.rhs) && binds.equals(o.binds);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * rhs.hashCode() + 5 * binds.hashCode();
  }
  
  public Alternative withPattern(hydra.langs.haskell.ast.Pattern pattern) {
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withRhs(hydra.langs.haskell.ast.CaseRhs rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withBinds(hydra.util.Opt<hydra.langs.haskell.ast.LocalBindings> binds) {
    if (binds == null) {
      throw new IllegalArgumentException("null value for 'binds' argument");
    }
    return new Alternative(pattern, rhs, binds);
  }
}