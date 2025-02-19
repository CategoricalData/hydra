// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A pattern-matching alternative
 */
public class Alternative implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Alternative");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public static final hydra.core.Name FIELD_NAME_BINDS = new hydra.core.Name("binds");
  
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  public final hydra.ext.haskell.ast.CaseRhs rhs;
  
  public final hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> binds;
  
  public Alternative (hydra.ext.haskell.ast.Pattern pattern, hydra.ext.haskell.ast.CaseRhs rhs, hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> binds) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((rhs));
    java.util.Objects.requireNonNull((binds));
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
  
  public Alternative withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withRhs(hydra.ext.haskell.ast.CaseRhs rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withBinds(hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> binds) {
    java.util.Objects.requireNonNull((binds));
    return new Alternative(pattern, rhs, binds);
  }
}