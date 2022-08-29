package hydra.ext.haskell.ast;

/**
 * A pattern-matching alternative
 */
public class Alternative {
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  public final hydra.ext.haskell.ast.CaseRhs rhs;
  
  public final java.util.Optional<hydra.ext.haskell.ast.LocalBindings> binds;
  
  public Alternative (hydra.ext.haskell.ast.Pattern pattern, hydra.ext.haskell.ast.CaseRhs rhs, java.util.Optional<hydra.ext.haskell.ast.LocalBindings> binds) {
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
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withRhs(hydra.ext.haskell.ast.CaseRhs rhs) {
    return new Alternative(pattern, rhs, binds);
  }
  
  public Alternative withBinds(java.util.Optional<hydra.ext.haskell.ast.LocalBindings> binds) {
    return new Alternative(pattern, rhs, binds);
  }
}