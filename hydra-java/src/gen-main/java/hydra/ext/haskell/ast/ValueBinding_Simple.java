// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class ValueBinding_Simple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.ValueBinding.Simple");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL_BINDINGS = new hydra.core.Name("localBindings");
  
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  public final hydra.ext.haskell.ast.RightHandSide rhs;
  
  public final hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> localBindings;
  
  public ValueBinding_Simple (hydra.ext.haskell.ast.Pattern pattern, hydra.ext.haskell.ast.RightHandSide rhs, hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> localBindings) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((rhs));
    java.util.Objects.requireNonNull((localBindings));
    this.pattern = pattern;
    this.rhs = rhs;
    this.localBindings = localBindings;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueBinding_Simple)) {
      return false;
    }
    ValueBinding_Simple o = (ValueBinding_Simple) (other);
    return pattern.equals(o.pattern) && rhs.equals(o.rhs) && localBindings.equals(o.localBindings);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * rhs.hashCode() + 5 * localBindings.hashCode();
  }
  
  public ValueBinding_Simple withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new ValueBinding_Simple(pattern, rhs, localBindings);
  }
  
  public ValueBinding_Simple withRhs(hydra.ext.haskell.ast.RightHandSide rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new ValueBinding_Simple(pattern, rhs, localBindings);
  }
  
  public ValueBinding_Simple withLocalBindings(hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> localBindings) {
    java.util.Objects.requireNonNull((localBindings));
    return new ValueBinding_Simple(pattern, rhs, localBindings);
  }
}
