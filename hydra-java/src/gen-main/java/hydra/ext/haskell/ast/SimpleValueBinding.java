// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class SimpleValueBinding implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.SimpleValueBinding");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL_BINDINGS = new hydra.core.Name("localBindings");
  
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  public final hydra.ext.haskell.ast.RightHandSide rhs;
  
  public final hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> localBindings;
  
  public SimpleValueBinding (hydra.ext.haskell.ast.Pattern pattern, hydra.ext.haskell.ast.RightHandSide rhs, hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> localBindings) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((rhs));
    java.util.Objects.requireNonNull((localBindings));
    this.pattern = pattern;
    this.rhs = rhs;
    this.localBindings = localBindings;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleValueBinding)) {
      return false;
    }
    SimpleValueBinding o = (SimpleValueBinding) (other);
    return pattern.equals(o.pattern) && rhs.equals(o.rhs) && localBindings.equals(o.localBindings);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * rhs.hashCode() + 5 * localBindings.hashCode();
  }
  
  public SimpleValueBinding withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new SimpleValueBinding(pattern, rhs, localBindings);
  }
  
  public SimpleValueBinding withRhs(hydra.ext.haskell.ast.RightHandSide rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new SimpleValueBinding(pattern, rhs, localBindings);
  }
  
  public SimpleValueBinding withLocalBindings(hydra.util.Opt<hydra.ext.haskell.ast.LocalBindings> localBindings) {
    java.util.Objects.requireNonNull((localBindings));
    return new SimpleValueBinding(pattern, rhs, localBindings);
  }
}