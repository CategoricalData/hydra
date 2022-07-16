package hydra.ext.haskell.ast;

public class ValueBinding_Simple {
  public final Pattern pattern;
  
  public final RightHandSide rhs;
  
  public final java.util.Optional<LocalBindings> localBindings;
  
  public ValueBinding_Simple (Pattern pattern, RightHandSide rhs, java.util.Optional<LocalBindings> localBindings) {
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
  
  public ValueBinding_Simple withPattern(Pattern pattern) {
    return new ValueBinding_Simple(pattern, rhs, localBindings);
  }
  
  public ValueBinding_Simple withRhs(RightHandSide rhs) {
    return new ValueBinding_Simple(pattern, rhs, localBindings);
  }
  
  public ValueBinding_Simple withLocalBindings(java.util.Optional<LocalBindings> localBindings) {
    return new ValueBinding_Simple(pattern, rhs, localBindings);
  }
}