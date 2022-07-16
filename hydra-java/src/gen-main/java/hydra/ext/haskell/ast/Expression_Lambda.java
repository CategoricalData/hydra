package hydra.ext.haskell.ast;

/**
 * A lambda expression
 */
public class Expression_Lambda {
  public final java.util.List<Pattern> bindings;
  
  public final Expression inner;
  
  public Expression_Lambda (java.util.List<Pattern> bindings, Expression inner) {
    this.bindings = bindings;
    this.inner = inner;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_Lambda)) {
      return false;
    }
    Expression_Lambda o = (Expression_Lambda) (other);
    return bindings.equals(o.bindings) && inner.equals(o.inner);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * inner.hashCode();
  }
  
  public Expression_Lambda withBindings(java.util.List<Pattern> bindings) {
    return new Expression_Lambda(bindings, inner);
  }
  
  public Expression_Lambda withInner(Expression inner) {
    return new Expression_Lambda(bindings, inner);
  }
}