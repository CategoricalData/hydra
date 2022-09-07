package hydra.ext.haskell.ast;

/**
 * A type signature expression
 */
public class Expression_TypeSignature {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.TypeSignature");
  
  public final hydra.ext.haskell.ast.Expression inner;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public Expression_TypeSignature (hydra.ext.haskell.ast.Expression inner, hydra.ext.haskell.ast.Type type) {
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_TypeSignature)) {
      return false;
    }
    Expression_TypeSignature o = (Expression_TypeSignature) (other);
    return inner.equals(o.inner) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * type.hashCode();
  }
  
  public Expression_TypeSignature withInner(hydra.ext.haskell.ast.Expression inner) {
    return new Expression_TypeSignature(inner, type);
  }
  
  public Expression_TypeSignature withType(hydra.ext.haskell.ast.Type type) {
    return new Expression_TypeSignature(inner, type);
  }
}