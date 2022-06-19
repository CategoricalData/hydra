package hydra.core;

/**
 * A data type
 */
public class Type<M> {
  public final TypeExpr<M> expr;
  
  public final M meta;
  
  public Type (TypeExpr<M> expr, M meta) {
    this.expr = expr;
    this.meta = meta;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type)) {
      return false;
    }
    Type o = (Type) (other);
    return expr.equals(o.expr) && meta.equals(o.meta);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * meta.hashCode();
  }
  
  public Type withExpr(TypeExpr<M> expr) {
    return new Type(expr, meta);
  }
  
  public Type withMeta(M meta) {
    return new Type(expr, meta);
  }
}