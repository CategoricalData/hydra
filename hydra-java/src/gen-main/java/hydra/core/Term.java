package hydra.core;

/**
 * A data term
 */
public class Term<M> {
  public final TermExpr<M> expr;
  
  public final M meta;
  
  public Term (TermExpr<M> expr, M meta) {
    this.expr = expr;
    this.meta = meta;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Term)) {
      return false;
    }
    Term o = (Term) (other);
    return expr.equals(o.expr) && meta.equals(o.meta);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * meta.hashCode();
  }
  
  public Term withExpr(TermExpr<M> expr) {
    return new Term(expr, meta);
  }
  
  public Term withMeta(M meta) {
    return new Term(expr, meta);
  }
}