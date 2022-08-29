package hydra.util.codetree.ast;

/**
 * Matching open and close bracket symbols
 */
public class Brackets {
  public final hydra.util.codetree.ast.Symbol open;
  
  public final hydra.util.codetree.ast.Symbol close;
  
  public Brackets (hydra.util.codetree.ast.Symbol open, hydra.util.codetree.ast.Symbol close) {
    this.open = open;
    this.close = close;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Brackets)) {
      return false;
    }
    Brackets o = (Brackets) (other);
    return open.equals(o.open) && close.equals(o.close);
  }
  
  @Override
  public int hashCode() {
    return 2 * open.hashCode() + 3 * close.hashCode();
  }
  
  public Brackets withOpen(hydra.util.codetree.ast.Symbol open) {
    return new Brackets(open, close);
  }
  
  public Brackets withClose(hydra.util.codetree.ast.Symbol close) {
    return new Brackets(open, close);
  }
}