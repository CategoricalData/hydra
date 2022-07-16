package hydra.util.codetree.ast;

/**
 * Matching open and close bracket symbols
 */
public class Brackets {
  public final Symbol open;
  
  public final Symbol close;
  
  public Brackets (Symbol open, Symbol close) {
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
  
  public Brackets withOpen(Symbol open) {
    return new Brackets(open, close);
  }
  
  public Brackets withClose(Symbol close) {
    return new Brackets(open, close);
  }
}