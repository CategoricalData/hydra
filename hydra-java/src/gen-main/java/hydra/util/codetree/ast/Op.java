package hydra.util.codetree.ast;

/**
 * An operator symbol
 */
public class Op {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.Op");
  
  public final hydra.util.codetree.ast.Symbol symbol;
  
  public final hydra.util.codetree.ast.Padding padding;
  
  public final hydra.util.codetree.ast.Precedence precedence;
  
  public final hydra.util.codetree.ast.Associativity associativity;
  
  public Op (hydra.util.codetree.ast.Symbol symbol, hydra.util.codetree.ast.Padding padding, hydra.util.codetree.ast.Precedence precedence, hydra.util.codetree.ast.Associativity associativity) {
    this.symbol = symbol;
    this.padding = padding;
    this.precedence = precedence;
    this.associativity = associativity;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Op)) {
      return false;
    }
    Op o = (Op) (other);
    return symbol.equals(o.symbol) && padding.equals(o.padding) && precedence.equals(o.precedence) && associativity.equals(o.associativity);
  }
  
  @Override
  public int hashCode() {
    return 2 * symbol.hashCode() + 3 * padding.hashCode() + 5 * precedence.hashCode() + 7 * associativity.hashCode();
  }
  
  public Op withSymbol(hydra.util.codetree.ast.Symbol symbol) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPadding(hydra.util.codetree.ast.Padding padding) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPrecedence(hydra.util.codetree.ast.Precedence precedence) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withAssociativity(hydra.util.codetree.ast.Associativity associativity) {
    return new Op(symbol, padding, precedence, associativity);
  }
}