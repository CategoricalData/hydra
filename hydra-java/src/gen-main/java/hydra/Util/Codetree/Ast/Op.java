package hydra.util.codetree.ast;

/**
 * An operator symbol
 */
public class Op {
  public final Symbol symbol;
  
  public final Padding padding;
  
  public final Precedence precedence;
  
  public final Associativity associativity;
  
  public Op (Symbol symbol, Padding padding, Precedence precedence, Associativity associativity) {
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
  
  public Op withSymbol(Symbol symbol) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPadding(Padding padding) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPrecedence(Precedence precedence) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withAssociativity(Associativity associativity) {
    return new Op(symbol, padding, precedence, associativity);
  }
}