// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An operator symbol
 */
public class Op implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ast.Op");
  
  public final hydra.ast.Symbol symbol;
  
  public final hydra.ast.Padding padding;
  
  public final hydra.ast.Precedence precedence;
  
  public final hydra.ast.Associativity associativity;
  
  public Op (hydra.ast.Symbol symbol, hydra.ast.Padding padding, hydra.ast.Precedence precedence, hydra.ast.Associativity associativity) {
    if (symbol == null) {
      throw new IllegalArgumentException("null value for 'symbol' argument");
    }
    if (padding == null) {
      throw new IllegalArgumentException("null value for 'padding' argument");
    }
    if (precedence == null) {
      throw new IllegalArgumentException("null value for 'precedence' argument");
    }
    if (associativity == null) {
      throw new IllegalArgumentException("null value for 'associativity' argument");
    }
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
  
  public Op withSymbol(hydra.ast.Symbol symbol) {
    if (symbol == null) {
      throw new IllegalArgumentException("null value for 'symbol' argument");
    }
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPadding(hydra.ast.Padding padding) {
    if (padding == null) {
      throw new IllegalArgumentException("null value for 'padding' argument");
    }
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPrecedence(hydra.ast.Precedence precedence) {
    if (precedence == null) {
      throw new IllegalArgumentException("null value for 'precedence' argument");
    }
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withAssociativity(hydra.ast.Associativity associativity) {
    if (associativity == null) {
      throw new IllegalArgumentException("null value for 'associativity' argument");
    }
    return new Op(symbol, padding, precedence, associativity);
  }
}