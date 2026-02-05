// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An operator symbol
 */
public class Op implements Serializable, Comparable<Op> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.Op");
  
  public static final hydra.core.Name FIELD_NAME_SYMBOL = new hydra.core.Name("symbol");
  
  public static final hydra.core.Name FIELD_NAME_PADDING = new hydra.core.Name("padding");
  
  public static final hydra.core.Name FIELD_NAME_PRECEDENCE = new hydra.core.Name("precedence");
  
  public static final hydra.core.Name FIELD_NAME_ASSOCIATIVITY = new hydra.core.Name("associativity");
  
  /**
   * The operator symbol
   */
  public final hydra.ast.Symbol symbol;
  
  /**
   * The padding around the operator
   */
  public final hydra.ast.Padding padding;
  
  /**
   * The precedence of the operator
   */
  public final hydra.ast.Precedence precedence;
  
  /**
   * The associativity of the operator
   */
  public final hydra.ast.Associativity associativity;
  
  public Op (hydra.ast.Symbol symbol, hydra.ast.Padding padding, hydra.ast.Precedence precedence, hydra.ast.Associativity associativity) {
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
    return java.util.Objects.equals(
      this.symbol,
      o.symbol) && java.util.Objects.equals(
      this.padding,
      o.padding) && java.util.Objects.equals(
      this.precedence,
      o.precedence) && java.util.Objects.equals(
      this.associativity,
      o.associativity);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(symbol) + 3 * java.util.Objects.hashCode(padding) + 5 * java.util.Objects.hashCode(precedence) + 7 * java.util.Objects.hashCode(associativity);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Op other) {
    int cmp = 0;
    cmp = ((Comparable) (symbol)).compareTo(other.symbol);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (padding)).compareTo(other.padding);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (precedence)).compareTo(other.precedence);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (associativity)).compareTo(other.associativity);
  }
  
  public Op withSymbol(hydra.ast.Symbol symbol) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPadding(hydra.ast.Padding padding) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withPrecedence(hydra.ast.Precedence precedence) {
    return new Op(symbol, padding, precedence, associativity);
  }
  
  public Op withAssociativity(hydra.ast.Associativity associativity) {
    return new Op(symbol, padding, precedence, associativity);
  }
}
