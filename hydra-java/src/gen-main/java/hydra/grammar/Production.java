// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * A BNF production
 */
public class Production implements Serializable, Comparable<Production> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.Production");
  
  public static final hydra.core.Name FIELD_NAME_SYMBOL = new hydra.core.Name("symbol");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  /**
   * The nonterminal symbol being defined
   */
  public final hydra.grammar.Symbol symbol;
  
  /**
   * The pattern which defines the symbol
   */
  public final hydra.grammar.Pattern pattern;
  
  public Production (hydra.grammar.Symbol symbol, hydra.grammar.Pattern pattern) {
    this.symbol = symbol;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Production)) {
      return false;
    }
    Production o = (Production) other;
    return java.util.Objects.equals(
      this.symbol,
      o.symbol) && java.util.Objects.equals(
      this.pattern,
      o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(symbol) + 3 * java.util.Objects.hashCode(pattern);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Production other) {
    int cmp = 0;
    cmp = ((Comparable) symbol).compareTo(other.symbol);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) pattern).compareTo(other.pattern);
  }
  
  public Production withSymbol(hydra.grammar.Symbol symbol) {
    return new Production(symbol, pattern);
  }
  
  public Production withPattern(hydra.grammar.Pattern pattern) {
    return new Production(symbol, pattern);
  }
}
