// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * A BNF production
 */
public class Production implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/grammar.Production");
  
  public final hydra.grammar.Symbol symbol;
  
  public final hydra.grammar.Pattern pattern;
  
  public Production (hydra.grammar.Symbol symbol, hydra.grammar.Pattern pattern) {
    if (symbol == null) {
      throw new IllegalArgumentException("null value for 'symbol' argument");
    }
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    this.symbol = symbol;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Production)) {
      return false;
    }
    Production o = (Production) (other);
    return symbol.equals(o.symbol) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * symbol.hashCode() + 3 * pattern.hashCode();
  }
  
  public Production withSymbol(hydra.grammar.Symbol symbol) {
    if (symbol == null) {
      throw new IllegalArgumentException("null value for 'symbol' argument");
    }
    return new Production(symbol, pattern);
  }
  
  public Production withPattern(hydra.grammar.Pattern pattern) {
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    return new Production(symbol, pattern);
  }
}