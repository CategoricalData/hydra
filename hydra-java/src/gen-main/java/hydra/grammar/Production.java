package hydra.grammar;

public class Production {
  public final Symbol symbol;
  
  public final Pattern pattern;
  
  public Production (Symbol symbol, Pattern pattern) {
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
  
  public Production withSymbol(Symbol symbol) {
    return new Production(symbol, pattern);
  }
  
  public Production withPattern(Pattern pattern) {
    return new Production(symbol, pattern);
  }
}