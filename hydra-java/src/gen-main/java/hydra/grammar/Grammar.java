package hydra.grammar;

/**
 * An enhanced Backus-Naur form (BNF) grammar
 */
public class Grammar {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/grammar.Grammar");
  
  /**
   * An enhanced Backus-Naur form (BNF) grammar
   */
  public final java.util.List<hydra.grammar.Production> value;
  
  public Grammar (java.util.List<hydra.grammar.Production> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Grammar)) {
      return false;
    }
    Grammar o = (Grammar) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}