package hydra.grammar;

public class Grammar {
  public final java.util.List<Production> value;
  
  public Grammar (java.util.List<Production> value) {
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