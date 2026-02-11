// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * An enhanced Backus-Naur form (BNF) grammar
 */
public class Grammar implements Serializable, Comparable<Grammar> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.Grammar");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.grammar.Production> value;
  
  public Grammar (java.util.List<hydra.grammar.Production> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Grammar)) {
      return false;
    }
    Grammar o = (Grammar) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Grammar other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
