// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * An enhanced Backus-Naur form (BNF) grammar
 */
public class Grammar implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.Grammar");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.grammar.Production> value;
  
  public Grammar (java.util.List<hydra.grammar.Production> value) {
    java.util.Objects.requireNonNull((value));
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
