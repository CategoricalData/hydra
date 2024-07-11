// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class AnonymousPatternPart implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.AnonymousPatternPart");
  
  public final hydra.langs.cypher.openCypher.PatternElement value;
  
  public AnonymousPatternPart (hydra.langs.cypher.openCypher.PatternElement value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnonymousPatternPart)) {
      return false;
    }
    AnonymousPatternPart o = (AnonymousPatternPart) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}