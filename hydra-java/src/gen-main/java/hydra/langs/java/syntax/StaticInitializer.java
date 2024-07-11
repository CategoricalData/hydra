// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class StaticInitializer implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.StaticInitializer");
  
  public final hydra.langs.java.syntax.Block value;
  
  public StaticInitializer (hydra.langs.java.syntax.Block value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StaticInitializer)) {
      return false;
    }
    StaticInitializer o = (StaticInitializer) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}