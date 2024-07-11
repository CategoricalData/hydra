// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class Percent implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.Percent");
  
  public final hydra.langs.shex.syntax.Hex hex;
  
  public final hydra.langs.shex.syntax.Hex hex2;
  
  public Percent (hydra.langs.shex.syntax.Hex hex, hydra.langs.shex.syntax.Hex hex2) {
    java.util.Objects.requireNonNull((hex));
    java.util.Objects.requireNonNull((hex2));
    this.hex = hex;
    this.hex2 = hex2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Percent)) {
      return false;
    }
    Percent o = (Percent) (other);
    return hex.equals(o.hex) && hex2.equals(o.hex2);
  }
  
  @Override
  public int hashCode() {
    return 2 * hex.hashCode() + 3 * hex2.hashCode();
  }
  
  public Percent withHex(hydra.langs.shex.syntax.Hex hex) {
    java.util.Objects.requireNonNull((hex));
    return new Percent(hex, hex2);
  }
  
  public Percent withHex2(hydra.langs.shex.syntax.Hex hex2) {
    java.util.Objects.requireNonNull((hex2));
    return new Percent(hex, hex2);
  }
}