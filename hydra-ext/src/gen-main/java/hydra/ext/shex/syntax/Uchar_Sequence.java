// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class Uchar_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.Uchar.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_HEX = new hydra.core.Name("hex");
  
  public static final hydra.core.Name FIELD_NAME_HEX2 = new hydra.core.Name("hex2");
  
  public static final hydra.core.Name FIELD_NAME_HEX3 = new hydra.core.Name("hex3");
  
  public static final hydra.core.Name FIELD_NAME_HEX4 = new hydra.core.Name("hex4");
  
  public final hydra.ext.shex.syntax.Hex hex;
  
  public final hydra.ext.shex.syntax.Hex hex2;
  
  public final hydra.ext.shex.syntax.Hex hex3;
  
  public final hydra.ext.shex.syntax.Hex hex4;
  
  public Uchar_Sequence (hydra.ext.shex.syntax.Hex hex, hydra.ext.shex.syntax.Hex hex2, hydra.ext.shex.syntax.Hex hex3, hydra.ext.shex.syntax.Hex hex4) {
    java.util.Objects.requireNonNull((hex));
    java.util.Objects.requireNonNull((hex2));
    java.util.Objects.requireNonNull((hex3));
    java.util.Objects.requireNonNull((hex4));
    this.hex = hex;
    this.hex2 = hex2;
    this.hex3 = hex3;
    this.hex4 = hex4;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Uchar_Sequence)) {
      return false;
    }
    Uchar_Sequence o = (Uchar_Sequence) (other);
    return hex.equals(o.hex) && hex2.equals(o.hex2) && hex3.equals(o.hex3) && hex4.equals(o.hex4);
  }
  
  @Override
  public int hashCode() {
    return 2 * hex.hashCode() + 3 * hex2.hashCode() + 5 * hex3.hashCode() + 7 * hex4.hashCode();
  }
  
  public Uchar_Sequence withHex(hydra.ext.shex.syntax.Hex hex) {
    java.util.Objects.requireNonNull((hex));
    return new Uchar_Sequence(hex, hex2, hex3, hex4);
  }
  
  public Uchar_Sequence withHex2(hydra.ext.shex.syntax.Hex hex2) {
    java.util.Objects.requireNonNull((hex2));
    return new Uchar_Sequence(hex, hex2, hex3, hex4);
  }
  
  public Uchar_Sequence withHex3(hydra.ext.shex.syntax.Hex hex3) {
    java.util.Objects.requireNonNull((hex3));
    return new Uchar_Sequence(hex, hex2, hex3, hex4);
  }
  
  public Uchar_Sequence withHex4(hydra.ext.shex.syntax.Hex hex4) {
    java.util.Objects.requireNonNull((hex4));
    return new Uchar_Sequence(hex, hex2, hex3, hex4);
  }
}