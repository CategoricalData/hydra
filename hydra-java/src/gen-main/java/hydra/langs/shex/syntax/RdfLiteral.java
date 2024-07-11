// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class RdfLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.RdfLiteral");
  
  public final hydra.langs.shex.syntax.String_ string;
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.RdfLiteral_Alts_Option> alts;
  
  public RdfLiteral (hydra.langs.shex.syntax.String_ string, hydra.util.Opt<hydra.langs.shex.syntax.RdfLiteral_Alts_Option> alts) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    this.string = string;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfLiteral)) {
      return false;
    }
    RdfLiteral o = (RdfLiteral) (other);
    return string.equals(o.string) && alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * alts.hashCode();
  }
  
  public RdfLiteral withString(hydra.langs.shex.syntax.String_ string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new RdfLiteral(string, alts);
  }
  
  public RdfLiteral withAlts(hydra.util.Opt<hydra.langs.shex.syntax.RdfLiteral_Alts_Option> alts) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    return new RdfLiteral(string, alts);
  }
}