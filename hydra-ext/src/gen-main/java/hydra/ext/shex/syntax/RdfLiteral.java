// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class RdfLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.RdfLiteral");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public final hydra.ext.shex.syntax.String_ string;
  
  public final hydra.util.Opt<hydra.ext.shex.syntax.RdfLiteral_Alts_Option> alts;
  
  public RdfLiteral (hydra.ext.shex.syntax.String_ string, hydra.util.Opt<hydra.ext.shex.syntax.RdfLiteral_Alts_Option> alts) {
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((alts));
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
  
  public RdfLiteral withString(hydra.ext.shex.syntax.String_ string) {
    java.util.Objects.requireNonNull((string));
    return new RdfLiteral(string, alts);
  }
  
  public RdfLiteral withAlts(hydra.util.Opt<hydra.ext.shex.syntax.RdfLiteral_Alts_Option> alts) {
    java.util.Objects.requireNonNull((alts));
    return new RdfLiteral(string, alts);
  }
}