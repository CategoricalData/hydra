// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class RdfLiteral implements Serializable, Comparable<RdfLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.RdfLiteral");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("String");
  
  public static final hydra.core.Name ALTS = new hydra.core.Name("Alts");
  
  public final hydra.ext.io.shex.syntax.String_ String_;
  
  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.RdfLiteral_Alts_Option> Alts;
  
  public RdfLiteral (hydra.ext.io.shex.syntax.String_ String_, hydra.util.Maybe<hydra.ext.io.shex.syntax.RdfLiteral_Alts_Option> Alts) {
    this.String_ = String_;
    this.Alts = Alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfLiteral)) {
      return false;
    }
    RdfLiteral o = (RdfLiteral) other;
    return java.util.Objects.equals(
      this.String_,
      o.String_) && java.util.Objects.equals(
      this.Alts,
      o.Alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(String_) + 3 * java.util.Objects.hashCode(Alts);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RdfLiteral other) {
    int cmp = 0;
    cmp = ((Comparable) String_).compareTo(other.String_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) Alts).compareTo(other.Alts);
  }
  
  public RdfLiteral withString(hydra.ext.io.shex.syntax.String_ String_) {
    return new RdfLiteral(String_, Alts);
  }
  
  public RdfLiteral withAlts(hydra.util.Maybe<hydra.ext.io.shex.syntax.RdfLiteral_Alts_Option> Alts) {
    return new RdfLiteral(String_, Alts);
  }
}
