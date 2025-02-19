// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StringLiteralLong2_Elmt_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public static final hydra.core.Name FIELD_NAME_REGEX = new hydra.core.Name("regex");
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option> alts;
  
  public final String regex;
  
  public StringLiteralLong2_Elmt_Sequence (hydra.util.Opt<hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option> alts, String regex) {
    java.util.Objects.requireNonNull((alts));
    java.util.Objects.requireNonNull((regex));
    this.alts = alts;
    this.regex = regex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong2_Elmt_Sequence)) {
      return false;
    }
    StringLiteralLong2_Elmt_Sequence o = (StringLiteralLong2_Elmt_Sequence) (other);
    return alts.equals(o.alts) && regex.equals(o.regex);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode() + 3 * regex.hashCode();
  }
  
  public StringLiteralLong2_Elmt_Sequence withAlts(hydra.util.Opt<hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option> alts) {
    java.util.Objects.requireNonNull((alts));
    return new StringLiteralLong2_Elmt_Sequence(alts, regex);
  }
  
  public StringLiteralLong2_Elmt_Sequence withRegex(String regex) {
    java.util.Objects.requireNonNull((regex));
    return new StringLiteralLong2_Elmt_Sequence(alts, regex);
  }
}