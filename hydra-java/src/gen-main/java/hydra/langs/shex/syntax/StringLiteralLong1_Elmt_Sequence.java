// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class StringLiteralLong1_Elmt_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.StringLiteralLong1.Elmt.Sequence");
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option> alts;
  
  public final String regex;
  
  public StringLiteralLong1_Elmt_Sequence (hydra.util.Opt<hydra.langs.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option> alts, String regex) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    if (regex == null) {
      throw new IllegalArgumentException("null value for 'regex' argument");
    }
    this.alts = alts;
    this.regex = regex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong1_Elmt_Sequence)) {
      return false;
    }
    StringLiteralLong1_Elmt_Sequence o = (StringLiteralLong1_Elmt_Sequence) (other);
    return alts.equals(o.alts) && regex.equals(o.regex);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode() + 3 * regex.hashCode();
  }
  
  public StringLiteralLong1_Elmt_Sequence withAlts(hydra.util.Opt<hydra.langs.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option> alts) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    return new StringLiteralLong1_Elmt_Sequence(alts, regex);
  }
  
  public StringLiteralLong1_Elmt_Sequence withRegex(String regex) {
    if (regex == null) {
      throw new IllegalArgumentException("null value for 'regex' argument");
    }
    return new StringLiteralLong1_Elmt_Sequence(alts, regex);
  }
}