// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StringLiteralLong2_Elmt_Sequence implements Serializable, Comparable<StringLiteralLong2_Elmt_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence");

  public static final hydra.core.Name ALTS = new hydra.core.Name("Alts");

  public static final hydra.core.Name REGEX = new hydra.core.Name("regex");

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option> Alts;

  public final String regex;

  public StringLiteralLong2_Elmt_Sequence (hydra.util.Maybe<hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option> Alts, String regex) {
    this.Alts = Alts;
    this.regex = regex;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong2_Elmt_Sequence)) {
      return false;
    }
    StringLiteralLong2_Elmt_Sequence o = (StringLiteralLong2_Elmt_Sequence) other;
    return java.util.Objects.equals(
      this.Alts,
      o.Alts) && java.util.Objects.equals(
      this.regex,
      o.regex);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Alts) + 3 * java.util.Objects.hashCode(regex);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringLiteralLong2_Elmt_Sequence other) {
    int cmp = 0;
    cmp = ((Comparable) Alts).compareTo(other.Alts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) regex).compareTo(other.regex);
  }

  public StringLiteralLong2_Elmt_Sequence withAlts(hydra.util.Maybe<hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option> Alts) {
    return new StringLiteralLong2_Elmt_Sequence(Alts, regex);
  }

  public StringLiteralLong2_Elmt_Sequence withRegex(String regex) {
    return new StringLiteralLong2_Elmt_Sequence(Alts, regex);
  }
}
