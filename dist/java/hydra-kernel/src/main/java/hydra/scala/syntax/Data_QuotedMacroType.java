// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_QuotedMacroType implements Serializable, Comparable<Data_QuotedMacroType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_QuotedMacroType");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.scala.syntax.Type tpe;

  public Data_QuotedMacroType (hydra.scala.syntax.Type tpe) {
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_QuotedMacroType)) {
      return false;
    }
    Data_QuotedMacroType o = (Data_QuotedMacroType) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_QuotedMacroType other) {
    return hydra.util.Comparing.compare(
      tpe,
      other.tpe);
  }
}
