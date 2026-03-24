// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_QuotedMacroExpr implements Serializable, Comparable<Data_QuotedMacroExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_QuotedMacroExpr");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.ext.scala.syntax.Data body;

  public Data_QuotedMacroExpr (hydra.ext.scala.syntax.Data body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_QuotedMacroExpr)) {
      return false;
    }
    Data_QuotedMacroExpr o = (Data_QuotedMacroExpr) other;
    return java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_QuotedMacroExpr other) {
    return ((Comparable) body).compareTo(other.body);
  }
}
