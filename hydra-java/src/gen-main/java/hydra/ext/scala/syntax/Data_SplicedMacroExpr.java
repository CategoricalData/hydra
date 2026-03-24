// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_SplicedMacroExpr implements Serializable, Comparable<Data_SplicedMacroExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_SplicedMacroExpr");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.ext.scala.syntax.Data body;

  public Data_SplicedMacroExpr (hydra.ext.scala.syntax.Data body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_SplicedMacroExpr)) {
      return false;
    }
    Data_SplicedMacroExpr o = (Data_SplicedMacroExpr) other;
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
  public int compareTo(Data_SplicedMacroExpr other) {
    return ((Comparable) body).compareTo(other.body);
  }
}
