// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_PolyFunction implements Serializable, Comparable<Data_PolyFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_PolyFunction");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final hydra.scala.syntax.Data body;

  public Data_PolyFunction (java.util.List<hydra.scala.syntax.Type_Param> tparams, hydra.scala.syntax.Data body) {
    this.tparams = tparams;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_PolyFunction)) {
      return false;
    }
    Data_PolyFunction o = (Data_PolyFunction) other;
    return java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tparams) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_PolyFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      tparams,
      other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Data_PolyFunction withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Data_PolyFunction(tparams, body);
  }

  public Data_PolyFunction withBody(hydra.scala.syntax.Data body) {
    return new Data_PolyFunction(tparams, body);
  }
}
