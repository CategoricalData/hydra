// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_ContextFunction implements Serializable, Comparable<Data_ContextFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_ContextFunction");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Data_Param> params;

  public final hydra.ext.scala.meta.Data body;

  public Data_ContextFunction (hydra.util.ConsList<hydra.ext.scala.meta.Data_Param> params, hydra.ext.scala.meta.Data body) {
    this.params = params;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ContextFunction)) {
      return false;
    }
    Data_ContextFunction o = (Data_ContextFunction) other;
    return java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(params) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_ContextFunction other) {
    int cmp = 0;
    cmp = ((Comparable) params).compareTo(other.params);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public Data_ContextFunction withParams(hydra.util.ConsList<hydra.ext.scala.meta.Data_Param> params) {
    return new Data_ContextFunction(params, body);
  }

  public Data_ContextFunction withBody(hydra.ext.scala.meta.Data body) {
    return new Data_ContextFunction(params, body);
  }
}
