// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Function implements Serializable, Comparable<Type_Function> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Function");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name RES = new hydra.core.Name("res");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Type> params;

  public final hydra.ext.scala.meta.Type res;

  public Type_Function (hydra.util.ConsList<hydra.ext.scala.meta.Type> params, hydra.ext.scala.meta.Type res) {
    this.params = params;
    this.res = res;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Function)) {
      return false;
    }
    Type_Function o = (Type_Function) other;
    return java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.res,
      o.res);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(params) + 3 * java.util.Objects.hashCode(res);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Function other) {
    int cmp = 0;
    cmp = ((Comparable) params).compareTo(other.params);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) res).compareTo(other.res);
  }

  public Type_Function withParams(hydra.util.ConsList<hydra.ext.scala.meta.Type> params) {
    return new Type_Function(params, res);
  }

  public Type_Function withRes(hydra.ext.scala.meta.Type res) {
    return new Type_Function(params, res);
  }
}
