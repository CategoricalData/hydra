// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Type_ImplicitFunction implements Serializable, Comparable<Type_ImplicitFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_ImplicitFunction");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name RES = new hydra.core.Name("res");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type> params;

  public final hydra.ext.scala.syntax.Type res;

  public Type_ImplicitFunction (hydra.util.ConsList<hydra.ext.scala.syntax.Type> params, hydra.ext.scala.syntax.Type res) {
    this.params = params;
    this.res = res;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_ImplicitFunction)) {
      return false;
    }
    Type_ImplicitFunction o = (Type_ImplicitFunction) other;
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
  public int compareTo(Type_ImplicitFunction other) {
    int cmp = 0;
    cmp = ((Comparable) params).compareTo(other.params);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) res).compareTo(other.res);
  }

  public Type_ImplicitFunction withParams(hydra.util.ConsList<hydra.ext.scala.syntax.Type> params) {
    return new Type_ImplicitFunction(params, res);
  }

  public Type_ImplicitFunction withRes(hydra.ext.scala.syntax.Type res) {
    return new Type_ImplicitFunction(params, res);
  }
}
