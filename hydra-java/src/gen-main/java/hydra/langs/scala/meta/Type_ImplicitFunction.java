// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_ImplicitFunction implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.ImplicitFunction");
  
  public final java.util.List<hydra.langs.scala.meta.Type> params;
  
  public final hydra.langs.scala.meta.Type res;
  
  public Type_ImplicitFunction (java.util.List<hydra.langs.scala.meta.Type> params, hydra.langs.scala.meta.Type res) {
    if (params == null) {
      throw new IllegalArgumentException("null value for 'params' argument");
    }
    if (res == null) {
      throw new IllegalArgumentException("null value for 'res' argument");
    }
    this.params = params;
    this.res = res;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_ImplicitFunction)) {
      return false;
    }
    Type_ImplicitFunction o = (Type_ImplicitFunction) (other);
    return params.equals(o.params) && res.equals(o.res);
  }
  
  @Override
  public int hashCode() {
    return 2 * params.hashCode() + 3 * res.hashCode();
  }
  
  public Type_ImplicitFunction withParams(java.util.List<hydra.langs.scala.meta.Type> params) {
    if (params == null) {
      throw new IllegalArgumentException("null value for 'params' argument");
    }
    return new Type_ImplicitFunction(params, res);
  }
  
  public Type_ImplicitFunction withRes(hydra.langs.scala.meta.Type res) {
    if (res == null) {
      throw new IllegalArgumentException("null value for 'res' argument");
    }
    return new Type_ImplicitFunction(params, res);
  }
}