// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_ImplicitFunction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_ImplicitFunction");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name FIELD_NAME_RES = new hydra.core.Name("res");
  
  public final java.util.List<hydra.ext.scala.meta.Type> params;
  
  public final hydra.ext.scala.meta.Type res;
  
  public Type_ImplicitFunction (java.util.List<hydra.ext.scala.meta.Type> params, hydra.ext.scala.meta.Type res) {
    java.util.Objects.requireNonNull((params));
    java.util.Objects.requireNonNull((res));
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
  
  public Type_ImplicitFunction withParams(java.util.List<hydra.ext.scala.meta.Type> params) {
    java.util.Objects.requireNonNull((params));
    return new Type_ImplicitFunction(params, res);
  }
  
  public Type_ImplicitFunction withRes(hydra.ext.scala.meta.Type res) {
    java.util.Objects.requireNonNull((res));
    return new Type_ImplicitFunction(params, res);
  }
}