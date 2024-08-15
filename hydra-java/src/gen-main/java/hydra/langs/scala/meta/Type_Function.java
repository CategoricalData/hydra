// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Function implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Function");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name FIELD_NAME_RES = new hydra.core.Name("res");
  
  public final java.util.List<hydra.langs.scala.meta.Type> params;
  
  public final hydra.langs.scala.meta.Type res;
  
  public Type_Function (java.util.List<hydra.langs.scala.meta.Type> params, hydra.langs.scala.meta.Type res) {
    java.util.Objects.requireNonNull((params));
    java.util.Objects.requireNonNull((res));
    this.params = params;
    this.res = res;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Function)) {
      return false;
    }
    Type_Function o = (Type_Function) (other);
    return params.equals(o.params) && res.equals(o.res);
  }
  
  @Override
  public int hashCode() {
    return 2 * params.hashCode() + 3 * res.hashCode();
  }
  
  public Type_Function withParams(java.util.List<hydra.langs.scala.meta.Type> params) {
    java.util.Objects.requireNonNull((params));
    return new Type_Function(params, res);
  }
  
  public Type_Function withRes(hydra.langs.scala.meta.Type res) {
    java.util.Objects.requireNonNull((res));
    return new Type_Function(params, res);
  }
}