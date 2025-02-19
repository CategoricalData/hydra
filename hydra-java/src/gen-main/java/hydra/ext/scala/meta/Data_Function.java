// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Function implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Function");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.scala.meta.Data_Param> params;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Data_Function (java.util.List<hydra.ext.scala.meta.Data_Param> params, hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((params));
    java.util.Objects.requireNonNull((body));
    this.params = params;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Function)) {
      return false;
    }
    Data_Function o = (Data_Function) (other);
    return params.equals(o.params) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * params.hashCode() + 3 * body.hashCode();
  }
  
  public Data_Function withParams(java.util.List<hydra.ext.scala.meta.Data_Param> params) {
    java.util.Objects.requireNonNull((params));
    return new Data_Function(params, body);
  }
  
  public Data_Function withBody(hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Data_Function(params, body);
  }
}