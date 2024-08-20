// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_PolyFunction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.PolyFunction");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Data_PolyFunction (java.util.List<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((body));
    this.tparams = tparams;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_PolyFunction)) {
      return false;
    }
    Data_PolyFunction o = (Data_PolyFunction) (other);
    return tparams.equals(o.tparams) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * tparams.hashCode() + 3 * body.hashCode();
  }
  
  public Data_PolyFunction withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Data_PolyFunction(tparams, body);
  }
  
  public Data_PolyFunction withBody(hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Data_PolyFunction(tparams, body);
  }
}
