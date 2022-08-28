package hydra.ext.scala.meta;

public class Data_Function {
  public final java.util.List<hydra.ext.scala.meta.Data_Param> params;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Data_Function (java.util.List<hydra.ext.scala.meta.Data_Param> params, hydra.ext.scala.meta.Data body) {
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
    return new Data_Function(params, body);
  }
  
  public Data_Function withBody(hydra.ext.scala.meta.Data body) {
    return new Data_Function(params, body);
  }
}