package hydra.ext.scala.meta;

public class Data_Function {
  public final java.util.List<Data_Param> params;
  
  public final Data body;
  
  public Data_Function (java.util.List<Data_Param> params, Data body) {
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
  
  public Data_Function withParams(java.util.List<Data_Param> params) {
    return new Data_Function(params, body);
  }
  
  public Data_Function withBody(Data body) {
    return new Data_Function(params, body);
  }
}