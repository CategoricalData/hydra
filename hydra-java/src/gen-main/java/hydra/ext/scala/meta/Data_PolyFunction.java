package hydra.ext.scala.meta;

public class Data_PolyFunction {
  public final java.util.List<Type_Param> tparams;
  
  public final Data body;
  
  public Data_PolyFunction (java.util.List<Type_Param> tparams, Data body) {
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
  
  public Data_PolyFunction withTparams(java.util.List<Type_Param> tparams) {
    return new Data_PolyFunction(tparams, body);
  }
  
  public Data_PolyFunction withBody(Data body) {
    return new Data_PolyFunction(tparams, body);
  }
}