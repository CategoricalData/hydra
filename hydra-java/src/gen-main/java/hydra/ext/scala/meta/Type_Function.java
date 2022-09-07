package hydra.ext.scala.meta;

public class Type_Function {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Function");
  
  public final java.util.List<hydra.ext.scala.meta.Type> params;
  
  public final hydra.ext.scala.meta.Type res;
  
  public Type_Function (java.util.List<hydra.ext.scala.meta.Type> params, hydra.ext.scala.meta.Type res) {
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
  
  public Type_Function withParams(java.util.List<hydra.ext.scala.meta.Type> params) {
    return new Type_Function(params, res);
  }
  
  public Type_Function withRes(hydra.ext.scala.meta.Type res) {
    return new Type_Function(params, res);
  }
}