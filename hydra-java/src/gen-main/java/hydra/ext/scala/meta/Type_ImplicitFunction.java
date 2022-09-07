package hydra.ext.scala.meta;

public class Type_ImplicitFunction {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.ImplicitFunction");
  
  public final java.util.List<hydra.ext.scala.meta.Type> params;
  
  public final hydra.ext.scala.meta.Type res;
  
  public Type_ImplicitFunction (java.util.List<hydra.ext.scala.meta.Type> params, hydra.ext.scala.meta.Type res) {
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
    return new Type_ImplicitFunction(params, res);
  }
  
  public Type_ImplicitFunction withRes(hydra.ext.scala.meta.Type res) {
    return new Type_ImplicitFunction(params, res);
  }
}