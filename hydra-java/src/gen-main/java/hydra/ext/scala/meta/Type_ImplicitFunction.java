package hydra.ext.scala.meta;

public class Type_ImplicitFunction {
  public final java.util.List<Type> params;
  
  public final Type res;
  
  public Type_ImplicitFunction (java.util.List<Type> params, Type res) {
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
  
  public Type_ImplicitFunction withParams(java.util.List<Type> params) {
    return new Type_ImplicitFunction(params, res);
  }
  
  public Type_ImplicitFunction withRes(Type res) {
    return new Type_ImplicitFunction(params, res);
  }
}