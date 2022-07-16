package hydra.ext.scala.meta;

public class Type_Function {
  public final java.util.List<Type> params;
  
  public final Type res;
  
  public Type_Function (java.util.List<Type> params, Type res) {
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
  
  public Type_Function withParams(java.util.List<Type> params) {
    return new Type_Function(params, res);
  }
  
  public Type_Function withRes(Type res) {
    return new Type_Function(params, res);
  }
}