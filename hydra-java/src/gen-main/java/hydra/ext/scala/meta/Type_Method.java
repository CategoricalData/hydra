package hydra.ext.scala.meta;

public class Type_Method {
  public final java.util.List<java.util.List<Data_Param>> paramss;
  
  public final Type tpe;
  
  public Type_Method (java.util.List<java.util.List<Data_Param>> paramss, Type tpe) {
    this.paramss = paramss;
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Method)) {
      return false;
    }
    Type_Method o = (Type_Method) (other);
    return paramss.equals(o.paramss) && tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramss.hashCode() + 3 * tpe.hashCode();
  }
  
  public Type_Method withParamss(java.util.List<java.util.List<Data_Param>> paramss) {
    return new Type_Method(paramss, tpe);
  }
  
  public Type_Method withTpe(Type tpe) {
    return new Type_Method(paramss, tpe);
  }
}