package hydra.ext.scala.meta;

public class Type_TypedParam {
  public final Name name;
  
  public final Type typ;
  
  public Type_TypedParam (Name name, Type typ) {
    this.name = name;
    this.typ = typ;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_TypedParam)) {
      return false;
    }
    Type_TypedParam o = (Type_TypedParam) (other);
    return name.equals(o.name) && typ.equals(o.typ);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typ.hashCode();
  }
  
  public Type_TypedParam withName(Name name) {
    return new Type_TypedParam(name, typ);
  }
  
  public Type_TypedParam withTyp(Type typ) {
    return new Type_TypedParam(name, typ);
  }
}