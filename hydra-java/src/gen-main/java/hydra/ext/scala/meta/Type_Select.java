package hydra.ext.scala.meta;

public class Type_Select {
  public final Data_Ref qual;
  
  public final Type_Name name;
  
  public Type_Select (Data_Ref qual, Type_Name name) {
    this.qual = qual;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Select)) {
      return false;
    }
    Type_Select o = (Type_Select) (other);
    return qual.equals(o.qual) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * qual.hashCode() + 3 * name.hashCode();
  }
  
  public Type_Select withQual(Data_Ref qual) {
    return new Type_Select(qual, name);
  }
  
  public Type_Select withName(Type_Name name) {
    return new Type_Select(qual, name);
  }
}