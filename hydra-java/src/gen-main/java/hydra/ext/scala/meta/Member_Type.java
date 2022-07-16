package hydra.ext.scala.meta;

public class Member_Type {
  public final Type_Name name;
  
  public Member_Type (Type_Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Member_Type)) {
      return false;
    }
    Member_Type o = (Member_Type) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}