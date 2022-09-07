package hydra.ext.scala.meta;

public class Member_Type {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Member.Type");
  
  public final hydra.ext.scala.meta.Type_Name name;
  
  public Member_Type (hydra.ext.scala.meta.Type_Name name) {
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