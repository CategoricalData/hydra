package hydra.ext.scala.meta;

public class Pat_Repeated {
  public final hydra.ext.scala.meta.Data_Name name;
  
  public Pat_Repeated (hydra.ext.scala.meta.Data_Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Repeated)) {
      return false;
    }
    Pat_Repeated o = (Pat_Repeated) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}