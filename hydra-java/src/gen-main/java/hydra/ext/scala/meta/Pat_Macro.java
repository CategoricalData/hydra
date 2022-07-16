package hydra.ext.scala.meta;

public class Pat_Macro {
  public final Data body;
  
  public Pat_Macro (Data body) {
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Macro)) {
      return false;
    }
    Pat_Macro o = (Pat_Macro) (other);
    return body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode();
  }
}