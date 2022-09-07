package hydra.ext.scala.meta;

public class Pat_Given {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.Given");
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public Pat_Given (hydra.ext.scala.meta.Type tpe) {
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Given)) {
      return false;
    }
    Pat_Given o = (Pat_Given) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}