package hydra.ext.scala.meta;

public class Mod_Private {
  public final Ref within;
  
  public Mod_Private (Ref within) {
    this.within = within;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Private)) {
      return false;
    }
    Mod_Private o = (Mod_Private) (other);
    return within.equals(o.within);
  }
  
  @Override
  public int hashCode() {
    return 2 * within.hashCode();
  }
}