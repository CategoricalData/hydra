package hydra.ext.scala.meta;

public class Mod_Protected {
  public final hydra.ext.scala.meta.Ref within;
  
  public Mod_Protected (hydra.ext.scala.meta.Ref within) {
    this.within = within;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Protected)) {
      return false;
    }
    Mod_Protected o = (Mod_Protected) (other);
    return within.equals(o.within);
  }
  
  @Override
  public int hashCode() {
    return 2 * within.hashCode();
  }
}