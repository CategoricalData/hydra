package hydra.ext.scala.meta;

public class Mod_Protected {
  public final Ref within;
  
  public Mod_Protected (Ref within) {
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