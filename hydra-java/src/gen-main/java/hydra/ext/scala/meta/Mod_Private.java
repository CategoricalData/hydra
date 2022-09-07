package hydra.ext.scala.meta;

public class Mod_Private {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Mod.Private");
  
  public final hydra.ext.scala.meta.Ref within;
  
  public Mod_Private (hydra.ext.scala.meta.Ref within) {
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