package hydra.ext.scala.meta;

public class Mod_Annot {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Mod.Annot");
  
  public final hydra.ext.scala.meta.Init init;
  
  public Mod_Annot (hydra.ext.scala.meta.Init init) {
    this.init = init;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Annot)) {
      return false;
    }
    Mod_Annot o = (Mod_Annot) (other);
    return init.equals(o.init);
  }
  
  @Override
  public int hashCode() {
    return 2 * init.hashCode();
  }
}