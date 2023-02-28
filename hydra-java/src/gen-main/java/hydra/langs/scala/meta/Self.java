package hydra.langs.scala.meta;

public class Self {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Self");
  
  public Self () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Self)) {
      return false;
    }
    Self o = (Self) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}