package hydra.ext.owl.syntax;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Classes
 */
public class Class_ {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.Class");
  
  public Class_ () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Class_)) {
      return false;
    }
    Class_ o = (Class_) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}