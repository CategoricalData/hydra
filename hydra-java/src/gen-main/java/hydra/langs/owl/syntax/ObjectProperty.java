package hydra.langs.owl.syntax;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Object_Properties
 */
public class ObjectProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.ObjectProperty");
  
  public ObjectProperty () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectProperty)) {
      return false;
    }
    ObjectProperty o = (ObjectProperty) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}