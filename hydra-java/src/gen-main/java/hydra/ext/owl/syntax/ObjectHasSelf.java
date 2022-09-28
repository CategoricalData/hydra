package hydra.ext.owl.syntax;

public class ObjectHasSelf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectHasSelf");
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression value;
  
  public ObjectHasSelf (hydra.ext.owl.syntax.ObjectPropertyExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectHasSelf)) {
      return false;
    }
    ObjectHasSelf o = (ObjectHasSelf) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}