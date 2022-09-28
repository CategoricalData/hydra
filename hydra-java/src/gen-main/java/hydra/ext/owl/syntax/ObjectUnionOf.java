package hydra.ext.owl.syntax;

public class ObjectUnionOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectUnionOf");
  
  public final java.util.List<hydra.ext.owl.syntax.ClassExpression> value;
  
  public ObjectUnionOf (java.util.List<hydra.ext.owl.syntax.ClassExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectUnionOf)) {
      return false;
    }
    ObjectUnionOf o = (ObjectUnionOf) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}