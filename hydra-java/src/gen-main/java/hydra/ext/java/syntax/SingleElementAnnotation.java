package hydra.ext.java.syntax;

public class SingleElementAnnotation {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.SingleElementAnnotation");
  
  public final hydra.ext.java.syntax.TypeName name;
  
  public final java.util.Optional<hydra.ext.java.syntax.ElementValue> value;
  
  public SingleElementAnnotation (hydra.ext.java.syntax.TypeName name, java.util.Optional<hydra.ext.java.syntax.ElementValue> value) {
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleElementAnnotation)) {
      return false;
    }
    SingleElementAnnotation o = (SingleElementAnnotation) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public SingleElementAnnotation withName(hydra.ext.java.syntax.TypeName name) {
    return new SingleElementAnnotation(name, value);
  }
  
  public SingleElementAnnotation withValue(java.util.Optional<hydra.ext.java.syntax.ElementValue> value) {
    return new SingleElementAnnotation(name, value);
  }
}