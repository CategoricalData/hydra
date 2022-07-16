package hydra.ext.java.syntax;

public class SingleElementAnnotation {
  public final TypeName name;
  
  public final java.util.Optional<ElementValue> value;
  
  public SingleElementAnnotation (TypeName name, java.util.Optional<ElementValue> value) {
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
  
  public SingleElementAnnotation withName(TypeName name) {
    return new SingleElementAnnotation(name, value);
  }
  
  public SingleElementAnnotation withValue(java.util.Optional<ElementValue> value) {
    return new SingleElementAnnotation(name, value);
  }
}