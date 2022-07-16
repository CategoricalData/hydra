package hydra.ext.java.syntax;

public class ResourceSpecification {
  public final java.util.List<Resource> value;
  
  public ResourceSpecification (java.util.List<Resource> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ResourceSpecification)) {
      return false;
    }
    ResourceSpecification o = (ResourceSpecification) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}