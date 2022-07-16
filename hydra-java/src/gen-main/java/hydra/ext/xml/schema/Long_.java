package hydra.ext.xml.schema;

public class Long_ {
  public final Long value;
  
  public Long_ (Long value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Long_)) {
      return false;
    }
    Long_ o = (Long_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}