package hydra.ext.coq.syntax;

public class Number_ {
  public final Double value;
  
  public Number_ (Double value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Number_)) {
      return false;
    }
    Number_ o = (Number_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}