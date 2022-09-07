package hydra.ext.xml.schema;

public class Integer_ {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.Integer");
  
  public final java.math.BigInteger value;
  
  public Integer_ (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Integer_)) {
      return false;
    }
    Integer_ o = (Integer_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}