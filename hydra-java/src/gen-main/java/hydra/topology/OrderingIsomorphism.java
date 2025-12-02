// Note: this is an automatically generated file. Do not edit.

package hydra.topology;

public class OrderingIsomorphism<A> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.topology.OrderingIsomorphism");
  
  public static final hydra.core.Name FIELD_NAME_ENCODE = new hydra.core.Name("encode");
  
  public static final hydra.core.Name FIELD_NAME_DECODE = new hydra.core.Name("decode");
  
  /**
   * Mapping from source ordering to target ordering
   */
  public final java.util.function.Function<java.util.List<A>, java.util.List<A>> encode;
  
  /**
   * Mapping from target ordering to source ordering
   */
  public final java.util.function.Function<java.util.List<A>, java.util.List<A>> decode;
  
  public OrderingIsomorphism (java.util.function.Function<java.util.List<A>, java.util.List<A>> encode, java.util.function.Function<java.util.List<A>, java.util.List<A>> decode) {
    java.util.Objects.requireNonNull((encode));
    java.util.Objects.requireNonNull((decode));
    this.encode = encode;
    this.decode = decode;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrderingIsomorphism)) {
      return false;
    }
    OrderingIsomorphism o = (OrderingIsomorphism) (other);
    return encode.equals(o.encode) && decode.equals(o.decode);
  }
  
  @Override
  public int hashCode() {
    return 2 * encode.hashCode() + 3 * decode.hashCode();
  }
  
  public OrderingIsomorphism withEncode(java.util.function.Function<java.util.List<A>, java.util.List<A>> encode) {
    java.util.Objects.requireNonNull((encode));
    return new OrderingIsomorphism(encode, decode);
  }
  
  public OrderingIsomorphism withDecode(java.util.function.Function<java.util.List<A>, java.util.List<A>> decode) {
    java.util.Objects.requireNonNull((decode));
    return new OrderingIsomorphism(encode, decode);
  }
}
