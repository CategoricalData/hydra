// Note: this is an automatically generated file. Do not edit.

package hydra.topology;

import java.io.Serializable;

public class OrderingIsomorphism<A> implements Serializable, Comparable<OrderingIsomorphism<A>> {
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
    this.encode = encode;
    this.decode = decode;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrderingIsomorphism)) {
      return false;
    }
    OrderingIsomorphism o = (OrderingIsomorphism) other;
    return java.util.Objects.equals(
      this.encode,
      o.encode) && java.util.Objects.equals(
      this.decode,
      o.decode);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(encode) + 3 * java.util.Objects.hashCode(decode);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrderingIsomorphism other) {
    int cmp = 0;
    cmp = Integer.compare(
      encode.hashCode(),
      other.encode.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      decode.hashCode(),
      other.decode.hashCode());
  }
  
  public OrderingIsomorphism withEncode(java.util.function.Function<java.util.List<A>, java.util.List<A>> encode) {
    return new OrderingIsomorphism(encode, decode);
  }
  
  public OrderingIsomorphism withDecode(java.util.function.Function<java.util.List<A>, java.util.List<A>> decode) {
    return new OrderingIsomorphism(encode, decode);
  }
}
