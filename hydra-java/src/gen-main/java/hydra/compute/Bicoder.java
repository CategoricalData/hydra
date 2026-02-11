// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * A two-level encoder and decoder, operating both at a type level and an instance (data) level
 */
public class Bicoder<S1, S2, T1, T2, V1, V2> implements Serializable, Comparable<Bicoder<S1, S2, T1, T2, V1, V2>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.compute.Bicoder");
  
  public static final hydra.core.Name FIELD_NAME_ENCODE = new hydra.core.Name("encode");
  
  public static final hydra.core.Name FIELD_NAME_DECODE = new hydra.core.Name("decode");
  
  /**
   * A function from source types to adapters
   */
  public final java.util.function.Function<T1, hydra.compute.Adapter<S1, S2, T1, T2, V1, V2>> encode;
  
  /**
   * A function from target types to adapters
   */
  public final java.util.function.Function<T2, hydra.compute.Adapter<S2, S1, T2, T1, V2, V1>> decode;
  
  public Bicoder (java.util.function.Function<T1, hydra.compute.Adapter<S1, S2, T1, T2, V1, V2>> encode, java.util.function.Function<T2, hydra.compute.Adapter<S2, S1, T2, T1, V2, V1>> decode) {
    this.encode = encode;
    this.decode = decode;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Bicoder)) {
      return false;
    }
    Bicoder o = (Bicoder) other;
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
  public int compareTo(Bicoder other) {
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
  
  public Bicoder withEncode(java.util.function.Function<T1, hydra.compute.Adapter<S1, S2, T1, T2, V1, V2>> encode) {
    return new Bicoder(encode, decode);
  }
  
  public Bicoder withDecode(java.util.function.Function<T2, hydra.compute.Adapter<S2, S1, T2, T1, V2, V1>> decode) {
    return new Bicoder(encode, decode);
  }
}
