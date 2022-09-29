package hydra.compute;

/**
 * An encoder and decoder; a bidirectional flow between two types
 */
public class Coder<S1, S2, V1, V2> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.Coder");
  
  public final java.util.function.Function<V1, hydra.compute.Flow<S1, V2>> encode;
  
  public final java.util.function.Function<V2, hydra.compute.Flow<S2, V1>> decode;
  
  public Coder (java.util.function.Function<V1, hydra.compute.Flow<S1, V2>> encode, java.util.function.Function<V2, hydra.compute.Flow<S2, V1>> decode) {
    this.encode = encode;
    this.decode = decode;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Coder)) {
      return false;
    }
    Coder o = (Coder) (other);
    return encode.equals(o.encode) && decode.equals(o.decode);
  }
  
  @Override
  public int hashCode() {
    return 2 * encode.hashCode() + 3 * decode.hashCode();
  }
  
  public Coder withEncode(java.util.function.Function<V1, hydra.compute.Flow<S1, V2>> encode) {
    return new Coder(encode, decode);
  }
  
  public Coder withDecode(java.util.function.Function<V2, hydra.compute.Flow<S2, V1>> decode) {
    return new Coder(encode, decode);
  }
}