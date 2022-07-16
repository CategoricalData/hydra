package hydra.evaluation;

/**
 * An encoder and decoder; a qualified bidirectional transformation between instances of two types
 */
public class Coder<A, B> {
  public final java.util.function.Function<A, Result<B>> encode;
  
  public final java.util.function.Function<B, Result<A>> decode;
  
  public Coder (java.util.function.Function<A, Result<B>> encode, java.util.function.Function<B, Result<A>> decode) {
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
  
  public Coder withEncode(java.util.function.Function<A, Result<B>> encode) {
    return new Coder(encode, decode);
  }
  
  public Coder withDecode(java.util.function.Function<B, Result<A>> decode) {
    return new Coder(encode, decode);
  }
}