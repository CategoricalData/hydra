// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * An encoder and decoder; a bidirectional transformation between two types
 */
public class Coder<V1, V2> implements Serializable, Comparable<Coder<V1, V2>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coders.Coder");

  public static final hydra.core.Name ENCODE = new hydra.core.Name("encode");

  public static final hydra.core.Name DECODE = new hydra.core.Name("decode");

  /**
   * A function which encodes source values as target values in a given context
   */
  public final java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.errors.Error_, V2>>> encode;

  /**
   * A function which decodes target values as source values in a given context
   */
  public final java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.errors.Error_, V1>>> decode;

  public Coder (java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.errors.Error_, V2>>> encode, java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.errors.Error_, V1>>> decode) {
    this.encode = encode;
    this.decode = decode;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Coder)) {
      return false;
    }
    Coder o = (Coder) other;
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
  public int compareTo(Coder other) {
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

  public Coder withEncode(java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.errors.Error_, V2>>> encode) {
    return new Coder(encode, decode);
  }

  public Coder withDecode(java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.errors.Error_, V1>>> decode) {
    return new Coder(encode, decode);
  }
}
