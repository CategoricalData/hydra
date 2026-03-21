// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms.
 */
public class TermCoder<A> implements Serializable, Comparable<TermCoder<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graph.TermCoder");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ENCODE = new hydra.core.Name("encode");

  public static final hydra.core.Name DECODE = new hydra.core.Name("decode");

  /**
   * The Hydra type of encoded terms
   */
  public final hydra.core.Type type;

  /**
   * An encode function from terms to native values
   */
  public final java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, A>>>> encode;

  /**
   * A decode function from native values to terms
   */
  public final java.util.function.Function<hydra.context.Context, java.util.function.Function<A, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> decode;

  public TermCoder (hydra.core.Type type, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, A>>>> encode, java.util.function.Function<hydra.context.Context, java.util.function.Function<A, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> decode) {
    this.type = type;
    this.encode = encode;
    this.decode = decode;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermCoder)) {
      return false;
    }
    TermCoder o = (TermCoder) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.encode,
      o.encode) && java.util.Objects.equals(
      this.decode,
      o.decode);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(encode) + 5 * java.util.Objects.hashCode(decode);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TermCoder other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
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

  public TermCoder withType(hydra.core.Type type) {
    return new TermCoder(type, encode, decode);
  }

  public TermCoder withEncode(java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, A>>>> encode) {
    return new TermCoder(type, encode, decode);
  }

  public TermCoder withDecode(java.util.function.Function<hydra.context.Context, java.util.function.Function<A, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> decode) {
    return new TermCoder(type, encode, decode);
  }
}
