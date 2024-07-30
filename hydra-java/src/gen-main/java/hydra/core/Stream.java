// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An infinite stream of terms
 */
public class Stream implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Stream");
  
  public final hydra.core.Term first;
  
  public final hydra.core.Stream rest;
  
  public Stream (hydra.core.Term first, hydra.core.Stream rest) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((rest));
    this.first = first;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Stream)) {
      return false;
    }
    Stream o = (Stream) (other);
    return first.equals(o.first) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * rest.hashCode();
  }
  
  public Stream withFirst(hydra.core.Term first) {
    java.util.Objects.requireNonNull((first));
    return new Stream(first, rest);
  }
  
  public Stream withRest(hydra.core.Stream rest) {
    java.util.Objects.requireNonNull((rest));
    return new Stream(first, rest);
  }
}