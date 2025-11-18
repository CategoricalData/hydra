// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import hydra.util.Maybe;

import java.io.Serializable;
import java.util.List;

/**
 * A tuple elimination; a projection from an integer-indexed product
 */
public class TupleProjection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.TupleProjection");
  
  public static final hydra.core.Name FIELD_NAME_ARITY = new hydra.core.Name("arity");
  
  public static final hydra.core.Name FIELD_NAME_INDEX = new hydra.core.Name("index");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  /**
   * The arity of the tuple
   */
  public final Integer arity;
  
  /**
   * The 0-indexed offset from the beginning of the tuple
   */
  public final Integer index;
  
  /**
   * An optional domain for the projection; this is a list of component types
   */
  public final Maybe<List<Type>> domain;
  
  public TupleProjection (Integer arity, Integer index, Maybe<List<Type>> domain) {
    java.util.Objects.requireNonNull((arity));
    java.util.Objects.requireNonNull((index));
    java.util.Objects.requireNonNull((domain));
    this.arity = arity;
    this.index = index;
    this.domain = domain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TupleProjection)) {
      return false;
    }
    TupleProjection o = (TupleProjection) (other);
    return arity.equals(o.arity) && index.equals(o.index) && domain.equals(o.domain);
  }
  
  @Override
  public int hashCode() {
    return 2 * arity.hashCode() + 3 * index.hashCode() + 5 * domain.hashCode();
  }
  
  public TupleProjection withArity(Integer arity) {
    java.util.Objects.requireNonNull((arity));
    return new TupleProjection(arity, index, domain);
  }
  
  public TupleProjection withIndex(Integer index) {
    java.util.Objects.requireNonNull((index));
    return new TupleProjection(arity, index, domain);
  }
  
  public TupleProjection withDomain(Maybe<List<Type>> domain) {
    java.util.Objects.requireNonNull((domain));
    return new TupleProjection(arity, index, domain);
  }
}
