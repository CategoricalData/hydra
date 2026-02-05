// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
 */
public class TermCoder<A> implements Serializable, Comparable<TermCoder<A>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.graph.TermCoder");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_CODER = new hydra.core.Name("coder");
  
  /**
   * The Hydra type of encoded terms
   */
  public final hydra.core.Type type;
  
  /**
   * A coder between Hydra terms and instances of the given type
   */
  public final hydra.compute.Coder<hydra.graph.Graph, hydra.graph.Graph, hydra.core.Term, A> coder;
  
  public TermCoder (hydra.core.Type type, hydra.compute.Coder<hydra.graph.Graph, hydra.graph.Graph, hydra.core.Term, A> coder) {
    this.type = type;
    this.coder = coder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermCoder)) {
      return false;
    }
    TermCoder o = (TermCoder) (other);
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.coder,
      o.coder);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(coder);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TermCoder other) {
    int cmp = 0;
    cmp = ((Comparable) (type)).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (coder)).compareTo(other.coder);
  }
  
  public TermCoder withType(hydra.core.Type type) {
    return new TermCoder(type, coder);
  }
  
  public TermCoder withCoder(hydra.compute.Coder<hydra.graph.Graph, hydra.graph.Graph, hydra.core.Term, A> coder) {
    return new TermCoder(type, coder);
  }
}
