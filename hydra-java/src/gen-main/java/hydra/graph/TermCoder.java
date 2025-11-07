// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

/**
 * A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
 */
public class TermCoder<A> {
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
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((coder));
    this.type = type;
    this.coder = coder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermCoder)) {
      return false;
    }
    TermCoder o = (TermCoder) (other);
    return type.equals(o.type) && coder.equals(o.coder);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * coder.hashCode();
  }
  
  public TermCoder withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TermCoder(type, coder);
  }
  
  public TermCoder withCoder(hydra.compute.Coder<hydra.graph.Graph, hydra.graph.Graph, hydra.core.Term, A> coder) {
    java.util.Objects.requireNonNull((coder));
    return new TermCoder(type, coder);
  }
}
