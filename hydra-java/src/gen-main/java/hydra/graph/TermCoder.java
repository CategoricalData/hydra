package hydra.graph;

/**
 * A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
 */
public class TermCoder<A, X> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.TermCoder");
  
  public final hydra.core.Type<A> type;
  
  public final hydra.compute.Coder<hydra.graph.Graph<A>, hydra.graph.Graph<A>, hydra.core.Term<A>, X> coder;
  
  public TermCoder (hydra.core.Type<A> type, hydra.compute.Coder<hydra.graph.Graph<A>, hydra.graph.Graph<A>, hydra.core.Term<A>, X> coder) {
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
  
  public TermCoder withType(hydra.core.Type<A> type) {
    return new TermCoder(type, coder);
  }
  
  public TermCoder withCoder(hydra.compute.Coder<hydra.graph.Graph<A>, hydra.graph.Graph<A>, hydra.core.Term<A>, X> coder) {
    return new TermCoder(type, coder);
  }
}