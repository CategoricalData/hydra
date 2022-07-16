package hydra.graph;

/**
 * A logical collection of elements; a graph subset with dependencies on zero or more other subsets
 */
public class Module<M> {
  public final Graph<M> graph;
  
  public final java.util.List<Module<M>> imports;
  
  public Module (Graph<M> graph, java.util.List<Module<M>> imports) {
    this.graph = graph;
    this.imports = imports;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) (other);
    return graph.equals(o.graph) && imports.equals(o.imports);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph.hashCode() + 3 * imports.hashCode();
  }
  
  public Module withGraph(Graph<M> graph) {
    return new Module(graph, imports);
  }
  
  public Module withImports(java.util.List<Module<M>> imports) {
    return new Module(graph, imports);
  }
}