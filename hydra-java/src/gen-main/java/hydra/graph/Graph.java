// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

/**
 * A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph
 */
public class Graph {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Graph");
  
  /**
   * All of the elements in the graph
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Element> elements;
  
  /**
   * The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
   */
  public final java.util.Map<hydra.core.Name, hydra.util.Opt<hydra.core.Term>> environment;
  
  /**
   * The typing environment of the graph
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types;
  
  /**
   * The body of the term which generated this context
   */
  public final hydra.core.Term body;
  
  /**
   * All supported primitive constants and functions, by name
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives;
  
  /**
   * The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph.
   */
  public final hydra.util.Opt<hydra.graph.Graph> schema;
  
  public Graph (java.util.Map<hydra.core.Name, hydra.graph.Element> elements, java.util.Map<hydra.core.Name, hydra.util.Opt<hydra.core.Term>> environment, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types, hydra.core.Term body, java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives, hydra.util.Opt<hydra.graph.Graph> schema) {
    java.util.Objects.requireNonNull((elements));
    java.util.Objects.requireNonNull((environment));
    java.util.Objects.requireNonNull((types));
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((primitives));
    java.util.Objects.requireNonNull((schema));
    this.elements = elements;
    this.environment = environment;
    this.types = types;
    this.body = body;
    this.primitives = primitives;
    this.schema = schema;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return elements.equals(o.elements) && environment.equals(o.environment) && types.equals(o.types) && body.equals(o.body) && primitives.equals(o.primitives) && schema.equals(o.schema);
  }
  
  @Override
  public int hashCode() {
    return 2 * elements.hashCode() + 3 * environment.hashCode() + 5 * types.hashCode() + 7 * body.hashCode() + 11 * primitives.hashCode() + 13 * schema.hashCode();
  }
  
  public Graph withElements(java.util.Map<hydra.core.Name, hydra.graph.Element> elements) {
    java.util.Objects.requireNonNull((elements));
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withEnvironment(java.util.Map<hydra.core.Name, hydra.util.Opt<hydra.core.Term>> environment) {
    java.util.Objects.requireNonNull((environment));
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types) {
    java.util.Objects.requireNonNull((types));
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withPrimitives(java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives) {
    java.util.Objects.requireNonNull((primitives));
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withSchema(hydra.util.Opt<hydra.graph.Graph> schema) {
    java.util.Objects.requireNonNull((schema));
    return new Graph(elements, environment, types, body, primitives, schema);
  }
}