// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph
 */
public class Graph implements Serializable, Comparable<Graph> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.graph.Graph");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENTS = new hydra.core.Name("elements");
  
  public static final hydra.core.Name FIELD_NAME_ENVIRONMENT = new hydra.core.Name("environment");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVES = new hydra.core.Name("primitives");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  /**
   * All of the elements in the graph
   */
  public final java.util.List<hydra.core.Binding> elements;
  
  /**
   * The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
   */
  public final java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> environment;
  
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
  public final hydra.util.Maybe<hydra.graph.Graph> schema;
  
  public Graph (java.util.List<hydra.core.Binding> elements, java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> environment, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types, hydra.core.Term body, java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives, hydra.util.Maybe<hydra.graph.Graph> schema) {
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
    return java.util.Objects.equals(
      this.elements,
      o.elements) && java.util.Objects.equals(
      this.environment,
      o.environment) && java.util.Objects.equals(
      this.types,
      o.types) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.primitives,
      o.primitives) && java.util.Objects.equals(
      this.schema,
      o.schema);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elements) + 3 * java.util.Objects.hashCode(environment) + 5 * java.util.Objects.hashCode(types) + 7 * java.util.Objects.hashCode(body) + 11 * java.util.Objects.hashCode(primitives) + 13 * java.util.Objects.hashCode(schema);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Graph other) {
    int cmp = 0;
    cmp = Integer.compare(
      elements.hashCode(),
      other.elements.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      environment.hashCode(),
      other.environment.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      types.hashCode(),
      other.types.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (body)).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      primitives.hashCode(),
      other.primitives.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      schema.hashCode(),
      other.schema.hashCode());
  }
  
  public Graph withElements(java.util.List<hydra.core.Binding> elements) {
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withEnvironment(java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> environment) {
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types) {
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withBody(hydra.core.Term body) {
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withPrimitives(java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives) {
    return new Graph(elements, environment, types, body, primitives, schema);
  }
  
  public Graph withSchema(hydra.util.Maybe<hydra.graph.Graph> schema) {
    return new Graph(elements, environment, types, body, primitives, schema);
  }
}
