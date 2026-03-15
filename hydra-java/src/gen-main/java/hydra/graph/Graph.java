// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A graph, or lexical environment which binds names to terms, types, primitives, and metadata
 */
public class Graph implements Serializable, Comparable<Graph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graph.Graph");
  
  public static final hydra.core.Name BOUND_TERMS = new hydra.core.Name("boundTerms");
  
  public static final hydra.core.Name BOUND_TYPES = new hydra.core.Name("boundTypes");
  
  public static final hydra.core.Name CLASS_CONSTRAINTS = new hydra.core.Name("classConstraints");
  
  public static final hydra.core.Name LAMBDA_VARIABLES = new hydra.core.Name("lambdaVariables");
  
  public static final hydra.core.Name METADATA = new hydra.core.Name("metadata");
  
  public static final hydra.core.Name PRIMITIVES = new hydra.core.Name("primitives");
  
  public static final hydra.core.Name SCHEMA_TYPES = new hydra.core.Name("schemaTypes");
  
  public static final hydra.core.Name TYPE_VARIABLES = new hydra.core.Name("typeVariables");
  
  /**
   * The terms bound by all term variables in scope
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> boundTerms;
  
  /**
   * The type schemes of all term variables in scope
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> boundTypes;
  
  /**
   * A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints;
  
  /**
   * The set of term variables introduced by specifically by lambdas
   */
  public final hydra.util.PersistentSet<hydra.core.Name> lambdaVariables;
  
  /**
   * Any additional metadata bound to term variables in scope
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> metadata;
  
  /**
   * All primitive functions and constants by name
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive> primitives;
  
  /**
   * All schema types (type schemes) in scope
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> schemaTypes;
  
  /**
   * The set of type variables introduced specifically by type lambdas
   */
  public final hydra.util.PersistentSet<hydra.core.Name> typeVariables;
  
  public Graph (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> boundTerms, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> boundTypes, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints, hydra.util.PersistentSet<hydra.core.Name> lambdaVariables, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> metadata, hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive> primitives, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> schemaTypes, hydra.util.PersistentSet<hydra.core.Name> typeVariables) {
    this.boundTerms = boundTerms;
    this.boundTypes = boundTypes;
    this.classConstraints = classConstraints;
    this.lambdaVariables = lambdaVariables;
    this.metadata = metadata;
    this.primitives = primitives;
    this.schemaTypes = schemaTypes;
    this.typeVariables = typeVariables;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) other;
    return java.util.Objects.equals(
      this.boundTerms,
      o.boundTerms) && java.util.Objects.equals(
      this.boundTypes,
      o.boundTypes) && java.util.Objects.equals(
      this.classConstraints,
      o.classConstraints) && java.util.Objects.equals(
      this.lambdaVariables,
      o.lambdaVariables) && java.util.Objects.equals(
      this.metadata,
      o.metadata) && java.util.Objects.equals(
      this.primitives,
      o.primitives) && java.util.Objects.equals(
      this.schemaTypes,
      o.schemaTypes) && java.util.Objects.equals(
      this.typeVariables,
      o.typeVariables);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(boundTerms) + 3 * java.util.Objects.hashCode(boundTypes) + 5 * java.util.Objects.hashCode(classConstraints) + 7 * java.util.Objects.hashCode(lambdaVariables) + 11 * java.util.Objects.hashCode(metadata) + 13 * java.util.Objects.hashCode(primitives) + 17 * java.util.Objects.hashCode(schemaTypes) + 19 * java.util.Objects.hashCode(typeVariables);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Graph other) {
    int cmp = 0;
    cmp = Integer.compare(
      boundTerms.hashCode(),
      other.boundTerms.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      boundTypes.hashCode(),
      other.boundTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      classConstraints.hashCode(),
      other.classConstraints.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      lambdaVariables.hashCode(),
      other.lambdaVariables.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      metadata.hashCode(),
      other.metadata.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      primitives.hashCode(),
      other.primitives.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      schemaTypes.hashCode(),
      other.schemaTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeVariables.hashCode(),
      other.typeVariables.hashCode());
  }
  
  public Graph withBoundTerms(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> boundTerms) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withBoundTypes(hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> boundTypes) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withClassConstraints(hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withLambdaVariables(hydra.util.PersistentSet<hydra.core.Name> lambdaVariables) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withMetadata(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> metadata) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withPrimitives(hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive> primitives) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withSchemaTypes(hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> schemaTypes) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
  
  public Graph withTypeVariables(hydra.util.PersistentSet<hydra.core.Name> typeVariables) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
}
