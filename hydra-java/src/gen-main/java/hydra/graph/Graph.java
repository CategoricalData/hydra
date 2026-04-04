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
  public final java.util.Map<hydra.core.Name, hydra.core.Term> boundTerms;

  /**
   * The type schemes of all term variables in scope
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeScheme> boundTypes;

  /**
   * A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints;

  /**
   * The set of term variables introduced by specifically by lambdas
   */
  public final java.util.Set<hydra.core.Name> lambdaVariables;

  /**
   * Any additional metadata bound to term variables in scope
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Term> metadata;

  /**
   * All primitive functions and constants by name
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives;

  /**
   * All schema types (type schemes) in scope
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes;

  /**
   * The set of type variables introduced specifically by type lambdas
   */
  public final java.util.Set<hydra.core.Name> typeVariables;

  public Graph (java.util.Map<hydra.core.Name, hydra.core.Term> boundTerms, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> boundTypes, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints, java.util.Set<hydra.core.Name> lambdaVariables, java.util.Map<hydra.core.Name, hydra.core.Term> metadata, java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes, java.util.Set<hydra.core.Name> typeVariables) {
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
    cmp = hydra.util.Comparing.compare(
      boundTerms,
      other.boundTerms);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      boundTypes,
      other.boundTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      classConstraints,
      other.classConstraints);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      lambdaVariables,
      other.lambdaVariables);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      metadata,
      other.metadata);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      primitives,
      other.primitives);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      schemaTypes,
      other.schemaTypes);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeVariables,
      other.typeVariables);
  }

  public Graph withBoundTerms(java.util.Map<hydra.core.Name, hydra.core.Term> boundTerms) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withBoundTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> boundTypes) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withClassConstraints(java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withLambdaVariables(java.util.Set<hydra.core.Name> lambdaVariables) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withMetadata(java.util.Map<hydra.core.Name, hydra.core.Term> metadata) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withPrimitives(java.util.Map<hydra.core.Name, hydra.graph.Primitive> primitives) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withSchemaTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }

  public Graph withTypeVariables(java.util.Set<hydra.core.Name> typeVariables) {
    return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
  }
}
