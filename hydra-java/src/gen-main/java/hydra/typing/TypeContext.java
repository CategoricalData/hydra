// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A typing environment used for type reconstruction (typeOf) over System F terms
 */
public class TypeContext implements Serializable, Comparable<TypeContext> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.TypeContext");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public static final hydra.core.Name FIELD_NAME_METADATA = new hydra.core.Name("metadata");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_VARIABLES = new hydra.core.Name("typeVariables");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA_VARIABLES = new hydra.core.Name("lambdaVariables");
  
  public static final hydra.core.Name FIELD_NAME_LET_VARIABLES = new hydra.core.Name("letVariables");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE_CONTEXT = new hydra.core.Name("inferenceContext");
  
  /**
   * A mapping of lambda- and let-bound variables to their types
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Type> types;
  
  /**
   * Any additional metadata about lambda- and let-bound variables
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Term> metadata;
  
  /**
   * The set of type variables introduced by enclosing type lambdas
   */
  public final java.util.Set<hydra.core.Name> typeVariables;
  
  /**
   * The set of term variables introduced by lambdas (even if untyped)
   */
  public final java.util.Set<hydra.core.Name> lambdaVariables;
  
  /**
   * The set of term variables introduced by let bindings (even if untyped)
   */
  public final java.util.Set<hydra.core.Name> letVariables;
  
  /**
   * The schema types, primitive types, and data types of the graph
   */
  public final hydra.typing.InferenceContext inferenceContext;
  
  public TypeContext (java.util.Map<hydra.core.Name, hydra.core.Type> types, java.util.Map<hydra.core.Name, hydra.core.Term> metadata, java.util.Set<hydra.core.Name> typeVariables, java.util.Set<hydra.core.Name> lambdaVariables, java.util.Set<hydra.core.Name> letVariables, hydra.typing.InferenceContext inferenceContext) {
    this.types = types;
    this.metadata = metadata;
    this.typeVariables = typeVariables;
    this.lambdaVariables = lambdaVariables;
    this.letVariables = letVariables;
    this.inferenceContext = inferenceContext;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeContext)) {
      return false;
    }
    TypeContext o = (TypeContext) other;
    return java.util.Objects.equals(
      this.types,
      o.types) && java.util.Objects.equals(
      this.metadata,
      o.metadata) && java.util.Objects.equals(
      this.typeVariables,
      o.typeVariables) && java.util.Objects.equals(
      this.lambdaVariables,
      o.lambdaVariables) && java.util.Objects.equals(
      this.letVariables,
      o.letVariables) && java.util.Objects.equals(
      this.inferenceContext,
      o.inferenceContext);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(types) + 3 * java.util.Objects.hashCode(metadata) + 5 * java.util.Objects.hashCode(typeVariables) + 7 * java.util.Objects.hashCode(lambdaVariables) + 11 * java.util.Objects.hashCode(letVariables) + 13 * java.util.Objects.hashCode(inferenceContext);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeContext other) {
    int cmp = 0;
    cmp = Integer.compare(
      types.hashCode(),
      other.types.hashCode());
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
      typeVariables.hashCode(),
      other.typeVariables.hashCode());
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
      letVariables.hashCode(),
      other.letVariables.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) inferenceContext).compareTo(other.inferenceContext);
  }
  
  public TypeContext withTypes(java.util.Map<hydra.core.Name, hydra.core.Type> types) {
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, letVariables, inferenceContext);
  }
  
  public TypeContext withMetadata(java.util.Map<hydra.core.Name, hydra.core.Term> metadata) {
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, letVariables, inferenceContext);
  }
  
  public TypeContext withTypeVariables(java.util.Set<hydra.core.Name> typeVariables) {
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, letVariables, inferenceContext);
  }
  
  public TypeContext withLambdaVariables(java.util.Set<hydra.core.Name> lambdaVariables) {
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, letVariables, inferenceContext);
  }
  
  public TypeContext withLetVariables(java.util.Set<hydra.core.Name> letVariables) {
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, letVariables, inferenceContext);
  }
  
  public TypeContext withInferenceContext(hydra.typing.InferenceContext inferenceContext) {
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, letVariables, inferenceContext);
  }
}
