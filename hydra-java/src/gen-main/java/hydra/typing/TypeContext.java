// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A typing environment used for type reconstruction (typeOf) over System F terms
 */
public class TypeContext implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.TypeContext");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public static final hydra.core.Name FIELD_NAME_METADATA = new hydra.core.Name("metadata");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_VARIABLES = new hydra.core.Name("typeVariables");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA_VARIABLES = new hydra.core.Name("lambdaVariables");
  
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
   * The set of term variables introduced by lambdas, as opposed to let bindings
   */
  public final java.util.Set<hydra.core.Name> lambdaVariables;
  
  /**
   * The schema types, primitive types, and data types of the graph
   */
  public final hydra.typing.InferenceContext inferenceContext;
  
  public TypeContext (java.util.Map<hydra.core.Name, hydra.core.Type> types, java.util.Map<hydra.core.Name, hydra.core.Term> metadata, java.util.Set<hydra.core.Name> typeVariables, java.util.Set<hydra.core.Name> lambdaVariables, hydra.typing.InferenceContext inferenceContext) {
    java.util.Objects.requireNonNull((types));
    java.util.Objects.requireNonNull((metadata));
    java.util.Objects.requireNonNull((typeVariables));
    java.util.Objects.requireNonNull((lambdaVariables));
    java.util.Objects.requireNonNull((inferenceContext));
    this.types = types;
    this.metadata = metadata;
    this.typeVariables = typeVariables;
    this.lambdaVariables = lambdaVariables;
    this.inferenceContext = inferenceContext;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeContext)) {
      return false;
    }
    TypeContext o = (TypeContext) (other);
    return types.equals(o.types) && metadata.equals(o.metadata) && typeVariables.equals(o.typeVariables) && lambdaVariables.equals(o.lambdaVariables) && inferenceContext.equals(o.inferenceContext);
  }
  
  @Override
  public int hashCode() {
    return 2 * types.hashCode() + 3 * metadata.hashCode() + 5 * typeVariables.hashCode() + 7 * lambdaVariables.hashCode() + 11 * inferenceContext.hashCode();
  }
  
  public TypeContext withTypes(java.util.Map<hydra.core.Name, hydra.core.Type> types) {
    java.util.Objects.requireNonNull((types));
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, inferenceContext);
  }
  
  public TypeContext withMetadata(java.util.Map<hydra.core.Name, hydra.core.Term> metadata) {
    java.util.Objects.requireNonNull((metadata));
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, inferenceContext);
  }
  
  public TypeContext withTypeVariables(java.util.Set<hydra.core.Name> typeVariables) {
    java.util.Objects.requireNonNull((typeVariables));
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, inferenceContext);
  }
  
  public TypeContext withLambdaVariables(java.util.Set<hydra.core.Name> lambdaVariables) {
    java.util.Objects.requireNonNull((lambdaVariables));
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, inferenceContext);
  }
  
  public TypeContext withInferenceContext(hydra.typing.InferenceContext inferenceContext) {
    java.util.Objects.requireNonNull((inferenceContext));
    return new TypeContext(types, metadata, typeVariables, lambdaVariables, inferenceContext);
  }
}
