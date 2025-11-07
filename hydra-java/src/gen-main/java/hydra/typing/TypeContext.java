// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A typing environment used for type reconstruction (typeOf) over System F terms
 */
public class TypeContext implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.TypeContext");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLES = new hydra.core.Name("variables");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE_CONTEXT = new hydra.core.Name("inferenceContext");
  
  /**
   * A mapping of lambda- and let-bound variables to their types
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Type> types;
  
  /**
   * The set of type variables introduced by enclosing type lambdas
   */
  public final java.util.Set<hydra.core.Name> variables;
  
  /**
   * The schema types, primitive types, and data types of the graph
   */
  public final hydra.typing.InferenceContext inferenceContext;
  
  public TypeContext (java.util.Map<hydra.core.Name, hydra.core.Type> types, java.util.Set<hydra.core.Name> variables, hydra.typing.InferenceContext inferenceContext) {
    java.util.Objects.requireNonNull((types));
    java.util.Objects.requireNonNull((variables));
    java.util.Objects.requireNonNull((inferenceContext));
    this.types = types;
    this.variables = variables;
    this.inferenceContext = inferenceContext;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeContext)) {
      return false;
    }
    TypeContext o = (TypeContext) (other);
    return types.equals(o.types) && variables.equals(o.variables) && inferenceContext.equals(o.inferenceContext);
  }
  
  @Override
  public int hashCode() {
    return 2 * types.hashCode() + 3 * variables.hashCode() + 5 * inferenceContext.hashCode();
  }
  
  public TypeContext withTypes(java.util.Map<hydra.core.Name, hydra.core.Type> types) {
    java.util.Objects.requireNonNull((types));
    return new TypeContext(types, variables, inferenceContext);
  }
  
  public TypeContext withVariables(java.util.Set<hydra.core.Name> variables) {
    java.util.Objects.requireNonNull((variables));
    return new TypeContext(types, variables, inferenceContext);
  }
  
  public TypeContext withInferenceContext(hydra.typing.InferenceContext inferenceContext) {
    java.util.Objects.requireNonNull((inferenceContext));
    return new TypeContext(types, variables, inferenceContext);
  }
}
