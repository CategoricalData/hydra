// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * The context provided to type inference, including various typing enviroments.
 */
public class InferenceContext implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.InferenceContext");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA_TYPES = new hydra.core.Name("schemaTypes");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE_TYPES = new hydra.core.Name("primitiveTypes");
  
  public static final hydra.core.Name FIELD_NAME_DATA_TYPES = new hydra.core.Name("dataTypes");
  
  public static final hydra.core.Name FIELD_NAME_DEBUG = new hydra.core.Name("debug");
  
  /**
   * A fixed typing environment which is derived from the schema of the graph.
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes;
  
  /**
   * A fixed typing environment which is derived from the set of primitives in the graph.
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeScheme> primitiveTypes;
  
  /**
   * A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions.
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeScheme> dataTypes;
  
  /**
   * Whether to enable debug output during type inference
   */
  public final Boolean debug;
  
  public InferenceContext (java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> primitiveTypes, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> dataTypes, Boolean debug) {
    java.util.Objects.requireNonNull((schemaTypes));
    java.util.Objects.requireNonNull((primitiveTypes));
    java.util.Objects.requireNonNull((dataTypes));
    java.util.Objects.requireNonNull((debug));
    this.schemaTypes = schemaTypes;
    this.primitiveTypes = primitiveTypes;
    this.dataTypes = dataTypes;
    this.debug = debug;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceContext)) {
      return false;
    }
    InferenceContext o = (InferenceContext) (other);
    return schemaTypes.equals(o.schemaTypes) && primitiveTypes.equals(o.primitiveTypes) && dataTypes.equals(o.dataTypes) && debug.equals(o.debug);
  }
  
  @Override
  public int hashCode() {
    return 2 * schemaTypes.hashCode() + 3 * primitiveTypes.hashCode() + 5 * dataTypes.hashCode() + 7 * debug.hashCode();
  }
  
  public InferenceContext withSchemaTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes) {
    java.util.Objects.requireNonNull((schemaTypes));
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, debug);
  }
  
  public InferenceContext withPrimitiveTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> primitiveTypes) {
    java.util.Objects.requireNonNull((primitiveTypes));
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, debug);
  }
  
  public InferenceContext withDataTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> dataTypes) {
    java.util.Objects.requireNonNull((dataTypes));
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, debug);
  }
  
  public InferenceContext withDebug(Boolean debug) {
    java.util.Objects.requireNonNull((debug));
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, debug);
  }
}
