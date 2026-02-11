// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * The context provided to type inference, including various typing environments.
 */
public class InferenceContext implements Serializable, Comparable<InferenceContext> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.InferenceContext");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA_TYPES = new hydra.core.Name("schemaTypes");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE_TYPES = new hydra.core.Name("primitiveTypes");
  
  public static final hydra.core.Name FIELD_NAME_DATA_TYPES = new hydra.core.Name("dataTypes");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_CONSTRAINTS = new hydra.core.Name("classConstraints");
  
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
   * A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.
   */
  public final java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints;
  
  /**
   * Whether to enable debug output during type inference
   */
  public final Boolean debug;
  
  public InferenceContext (java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> primitiveTypes, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> dataTypes, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints, Boolean debug) {
    this.schemaTypes = schemaTypes;
    this.primitiveTypes = primitiveTypes;
    this.dataTypes = dataTypes;
    this.classConstraints = classConstraints;
    this.debug = debug;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceContext)) {
      return false;
    }
    InferenceContext o = (InferenceContext) other;
    return java.util.Objects.equals(
      this.schemaTypes,
      o.schemaTypes) && java.util.Objects.equals(
      this.primitiveTypes,
      o.primitiveTypes) && java.util.Objects.equals(
      this.dataTypes,
      o.dataTypes) && java.util.Objects.equals(
      this.classConstraints,
      o.classConstraints) && java.util.Objects.equals(
      this.debug,
      o.debug);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(schemaTypes) + 3 * java.util.Objects.hashCode(primitiveTypes) + 5 * java.util.Objects.hashCode(dataTypes) + 7 * java.util.Objects.hashCode(classConstraints) + 11 * java.util.Objects.hashCode(debug);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InferenceContext other) {
    int cmp = 0;
    cmp = Integer.compare(
      schemaTypes.hashCode(),
      other.schemaTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      primitiveTypes.hashCode(),
      other.primitiveTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      dataTypes.hashCode(),
      other.dataTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      classConstraints.hashCode(),
      other.classConstraints.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) debug).compareTo(other.debug);
  }
  
  public InferenceContext withSchemaTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes) {
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, classConstraints, debug);
  }
  
  public InferenceContext withPrimitiveTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> primitiveTypes) {
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, classConstraints, debug);
  }
  
  public InferenceContext withDataTypes(java.util.Map<hydra.core.Name, hydra.core.TypeScheme> dataTypes) {
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, classConstraints, debug);
  }
  
  public InferenceContext withClassConstraints(java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> classConstraints) {
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, classConstraints, debug);
  }
  
  public InferenceContext withDebug(Boolean debug) {
    return new InferenceContext(schemaTypes, primitiveTypes, dataTypes, classConstraints, debug);
  }
}
