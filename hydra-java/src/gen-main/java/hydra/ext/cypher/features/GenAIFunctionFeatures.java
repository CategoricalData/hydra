// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * GenAI functions
 */
public class GenAIFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.GenAIFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_GENAI_VECTOR_ENCODE = new hydra.core.Name("genai.vector.encode");
  
  /**
   * The genai.vector.encode() function. Encode a given resource as a vector using the named provider. Introduced in 5.17.
   */
  public final Boolean genai_vector_encode;
  
  public GenAIFunctionFeatures (Boolean genai_vector_encode) {
    java.util.Objects.requireNonNull((genai_vector_encode));
    this.genai_vector_encode = genai_vector_encode;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenAIFunctionFeatures)) {
      return false;
    }
    GenAIFunctionFeatures o = (GenAIFunctionFeatures) (other);
    return genai_vector_encode.equals(o.genai_vector_encode);
  }
  
  @Override
  public int hashCode() {
    return 2 * genai_vector_encode.hashCode();
  }
}
