// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * GenAI functions
 */
public class GenAIFunctionFeatures implements Serializable, Comparable<GenAIFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.GenAIFunctionFeatures");

  public static final hydra.core.Name GENAI_VECTOR_ENCODE = new hydra.core.Name("genai.vector.encode");

  /**
   * The genai.vector.encode() function. Encode a given resource as a vector using the named provider. Introduced in 5.17.
   */
  public final Boolean genai_vector_encode;

  public GenAIFunctionFeatures (Boolean genai_vector_encode) {
    this.genai_vector_encode = genai_vector_encode;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenAIFunctionFeatures)) {
      return false;
    }
    GenAIFunctionFeatures o = (GenAIFunctionFeatures) other;
    return java.util.Objects.equals(
      this.genai_vector_encode,
      o.genai_vector_encode);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(genai_vector_encode);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GenAIFunctionFeatures other) {
    return ((Comparable) genai_vector_encode).compareTo(other.genai_vector_encode);
  }
}
