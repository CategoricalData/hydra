// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * GenAI functions
 */
public class GenAIFunctionFeatures implements Serializable, Comparable<GenAIFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.GenAIFunctionFeatures");

  public static final hydra.core.Name GENAI_VECTOR_ENCODE = new hydra.core.Name("genaiVectorEncode");

  /**
   * The genai.vector.encode() function. Encode a given resource as a vector using the named provider. Introduced in 5.17.
   */
  public final Boolean genaiVectorEncode;

  public GenAIFunctionFeatures (Boolean genaiVectorEncode) {
    this.genaiVectorEncode = genaiVectorEncode;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenAIFunctionFeatures)) {
      return false;
    }
    GenAIFunctionFeatures o = (GenAIFunctionFeatures) other;
    return java.util.Objects.equals(
      this.genaiVectorEncode,
      o.genaiVectorEncode);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(genaiVectorEncode);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GenAIFunctionFeatures other) {
    return hydra.util.Comparing.compare(
      genaiVectorEncode,
      other.genaiVectorEncode);
  }
}
