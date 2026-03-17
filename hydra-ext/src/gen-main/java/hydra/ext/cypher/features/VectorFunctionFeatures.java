// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Vector functions
 */
public class VectorFunctionFeatures implements Serializable, Comparable<VectorFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.VectorFunctionFeatures");

  public static final hydra.core.Name VECTOR_SIMILARITY_COSINE = new hydra.core.Name("vector.similarity.cosine");

  public static final hydra.core.Name VECTOR_SIMILARITY_EUCLIDEAN = new hydra.core.Name("vector.similarity.euclidean");

  /**
   * The vector.similarity.cosine() function. Returns a FLOAT representing the similarity between the argument vectors based on their cosine.
   */
  public final Boolean vector_similarity_cosine;

  /**
   * The vector.similarity.euclidean() function. Returns a FLOAT representing the similarity between the argument vectors based on their Euclidean distance.
   */
  public final Boolean vector_similarity_euclidean;

  public VectorFunctionFeatures (Boolean vector_similarity_cosine, Boolean vector_similarity_euclidean) {
    this.vector_similarity_cosine = vector_similarity_cosine;
    this.vector_similarity_euclidean = vector_similarity_euclidean;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VectorFunctionFeatures)) {
      return false;
    }
    VectorFunctionFeatures o = (VectorFunctionFeatures) other;
    return java.util.Objects.equals(
      this.vector_similarity_cosine,
      o.vector_similarity_cosine) && java.util.Objects.equals(
      this.vector_similarity_euclidean,
      o.vector_similarity_euclidean);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(vector_similarity_cosine) + 3 * java.util.Objects.hashCode(vector_similarity_euclidean);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VectorFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) vector_similarity_cosine).compareTo(other.vector_similarity_cosine);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) vector_similarity_euclidean).compareTo(other.vector_similarity_euclidean);
  }

  public VectorFunctionFeatures withVector_similarity_cosine(Boolean vector_similarity_cosine) {
    return new VectorFunctionFeatures(vector_similarity_cosine, vector_similarity_euclidean);
  }

  public VectorFunctionFeatures withVector_similarity_euclidean(Boolean vector_similarity_euclidean) {
    return new VectorFunctionFeatures(vector_similarity_cosine, vector_similarity_euclidean);
  }
}
