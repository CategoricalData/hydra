// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Vector functions
 */
public class VectorFunctionFeatures implements Serializable, Comparable<VectorFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.VectorFunctionFeatures");

  public static final hydra.core.Name VECTOR_SIMILARITY_COSINE = new hydra.core.Name("vectorSimilarityCosine");

  public static final hydra.core.Name VECTOR_SIMILARITY_EUCLIDEAN = new hydra.core.Name("vectorSimilarityEuclidean");

  /**
   * The vector.similarity.cosine() function. Returns a FLOAT representing the similarity between the argument vectors based on their cosine.
   */
  public final Boolean vectorSimilarityCosine;

  /**
   * The vector.similarity.euclidean() function. Returns a FLOAT representing the similarity between the argument vectors based on their Euclidean distance.
   */
  public final Boolean vectorSimilarityEuclidean;

  public VectorFunctionFeatures (Boolean vectorSimilarityCosine, Boolean vectorSimilarityEuclidean) {
    this.vectorSimilarityCosine = vectorSimilarityCosine;
    this.vectorSimilarityEuclidean = vectorSimilarityEuclidean;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VectorFunctionFeatures)) {
      return false;
    }
    VectorFunctionFeatures o = (VectorFunctionFeatures) other;
    return java.util.Objects.equals(
      this.vectorSimilarityCosine,
      o.vectorSimilarityCosine) && java.util.Objects.equals(
      this.vectorSimilarityEuclidean,
      o.vectorSimilarityEuclidean);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(vectorSimilarityCosine) + 3 * java.util.Objects.hashCode(vectorSimilarityEuclidean);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VectorFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) vectorSimilarityCosine).compareTo(other.vectorSimilarityCosine);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) vectorSimilarityEuclidean).compareTo(other.vectorSimilarityEuclidean);
  }

  public VectorFunctionFeatures withVectorSimilarityCosine(Boolean vectorSimilarityCosine) {
    return new VectorFunctionFeatures(vectorSimilarityCosine, vectorSimilarityEuclidean);
  }

  public VectorFunctionFeatures withVectorSimilarityEuclidean(Boolean vectorSimilarityEuclidean) {
    return new VectorFunctionFeatures(vectorSimilarityCosine, vectorSimilarityEuclidean);
  }
}
