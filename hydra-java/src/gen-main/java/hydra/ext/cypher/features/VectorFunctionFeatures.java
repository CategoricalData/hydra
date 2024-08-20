// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Vector functions
 */
public class VectorFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.VectorFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_VECTOR_SIMILARITY_COSINE = new hydra.core.Name("vector.similarity.cosine");
  
  public static final hydra.core.Name FIELD_NAME_VECTOR_SIMILARITY_EUCLIDEAN = new hydra.core.Name("vector.similarity.euclidean");
  
  /**
   * The vector.similarity.cosine() function. Returns a FLOAT representing the similarity between the argument vectors based on their cosine.
   */
  public final Boolean vector_similarity_cosine;
  
  /**
   * The vector.similarity.euclidean() function. Returns a FLOAT representing the similarity between the argument vectors based on their Euclidean distance.
   */
  public final Boolean vector_similarity_euclidean;
  
  public VectorFunctionFeatures (Boolean vector_similarity_cosine, Boolean vector_similarity_euclidean) {
    java.util.Objects.requireNonNull((vector_similarity_cosine));
    java.util.Objects.requireNonNull((vector_similarity_euclidean));
    this.vector_similarity_cosine = vector_similarity_cosine;
    this.vector_similarity_euclidean = vector_similarity_euclidean;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VectorFunctionFeatures)) {
      return false;
    }
    VectorFunctionFeatures o = (VectorFunctionFeatures) (other);
    return vector_similarity_cosine.equals(o.vector_similarity_cosine) && vector_similarity_euclidean.equals(o.vector_similarity_euclidean);
  }
  
  @Override
  public int hashCode() {
    return 2 * vector_similarity_cosine.hashCode() + 3 * vector_similarity_euclidean.hashCode();
  }
  
  public VectorFunctionFeatures withVector_similarity_cosine(Boolean vector_similarity_cosine) {
    java.util.Objects.requireNonNull((vector_similarity_cosine));
    return new VectorFunctionFeatures(vector_similarity_cosine, vector_similarity_euclidean);
  }
  
  public VectorFunctionFeatures withVector_similarity_euclidean(Boolean vector_similarity_euclidean) {
    java.util.Objects.requireNonNull((vector_similarity_euclidean));
    return new VectorFunctionFeatures(vector_similarity_cosine, vector_similarity_euclidean);
  }
}