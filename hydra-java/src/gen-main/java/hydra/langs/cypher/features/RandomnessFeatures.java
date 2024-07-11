// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for random value generation.
 */
public class RandomnessFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.RandomnessFeatures");
  
  /**
   * Whether to expect the rand() function.
   */
  public final Boolean rand;
  
  /**
   * Whether to expect the randomUUID() function.
   */
  public final Boolean randomUUID;
  
  public RandomnessFeatures (Boolean rand, Boolean randomUUID) {
    java.util.Objects.requireNonNull((rand));
    java.util.Objects.requireNonNull((randomUUID));
    this.rand = rand;
    this.randomUUID = randomUUID;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RandomnessFeatures)) {
      return false;
    }
    RandomnessFeatures o = (RandomnessFeatures) (other);
    return rand.equals(o.rand) && randomUUID.equals(o.randomUUID);
  }
  
  @Override
  public int hashCode() {
    return 2 * rand.hashCode() + 3 * randomUUID.hashCode();
  }
  
  public RandomnessFeatures withRand(Boolean rand) {
    java.util.Objects.requireNonNull((rand));
    return new RandomnessFeatures(rand, randomUUID);
  }
  
  public RandomnessFeatures withRandomUUID(Boolean randomUUID) {
    java.util.Objects.requireNonNull((randomUUID));
    return new RandomnessFeatures(rand, randomUUID);
  }
}