// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * An Inductive or CoInductive definition with one or more mutually inductive bodies
 */
public class InductiveDefinition implements Serializable, Comparable<InductiveDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.InductiveDefinition");

  public static final hydra.core.Name LOCALITY = new hydra.core.Name("locality");

  public static final hydra.core.Name COINDUCTIVE = new hydra.core.Name("coinductive");

  public static final hydra.core.Name BODIES = new hydra.core.Name("bodies");

  public final hydra.util.Maybe<hydra.coq.syntax.Locality> locality;

  public final Boolean coinductive;

  public final java.util.List<hydra.coq.syntax.InductiveBody> bodies;

  public InductiveDefinition (hydra.util.Maybe<hydra.coq.syntax.Locality> locality, Boolean coinductive, java.util.List<hydra.coq.syntax.InductiveBody> bodies) {
    this.locality = locality;
    this.coinductive = coinductive;
    this.bodies = bodies;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InductiveDefinition)) {
      return false;
    }
    InductiveDefinition o = (InductiveDefinition) other;
    return java.util.Objects.equals(
      this.locality,
      o.locality) && java.util.Objects.equals(
      this.coinductive,
      o.coinductive) && java.util.Objects.equals(
      this.bodies,
      o.bodies);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(locality) + 3 * java.util.Objects.hashCode(coinductive) + 5 * java.util.Objects.hashCode(bodies);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InductiveDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      locality,
      other.locality);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      coinductive,
      other.coinductive);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      bodies,
      other.bodies);
  }

  public InductiveDefinition withLocality(hydra.util.Maybe<hydra.coq.syntax.Locality> locality) {
    return new InductiveDefinition(locality, coinductive, bodies);
  }

  public InductiveDefinition withCoinductive(Boolean coinductive) {
    return new InductiveDefinition(locality, coinductive, bodies);
  }

  public InductiveDefinition withBodies(java.util.List<hydra.coq.syntax.InductiveBody> bodies) {
    return new InductiveDefinition(locality, coinductive, bodies);
  }
}
