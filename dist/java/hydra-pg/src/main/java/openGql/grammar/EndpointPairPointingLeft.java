// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EndpointPairPointingLeft implements Serializable, Comparable<EndpointPairPointingLeft> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EndpointPairPointingLeft");

  public static final hydra.core.Name DESTINATION_ALIAS = new hydra.core.Name("destinationAlias");

  public static final hydra.core.Name SOURCE_ALIAS = new hydra.core.Name("sourceAlias");

  public final String destinationAlias;

  public final String sourceAlias;

  public EndpointPairPointingLeft (String destinationAlias, String sourceAlias) {
    this.destinationAlias = destinationAlias;
    this.sourceAlias = sourceAlias;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EndpointPairPointingLeft)) {
      return false;
    }
    EndpointPairPointingLeft o = (EndpointPairPointingLeft) other;
    return java.util.Objects.equals(
      this.destinationAlias,
      o.destinationAlias) && java.util.Objects.equals(
      this.sourceAlias,
      o.sourceAlias);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(destinationAlias) + 3 * java.util.Objects.hashCode(sourceAlias);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EndpointPairPointingLeft other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      destinationAlias,
      other.destinationAlias);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sourceAlias,
      other.sourceAlias);
  }

  public EndpointPairPointingLeft withDestinationAlias(String destinationAlias) {
    return new EndpointPairPointingLeft(destinationAlias, sourceAlias);
  }

  public EndpointPairPointingLeft withSourceAlias(String sourceAlias) {
    return new EndpointPairPointingLeft(destinationAlias, sourceAlias);
  }
}
