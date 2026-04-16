// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EndpointPairUndirected implements Serializable, Comparable<EndpointPairUndirected> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EndpointPairUndirected");

  public static final hydra.core.Name SOURCE_ALIAS = new hydra.core.Name("sourceAlias");

  public static final hydra.core.Name CONNECTOR = new hydra.core.Name("connector");

  public static final hydra.core.Name DESTINATION_ALIAS = new hydra.core.Name("destinationAlias");

  public final String sourceAlias;

  public final openGql.grammar.ConnectorUndirected connector;

  public final String destinationAlias;

  public EndpointPairUndirected (String sourceAlias, openGql.grammar.ConnectorUndirected connector, String destinationAlias) {
    this.sourceAlias = sourceAlias;
    this.connector = connector;
    this.destinationAlias = destinationAlias;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EndpointPairUndirected)) {
      return false;
    }
    EndpointPairUndirected o = (EndpointPairUndirected) other;
    return java.util.Objects.equals(
      this.sourceAlias,
      o.sourceAlias) && java.util.Objects.equals(
      this.connector,
      o.connector) && java.util.Objects.equals(
      this.destinationAlias,
      o.destinationAlias);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(sourceAlias) + 3 * java.util.Objects.hashCode(connector) + 5 * java.util.Objects.hashCode(destinationAlias);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EndpointPairUndirected other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      sourceAlias,
      other.sourceAlias);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      connector,
      other.connector);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      destinationAlias,
      other.destinationAlias);
  }

  public EndpointPairUndirected withSourceAlias(String sourceAlias) {
    return new EndpointPairUndirected(sourceAlias, connector, destinationAlias);
  }

  public EndpointPairUndirected withConnector(openGql.grammar.ConnectorUndirected connector) {
    return new EndpointPairUndirected(sourceAlias, connector, destinationAlias);
  }

  public EndpointPairUndirected withDestinationAlias(String destinationAlias) {
    return new EndpointPairUndirected(sourceAlias, connector, destinationAlias);
  }
}
