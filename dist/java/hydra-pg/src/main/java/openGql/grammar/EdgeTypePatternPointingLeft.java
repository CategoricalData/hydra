// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeTypePatternPointingLeft implements Serializable, Comparable<EdgeTypePatternPointingLeft> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePatternPointingLeft");

  public static final hydra.core.Name DESTINATION = new hydra.core.Name("destination");

  public static final hydra.core.Name ARC = new hydra.core.Name("arc");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final openGql.grammar.DestinationNodeTypeReference destination;

  public final openGql.grammar.EdgeTypeFiller arc;

  public final openGql.grammar.SourceNodeTypeReference source;

  public EdgeTypePatternPointingLeft (openGql.grammar.DestinationNodeTypeReference destination, openGql.grammar.EdgeTypeFiller arc, openGql.grammar.SourceNodeTypeReference source) {
    this.destination = destination;
    this.arc = arc;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeTypePatternPointingLeft)) {
      return false;
    }
    EdgeTypePatternPointingLeft o = (EdgeTypePatternPointingLeft) other;
    return java.util.Objects.equals(
      this.destination,
      o.destination) && java.util.Objects.equals(
      this.arc,
      o.arc) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(destination) + 3 * java.util.Objects.hashCode(arc) + 5 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeTypePatternPointingLeft other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      destination,
      other.destination);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      arc,
      other.arc);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public EdgeTypePatternPointingLeft withDestination(openGql.grammar.DestinationNodeTypeReference destination) {
    return new EdgeTypePatternPointingLeft(destination, arc, source);
  }

  public EdgeTypePatternPointingLeft withArc(openGql.grammar.EdgeTypeFiller arc) {
    return new EdgeTypePatternPointingLeft(destination, arc, source);
  }

  public EdgeTypePatternPointingLeft withSource(openGql.grammar.SourceNodeTypeReference source) {
    return new EdgeTypePatternPointingLeft(destination, arc, source);
  }
}
