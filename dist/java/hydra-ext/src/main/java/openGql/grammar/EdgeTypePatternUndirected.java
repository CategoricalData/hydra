// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeTypePatternUndirected implements Serializable, Comparable<EdgeTypePatternUndirected> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePatternUndirected");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name ARC = new hydra.core.Name("arc");

  public static final hydra.core.Name DESTINATION = new hydra.core.Name("destination");

  public final openGql.grammar.SourceNodeTypeReference source;

  public final openGql.grammar.EdgeTypeFiller arc;

  public final openGql.grammar.DestinationNodeTypeReference destination;

  public EdgeTypePatternUndirected (openGql.grammar.SourceNodeTypeReference source, openGql.grammar.EdgeTypeFiller arc, openGql.grammar.DestinationNodeTypeReference destination) {
    this.source = source;
    this.arc = arc;
    this.destination = destination;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeTypePatternUndirected)) {
      return false;
    }
    EdgeTypePatternUndirected o = (EdgeTypePatternUndirected) other;
    return java.util.Objects.equals(
      this.source,
      o.source) && java.util.Objects.equals(
      this.arc,
      o.arc) && java.util.Objects.equals(
      this.destination,
      o.destination);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(source) + 3 * java.util.Objects.hashCode(arc) + 5 * java.util.Objects.hashCode(destination);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeTypePatternUndirected other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      source,
      other.source);
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
      destination,
      other.destination);
  }

  public EdgeTypePatternUndirected withSource(openGql.grammar.SourceNodeTypeReference source) {
    return new EdgeTypePatternUndirected(source, arc, destination);
  }

  public EdgeTypePatternUndirected withArc(openGql.grammar.EdgeTypeFiller arc) {
    return new EdgeTypePatternUndirected(source, arc, destination);
  }

  public EdgeTypePatternUndirected withDestination(openGql.grammar.DestinationNodeTypeReference destination) {
    return new EdgeTypePatternUndirected(source, arc, destination);
  }
}
