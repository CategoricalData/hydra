// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class NodeId implements Serializable, Comparable<NodeId> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.NodeId");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name PORT = new hydra.core.Name("port");

  public final hydra.graphviz.dot.Id id;

  public final hydra.util.Maybe<hydra.graphviz.dot.Port> port;

  public NodeId (hydra.graphviz.dot.Id id, hydra.util.Maybe<hydra.graphviz.dot.Port> port) {
    this.id = id;
    this.port = port;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeId)) {
      return false;
    }
    NodeId o = (NodeId) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.port,
      o.port);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(port);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeId other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      port,
      other.port);
  }

  public NodeId withId(hydra.graphviz.dot.Id id) {
    return new NodeId(id, port);
  }

  public NodeId withPort(hydra.util.Maybe<hydra.graphviz.dot.Port> port) {
    return new NodeId(id, port);
  }
}
