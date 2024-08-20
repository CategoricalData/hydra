// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class NodeId implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.NodeId");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_PORT = new hydra.core.Name("port");
  
  public final hydra.ext.org.graphviz.dot.Id id;
  
  public final hydra.util.Opt<hydra.ext.org.graphviz.dot.Port> port;
  
  public NodeId (hydra.ext.org.graphviz.dot.Id id, hydra.util.Opt<hydra.ext.org.graphviz.dot.Port> port) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((port));
    this.id = id;
    this.port = port;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeId)) {
      return false;
    }
    NodeId o = (NodeId) (other);
    return id.equals(o.id) && port.equals(o.port);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * port.hashCode();
  }
  
  public NodeId withId(hydra.ext.org.graphviz.dot.Id id) {
    java.util.Objects.requireNonNull((id));
    return new NodeId(id, port);
  }
  
  public NodeId withPort(hydra.util.Opt<hydra.ext.org.graphviz.dot.Port> port) {
    java.util.Objects.requireNonNull((port));
    return new NodeId(id, port);
  }
}