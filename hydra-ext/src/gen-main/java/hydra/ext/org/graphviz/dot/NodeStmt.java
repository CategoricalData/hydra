// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class NodeStmt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.NodeStmt");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public final hydra.ext.org.graphviz.dot.NodeId id;
  
  public final hydra.util.Opt<hydra.ext.org.graphviz.dot.AttrList> attributes;
  
  public NodeStmt (hydra.ext.org.graphviz.dot.NodeId id, hydra.util.Opt<hydra.ext.org.graphviz.dot.AttrList> attributes) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((attributes));
    this.id = id;
    this.attributes = attributes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeStmt)) {
      return false;
    }
    NodeStmt o = (NodeStmt) (other);
    return id.equals(o.id) && attributes.equals(o.attributes);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * attributes.hashCode();
  }
  
  public NodeStmt withId(hydra.ext.org.graphviz.dot.NodeId id) {
    java.util.Objects.requireNonNull((id));
    return new NodeStmt(id, attributes);
  }
  
  public NodeStmt withAttributes(hydra.util.Opt<hydra.ext.org.graphviz.dot.AttrList> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new NodeStmt(id, attributes);
  }
}