// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TerminatedTraversal implements Serializable, Comparable<TerminatedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TerminatedTraversal");
  
  public static final hydra.core.Name ROOT = new hydra.core.Name("root");
  
  public static final hydra.core.Name TERMINAL = new hydra.core.Name("terminal");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal root;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod terminal;
  
  public TerminatedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal root, hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod terminal) {
    this.root = root;
    this.terminal = terminal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TerminatedTraversal)) {
      return false;
    }
    TerminatedTraversal o = (TerminatedTraversal) other;
    return java.util.Objects.equals(
      this.root,
      o.root) && java.util.Objects.equals(
      this.terminal,
      o.terminal);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(root) + 3 * java.util.Objects.hashCode(terminal);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TerminatedTraversal other) {
    int cmp = 0;
    cmp = ((Comparable) root).compareTo(other.root);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) terminal).compareTo(other.terminal);
  }
  
  public TerminatedTraversal withRoot(hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal root) {
    return new TerminatedTraversal(root, terminal);
  }
  
  public TerminatedTraversal withTerminal(hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod terminal) {
    return new TerminatedTraversal(root, terminal);
  }
}
