// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class RootTraversalQuery implements Serializable, Comparable<RootTraversalQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RootTraversalQuery");

  public static final hydra.core.Name ROOT = new hydra.core.Name("root");

  public static final hydra.core.Name TERMINAL_METHOD = new hydra.core.Name("terminalMethod");

  public final hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal root;

  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod> terminalMethod;

  public RootTraversalQuery (hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal root, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod> terminalMethod) {
    this.root = root;
    this.terminalMethod = terminalMethod;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RootTraversalQuery)) {
      return false;
    }
    RootTraversalQuery o = (RootTraversalQuery) other;
    return java.util.Objects.equals(
      this.root,
      o.root) && java.util.Objects.equals(
      this.terminalMethod,
      o.terminalMethod);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(root) + 3 * java.util.Objects.hashCode(terminalMethod);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RootTraversalQuery other) {
    int cmp = 0;
    cmp = ((Comparable) root).compareTo(other.root);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) terminalMethod).compareTo(other.terminalMethod);
  }

  public RootTraversalQuery withRoot(hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal root) {
    return new RootTraversalQuery(root, terminalMethod);
  }

  public RootTraversalQuery withTerminalMethod(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod> terminalMethod) {
    return new RootTraversalQuery(root, terminalMethod);
  }
}
