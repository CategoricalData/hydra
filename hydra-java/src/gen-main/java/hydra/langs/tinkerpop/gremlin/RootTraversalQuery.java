// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class RootTraversalQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.RootTraversalQuery");
  
  public final hydra.langs.tinkerpop.gremlin.RootTraversal root;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalTerminalMethod> terminalMethod;
  
  public RootTraversalQuery (hydra.langs.tinkerpop.gremlin.RootTraversal root, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalTerminalMethod> terminalMethod) {
    if (root == null) {
      throw new IllegalArgumentException("null value for 'root' argument");
    }
    if (terminalMethod == null) {
      throw new IllegalArgumentException("null value for 'terminalMethod' argument");
    }
    this.root = root;
    this.terminalMethod = terminalMethod;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RootTraversalQuery)) {
      return false;
    }
    RootTraversalQuery o = (RootTraversalQuery) (other);
    return root.equals(o.root) && terminalMethod.equals(o.terminalMethod);
  }
  
  @Override
  public int hashCode() {
    return 2 * root.hashCode() + 3 * terminalMethod.hashCode();
  }
  
  public RootTraversalQuery withRoot(hydra.langs.tinkerpop.gremlin.RootTraversal root) {
    if (root == null) {
      throw new IllegalArgumentException("null value for 'root' argument");
    }
    return new RootTraversalQuery(root, terminalMethod);
  }
  
  public RootTraversalQuery withTerminalMethod(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalTerminalMethod> terminalMethod) {
    if (terminalMethod == null) {
      throw new IllegalArgumentException("null value for 'terminalMethod' argument");
    }
    return new RootTraversalQuery(root, terminalMethod);
  }
}