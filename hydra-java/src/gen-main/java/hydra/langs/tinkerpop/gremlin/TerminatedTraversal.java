// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TerminatedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TerminatedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_ROOT = new hydra.core.Name("root");
  
  public static final hydra.core.Name FIELD_NAME_TERMINAL = new hydra.core.Name("terminal");
  
  public final hydra.langs.tinkerpop.gremlin.RootTraversal root;
  
  public final hydra.langs.tinkerpop.gremlin.TraversalTerminalMethod terminal;
  
  public TerminatedTraversal (hydra.langs.tinkerpop.gremlin.RootTraversal root, hydra.langs.tinkerpop.gremlin.TraversalTerminalMethod terminal) {
    java.util.Objects.requireNonNull((root));
    java.util.Objects.requireNonNull((terminal));
    this.root = root;
    this.terminal = terminal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TerminatedTraversal)) {
      return false;
    }
    TerminatedTraversal o = (TerminatedTraversal) (other);
    return root.equals(o.root) && terminal.equals(o.terminal);
  }
  
  @Override
  public int hashCode() {
    return 2 * root.hashCode() + 3 * terminal.hashCode();
  }
  
  public TerminatedTraversal withRoot(hydra.langs.tinkerpop.gremlin.RootTraversal root) {
    java.util.Objects.requireNonNull((root));
    return new TerminatedTraversal(root, terminal);
  }
  
  public TerminatedTraversal withTerminal(hydra.langs.tinkerpop.gremlin.TraversalTerminalMethod terminal) {
    java.util.Objects.requireNonNull((terminal));
    return new TerminatedTraversal(root, terminal);
  }
}