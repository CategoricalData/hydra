// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class Subgraph implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.Subgraph");
  
  public static final hydra.core.Name FIELD_NAME_SUBGRAPH_ID = new hydra.core.Name("subgraphId");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public final hydra.util.Opt<hydra.ext.org.graphviz.dot.SubgraphId> subgraphId;
  
  public final java.util.List<hydra.ext.org.graphviz.dot.Stmt> statements;
  
  public Subgraph (hydra.util.Opt<hydra.ext.org.graphviz.dot.SubgraphId> subgraphId, java.util.List<hydra.ext.org.graphviz.dot.Stmt> statements) {
    java.util.Objects.requireNonNull((subgraphId));
    java.util.Objects.requireNonNull((statements));
    this.subgraphId = subgraphId;
    this.statements = statements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Subgraph)) {
      return false;
    }
    Subgraph o = (Subgraph) (other);
    return subgraphId.equals(o.subgraphId) && statements.equals(o.statements);
  }
  
  @Override
  public int hashCode() {
    return 2 * subgraphId.hashCode() + 3 * statements.hashCode();
  }
  
  public Subgraph withSubgraphId(hydra.util.Opt<hydra.ext.org.graphviz.dot.SubgraphId> subgraphId) {
    java.util.Objects.requireNonNull((subgraphId));
    return new Subgraph(subgraphId, statements);
  }
  
  public Subgraph withStatements(java.util.List<hydra.ext.org.graphviz.dot.Stmt> statements) {
    java.util.Objects.requireNonNull((statements));
    return new Subgraph(subgraphId, statements);
  }
}