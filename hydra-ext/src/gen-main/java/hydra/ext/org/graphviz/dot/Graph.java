// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class Graph implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.Graph");
  
  public static final hydra.core.Name FIELD_NAME_STRICT = new hydra.core.Name("strict");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTED = new hydra.core.Name("directed");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public final Boolean strict;
  
  public final Boolean directed;
  
  public final hydra.util.Opt<hydra.ext.org.graphviz.dot.Id> id;
  
  public final java.util.List<hydra.ext.org.graphviz.dot.Stmt> statements;
  
  public Graph (Boolean strict, Boolean directed, hydra.util.Opt<hydra.ext.org.graphviz.dot.Id> id, java.util.List<hydra.ext.org.graphviz.dot.Stmt> statements) {
    java.util.Objects.requireNonNull((strict));
    java.util.Objects.requireNonNull((directed));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((statements));
    this.strict = strict;
    this.directed = directed;
    this.id = id;
    this.statements = statements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return strict.equals(o.strict) && directed.equals(o.directed) && id.equals(o.id) && statements.equals(o.statements);
  }
  
  @Override
  public int hashCode() {
    return 2 * strict.hashCode() + 3 * directed.hashCode() + 5 * id.hashCode() + 7 * statements.hashCode();
  }
  
  public Graph withStrict(Boolean strict) {
    java.util.Objects.requireNonNull((strict));
    return new Graph(strict, directed, id, statements);
  }
  
  public Graph withDirected(Boolean directed) {
    java.util.Objects.requireNonNull((directed));
    return new Graph(strict, directed, id, statements);
  }
  
  public Graph withId(hydra.util.Opt<hydra.ext.org.graphviz.dot.Id> id) {
    java.util.Objects.requireNonNull((id));
    return new Graph(strict, directed, id, statements);
  }
  
  public Graph withStatements(java.util.List<hydra.ext.org.graphviz.dot.Stmt> statements) {
    java.util.Objects.requireNonNull((statements));
    return new Graph(strict, directed, id, statements);
  }
}