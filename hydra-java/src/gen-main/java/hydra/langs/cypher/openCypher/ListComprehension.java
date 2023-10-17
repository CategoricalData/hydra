package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ListComprehension implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ListComprehension");
  
  public final hydra.langs.cypher.openCypher.FilterExpression head;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> body;
  
  public ListComprehension (hydra.langs.cypher.openCypher.FilterExpression head, java.util.Optional<hydra.langs.cypher.openCypher.Expression> body) {
    this.head = head;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListComprehension)) {
      return false;
    }
    ListComprehension o = (ListComprehension) (other);
    return head.equals(o.head) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * body.hashCode();
  }
  
  public ListComprehension withHead(hydra.langs.cypher.openCypher.FilterExpression head) {
    return new ListComprehension(head, body);
  }
  
  public ListComprehension withBody(java.util.Optional<hydra.langs.cypher.openCypher.Expression> body) {
    return new ListComprehension(head, body);
  }
}