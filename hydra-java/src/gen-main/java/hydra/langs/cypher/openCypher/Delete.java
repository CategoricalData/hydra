package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Delete implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Delete");
  
  public final Boolean detach;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Expression> expressions;
  
  public Delete (Boolean detach, java.util.List<hydra.langs.cypher.openCypher.Expression> expressions) {
    this.detach = detach;
    this.expressions = expressions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Delete)) {
      return false;
    }
    Delete o = (Delete) (other);
    return detach.equals(o.detach) && expressions.equals(o.expressions);
  }
  
  @Override
  public int hashCode() {
    return 2 * detach.hashCode() + 3 * expressions.hashCode();
  }
  
  public Delete withDetach(Boolean detach) {
    return new Delete(detach, expressions);
  }
  
  public Delete withExpressions(java.util.List<hydra.langs.cypher.openCypher.Expression> expressions) {
    return new Delete(detach, expressions);
  }
}