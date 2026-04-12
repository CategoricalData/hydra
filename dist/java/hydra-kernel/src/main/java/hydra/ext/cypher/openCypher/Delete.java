// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Delete implements Serializable, Comparable<Delete> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Delete");

  public static final hydra.core.Name DETACH = new hydra.core.Name("detach");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  public final Boolean detach;

  public final java.util.List<hydra.ext.cypher.openCypher.Expression> expressions;

  public Delete (Boolean detach, java.util.List<hydra.ext.cypher.openCypher.Expression> expressions) {
    this.detach = detach;
    this.expressions = expressions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Delete)) {
      return false;
    }
    Delete o = (Delete) other;
    return java.util.Objects.equals(
      this.detach,
      o.detach) && java.util.Objects.equals(
      this.expressions,
      o.expressions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(detach) + 3 * java.util.Objects.hashCode(expressions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Delete other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      detach,
      other.detach);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expressions,
      other.expressions);
  }

  public Delete withDetach(Boolean detach) {
    return new Delete(detach, expressions);
  }

  public Delete withExpressions(java.util.List<hydra.ext.cypher.openCypher.Expression> expressions) {
    return new Delete(detach, expressions);
  }
}
