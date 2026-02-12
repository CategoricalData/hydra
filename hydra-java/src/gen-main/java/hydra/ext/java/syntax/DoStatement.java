// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class DoStatement implements Serializable, Comparable<DoStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.DoStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_CONDE = new hydra.core.Name("conde");
  
  public final hydra.ext.java.syntax.Statement body;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> conde;
  
  public DoStatement (hydra.ext.java.syntax.Statement body, hydra.util.Maybe<hydra.ext.java.syntax.Expression> conde) {
    this.body = body;
    this.conde = conde;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoStatement)) {
      return false;
    }
    DoStatement o = (DoStatement) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.conde,
      o.conde);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(conde);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DoStatement other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      conde.hashCode(),
      other.conde.hashCode());
  }
  
  public DoStatement withBody(hydra.ext.java.syntax.Statement body) {
    return new DoStatement(body, conde);
  }
  
  public DoStatement withConde(hydra.util.Maybe<hydra.ext.java.syntax.Expression> conde) {
    return new DoStatement(body, conde);
  }
}
