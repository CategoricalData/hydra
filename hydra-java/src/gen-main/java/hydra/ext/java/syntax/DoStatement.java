// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class DoStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.DoStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_CONDE = new hydra.core.Name("conde");
  
  public final hydra.ext.java.syntax.Statement body;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Expression> conde;
  
  public DoStatement (hydra.ext.java.syntax.Statement body, hydra.util.Opt<hydra.ext.java.syntax.Expression> conde) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((conde));
    this.body = body;
    this.conde = conde;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoStatement)) {
      return false;
    }
    DoStatement o = (DoStatement) (other);
    return body.equals(o.body) && conde.equals(o.conde);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * conde.hashCode();
  }
  
  public DoStatement withBody(hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((body));
    return new DoStatement(body, conde);
  }
  
  public DoStatement withConde(hydra.util.Opt<hydra.ext.java.syntax.Expression> conde) {
    java.util.Objects.requireNonNull((conde));
    return new DoStatement(body, conde);
  }
}