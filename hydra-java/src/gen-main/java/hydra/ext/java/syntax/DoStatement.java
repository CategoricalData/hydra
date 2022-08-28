package hydra.ext.java.syntax;

public class DoStatement {
  public final hydra.ext.java.syntax.Statement body;
  
  public final java.util.Optional<hydra.ext.java.syntax.Expression> conde;
  
  public DoStatement (hydra.ext.java.syntax.Statement body, java.util.Optional<hydra.ext.java.syntax.Expression> conde) {
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
    return new DoStatement(body, conde);
  }
  
  public DoStatement withConde(java.util.Optional<hydra.ext.java.syntax.Expression> conde) {
    return new DoStatement(body, conde);
  }
}