package hydra.ext.java.syntax;

public class MethodInvocation {
  public final hydra.ext.java.syntax.MethodInvocation_Header header;
  
  public final java.util.List<hydra.ext.java.syntax.Expression> arguments;
  
  public MethodInvocation (hydra.ext.java.syntax.MethodInvocation_Header header, java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    this.header = header;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodInvocation)) {
      return false;
    }
    MethodInvocation o = (MethodInvocation) (other);
    return header.equals(o.header) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * header.hashCode() + 3 * arguments.hashCode();
  }
  
  public MethodInvocation withHeader(hydra.ext.java.syntax.MethodInvocation_Header header) {
    return new MethodInvocation(header, arguments);
  }
  
  public MethodInvocation withArguments(java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    return new MethodInvocation(header, arguments);
  }
}