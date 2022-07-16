package hydra.ext.java.syntax;

public class MethodInvocation {
  public final MethodInvocation_Header header;
  
  public final java.util.List<Expression> arguments;
  
  public MethodInvocation (MethodInvocation_Header header, java.util.List<Expression> arguments) {
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
  
  public MethodInvocation withHeader(MethodInvocation_Header header) {
    return new MethodInvocation(header, arguments);
  }
  
  public MethodInvocation withArguments(java.util.List<Expression> arguments) {
    return new MethodInvocation(header, arguments);
  }
}