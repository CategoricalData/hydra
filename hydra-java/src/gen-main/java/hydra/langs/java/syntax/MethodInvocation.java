// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class MethodInvocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.MethodInvocation");
  
  public final hydra.langs.java.syntax.MethodInvocation_Header header;
  
  public final java.util.List<hydra.langs.java.syntax.Expression> arguments;
  
  public MethodInvocation (hydra.langs.java.syntax.MethodInvocation_Header header, java.util.List<hydra.langs.java.syntax.Expression> arguments) {
    if (header == null) {
      throw new IllegalArgumentException("null value for 'header' argument");
    }
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
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
  
  public MethodInvocation withHeader(hydra.langs.java.syntax.MethodInvocation_Header header) {
    if (header == null) {
      throw new IllegalArgumentException("null value for 'header' argument");
    }
    return new MethodInvocation(header, arguments);
  }
  
  public MethodInvocation withArguments(java.util.List<hydra.langs.java.syntax.Expression> arguments) {
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    return new MethodInvocation(header, arguments);
  }
}