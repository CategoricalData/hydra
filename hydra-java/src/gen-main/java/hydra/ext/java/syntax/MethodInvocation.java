// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodInvocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodInvocation");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.java.syntax.MethodInvocation_Header header;
  
  public final java.util.List<hydra.ext.java.syntax.Expression> arguments;
  
  public MethodInvocation (hydra.ext.java.syntax.MethodInvocation_Header header, java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((arguments));
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
    java.util.Objects.requireNonNull((header));
    return new MethodInvocation(header, arguments);
  }
  
  public MethodInvocation withArguments(java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new MethodInvocation(header, arguments);
  }
}