// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class MethodInvocation implements Serializable, Comparable<MethodInvocation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.MethodInvocation");

  public static final hydra.core.Name HEADER = new hydra.core.Name("header");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public final hydra.java.syntax.MethodInvocation_Header header;

  public final java.util.List<hydra.java.syntax.Expression> arguments;

  public MethodInvocation (hydra.java.syntax.MethodInvocation_Header header, java.util.List<hydra.java.syntax.Expression> arguments) {
    this.header = header;
    this.arguments = arguments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodInvocation)) {
      return false;
    }
    MethodInvocation o = (MethodInvocation) other;
    return java.util.Objects.equals(
      this.header,
      o.header) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(header) + 3 * java.util.Objects.hashCode(arguments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodInvocation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      header,
      other.header);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      arguments,
      other.arguments);
  }

  public MethodInvocation withHeader(hydra.java.syntax.MethodInvocation_Header header) {
    return new MethodInvocation(header, arguments);
  }

  public MethodInvocation withArguments(java.util.List<hydra.java.syntax.Expression> arguments) {
    return new MethodInvocation(header, arguments);
  }
}
