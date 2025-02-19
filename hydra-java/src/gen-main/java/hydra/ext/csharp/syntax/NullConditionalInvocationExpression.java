// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NullConditionalInvocationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NullConditionalInvocationExpression");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.NullConditionalInvocationExpressionHead head;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments;
  
  public NullConditionalInvocationExpression (hydra.ext.csharp.syntax.NullConditionalInvocationExpressionHead head, hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments) {
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((arguments));
    this.head = head;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullConditionalInvocationExpression)) {
      return false;
    }
    NullConditionalInvocationExpression o = (NullConditionalInvocationExpression) (other);
    return head.equals(o.head) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * arguments.hashCode();
  }
  
  public NullConditionalInvocationExpression withHead(hydra.ext.csharp.syntax.NullConditionalInvocationExpressionHead head) {
    java.util.Objects.requireNonNull((head));
    return new NullConditionalInvocationExpression(head, arguments);
  }
  
  public NullConditionalInvocationExpression withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new NullConditionalInvocationExpression(head, arguments);
  }
}