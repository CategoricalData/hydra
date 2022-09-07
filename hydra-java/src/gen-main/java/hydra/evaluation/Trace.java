package hydra.evaluation;

/**
 * A container for logging and error information
 */
public class Trace {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/evaluation.Trace");
  
  public final java.util.List<String> stack;
  
  public final java.util.List<java.util.List<String>> messages;
  
  public Trace (java.util.List<String> stack, java.util.List<java.util.List<String>> messages) {
    this.stack = stack;
    this.messages = messages;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Trace)) {
      return false;
    }
    Trace o = (Trace) (other);
    return stack.equals(o.stack) && messages.equals(o.messages);
  }
  
  @Override
  public int hashCode() {
    return 2 * stack.hashCode() + 3 * messages.hashCode();
  }
  
  public Trace withStack(java.util.List<String> stack) {
    return new Trace(stack, messages);
  }
  
  public Trace withMessages(java.util.List<java.util.List<String>> messages) {
    return new Trace(stack, messages);
  }
}