// Note: this is an automatically generated file. Do not edit.

package hydra.context;

import java.io.Serializable;

/**
 * An execution context for tracing and diagnostics, threaded through function calls
 */
public class Context implements Serializable, Comparable<Context> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.context.Context");
  
  public static final hydra.core.Name TRACE = new hydra.core.Name("trace");
  
  public static final hydra.core.Name MESSAGES = new hydra.core.Name("messages");
  
  public static final hydra.core.Name OTHER = new hydra.core.Name("other");
  
  /**
   * A stack of context labels describing the current execution path
   */
  public final java.util.List<String> trace;
  
  /**
   * A log of warnings and/or info messages
   */
  public final java.util.List<String> messages;
  
  /**
   * A map of string keys to arbitrary terms as values, for application-specific use
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Term> other;
  
  public Context (java.util.List<String> trace, java.util.List<String> messages, java.util.Map<hydra.core.Name, hydra.core.Term> other) {
    this.trace = trace;
    this.messages = messages;
    this.other = other;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Context)) {
      return false;
    }
    Context o = (Context) other;
    return java.util.Objects.equals(
      this.trace,
      o.trace) && java.util.Objects.equals(
      this.messages,
      o.messages) && java.util.Objects.equals(
      this.other,
      o.other);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(trace) + 3 * java.util.Objects.hashCode(messages) + 5 * java.util.Objects.hashCode(other);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Context other) {
    int cmp = 0;
    cmp = Integer.compare(
      trace.hashCode(),
      other.trace.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      messages.hashCode(),
      other.messages.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      other.hashCode(),
      other.other.hashCode());
  }
  
  public Context withTrace(java.util.List<String> trace) {
    return new Context(trace, messages, other);
  }
  
  public Context withMessages(java.util.List<String> messages) {
    return new Context(trace, messages, other);
  }
  
  public Context withOther(java.util.Map<hydra.core.Name, hydra.core.Term> other) {
    return new Context(trace, messages, other);
  }
}
