// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * A container for logging and error information
 */
public class Trace implements Serializable, Comparable<Trace> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.compute.Trace");
  
  public static final hydra.core.Name FIELD_NAME_STACK = new hydra.core.Name("stack");
  
  public static final hydra.core.Name FIELD_NAME_MESSAGES = new hydra.core.Name("messages");
  
  public static final hydra.core.Name FIELD_NAME_OTHER = new hydra.core.Name("other");
  
  /**
   * A stack of context labels
   */
  public final java.util.List<String> stack;
  
  /**
   * A log of warnings and/or info messages
   */
  public final java.util.List<String> messages;
  
  /**
   * A map of string keys to arbitrary terms as values, for application-specific use
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Term> other;
  
  public Trace (java.util.List<String> stack, java.util.List<String> messages, java.util.Map<hydra.core.Name, hydra.core.Term> other) {
    this.stack = stack;
    this.messages = messages;
    this.other = other;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Trace)) {
      return false;
    }
    Trace o = (Trace) other;
    return java.util.Objects.equals(
      this.stack,
      o.stack) && java.util.Objects.equals(
      this.messages,
      o.messages) && java.util.Objects.equals(
      this.other,
      o.other);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(stack) + 3 * java.util.Objects.hashCode(messages) + 5 * java.util.Objects.hashCode(other);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Trace other) {
    int cmp = 0;
    cmp = Integer.compare(
      stack.hashCode(),
      other.stack.hashCode());
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
  
  public Trace withStack(java.util.List<String> stack) {
    return new Trace(stack, messages, other);
  }
  
  public Trace withMessages(java.util.List<String> messages) {
    return new Trace(stack, messages, other);
  }
  
  public Trace withOther(java.util.Map<hydra.core.Name, hydra.core.Term> other) {
    return new Trace(stack, messages, other);
  }
}
