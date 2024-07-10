// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * A container for logging and error information
 */
public class Trace implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.Trace");
  
  public final java.util.List<String> stack;
  
  public final java.util.List<String> messages;
  
  /**
   * A map of string keys to arbitrary terms as values, for application-specific use
   */
  public final java.util.Map<String, hydra.core.Term<hydra.compute.Kv>> other;
  
  public Trace (java.util.List<String> stack, java.util.List<String> messages, java.util.Map<String, hydra.core.Term<hydra.compute.Kv>> other) {
    if (stack == null) {
      throw new IllegalArgumentException("null value for 'stack' argument");
    }
    if (messages == null) {
      throw new IllegalArgumentException("null value for 'messages' argument");
    }
    if (other == null) {
      throw new IllegalArgumentException("null value for 'other' argument");
    }
    this.stack = stack;
    this.messages = messages;
    this.other = other;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Trace)) {
      return false;
    }
    Trace o = (Trace) (other);
    return stack.equals(o.stack) && messages.equals(o.messages) && other.equals(o.other);
  }
  
  @Override
  public int hashCode() {
    return 2 * stack.hashCode() + 3 * messages.hashCode() + 5 * other.hashCode();
  }
  
  public Trace withStack(java.util.List<String> stack) {
    if (stack == null) {
      throw new IllegalArgumentException("null value for 'stack' argument");
    }
    return new Trace(stack, messages, other);
  }
  
  public Trace withMessages(java.util.List<String> messages) {
    if (messages == null) {
      throw new IllegalArgumentException("null value for 'messages' argument");
    }
    return new Trace(stack, messages, other);
  }
  
  public Trace withOther(java.util.Map<String, hydra.core.Term<hydra.compute.Kv>> other) {
    if (other == null) {
      throw new IllegalArgumentException("null value for 'other' argument");
    }
    return new Trace(stack, messages, other);
  }
}