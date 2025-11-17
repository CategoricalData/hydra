package hydra.tools;

import hydra.compute.Trace;
import java.io.Serializable;


/**
 * Exception thrown when a flow computation fails.
 */
public class FlowException extends RuntimeException implements Serializable {
  /**
   * The trace information from the failed flow.
   */
  public final Trace trace;

  /**
   * Construct a new FlowException with the given trace.
   * @param trace the trace information
   */
  public FlowException(Trace trace) {
    super(createMessage(trace));
    this.trace = trace;
  }

  /**
   * Create an error message from the trace.
   * @param trace the trace information
   * @return the error message
   */
  private static String createMessage(Trace trace) {
    StringBuilder sb = new StringBuilder();
    sb.append("Failure at ");
    boolean first = true;
    for (int i = trace.stack.size() - 1; i >= 0; i--) {
      if (first) {
        first = false;
      } else {
        sb.append(" > ");
      }
      sb.append(trace.stack.get(i));
    }

    if (!trace.messages.isEmpty()) {
      sb.append(": ");
      first = true;
      for (String m : trace.messages) {
        if (first) {
          first = false;
        } else {
          sb.append(", ");
        }
        sb.append(m);
      }
    }

    return sb.toString();
  }
}
