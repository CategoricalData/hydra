package hydra.tools;

import hydra.compute.Trace;


public class FlowException extends RuntimeException {
  public final Trace trace;

  public FlowException(Trace trace) {
    super(createMessage(trace));
    this.trace = trace;
  }

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
