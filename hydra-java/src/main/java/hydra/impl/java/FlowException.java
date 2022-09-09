package hydra.impl.java;

import hydra.evaluation.Trace;
import java.util.List;


public class FlowException extends Exception {
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
      for (List<String> m : trace.messages) {
        if (first) {
          first = false;
        } else {
          sb.append(", ");
        }
        sb.append(m.get(0));
      }
    }

    return sb.toString();
  }
}
