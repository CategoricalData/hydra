package hydra.impl.java.dsl;

import hydra.evaluation.Flow;
import hydra.evaluation.FlowWrapper;
import hydra.evaluation.Trace;
import hydra.impl.java.FlowException;
import java.util.Collections;


public final class Flows {
  private Flows() {
  }

  private static final Trace EMPTY_TRACE
      = new Trace(Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());

  public static <S, A> A fromFlow(Flow<S, A> flow) throws FlowException {
    FlowWrapper<S, A> wrapper = flow.value.apply(null).apply(EMPTY_TRACE);

    if (!wrapper.value.isPresent()) {
      throw new FlowException(wrapper.trace);
    } else {
      return wrapper.value.get();
    }
  }
}
