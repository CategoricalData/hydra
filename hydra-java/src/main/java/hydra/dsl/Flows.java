package hydra.dsl;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Trace;
import hydra.FlowException;
import java.util.Collections;


public final class Flows {
  private Flows() {
  }

  private static final Trace EMPTY_TRACE
      = new Trace(Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());

  public static <S, X> X fromFlow(Flow<S, X> flow) throws FlowException {
    FlowState<S, X> wrapper = flow.value.apply(null).apply(EMPTY_TRACE);

    if (!wrapper.value.isPresent()) {
      throw new FlowException(wrapper.trace);
    } else {
      return wrapper.value.get();
    }
  }
}
