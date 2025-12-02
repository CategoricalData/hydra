// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a fold operation over a term and compares the result
 */
public class FoldOverTermTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.FoldOverTermTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_ORDER = new hydra.core.Name("traversalOrder");
  
  public static final hydra.core.Name FIELD_NAME_OPERATION = new hydra.core.Name("operation");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to fold over
   */
  public final hydra.core.Term input;
  
  /**
   * The traversal order (pre or post)
   */
  public final hydra.coders.TraversalOrder traversalOrder;
  
  /**
   * The fold operation to apply
   */
  public final hydra.testing.FoldOperation operation;
  
  /**
   * The expected result of the fold
   */
  public final hydra.core.Term output;
  
  public FoldOverTermTestCase (hydra.core.Term input, hydra.coders.TraversalOrder traversalOrder, hydra.testing.FoldOperation operation, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((traversalOrder));
    java.util.Objects.requireNonNull((operation));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.traversalOrder = traversalOrder;
    this.operation = operation;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FoldOverTermTestCase)) {
      return false;
    }
    FoldOverTermTestCase o = (FoldOverTermTestCase) (other);
    return input.equals(o.input) && traversalOrder.equals(o.traversalOrder) && operation.equals(o.operation) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * traversalOrder.hashCode() + 5 * operation.hashCode() + 7 * output.hashCode();
  }
  
  public FoldOverTermTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
  
  public FoldOverTermTestCase withTraversalOrder(hydra.coders.TraversalOrder traversalOrder) {
    java.util.Objects.requireNonNull((traversalOrder));
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
  
  public FoldOverTermTestCase withOperation(hydra.testing.FoldOperation operation) {
    java.util.Objects.requireNonNull((operation));
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
  
  public FoldOverTermTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
}
