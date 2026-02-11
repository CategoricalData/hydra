// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a fold operation over a term and compares the result
 */
public class FoldOverTermTestCase implements Serializable, Comparable<FoldOverTermTestCase> {
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
    FoldOverTermTestCase o = (FoldOverTermTestCase) other;
    return java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.traversalOrder,
      o.traversalOrder) && java.util.Objects.equals(
      this.operation,
      o.operation) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input) + 3 * java.util.Objects.hashCode(traversalOrder) + 5 * java.util.Objects.hashCode(operation) + 7 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FoldOverTermTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) traversalOrder).compareTo(other.traversalOrder);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) operation).compareTo(other.operation);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public FoldOverTermTestCase withInput(hydra.core.Term input) {
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
  
  public FoldOverTermTestCase withTraversalOrder(hydra.coders.TraversalOrder traversalOrder) {
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
  
  public FoldOverTermTestCase withOperation(hydra.testing.FoldOperation operation) {
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
  
  public FoldOverTermTestCase withOutput(hydra.core.Term output) {
    return new FoldOverTermTestCase(input, traversalOrder, operation, output);
  }
}
