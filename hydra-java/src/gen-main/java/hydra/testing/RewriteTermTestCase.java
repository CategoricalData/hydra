// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a term rewriter and compares the result
 */
public class RewriteTermTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.RewriteTermTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_REWRITER = new hydra.core.Name("rewriter");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to rewrite
   */
  public final hydra.core.Term input;
  
  /**
   * The rewriter to apply
   */
  public final hydra.testing.TermRewriter rewriter;
  
  /**
   * The expected rewritten term
   */
  public final hydra.core.Term output;
  
  public RewriteTermTestCase (hydra.core.Term input, hydra.testing.TermRewriter rewriter, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((rewriter));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.rewriter = rewriter;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RewriteTermTestCase)) {
      return false;
    }
    RewriteTermTestCase o = (RewriteTermTestCase) (other);
    return input.equals(o.input) && rewriter.equals(o.rewriter) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * rewriter.hashCode() + 5 * output.hashCode();
  }
  
  public RewriteTermTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new RewriteTermTestCase(input, rewriter, output);
  }
  
  public RewriteTermTestCase withRewriter(hydra.testing.TermRewriter rewriter) {
    java.util.Objects.requireNonNull((rewriter));
    return new RewriteTermTestCase(input, rewriter, output);
  }
  
  public RewriteTermTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new RewriteTermTestCase(input, rewriter, output);
  }
}
