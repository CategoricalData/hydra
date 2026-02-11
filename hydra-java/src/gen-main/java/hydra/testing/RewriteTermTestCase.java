// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a term rewriter and compares the result
 */
public class RewriteTermTestCase implements Serializable, Comparable<RewriteTermTestCase> {
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
    this.input = input;
    this.rewriter = rewriter;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RewriteTermTestCase)) {
      return false;
    }
    RewriteTermTestCase o = (RewriteTermTestCase) other;
    return java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.rewriter,
      o.rewriter) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input) + 3 * java.util.Objects.hashCode(rewriter) + 5 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RewriteTermTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rewriter).compareTo(other.rewriter);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public RewriteTermTestCase withInput(hydra.core.Term input) {
    return new RewriteTermTestCase(input, rewriter, output);
  }
  
  public RewriteTermTestCase withRewriter(hydra.testing.TermRewriter rewriter) {
    return new RewriteTermTestCase(input, rewriter, output);
  }
  
  public RewriteTermTestCase withOutput(hydra.core.Term output) {
    return new RewriteTermTestCase(input, rewriter, output);
  }
}
