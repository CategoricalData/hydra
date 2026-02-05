// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a type rewriter and compares the result
 */
public class RewriteTypeTestCase implements Serializable, Comparable<RewriteTypeTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.RewriteTypeTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_REWRITER = new hydra.core.Name("rewriter");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The type to rewrite
   */
  public final hydra.core.Type input;
  
  /**
   * The rewriter to apply
   */
  public final hydra.testing.TypeRewriter rewriter;
  
  /**
   * The expected rewritten type
   */
  public final hydra.core.Type output;
  
  public RewriteTypeTestCase (hydra.core.Type input, hydra.testing.TypeRewriter rewriter, hydra.core.Type output) {
    this.input = input;
    this.rewriter = rewriter;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RewriteTypeTestCase)) {
      return false;
    }
    RewriteTypeTestCase o = (RewriteTypeTestCase) (other);
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
  public int compareTo(RewriteTypeTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (rewriter)).compareTo(other.rewriter);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public RewriteTypeTestCase withInput(hydra.core.Type input) {
    return new RewriteTypeTestCase(input, rewriter, output);
  }
  
  public RewriteTypeTestCase withRewriter(hydra.testing.TypeRewriter rewriter) {
    return new RewriteTypeTestCase(input, rewriter, output);
  }
  
  public RewriteTypeTestCase withOutput(hydra.core.Type output) {
    return new RewriteTypeTestCase(input, rewriter, output);
  }
}
