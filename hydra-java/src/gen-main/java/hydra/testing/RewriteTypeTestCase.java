// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a type rewriter and compares the result
 */
public class RewriteTypeTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((rewriter));
    java.util.Objects.requireNonNull((output));
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
    return input.equals(o.input) && rewriter.equals(o.rewriter) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * rewriter.hashCode() + 5 * output.hashCode();
  }
  
  public RewriteTypeTestCase withInput(hydra.core.Type input) {
    java.util.Objects.requireNonNull((input));
    return new RewriteTypeTestCase(input, rewriter, output);
  }
  
  public RewriteTypeTestCase withRewriter(hydra.testing.TypeRewriter rewriter) {
    java.util.Objects.requireNonNull((rewriter));
    return new RewriteTypeTestCase(input, rewriter, output);
  }
  
  public RewriteTypeTestCase withOutput(hydra.core.Type output) {
    java.util.Objects.requireNonNull((output));
    return new RewriteTypeTestCase(input, rewriter, output);
  }
}
