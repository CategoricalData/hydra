// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import hydra.util.Maybe;

import java.io.Serializable;

/**
 * Formatting option for code blocks
 */
public class BlockStyle implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.BlockStyle");
  
  public static final hydra.core.Name FIELD_NAME_INDENT = new hydra.core.Name("indent");
  
  public static final hydra.core.Name FIELD_NAME_NEWLINE_BEFORE_CONTENT = new hydra.core.Name("newlineBeforeContent");
  
  public static final hydra.core.Name FIELD_NAME_NEWLINE_AFTER_CONTENT = new hydra.core.Name("newlineAfterContent");
  
  /**
   * An optional indentation string
   */
  public final Maybe<String> indent;
  
  /**
   * Whether to place a newline before the content
   */
  public final Boolean newlineBeforeContent;
  
  /**
   * Whether to place a newline after the content
   */
  public final Boolean newlineAfterContent;
  
  public BlockStyle (Maybe<String> indent, Boolean newlineBeforeContent, Boolean newlineAfterContent) {
    java.util.Objects.requireNonNull((indent));
    java.util.Objects.requireNonNull((newlineBeforeContent));
    java.util.Objects.requireNonNull((newlineAfterContent));
    this.indent = indent;
    this.newlineBeforeContent = newlineBeforeContent;
    this.newlineAfterContent = newlineAfterContent;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlockStyle)) {
      return false;
    }
    BlockStyle o = (BlockStyle) (other);
    return indent.equals(o.indent) && newlineBeforeContent.equals(o.newlineBeforeContent) && newlineAfterContent.equals(o.newlineAfterContent);
  }
  
  @Override
  public int hashCode() {
    return 2 * indent.hashCode() + 3 * newlineBeforeContent.hashCode() + 5 * newlineAfterContent.hashCode();
  }
  
  public BlockStyle withIndent(Maybe<String> indent) {
    java.util.Objects.requireNonNull((indent));
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
  
  public BlockStyle withNewlineBeforeContent(Boolean newlineBeforeContent) {
    java.util.Objects.requireNonNull((newlineBeforeContent));
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
  
  public BlockStyle withNewlineAfterContent(Boolean newlineAfterContent) {
    java.util.Objects.requireNonNull((newlineAfterContent));
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
}
