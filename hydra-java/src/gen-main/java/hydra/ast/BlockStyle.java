// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Formatting option for code blocks
 */
public class BlockStyle implements Serializable, Comparable<BlockStyle> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.BlockStyle");
  
  public static final hydra.core.Name FIELD_NAME_INDENT = new hydra.core.Name("indent");
  
  public static final hydra.core.Name FIELD_NAME_NEWLINE_BEFORE_CONTENT = new hydra.core.Name("newlineBeforeContent");
  
  public static final hydra.core.Name FIELD_NAME_NEWLINE_AFTER_CONTENT = new hydra.core.Name("newlineAfterContent");
  
  /**
   * An optional indentation string
   */
  public final hydra.util.Maybe<String> indent;
  
  /**
   * Whether to place a newline before the content
   */
  public final Boolean newlineBeforeContent;
  
  /**
   * Whether to place a newline after the content
   */
  public final Boolean newlineAfterContent;
  
  public BlockStyle (hydra.util.Maybe<String> indent, Boolean newlineBeforeContent, Boolean newlineAfterContent) {
    this.indent = indent;
    this.newlineBeforeContent = newlineBeforeContent;
    this.newlineAfterContent = newlineAfterContent;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlockStyle)) {
      return false;
    }
    BlockStyle o = (BlockStyle) other;
    return java.util.Objects.equals(
      this.indent,
      o.indent) && java.util.Objects.equals(
      this.newlineBeforeContent,
      o.newlineBeforeContent) && java.util.Objects.equals(
      this.newlineAfterContent,
      o.newlineAfterContent);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(indent) + 3 * java.util.Objects.hashCode(newlineBeforeContent) + 5 * java.util.Objects.hashCode(newlineAfterContent);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BlockStyle other) {
    int cmp = 0;
    cmp = Integer.compare(
      indent.hashCode(),
      other.indent.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) newlineBeforeContent).compareTo(other.newlineBeforeContent);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) newlineAfterContent).compareTo(other.newlineAfterContent);
  }
  
  public BlockStyle withIndent(hydra.util.Maybe<String> indent) {
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
  
  public BlockStyle withNewlineBeforeContent(Boolean newlineBeforeContent) {
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
  
  public BlockStyle withNewlineAfterContent(Boolean newlineAfterContent) {
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
}
