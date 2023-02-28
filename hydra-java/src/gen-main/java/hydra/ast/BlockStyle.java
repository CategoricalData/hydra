package hydra.ast;

/**
 * Formatting option for code blocks
 */
public class BlockStyle {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ast.BlockStyle");
  
  public final Boolean indent;
  
  public final Boolean newlineBeforeContent;
  
  public final Boolean newlineAfterContent;
  
  public BlockStyle (Boolean indent, Boolean newlineBeforeContent, Boolean newlineAfterContent) {
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
  
  public BlockStyle withIndent(Boolean indent) {
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
  
  public BlockStyle withNewlineBeforeContent(Boolean newlineBeforeContent) {
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
  
  public BlockStyle withNewlineAfterContent(Boolean newlineAfterContent) {
    return new BlockStyle(indent, newlineBeforeContent, newlineAfterContent);
  }
}