// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A try statement
 */
public class TryStatement implements Serializable, Comparable<TryStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.TryStatement");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name HANDLER = new hydra.core.Name("handler");

  public static final hydra.core.Name FINALIZER = new hydra.core.Name("finalizer");

  public final java.util.List<hydra.javaScript.syntax.Statement> block;

  public final hydra.util.Maybe<hydra.javaScript.syntax.CatchClause> handler;

  public final hydra.util.Maybe<java.util.List<hydra.javaScript.syntax.Statement>> finalizer;

  public TryStatement (java.util.List<hydra.javaScript.syntax.Statement> block, hydra.util.Maybe<hydra.javaScript.syntax.CatchClause> handler, hydra.util.Maybe<java.util.List<hydra.javaScript.syntax.Statement>> finalizer) {
    this.block = block;
    this.handler = handler;
    this.finalizer = finalizer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement)) {
      return false;
    }
    TryStatement o = (TryStatement) other;
    return java.util.Objects.equals(
      this.block,
      o.block) && java.util.Objects.equals(
      this.handler,
      o.handler) && java.util.Objects.equals(
      this.finalizer,
      o.finalizer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(block) + 3 * java.util.Objects.hashCode(handler) + 5 * java.util.Objects.hashCode(finalizer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      block,
      other.block);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      handler,
      other.handler);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      finalizer,
      other.finalizer);
  }

  public TryStatement withBlock(java.util.List<hydra.javaScript.syntax.Statement> block) {
    return new TryStatement(block, handler, finalizer);
  }

  public TryStatement withHandler(hydra.util.Maybe<hydra.javaScript.syntax.CatchClause> handler) {
    return new TryStatement(block, handler, finalizer);
  }

  public TryStatement withFinalizer(hydra.util.Maybe<java.util.List<hydra.javaScript.syntax.Statement>> finalizer) {
    return new TryStatement(block, handler, finalizer);
  }
}
