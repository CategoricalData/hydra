// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class WithStatement implements Serializable, Comparable<WithStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.WithStatement");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_ITEMS = new hydra.core.Name("items");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final Boolean async;
  
  public final java.util.List<hydra.ext.python.syntax.WithItem> items;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public final hydra.ext.python.syntax.Block body;
  
  public WithStatement (Boolean async, java.util.List<hydra.ext.python.syntax.WithItem> items, hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment, hydra.ext.python.syntax.Block body) {
    this.async = async;
    this.items = items;
    this.typeComment = typeComment;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WithStatement)) {
      return false;
    }
    WithStatement o = (WithStatement) other;
    return java.util.Objects.equals(
      this.async,
      o.async) && java.util.Objects.equals(
      this.items,
      o.items) && java.util.Objects.equals(
      this.typeComment,
      o.typeComment) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(async) + 3 * java.util.Objects.hashCode(items) + 5 * java.util.Objects.hashCode(typeComment) + 7 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WithStatement other) {
    int cmp = 0;
    cmp = ((Comparable) async).compareTo(other.async);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      items.hashCode(),
      other.items.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeComment.hashCode(),
      other.typeComment.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public WithStatement withAsync(Boolean async) {
    return new WithStatement(async, items, typeComment, body);
  }
  
  public WithStatement withItems(java.util.List<hydra.ext.python.syntax.WithItem> items) {
    return new WithStatement(async, items, typeComment, body);
  }
  
  public WithStatement withTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    return new WithStatement(async, items, typeComment, body);
  }
  
  public WithStatement withBody(hydra.ext.python.syntax.Block body) {
    return new WithStatement(async, items, typeComment, body);
  }
}
