// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class WithStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.WithStatement");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_ITEMS = new hydra.core.Name("items");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final Boolean async;
  
  public final java.util.List<hydra.ext.python.syntax.WithItem> items;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public final hydra.ext.python.syntax.Block body;
  
  public WithStatement (Boolean async, java.util.List<hydra.ext.python.syntax.WithItem> items, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment, hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((async));
    java.util.Objects.requireNonNull((items));
    java.util.Objects.requireNonNull((typeComment));
    java.util.Objects.requireNonNull((body));
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
    WithStatement o = (WithStatement) (other);
    return async.equals(o.async) && items.equals(o.items) && typeComment.equals(o.typeComment) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * async.hashCode() + 3 * items.hashCode() + 5 * typeComment.hashCode() + 7 * body.hashCode();
  }
  
  public WithStatement withAsync(Boolean async) {
    java.util.Objects.requireNonNull((async));
    return new WithStatement(async, items, typeComment, body);
  }
  
  public WithStatement withItems(java.util.List<hydra.ext.python.syntax.WithItem> items) {
    java.util.Objects.requireNonNull((items));
    return new WithStatement(async, items, typeComment, body);
  }
  
  public WithStatement withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new WithStatement(async, items, typeComment, body);
  }
  
  public WithStatement withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new WithStatement(async, items, typeComment, body);
  }
}